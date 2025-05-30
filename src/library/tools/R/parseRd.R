#  File src/library/tools/R/parseRd.R
#  Part of the R package, https://www.R-project.org
#
#  Copyright (C) 1995-2022 The R Core Team
#
#  This program is free software; you can redistribute it and/or modify
#  it under the terms of the GNU General Public License as published by
#  the Free Software Foundation; either version 2 of the License, or
#  (at your option) any later version.
#
#  This program is distributed in the hope that it will be useful,
#  but WITHOUT ANY WARRANTY; without even the implied warranty of
#  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
#  GNU General Public License for more details.
#
#  A copy of the GNU General Public License is available at
#  https://www.R-project.org/Licenses/

parse_Rd <- function(file, srcfile = NULL, encoding = "unknown",
                     verbose = FALSE, fragment = FALSE,
                     warningCalls = TRUE,
                     macros = file.path(R.home("share"), "Rd", "macros", "system.Rd"),
		     permissive = FALSE)
{
    if(is.character(file)) {
        file0 <- file
        if(file == "") {
            file <- stdin()
        } else {
            ## keep.source is FALSE in batch use
            ## encoding issues here, for now use file encoding
            if (missing(srcfile)) ## && isTRUE(getOption("keep.source")))
                srcfile <- srcfile(file)
        }
    } else file0 <- "<connection>"
    lines <- readLines(file, warn = FALSE)
    if(is.character(macros))
    	macros <- initialRdMacros(macros = macros)
    ## remove old-style marking for data, keep line nos
    lines[lines == "\\non_function{}"] <- ""
    ## Extract the encoding if marked in the file:
    ## do this in two steps to minimize warnings in MBCS locales
    ## Note this is required to be on a line by itself,
    ## but some people have preceding whitespace
    enc <- grep("\\encoding{", lines, fixed = TRUE, useBytes=TRUE, value=TRUE)
    enc <- grep("^[[:space:]]*\\\\encoding\\{([^}]*)\\}.*", enc, value = TRUE)
    if(length(enc)) {
        if(length(enc) > 1L)
            warning(file0, ": multiple \\encoding lines, using the first",
                    domain = NA, call. = warningCalls)
        ## keep first one
        enc <- enc[1L]
        enc <- sub("^[[:space:]]*\\\\encoding\\{([^}]*)\\}.*", "\\1", enc)
        if(verbose) message("found encoding ", sQuote(enc), domain = NA)
        encoding <- if(enc %in% c("UTF-8", "utf-8", "utf8")) "UTF-8" else enc
    }
    if (length(encoding) != 1L || encoding == "unknown") encoding <- ""

    ## the internal function must get some sort of srcfile
    if (!inherits(srcfile, "srcfile"))
    	srcfile <- srcfile(file0)
    basename <- basename(srcfile$filename)
    srcfile$encoding <- encoding
    srcfile$Enc <- "UTF-8"

    if (encoding == "ASCII") {
        if (anyNA(iconv(lines, "", "ASCII")))
            stop(file0, ": non-ASCII input and no declared encoding",
                 domain = NA, call. = warningCalls)
    } else {
        validate <- config_val_to_logical(Sys.getenv("_R_CHECK_VALIDATE_UTF8_",
                                                     "FALSE"))
        if (encoding %in% c("unknown", "")  && l10n_info()[["UTF-8"]])
            encoding <- "UTF-8"
        if (encoding %in% c("UTF-8", "UTF8")) { ## package qgraph used UTF8
            valid <- validUTF8(lines)
            if (any(!valid))
                if (validate)
                    stop(sprintf("invalid UTF-8 in file %s", sQuote(file0)),
                         domain = NA, call. = FALSE)
                else
                    warning(sprintf("invalid UTF-8 in file %s", sQuote(file0)),
                            domain = NA, call. = FALSE)
        }
	if (encoding != "UTF-8")
    	    lines <- iconv(lines, encoding, "UTF-8", sub = "byte")
        ## Strip UTF-8 BOM if necessary.
        bytes <- charToRaw(lines[1L])
        if(identical(as.integer(bytes[1L : 3L]),
                     c(0xefL, 0xbbL, 0xbfL)))
            lines[1L] <- rawToChar(bytes[-(1L : 3L)])
    }

    tcon <- file()
    writeLines(lines, tcon, useBytes = TRUE)
    on.exit(close(tcon))

    warndups <- config_val_to_logical(Sys.getenv("_R_WARN_DUPLICATE_RD_MACROS_", "FALSE"))

    result <- if(permissive)
                  ## FIXME:  this should test for a special class of warning rather than testing the
                  ##         message, but those are currently not easily generated from C code.
                  withCallingHandlers(.External2(C_parseRd, tcon, srcfile, "UTF-8",
                                                 verbose, basename, fragment,
                                                 warningCalls, macros, warndups),
		       warning = function(w)
			    if (grepl("unknown macro", conditionMessage(w)))
				tryInvokeRestart("muffleWarning"))
              else
                  .External2(C_parseRd, tcon, srcfile, "UTF-8",
                             verbose, basename, fragment, warningCalls,
                             macros, warndups)
    result <- expandDynamicFlags(result)
    if (permissive)
	permissify(result)
    else
        result
}

print.Rd <- function(x, deparse = FALSE, ...)
{
    cat(as.character.Rd(x, deparse = deparse), sep = "")
    invisible(x)
}

as.character.Rd <- function(x, deparse = FALSE, ...)
{
    ZEROARG <- c("\\cr", "\\dots", "\\ldots", "\\R", "\\tab") # Only these cause trouble when {} is added
    MULTIARG <- c("\\section", "\\subsection", "\\item", "\\enc",
                  "\\method", "\\S3method", "\\S4method", "\\tabular",
                  "\\if", "\\href", "\\ifelse")
    USERMACROS <- c("USERMACRO", "\\newcommand", "\\renewcommand")
    EQN <- c("\\deqn", "\\eqn", "\\figure")
    modes <- c(RLIKE = 1L, LATEXLIKE = 2L, VERBATIM = 3L, INOPTION = 4L, COMMENTMODE = 5L, UNKNOWNMODE = 6L)
    tags  <- c(RCODE = 1L, TEXT      = 2L, VERB     = 3L,                COMMENT     = 5L, UNKNOWN     = 6L)
    state <- c(braceDepth = 0L, inRString = 0L)
    needBraces <- FALSE  # if next character is alphabetic, separate by braces.
    inEqn <- 0L

    pr <- function(x, quoteBraces) {
        tag <- attr(x, "Rd_tag")
        if (is.null(tag) || tag == "LIST") tag <- ""
    	if (is.list(x)) {
    	    savestate <- state
    	    state <<- c(0L, 0L)
    	    needBraces <<- FALSE
    	    if (tag == "Rd") { # a whole file
    	        result <- character()
    	    	for (i in seq_along(x))
                    result <- c(result, pr(x[[i]], quoteBraces))
    	    } else if (startsWith(tag, "#")) {
    	    	if (deparse) {
    	    	    dep <- deparseRdElement(x[[1L]][[1L]],
                                            c(state, modes["LATEXLIKE"],
                                              inEqn,
                                              as.integer(quoteBraces)))
    	    	    result <- c(tag, dep[[1L]])
    	    	} else
    	    	    result <- c(tag, x[[1L]][[1L]])
    	    	for (i in seq_along(x[[2L]]))
                    result <- c(result, pr(x[[2L]][[i]], quoteBraces))
    	    	result <- c(result, "#endif\n")
    	    } else if (tag %in% ZEROARG) {
    	    	result <- tag
    	    	needBraces <<- TRUE
    	    } else if (tag %in% MULTIARG) {
    	    	result <- tag
    	    	for (i in seq_along(x))
                    result <- c(result, pr(x[[i]], quoteBraces))
    	    } else if (tag %in% EQN) {
    	    	result <- tag
    	    	inEqn <<- 1L
    	    	result <- c(result, pr(x[[1L]], quoteBraces))
    	    	inEqn <<- 0L
    	    	if (length(x) > 1L)
    	    	    result <- c(result, pr(x[[2L]], quoteBraces))
    	    } else {
    	    	result <- tag
    	    	if (!is.null(option <- attr(x, "Rd_option")))
    	    	    result <- c(result, "[", pr(option, quoteBraces), "]")
    	    	result <- c(result, "{")
    	    	for (i in seq_along(x))
                    result <- c(result, pr(x[[i]], quoteBraces))
    	    	result <- c(result, "}")
    	    }
    	    if (state[1L])  # If braces didn't match within the list, try again, quoting them
    	    	result <- pr(x, TRUE)
    	    state <<- savestate
    	} else if (tag %in% USERMACROS) {
    	    	result <- c()
    	} else {
    	    if (deparse) {
    		dep <- deparseRdElement(as.character(x), c(state, tags[tag], inEqn, as.integer(quoteBraces)))
    	    	result <- dep[[1L]]
    	    	state <<- dep[[2L]][1L:2L]
    	    } else {
	        if (inherits(x, "Rd"))
		    class(x) <- setdiff(class(x), "Rd") # Avoid infinite recursion from misuse (PR#16448)
    	    	result <- as.character(x)
	    }
    	    if (needBraces) {
    	    	if (grepl("^[[:alpha:]]", result)) result <- c("{}", result)
    	    	needBraces <<- FALSE
    	    }
        }
    	result
    }
    if (is.null(attr(x, "Rd_tag"))) attr(x, "Rd_tag") <- "Rd"
    pr(x, quoteBraces = FALSE)
}

deparseRdElement <- function(element, state)
    .Call(C_deparseRd, element, state)

# Convert unknown tags into text displaying the tag with braces if necessary
# This allows unknown LateX macros to be embedded in the text, and to be just passed
# through.

permissify <- function(Rd)
{
    tags <- RdTags(Rd)
    oldclass <- class(Rd)
    oldsrcref <- utils::getSrcref(Rd)
    oldtag <- attr(Rd, "Rd_tag")
    i <- 0
    while (i < length(tags)) {
        i <- i+1
   	if (tags[i] == "UNKNOWN") {
   	    Rd[[i]] <- tagged(Rd[[i]], "TEXT", utils::getSrcref(Rd[[i]]))
            while (i < length(tags)) {
		if (tags[i+1] == "LIST") {
		    Rd <- c(Rd[seq_len(i)],
                            list(tagged("{", "TEXT", utils::getSrcref(Rd[[i+1]]))),
                            permissify(Rd[[i+1]]),
                            list(tagged("}", "TEXT", utils::getSrcref(Rd[[i+1]]))),
			    Rd[seq_along(Rd)[-seq_len(i+1)]])
		    tags <- RdTags(Rd)
		    i <- i+3
		} else if (tags[i+1] == "TEXT" && grepl("^ *$", Rd[[i+1]]))
		    i <- i + 1
		else
		    break
            }
        } else if (is.recursive(Rd[[i]]))
            Rd[[i]] <- permissify(Rd[[i]])
    }
    class(Rd) <- oldclass
    attr(Rd, "srcref") <- oldsrcref
    attr(Rd, "Rd_tag") <- oldtag
    Rd
}
