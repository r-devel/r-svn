#  File src/library/base/R/grep.R
#  Part of the R package, https://www.R-project.org
#
#  Copyright (C) 1995-2021 The R Core Team
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


## Q: Why are we using   as.character(.)   all over the place instead of doing that in C ?
## A: These must work for objects which have their own as.character(.) methods *and*
##    as.character() is fast [Primitive]

strsplit <-
function(x, split, fixed = FALSE, perl = FALSE, useBytes = FALSE)
    .Internal(strsplit(x, as.character(split), fixed, perl, useBytes))

grep <-
function(pattern, x, ignore.case = FALSE, perl = FALSE,
         value = FALSE, fixed = FALSE, useBytes = FALSE, invert = FALSE)
{
    pattern <- as.character(pattern)
    if(is.factor(x) && length(levx <- levels(x)) < length(x) &&
       !is.na(pattern[1L]))
    {
        value <- is.character(
            idxna <- suppressWarnings( # not repeat warning
                grep(pattern, NA_character_, ignore.case, perl,
                     value, fixed, useBytes, invert)))
        idx <- logical(length(levx))
        idx[grep(pattern, levx, ignore.case, perl, # value =
                 FALSE, fixed, useBytes, invert)] <- TRUE
        idx <- idx[x]
        if(length(idxna)) idx[is.na(x)] <- TRUE
        idx <- which(idx)
        if(value) {
            idx <- x[idx]
            structure(as.character(idx), names=names(idx))
        } else
            idx
    }
    else {
        ## when value = TRUE we return names
        if(!is.character(x)) x <- structure(as.character(x), names=names(x))
        .Internal(grep(pattern, x, ignore.case, value,
                       perl, fixed, useBytes, invert))
    }
}

grepv <-
function(pattern, x, ignore.case = FALSE, perl = FALSE,
         value = TRUE, fixed = FALSE, useBytes = FALSE, invert = FALSE)
    grep(pattern, x, ignore.case, perl, value, fixed, useBytes, invert)

grepl <-
function(pattern, x, ignore.case = FALSE, perl = FALSE,
         fixed = FALSE, useBytes = FALSE)
{
    if(is.factor(x) && length(levels(x)) < length(x)) {
        out <- grepl(pattern, c(levels(x), NA_character_),
                     ignore.case, perl, fixed, useBytes)
        outna <- out[length(out)]
        out <- out[x]
        out[is.na(x)] <- outna
        out
    } else {
        if(!is.character(x)) x <- as.character(x)
        .Internal(grepl(as.character(pattern), x, ignore.case, FALSE,
                        perl, fixed, useBytes, FALSE))
    }
}

sub <-
function(pattern, replacement, x, ignore.case = FALSE,
         perl = FALSE, fixed = FALSE, useBytes = FALSE)
{
    if(is.factor(x) && length(levels(x)) < length(x)) {
        sub(pattern, replacement, levels(x), ignore.case, perl, fixed, useBytes)[x]
    } else {
        if (!is.character(x)) x <- as.character(x)
        .Internal(sub(as.character(pattern), as.character(replacement), x,
                      ignore.case, perl, fixed, useBytes))
    }
}

gsub <-
function(pattern, replacement, x, ignore.case = FALSE,
         perl = FALSE, fixed = FALSE, useBytes = FALSE)
{
    if(is.factor(x) && length(levels(x)) < length(x)) {
        gsub(pattern, replacement, levels(x), ignore.case, perl, fixed, useBytes)[x]
    } else {
        if (!is.character(x)) x <- as.character(x)
        .Internal(gsub(as.character(pattern), as.character(replacement), x,
                       ignore.case, perl, fixed, useBytes))
    }
}

regexpr <-
function(pattern, text, ignore.case = FALSE, perl = FALSE,
         fixed = FALSE, useBytes = FALSE)
{
    if (is.factor(text) && length(levels(text)) < length(text)) {
        out <- regexpr(pattern, levels(text), ignore.case, perl, fixed, useBytes)
        structure(out[text],
            match.length   = attr(out, "match.length")[text],
            index.type     = attr(out, "index.type"),
            useBytes       = attr(out, "useBytes"),
            capture.start  = attr(out, "capture.start")[ text, , drop = FALSE],
            capture.length = attr(out, "capture.length")[text, , drop = FALSE],
            capture.names  = attr(out, "capture.names"))
    } else {
        if (!is.character(text)) text <- as.character(text)
        .Internal(regexpr(as.character(pattern), text,
                          ignore.case, perl, fixed, useBytes))
    }
}

gregexpr <-
function(pattern, text, ignore.case = FALSE, perl = FALSE,
         fixed = FALSE, useBytes = FALSE) {
    if (is.factor(text) && length(levels(text)) < length(text)) {
        out <- gregexpr(pattern, c(levels(text), NA_character_),
                        ignore.case, perl, fixed, useBytes)
        outna <- out[length(out)]
        out <- out[text]
        out[is.na(text)] <- outna
        out
    } else {
        if (!is.character(text)) text <- as.character(text)
        .Internal(gregexpr(as.character(pattern), text,
                           ignore.case, perl, fixed, useBytes))
    }
}

grepRaw <-
function(pattern, x, offset = 1L, ignore.case = FALSE, value = FALSE,
         fixed = FALSE, all = FALSE, invert = FALSE)
{
    if (!is.raw(pattern)) pattern <- charToRaw(as.character(pattern))
    if (!is.raw(x)) x <- charToRaw(as.character(x))
    .Internal(grepRaw(pattern, x, offset, ignore.case, fixed, value, all, invert))
}

regexec <-
function(pattern, text, ignore.case = FALSE, perl = FALSE,
         fixed = FALSE, useBytes = FALSE)
{
    if (is.factor(text) && length(levels(text)) < length(text)) {
        out <- regexec(pattern, c(levels(text), NA_character_),
                       ignore.case, perl, fixed, useBytes)
        outna <- out[length(out)]
        out <- out[text]
        out[is.na(text)] <- outna
        return(out)
    }

    if (!is.character(text)) text <- as.character(text)
    if(!perl || fixed) {
        if(perl) warning(
              gettextf("argument '%s' will be ignored", "perl = TRUE"),
              domain = NA)
        return(.Internal(regexec(as.character(pattern), text, ignore.case, fixed,
                                 useBytes)))
    }

    ## For perl = TRUE, re-use regexpr(perl = TRUE) which always
    ## captures subexpressions.

    m <- regexpr(pattern, text,
                 ignore.case = ignore.case, useBytes = useBytes,
                 perl = TRUE)
    nm <- attr(m, 'capture.names')
    nm <- if(any(nzchar(nm))) c("", nm)
    match_data_from_pos_and_len <- function(pos, len, nm=NULL) {
        attr(pos, "match.length") <- len
        names(pos) <- nm
        pos
    }
    y <- vector("list", length(text))
    y[is.na(m)] <- list(match_data_from_pos_and_len(NA_integer_, NA_integer_))
    ind <- !is.na(m) & (m == -1L)
    if(any(ind)) {
        y[ind] <- list(match_data_from_pos_and_len(-1L, -1L))
    }
    ind <- !is.na(m) & !ind
    if(any(ind)) {
        pos <- cbind(m[ind],
                     attr(m, "capture.start")[ind, , drop = FALSE])
        len <- cbind(attr(m, "match.length")[ind],
                     attr(m, "capture.length")[ind, , drop = FALSE])
        y[ind] <- Map(match_data_from_pos_and_len,
                      split(pos, row(pos)),
                      split(len, row(len)),
                      list(nm))
    }
    if(identical(attr(m, "useBytes"), TRUE))
        y <- lapply(y, `attr<-`, "useBytes", TRUE)
    lapply(y, `attr<-`, "index.type", attr(m, "index.type"))
}

gregexec <- function(pattern, text, ignore.case = FALSE, perl = FALSE,
                     fixed = FALSE, useBytes = FALSE) {
    if(is.factor(text) && length(levels(text)) < length(text)) {
        out <- gregexec(pattern, c(levels(text), NA_character_),
                        ignore.case, perl, fixed, useBytes)
        outna <- out[length(out)]
        out <- out[text]
        out[is.na(text)] <- outna
        return(out)
    }

    dat <- gregexpr(pattern = pattern, text=text, ignore.case = ignore.case,
                    fixed = fixed, useBytes = useBytes, perl = perl)
    if(perl && !fixed) {
        ## Perl generates match data, so use that
        capt.attr <- c('capture.start', 'capture.length', 'capture.names')
        process <- function(x) {
            if(anyNA(x) || any(x < 0)) y <- x
            else {
                ## Interleave matches with captures
                y <- t(cbind(x, attr(x, "capture.start")))
                attributes(y)[names(attributes(x))] <- attributes(x)
                ml <- t(cbind(attr(x, "match.length"), attr(x, "capture.length")))
                nm <- attr(x, 'capture.names')
                ## Remove empty names that `gregexpr` returns
                dimnames(ml) <- dimnames(y) <-
                    if(any(nzchar(nm))) list(c("", nm), NULL)
                attr(y, "match.length") <- ml
                y
            }
            attributes(y)[capt.attr] <- NULL
            y
        }
        lapply(dat, process)
    } else {
        ## For TRE or fixed we must compute the match data ourselves
        m1 <- lapply(regmatches(text, dat),
                     regexec, pattern = pattern, ignore.case = ignore.case,
                     perl = perl, fixed = fixed, useBytes = useBytes)
        mlen <- lengths(m1)
        res <- vector("list", length(m1))
        im <- mlen > 0
        res[!im] <- dat[!im]   # -1, NA
        res[im] <- Map(
            function(outer, inner) {
                tmp <- do.call(cbind, inner)
                attributes(tmp)[names(attributes(inner))] <- attributes(inner)
                attr(tmp, 'match.length') <-
                    do.call(cbind, lapply(inner, `attr`, 'match.length'))
                # useBytes/index.type should be same for all so use outer vals
                attr(tmp, 'useBytes') <- attr(outer, 'useBytes')
                attr(tmp, 'index.type') <- attr(outer, 'index.type')
                tmp + rep(outer - 1L, each = nrow(tmp))
            },
            dat[im],
            m1[im]
        )
        res
    }
}

agrep <-
function(pattern, x, max.distance = 0.1, costs = NULL,
         ignore.case = FALSE, value = FALSE, fixed = TRUE,
         useBytes = FALSE)
{
    pattern <- as.character(pattern)
    if(!is.character(x)) x <- as.character(x)

    ## TRE needs integer costs: coerce here for simplicity.
    costs <- as.integer(.amatch_costs(costs))
    bounds <- .amatch_bounds(max.distance)

    .Internal(agrep(pattern, x, ignore.case, value, costs, bounds,
                    useBytes, fixed))
}

agrepl <-
function(pattern, x, max.distance = 0.1, costs = NULL,
         ignore.case = FALSE, fixed = TRUE, useBytes = FALSE)
{
    pattern <- as.character(pattern)
    if(!is.character(x)) x <- as.character(x)

    ## TRE needs integer costs: coerce here for simplicity.
    costs <- as.integer(.amatch_costs(costs))
    bounds <- .amatch_bounds(max.distance)

    .Internal(agrepl(pattern, x, ignore.case, FALSE, costs, bounds,
                     useBytes, fixed))
}

.amatch_bounds <-
function(x = 0.1)
{
    ## Expand max match distance argument for agrep() et al into bounds
    ## for the TRE regaparams struct.

    ## Note that TRE allows for possibly different (integer) costs for
    ## insertions, deletions and substitutions, and allows for specifying
    ## separate bounds for these numbers as well as the total number of
    ## "errors" (transformations) and the total cost.
    ##
    ## When using unit costs (and older versions of agrep() did not
    ## allow otherwise), the total number of errors is the same as the
    ## total cost, and bounds on the total number of errors imply the
    ## same bounds for the individual transformation counts.  This no
    ## longer holds when using possibly different costs.
    ##
    ## See ? agrep for details on handling the match distance argument.
    ##
    ## Older versions of agrep() expanded fractions (of the pattern
    ## length) in R code: but as the C code determines whether matching
    ## used bytes or characters, only the C code can determine the
    ## pattern length and hence expand fractions.
    ##
    ## Unspecified bounds are taken as NA_real_, and set to INT_MAX by
    ## the C code.

    if(!is.list(x)) {
        ## Sanity checks.
        if(!is.numeric(x) || (x < 0))
            stop("match distance components must be non-negative")
        bounds <- c(as.double(x), rep.int(NA_real_, 4L))
    } else {
        table <-
            c("cost", "insertions", "deletions", "substitutions", "all")
        ## Partial matching.
        pos <- pmatch(names(x), table)
        if(anyNA(pos)) {
            warning("unknown match distance components ignored")
            x <- x[!is.na(pos)]
        }
        names(x) <- table[pos]
        ## Sanity checks.
        x <- unlist(x)
        if(!all(is.numeric(x)) || any(x < 0))
            stop("match distance components must be non-negative")
        ## Defaults.
        if(!is.na(x["cost"])) {
            bounds <- rep.int(NA_real_, 5L)
        } else {
            ## If 'cost' is missing: if 'all' is missing it is set to
            ## 0.1, and the other transformation number bounds default
            ## to 'all'.
            if(is.na(x["all"]))
                x["all"] <- 0.1
            bounds <- c(NA_real_, rep.int(x["all"], 4L))
        }
        names(bounds) <- table
        bounds[names(x)] <- x
    }

    bounds
}

.amatch_costs <-
function(x = NULL)
{
    costs <- c(insertions = 1, deletions = 1, substitutions = 1)
    if(!is.null(x)) {
        x <- as.list(x)
        ## Partial matching.
        pos <- pmatch(names(x), names(costs))
        if(anyNA(pos)) {
            warning("unknown cost components ignored")
            x <- x[!is.na(pos)]
        }
        ## Sanity checks.
        x <- unlist(x)
        if(!all(is.numeric(x)) || any(x < 0))
            stop("cost components must be non-negative")
        costs[pos] <- x
    }

    costs
}

regmatches <-
function(x, m, invert = FALSE)
{
    if(length(x) != length(m))
        stop(gettextf("%s and %s must have the same length",
                      sQuote("x"), sQuote("m")),
             domain = NA)

    ili <- is.list(m)

    ## Handle useBytes/encoding issues.
    ## Match positions from regexpr(), gregexpr() and regexec() are in
    ## characters unless 'useBytes = TRUE' was given, now recorded via
    ## the 'index.type' attribute (in addition to the 'useBytes' one
    ## being TRUE when 'useBytes = TRUE' was given *or* all character
    ## string involved were ASCII).
    ## To convince substring() and nchar() used below accordingly that
    ## match data positions are in bytes, we set the input encoding to
    ## "bytes" for the former and call the latter with 'type = "bytes"'.
    itype <- "chars"
    useBytes <- if(ili)
        any(unlist(lapply(m, attr, "index.type")) == "bytes")
    else
        any(attr(m, "index.type") == "bytes")
    if(useBytes) {
        itype <- Encoding(x) <- "bytes"
    }

    ## For NA matches (from matching a non-NA pattern on an NA string),
    ## direct matches give nothing and inverse matches give NA (as
    ## nothing was matched).

    if(!ili && isFALSE(invert)) {
        so <- m[ind <- (!is.na(m) & (m > -1L))]
        eo <- so + attr(m, "match.length")[ind] - 1L
        return(substring(x[ind], so, eo))
    }

    y <- if(is.na(invert)) {
        Map(function(u, so, ml) {
                if((n <- length(so)) == 1L) {
                    if(is.na(so) )
                        return(NA_character_) # Or u ...
                    else if(so == -1L)
                        return(u)
                }
                eo <- so + ml - 1L
                if(n > 1L) {
                    ## regexec() could give overlapping matches.
                    ## Matches are non-overlapping iff
                    ##   eo[i] < so[i + 1], i = 1, ..., n - 1.
                    if(any(eo[-n] >= so[-1L]))
                        stop(gettextf("need non-overlapping matches for %s",
                                      sQuote("invert = NA")),
                             domain = NA)
                }
                beg <- c(1L, c(rbind(so, eo + 1L)))
                end <- c(c(rbind(so - 1L, eo)), nchar(u, itype))
                substring(u, beg, end)
            },
            x, m,
            if(ili)
                lapply(m, attr, "match.length")
            else
                attr(m, "match.length"),
            USE.NAMES = FALSE)
    } else if(invert) {
        Map(function(u, so, ml) {
                if((n <- length(so)) == 1L) {
                    if(is.na(so) )
                        return(NA_character_) # Or u ...
                    else if(so == -1L)
                        return(u)
                }
                beg <- if(n > 1L) {
                    ## See above.
                    eo <- so + ml - 1L
                    if(any(eo[-n] >= so[-1L]))
                        stop(gettextf("need non-overlapping matches for %s",
                                      sQuote("invert = TRUE")),
                             domain = NA)
                    c(1L, eo + 1L)
                } else {
                    c(1L, so + ml)
                }
                end <- c(so - 1L, nchar(u, itype))
                substring(u, beg, end)
            },
            x, m,
            if(ili)
                lapply(m, attr, "match.length")
            else
                attr(m, "match.length"),
            USE.NAMES = FALSE)
    } else {
        Map(function(u, so, ml) {
                if(length(so) == 1L) {
                    if(is.na(so) || (so == -1L))
                        return(character())
                }
                tmp <- substring(u, so, so + ml - 1L)
                ## Copy dims and dimnames from gregexec, and names
                ## from regexec.  These may appear with perl=TRUE.
                dim(tmp) <- dim(so)
                dimnames(tmp) <- dimnames(so)
                names(tmp) <- names(so)
                tmp
            },
            x, m,
            lapply(m, attr, "match.length"),
            USE.NAMES = FALSE)
    }

    names(y) <- names(x)
    y
}

## Suppose matching partitions a string as
##   n0 m1 n1 ... mk nk
## where the m and n substrings are the matched and non-matched parts,
## respectively, and n0 and/or nk can be empty.
## (regexec() can give overlapping matches, in which case extracting
## inverted matches or replacing cannot work.)
## For list match data, k can be any non-negative integer.
## Extraction and replacement straightforwardly work on the m or n
## sequences, depending on whether invert is FALSE or TRUE.
## For vector match data from regexpr(), k can be 0 or 1.
## If k = 0 (no match):
##                    invert
##               FALSE      TRUE
##   extract      drop       n0
##   replace       n0        r0
## If k = 1:
##                    invert
##               FALSE      TRUE
##   extract       m1     c(n0, n1)
##   replace    n0 r1 n1  r0 m1 r1

`regmatches<-` <-
function(x, m, invert = FALSE, value)
{
    if(!length(x)) return(x)

    ili <- is.list(m)

    if(!ili && invert && any(m == -1L)) {
        ## regmatches() drops empty matches for vector match data if
        ## invert is FALSE (see above): we need to work around this when
        ## replacing non-matches (PR #15723).
        y <- rep_len(list(character()), length(x))
        y[m > -1L] <- as.list(regmatches(x, m, FALSE))
    } else {
        y <- regmatches(x, m, !invert)
    }

    ## <FIXME>
    ## It might be simpler to try reducing the vector case to the list
    ## case, transforming m and value as needed,
    ## </FIXME>

    if(!ili && !invert) {
        ## For non-list m and invert = FALSE, we need a character vector
        ## of replacement values with length the number of matched
        ## elements.
        value <- as.character(value)
        if(anyNA(value))
            stop("missing replacement values are not allowed")
        ## Entries for matched elements have length 2.
        pos <- which(lengths(y) == 2L)
        np <- length(pos)
        nv <- length(value)
        if(np != nv) {
            if(!nv)
                stop("must have replacement values for matches")
            value <- rep_len(value, np)
        }
        y <- y[pos]
        x[pos] <- paste0(vapply(y, `[`, "", 1L),
                         value,
                         vapply(y, `[`, "", 2L))
        return(x)
    }

    ## We need a list of character vectors without missings, which has
    ## the same length as x.
    value <- lapply(value, as.character)
    if(anyNA(value)) # {recursively!}
        stop("missing replacement values are not allowed")
    if(!length(value))
        stop("value does not provide any replacement values")
    value <- rep_len(value, length(x))

    y <- if(invert) {
        ## Replace non-matches.
        ## An element of x with k matches has a corresponding y element
        ## of length k, and needs k + 1 replacement values.
        Map(function(u, v) {
            nu <- length(u)
            nv <- length(v)
            if(nv != (nu + 1L)) {
                if(!nv)
                    stop("must have replacements for non-matches")
                v <- rep_len(v, nu + 1L)
            }
            paste0(v, c(u, ""), collapse = "")
        },
            y, value, USE.NAMES = FALSE)
    } else {
        ## Replace matches.
        ## An element of x with k matches has a corresponding y element
        ## of length k + 1, and needs k replacement values.
        Map(function(u, v) {
            nu <- length(u)
            nv <- length(v)
            if(nv != (nu - 1L)) {
                if(!nv)
                    stop("must have replacements for matches")
                v <- rep_len(v, nu - 1L)
            }
            paste0(u, c(v, ""), collapse = "")
        },
            y, value, USE.NAMES = FALSE)
    }

    y <- unlist(y)
    names(y) <- names(x)

    y
}

pcre_config <- function() .Internal(pcre_config())
