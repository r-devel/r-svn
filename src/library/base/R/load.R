#  File src/library/base/R/load.R
#  Part of the R package, https://www.R-project.org
#
#  Copyright (C) 1995-2026 The R Core Team
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

load <- function (file, envir = parent.frame(), verbose = FALSE)
{
    if (is.character(file)) {
        ## files are allowed to be of an earlier format
        ## gzfile can open gzip, bzip2, xz and uncompressed files.
        con <- gzfile(file)
        on.exit(close(con))
        ## Since the connection is not open this opens it in binary mode
        ## and closes it again.
        magic <- readChar(con, 5L, useBytes = TRUE)
	if (!length(magic)) stop("empty (zero-byte) input file")
	if (!grepl("RD[ABX][2-9]\n", magic)) {
            ## a check while we still know the call to load()
            if(grepl("RD[ABX][2-9]\r", magic))
                stop("input has been corrupted, with LF replaced by CR")
            ## Not a version 2 or higher magic number, so try the pre-R-1.4.0 code
            warning(sprintf("file %s has magic number '%s'\n",
                            sQuote(basename(file)),
                            gsub("[\n\r]*", "", magic)),
                    "  ",
                    "Use of save versions prior to 2 is deprecated",
                    domain = NA, call. = FALSE)
            return(.Internal(load(file, envir)))
        }
    } else if (inherits(file, "connection")) {
        con <- if(inherits(file, "gzfile") || inherits(file, "gzcon")) file
               else gzcon(file)
    } else stop("bad 'file' argument")

    if (verbose)
    	cat("Loading objects:\n")

    .Internal(loadFromConn2(con, envir, verbose))
}

save <- function(..., list = character(),
                 file = stop("'file' must be specified"),
                 ascii = FALSE, version = NULL, envir = parent.frame(),
                 compress = isTRUE(!ascii), compression_level,
                 eval.promises = TRUE, precheck = TRUE)
{
    opts <- getOption("save.defaults")
    if (missing(compress) && ! is.null(opts$compress))
        compress <- opts$compress
    if (missing(compression_level) && ! is.null(opts$compression_level))
        compression_level <- opts$compression_level
    if (missing(ascii) && ! is.null(opts$ascii))
        ascii <- opts$ascii
    if (missing(version)) version <- opts$version
    if (!is.null(version) && version < 2)
        warning("Use of save versions prior to 2 is deprecated", domain = NA)

    names <- as.character(substitute(list(...)))[-1L]
    if(missing(list) && !length(names))
	warning("nothing specified to be save()d")
    list <- c(list, names)
    if (!is.null(version) && version == 1)
        .Internal(save(list, file, ascii, version, envir, eval.promises))
    else {
        if (precheck) {
            ## check for existence of objects before opening connection
            ## (and e.g. clobering file)
	    ok <- vapply(list, exists, NA, envir=envir)
            if(!all(ok)) {
                n <- sum(!ok)
                stop(sprintf(ngettext(n,
                                      "object %s not found",
                                      "objects %s not found"
                                      ),
                             paste(sQuote(list[!ok]), collapse = ", ")
                             ), domain = NA)
            }
        }
        ## The connection is created but not opened: saveToConn()
        ## gathers the objects, settles the version they need --
        ## announcing one raised past the default, refusing one that
        ## cannot hold what is being saved -- and only then opens the
        ## connection.  Everything before the open can fail or be
        ## unwound out of, and opening here instead would truncate the
        ## file first -- which save.image(), passing precheck = FALSE,
        ## would have paid for with the previous .RData.  This also
        ## reads each object exactly once, where settling the version
        ## here would read them a second time.
        if (is.character(file)) {
	    if(!nzchar(file))
                stop(gettextf("'%s' must be a non-empty character string", "file"), domain = NA)
	    if(!is.character(compress)) {
		if(!is.logical(compress))
		    stop("'compress' must be logical or character")
		compress <- if(compress) "gzip" else "no compression"
	    }
	    con <- switch(compress,
			  "bzip2" = {
			      if (!missing(compression_level))
				  bzfile(file, compression = compression_level)
			      else bzfile(file)
			  }, "xz" = {
			      if (!missing(compression_level))
				  xzfile(file, compression = compression_level)
			      else xzfile(file, compression = 9)
			  }, "gzip" = {
			      if (!missing(compression_level))
				  gzfile(file, compression = compression_level)
			      else gzfile(file)
			  }, "zstd" = {
			      if (!missing(compression_level))
				  zstdfile(file, compression = compression_level)
			      else zstdfile(file)
			  },
			  "no compression" = file(file),

			  ## otherwise:
			  stop(gettextf("'compress = \"%s\"' is invalid", compress)))
	    on.exit(close(con))
	}
	else if (inherits(file, "connection"))
	    con <- file
	else stop("bad file argument")
	if(isOpen(con) && !ascii && summary(con)$text != "binary")
	    stop("can only save to a binary connection")
	.Internal(saveToConn(list, con, ascii, version, envir, eval.promises))
    }
}

save.image <- function (file = ".RData", version = NULL, ascii = FALSE,
                        compress = !ascii, safe = TRUE)
{
    if (!is.character(file) || length(file) != 1 || file == "")
        stop(gettextf("'%s' must be a non-empty character string", "file"), domain = NA)
    opts <- getOption("save.image.defaults")
    if(is.null(opts)) opts <- getOption("save.defaults")

    if (missing(safe) && ! is.null(opts$safe))
        safe <- opts$safe
    if (missing(ascii) && ! is.null(opts$ascii))
        ascii <- opts$ascii
    if (missing(compress) && ! is.null(opts$compress))
        compress <- opts$compress
    if (missing(version)) version <- opts$version

    if (safe) {
        ## find a temporary file name in the same directory so we can
        ## rename it to the final output file on success
        outfile <- paste0(file, "Tmp")
        i <- 0
        while (file.exists(outfile)) {
            i <- i + 1
            outfile <- paste0(file, "Tmp", i)
        }
    }
    else outfile <- file

    ## save() can now fail before it opens anything -- a version too low
    ## for what is being saved is settled first -- so with safe = FALSE
    ## the destination may still be the caller's previous file, untouched.
    ## Remove only a file this call is responsible for, and only if it is
    ## there: removing one that was never created merely warns.
    preexisting <- !safe && file.exists(outfile)
    on.exit(if (!preexisting && file.exists(outfile)) file.remove(outfile))
    save(list = names(.GlobalEnv), file = outfile,
         version = version, ascii = ascii, compress = compress,
         envir = .GlobalEnv, precheck = FALSE)
    if (safe)
        if (! file.rename(outfile, file)) {
            on.exit()
            stop(gettextf("image could not be renamed and is left in %s",
                          outfile), domain = NA)
        }
    on.exit()
}

sys.load.image <- function(name, quiet)
{
    if (file.exists(name)) {
        load(name, envir = .GlobalEnv)
        sample.kind <- .Internal(RNGkind(NULL, NULL, NULL, NULL))[3L]
        if (sample.kind == 0L)
            warning("non-uniform 'Rounding' sampler used", domain = NA)
        if (! quiet)
	    message("[Previously saved workspace restored]", "\n")
    }
}

sys.save.image <- function(name)
{
    ## Ensure that there is a reasonable chance that we can open a
    ## connection.
    closeAllConnections()
    save.image(name)
}

findPackageEnv <- function(info)
{
    if(info %in% search()) return(as.environment(info))
    message(gettextf("Attempting to load the environment %s", sQuote(info)),
            domain = NA)
    if(require(substr(info, 9L, 1000L), character.only = TRUE, quietly = TRUE))
        return(as.environment(info))
    message("Specified environment not found: using '.GlobalEnv' instead")
    .GlobalEnv
}
