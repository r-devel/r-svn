#  File src/library/utils/R/tar.R
#  Part of the R package, https://www.R-project.org
#
#  Copyright (C) 1995-2025 The R Core Team
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

untar <- function(tarfile, files = NULL, list = FALSE, exdir = ".",
                  compressed = NA, extras = NULL, verbose = FALSE,
                  restore_times = TRUE,
                  support_old_tars = Sys.getenv("R_SUPPORT_OLD_TARS", FALSE),
                  tar = Sys.getenv("TAR"))
{
    allow_all <- isTRUE(grepl("-P", extras))
    if (inherits(tarfile, "connection") || identical(tar, "internal")) {
        if (!missing(compressed))
            warning("argument 'compressed' is ignored for the internal method")
        return(untar2(tarfile, files, list, exdir, restore_times, allow_all))
    }

    if (!(is.character(tarfile) && length(tarfile) == 1L))
        stop("invalid 'tarfile' argument")
    tarfile <- path.expand(tarfile)
    support_old_tars <- isTRUE(as.logical(support_old_tars))

    TAR <- tar
    if (!nzchar(TAR) && .Platform$OS.type == "windows" &&
        nzchar(Sys.which("tar.exe"))) TAR <- "tar.exe"
    if (!nzchar(TAR) || TAR == "internal")
        return(untar2(tarfile, files, list, exdir, restore_times, allow_all))

    ## The ability of external tar commands to handle compressed tarfiles
    ## automagically varies and is poorly documented.
    ## E.g. macOS used to say its tar handles bzip2 but did not mention xz nor lzma.
    ##
    ## But as all commonly-used tars do (some commercial Unix do not,
    ## but GNU tar is commonly used there).
    ##
    ## OTOH some (e.g. macOS) need external commands which may not be present.
    cflag <- ""
    if (!missing(compressed))
        warning("untar(compressed=) is deprecated", call. = FALSE, domain = NA)
    if (is.character(compressed)) {
        cflag <- switch(match.arg(compressed, c("gzip", "bzip2", "xz", "zstd")),
                        "gzip" = "z", "bzip2" = "j", "xz" = "J",
                        "zstd" = "-zstd")
    } else if (is.logical(compressed)) {
        if (is.na(compressed) && support_old_tars) {
            magic <- readBin(tarfile, "raw", n = 6L)
            if(all(magic[1:2] == c(0x1f, 0x8b))) cflag <- "z"
            else if(all(magic[1:2] == c(0x1f, 0x9d))) cflag <- "z" # compress
            else if(rawToChar(magic[1:3]) == "BZh") cflag <- "j"
            ## (https://tukaani.org/xz/xz-file-format.txt)
            else if(all(magic[1:6] == c(0xFD, 0x37, 0x7A, 0x58, 0x5A, 0x00))) cflag <- "J"
            else if(all(magic[1:4] == c(0x28, 0xb5, 0x2f, 0xfd))) cflag <- "-zstd"
        } else if (isTRUE(compressed)) cflag <- "z"
    } else stop("'compressed' must be logical or character")

    if (support_old_tars) {
        if (cflag == "z")
            if (nzchar(ZIP <- Sys.getenv("R_GZIPCMD"))) {
                TAR <- paste(ZIP, "-dc", shQuote(tarfile), "|", TAR)
                tarfile <- "-"
                cflag <- ""
            } else stop(sprintf("No %s command found", sQuote("gzip")))
        if (cflag == "j")
            if (nzchar(ZIP <- Sys.getenv("R_BZIPCMD"))) {
                TAR <- paste(ZIP,  "-dc", shQuote(tarfile), "|", TAR)
                tarfile <- "-"
                cflag <- ""
            } else stop(sprintf("No %s command found", sQuote("bzip2")))
        if (cflag == "J")
            if (nzchar(Sys.which("xz"))) {
                TAR <- paste("xz -dc", shQuote(tarfile), "|", TAR)
                tarfile <- "-"
                cflag <- ""
            } else stop(sprintf("No %s command found", sQuote("xz")))
        if (cflag == "-zstd")
            if (nzchar(Sys.which("zstd"))) {
                TAR <- paste("zstd -dc", shQuote(tarfile), "|", TAR)
                tarfile <- "-"
                cflag <- ""
            } else stop(sprintf("No %s command found", sQuote("zstd")))
    }

    if (list) {
        ## TAR might be a command+flags or piped commands, so don't quote it
        cmd <- paste0(TAR, " -", cflag, "tf ", shQuote(tarfile))
        if (length(extras)) cmd <- paste(cmd, extras, collapse = " ")
        if (verbose) message("untar: using cmd = ", sQuote(cmd), domain = NA)
        system(cmd, intern = TRUE)
    } else {
        if (!restore_times) cflag <- paste0(cflag, "m")
        cmd <- paste0(TAR, " -", cflag, "xf ", shQuote(tarfile))
        if (!missing(exdir)) {
            if (!dir.exists(exdir)) {
                if(!dir.create(exdir, showWarnings = TRUE, recursive = TRUE))
                    stop(gettextf("failed to create directory %s", sQuote(exdir)),
                         domain = NA)
            }
            cmd <- if(.Platform$OS.type == "windows")
                ## some versions of tar.exe need / here
                paste(cmd, "-C", shQuote(gsub("\\", "/", exdir, fixed=TRUE)))
            else
                paste(cmd, "-C", shQuote(exdir))
        }
        if (length(extras)) cmd <- paste(cmd, extras, collapse = " ")
        if (length(files))
            cmd <- paste(cmd, paste(shQuote(files), collapse = " "))
        if (verbose) message("untar: using cmd = ", sQuote(cmd), domain = NA)
        res <- system(cmd)
        if (res) warning(sQuote(cmd), " returned error code ", res,
                         domain = NA)
        invisible(res)
    }
}

##' R's "internal" untar() -- called from untar(), *not* exported
untar2 <- function(tarfile, files = NULL, list = FALSE, exdir = ".",
                   restore_times = TRUE, allow_all_paths = FALSE)
{
    ## might be used with len = 12, so result of more than max int
    getOctD <- function(block, offset, len)
    {
        x <- 0.0
        for(i in offset + seq_len(len)) {
            z <- block[i]
            if(!as.integer(z)) break; # terminate on nul
            switch(rawToChar(z),
                   " " = {},
                   "0"=,"1"=,"2"=,"3"=,"4"=,"5"=,"6"=,"7"=
                   {x <- 8*x + (as.integer(z)-48L)},
                   stop("invalid octal digit")
                   )
        }
        x
    }
    getOct <- function(x, offset, len)
        as.integer(getOctD(x, offset, len))
    checkPath <- function(path) {
        if (.Platform$OS.type == "windows") {
            path <- gsub("\\\\", "/", path)
            while(grepl("^/", path) || grepl("^[a-zA-Z]:", path)) {
                if (grepl("^/", path)) {
                    warning(gettextf("removing leading '/' from '%s'", path),
                            call. = FALSE, domain = NA)
                    path <- sub("^/+", "", path)
                }
                if (grepl("^[a-zA-Z]:", path)) {
                    drv <- sub("^([a-zA-Z]:).*", "\\1", path)
                    warning(sprintf("removing drive '%s'", drv))
                    path <- sub("^[a-zA-Z]:", "", path)
                }
            }
        } else {
            if (grepl("^/", path)) {
                warning(gettextf("removing leading '/' from '%s'", path),
                        call. = FALSE, domain = NA)
                path <- sub("^/+", "", path)
            }
        }
        if (grepl("^~", path))
            stop("path starts with '~'")
        parts <- strsplit(path, "/")[[1]]
        if (".." %in% parts)
            stop("path contains '..'")
        if (length(parts) == 0)
            return(".")
        p <- ""
        for(el in parts) {
            p <- if (nzchar(p)) file.path(p, el) else el
            if(isTRUE(nzchar(Sys.readlink(p), keepNA = TRUE)))
                stop("cannot extract through symlink")
        }
        p
    }
    mydir.create <- function(path, ..., .checkPath = TRUE) {
        ## for Windows' sake
        path <- sub("[\\/]$", "", path)
        ## address path traversal vulnerability  (PR17853):
        if (.checkPath)
            path <- checkPath(path)
        if(!dir.exists(path) &&
           !dir.create(path, showWarnings = TRUE, recursive = TRUE, ...))
            stop(gettextf("failed to create directory %s", sQuote(path)),
                 domain = NA)
        path
    }

    warn1 <- character()

    ## A tar file is a set of 512 byte records,
    ## a header record followed by file contents (zero-padded).
    ## See https://en.wikipedia.org/wiki/Tar_%28file_format%29
    if(is.character(tarfile) && length(tarfile) == 1L) {
        con <- gzfile(path.expand(tarfile), "rb") # reads compressed formats
        on.exit(close(con))
    } else if(inherits(tarfile, "connection")) {
        con <- tarfile
        ## solves file("foo.tar.gz") automagically, but unneeded for "*.tar":
        ## if(summary(con)$class != "gzcon") con <- gzcon(con)
        ## ==> prefer the gzfile() error message below
    }
    else stop("'tarfile' must be a character string or a connection")
    ## now 'con' is a connection
    if (exdir != ".") {
        mydir.create(exdir, .checkPath = FALSE)
        od <- setwd(exdir)
        on.exit(setwd(od), add = TRUE)
    }
    contents <- character()
    llink <- lname <- lsize <- NULL
    repeat{
        block <- readBin(con, "raw", n = 512L)
        if(!length(block)) break
        if(length(block) < 512L)
            stop(if(is.character(tarfile)) "incomplete block on file"
		 else "incomplete block: rather use gzfile(.) created connection?")
        if(all(block == 0)) break
        ## This should be non-empty, but whole name could be in prefix
        w <- which(block[1:100] > 0)
        ns <- if(length(w)) max(w) else 0
        name <- rawToChar(block[seq_len(ns)])
        magic <- rawToChar(block[258:262])
        if ((magic == "ustar") && block[346L] > 0) {
            ns <- max(which(block[346:500] > 0))
            prefix <- rawToChar(block[345L+seq_len(ns)])
            name <- file.path(prefix, name)
            ns <- nchar(name, "b")
        }
        if (ns <= 0) stop("invalid name field in tarball")
        ## mode zero-padded 8 bytes (including nul) at 101
        ## Aargh: bsdtar has this one incorrectly with 6 bytes+space
        mode <- as.octmode(getOct(block, 100, 8))
        size <- getOctD(block, 124, 12)
        ts <- getOctD(block, 136, 12)
        ft <- as.POSIXct(as.numeric(ts), origin = "1970-01-01", tz = "UTC")
        csum <- getOct(block, 148, 8)
        block[149:156] <- charToRaw(" ")
        xx <- as.integer(block)
        checksum <- sum(xx) %% 2^24 # 6 bytes
        if(csum != checksum) {
            ## try it with signed bytes.
            checksum <- sum(ifelse(xx > 127L, xx - 128L, xx)) %% 2^24 # 6 bytes
            if(csum != checksum)
                warning(gettextf("checksum error for entry '%s'", name),
                        domain = NA)
        }
        type <- block[157L]
        ctype <- rawToChar(type)
#        message(sprintf("%s, %d: '%s'", ctype, size, name))
        if(type %in% c(0L, 7L) || ctype == "0") {
            ## regular or high-performance file
            if(!is.null(lname)) {name <- lname; lname <- NULL}
            if(!is.null(lsize)) {size <- lsize; lsize <- NULL}
            contents <- c(contents, name)
            remain <- size
            dothis <- !list
            if(dothis && length(files)) dothis <- name %in% files
            if(dothis) {
                dname <- mydir.create(dirname(name))
                fname <- file.path(dname, basename(name))
                unlink(fname)
                out <- file(fname, "wb")
            }
            for(i in seq_len(ceiling(size/512L))) {
                block <- readBin(con, "raw", n = 512L)
                if(length(block) < 512L)
                    stop("incomplete block on file")
                if (dothis) {
                    writeBin(block[seq_len(min(512L, remain))], out)
                    remain <- remain - 512L
                }
            }
            if(dothis) {
                close(out)
                Sys.chmod(fname, mode, FALSE) # override umask
                if(restore_times) Sys.setFileTime(fname, ft)
            }
        } else if(ctype %in% c("1", "2")) {
            ## hard and symbolic links
            contents <- c(contents, name)
            ns <- max(which(block[158:257] > 0))
            name2 <- rawToChar(block[157L + seq_len(ns)])
            if(!is.null(lname)) {name <- lname; lname <- NULL}
            if(!is.null(llink)) {name2 <- llink; llink <- NULL}
            if(!list) {
                if(ctype == "1") {
                    dname <- mydir.create(dirname(name))
                    fname <- file.path(dname, basename(name))
                    unlink(fname)
                    if (!file.link(name2, name)) { # will give a warning
                        ## link failed, so try a file copy
                        if(file.copy(name2, fname))
                            warn1 <- c(warn1, "restoring hard link as a file copy")
                        else
                            warning(gettextf("failed to copy %s to %s", sQuote(name2), sQuote(fname)), domain = NA)
                    }
                } else {
                    if(.Platform$OS.type == "windows") {
                        dname <- mydir.create(dirname(name))
                        fname <- file.path(dname, basename(name))
                        unlink(fname)
                        from <- file.path(dname, name2)
                        if (dir.exists(from)) {
                            tmpd <- tempfile(pattern = "untar_", tmpdir = dname)
                            dir.create(tmpd)
                            if (!file.copy(from, tmpd, recursive = TRUE) ||
                                !file.rename(file.path(tmpd, basename(name2)), fname) ||
                                !unlink(tmpd, recursive = TRUE))
                                warning(gettextf("failed to copy %s to %s", sQuote(from), sQuote(fname)), domain = NA)
                            else
                                warn1 <- c(warn1, "restoring symbolic link as a file copy")
                        } else if (!file.exists(from)) {
                            warning(gettextf("cannot restore symbolic link from %s to %s as a file copy, because the source doesn't exist; try extracting again?",
                                             sQuote(from), sQuote(fname)), domain = NA)
                        } else {
                            if (!file.copy(from, fname))
                                warning(gettextf("failed to copy %s to %s", sQuote(from), sQuote(fname)), domain = NA)
                            else
                                warn1 <- c(warn1, "restoring symbolic link as a file copy")
                       }
                   } else {
                       dname <- mydir.create(dirname(name))
                       od0 <- setwd(dname)
                       nm <- basename(name)
                       unlink(nm)
                       if(!file.symlink(name2, nm)) { # will give a warning
                        ## so try a file copy: will not work for links to dirs
                        if (file.copy(name2, nm))
                            warn1 <- c(warn1, "restoring symbolic link as a file copy")
                           else
                               warning(gettextf("failed to copy %s to %s", sQuote(from), sQuote(name)), domain = NA)
                       }
                       setwd(od0)
                   }
                }
            }
        } else if(ctype %in% c("3", "4")) {
            ## 3 and 4 are devices
            warn1 <- c(warn1, "skipping devices")
        } else if(ctype == "5") {
            ## directory
            contents <- c(contents, name)
            if(!list) {
                dname <- mydir.create(dirname(name))
                fname <- file.path(dname, basename(name))
                unlink(fname)
                if(!dir.exists(fname) && !dir.create(fname, mode = mode))
                    stop(gettextf("failed to create directory %s", sQuote(fname)))
                ## no point in setting time, as dir will be populated later.
            }
        } else if(ctype == "6") {
            ## 6 is a fifo
            warn1 <- c(warn1, "skipping fifos")
       } else if(ctype %in% c("L", "K")) {
            ## These are GNU extensions that are widely supported
            ## They use one or more blocks to store the name of
            ## a file or link or of a link target.
            name_size <- 512L * ceiling(size/512L)
            block <- readBin(con, "raw", n = name_size)
            if(length(block) < name_size)
                stop("incomplete block on file")
            ns <- max(which(block > 0)) # size on file may or may not include final nul
            if(ctype == "L")
                lname <- rawToChar(block[seq_len(ns)])
            else
                llink <- rawToChar(block[seq_len(ns)])
        } else if(ctype == "x") {
            ## pax headers misused by bsdtar.
            isUTF8 <- FALSE
            warn1 <- c(warn1, "using pax extended headers")
            info <- readBin(con, "raw", n = 512L*ceiling(size/512L))
            info <- strsplit(rawToChar(info), "\n", fixed = TRUE)[[1]]
            hcs <- grep("[0-9]* hdrcharset=", info, useBytes = TRUE,
                        value = TRUE)
            if(length(hcs)) {
                hcs <- sub("[0-9]* hdrcharset=", hcs, useBytes = TRUE)
                isUTF8 <- identical(hcs, "ISO-IR 10646 2000 UTF-8")
            }
            path <- grep("[0-9]* path=", info, useBytes = TRUE, value = TRUE)
            if(length(path)) {
                lname <- sub("[0-9]* path=", "", path, useBytes = TRUE)
                if(isUTF8) Encoding(lname) <- "UTF-8"
            }
            linkpath <- grep("[0-9]* linkpath=", info, useBytes = TRUE,
                             value = TRUE)
            if(length(linkpath)) {
                llink <- sub("[0-9]* linkpath=", "", linkpath, useBytes = TRUE)
                if(isUTF8) Encoding(llink) <- "UTF-8"
            }
            size <- grep("[0-9]* size=", info, useBytes = TRUE, value = TRUE)
            if(length(size))
                lsize <- as.integer(sub("[0-9]* size=", "", size))
         } else if(ctype == "g") {
            warn1 <- c(warn1, "skipping pax global extended headers")
            readBin(con, "raw", n = 512L*ceiling(size/512L))
        } else stop("unsupported entry type ", sQuote(ctype))
    }
    if(length(warn1)) {
        warn1 <- unique(warn1)
        for (w in warn1) warning(w, domain = NA)
    }
    if(list) contents else invisible(0L)
}

tar <- function(tarfile, files = NULL,
                compression = c("none", "gzip", "bzip2", "xz", "zstd"),
                compression_level = 6, tar = Sys.getenv("tar"),
                extra_flags = "")
{
    if(is.character(tarfile)) {
        if(nzchar(tar) && tar != "internal") {
            ## Assume external command will expand directories,
            ## so keep command-line as simple as possible
            ## But files = '.' will not work as tarfile would be included.
            if(is.null(files)) {
                files <- list.files(all.files = TRUE, full.names = TRUE,
                                    include.dirs = TRUE)
                files <- setdiff(files, c("./.", "./.."))
            }

            ## Could pipe through gzip etc: might be safer for xz
            ## as -J was lzma in GNU tar 1.20:21
            ## NetBSD < 8 used --xz not -J
            ## OpenBSD and Heirloom Toolchest had no support for xz
            flags <- switch(match.arg(compression),
                            "none" = "-cf",
                            "gzip" = "-zcf",
                            "bzip2" = "-jcf",
                            "xz" = "-Jcf",
                            "zstd" = "--zstd -cf")

            if (grepl("darwin", R.version$os)) {
                ## Precaution for macOS to omit resource forks
                ## This is supposed to work for  >= 10.5 (Leopard).
                tar <- paste("COPYFILE_DISABLE=1", tar)
            }
            if (is.null(extra_flags)) extra_flags <- ""
            ## precaution added in R 3.5.0 for over-long command lines
            nc <- nchar(ff <- paste(shQuote(files), collapse=" "))
            ## -T was not supported by Solaris nor Heirloom Toolchest's tar
            if(nc > 1000 &&
               any(grepl("(GNU tar|libarchive)",
                         tryCatch(system(paste(tar, "--version"), intern = TRUE),
                                  error = function(e) "")))) {
                tf <- tempfile("Rtar"); on.exit(unlink(tf))
                writeLines(files, tf)
                cmd <- paste(tar, extra_flags, flags, shQuote(tarfile),
                             "-T", shQuote(tf))
            } else {
                ## 'tar' might be a command + flags, so don't quote it
                cmd <- paste(tar, extra_flags, flags, shQuote(tarfile), ff)
            }
            return(invisible(system(cmd)))
        }

### ----- from here on, using internal code -----
        ## must do this before tarfile is created
        if(is.null(files)) files <- "."
        isd <- dir.exists(files)
        files <- c(
            list.files(files[isd], recursive = TRUE, all.files = TRUE,
                       full.names = TRUE, include.dirs = TRUE),
            files[!isd]
        )
        con <- switch(match.arg(compression),
                      "none" =  file(tarfile, "wb"),
                      "gzip" =  gzfile(tarfile, "wb", compression = compression_level),
                      "bzip2" = bzfile(tarfile, "wb", compression = compression_level),
                      "xz" = xzfile(tarfile, "wb", compression = compression_level),
                      "zstd" = zstdfile(tarfile, "wb", compression = compression_level))
        on.exit(close(con))
    } else if(inherits(tarfile, "connection")) con <- tarfile
    else stop("'tarfile' must be a character string or a connection")

    ## (Comment from 2013)
    ## FIXME: eventually we should use the pax extension, but
    ## that was first supported in R 2.15.3.
    GNUname <- function(name, link = FALSE)
    {
        header <- raw(512L)
        n1 <- charToRaw("ExtendedName")
        header[seq_along(n1)] <- n1
        header[157L] <- charToRaw(ifelse(link, "K", "L"))
        size <- length(name)
        header[125:135] <- charToRaw(sprintf("%011o", as.integer(size)))
        header[149:156] <- charToRaw(" ")
        checksum <- sum(as.integer(header)) %% 2^24 # 6 bytes
        header[149:154] <- charToRaw(sprintf("%06o", as.integer(checksum)))
        header[155L] <- as.raw(0L)
        writeBin(header, con)
        writeBin(name, con)
        ssize <- 512L * ceiling(size/512L)
        if(ssize > size) writeBin(raw(ssize - size), con)
    }
    warn1 <- character()

    invalid_uid <- invalid_gid <- FALSE
    for (f in unique(files)) {
        info <- file.info(f)
        if(is.na(info$size)) {
            warning(gettextf("file '%s' not found", f), domain = NA)
            next
        }
        header <- raw(512L)
        ## add trailing / to dirs.
        if(info$isdir && !endsWith(f, "/")) f <- paste0(f, "/")
        name <- charToRaw(f)
        if(length(name) > 100L) {
            OK <- TRUE
            ## best possible case: 155+/+100
            if(length(name) > 256L) OK <- FALSE
            else {
                ## do not want to split on terminal /
                m <- length(name)
                s <- max(which(name[1:min(156, m - 1L)] == charToRaw("/")))
                if(is.infinite(s) || s + 100L < length(name)) OK <- FALSE
            }
            warning("storing paths of more than 100 bytes is not portable:\n  ",
                    sQuote(f), domain = NA)
            if (OK) {
                prefix <- name[1:(s-1L)]
                name <- name[-(1:s)]
                header[345L+seq_along(prefix)] <- prefix
            } else {
                GNUname(name)
                name <- charToRaw("dummy")
                warn1 <- c(warn1, "using GNU extension for long pathname")
            }
        }
        header[seq_along(name)] <- name
        mode <- info$mode
        ## for use by R CMD build
        if (is.null(extra_flags) && grepl("/(configure|cleanup)$", f) &&
            (mode & "111") != as.octmode("111")) {
            warning(gettextf("file '%s' did not have execute permissions: corrected", f), domain = NA, call. = FALSE)
            mode <- mode | "111"
        }
        header[101:107] <- charToRaw(sprintf("%07o", mode))
        ## Windows does not have uid, gid: defaults to 0, which isn't great
        uid <- info$uid
        ## uids are supposed to be less than 'nobody' (32767)
        ## but it seems there are broken ones around: PR#15436
        if(!is.null(uid) && !is.na(uid)) {
            if(uid < 0L || uid > 32767L) {invalid_uid <- TRUE; uid <- 32767L}
            header[109:115] <- charToRaw(sprintf("%07o", uid))
        }
        gid <- info$gid
        if(!is.null(gid) && !is.na(gid)) {
            if(gid < 0L || gid > 32767L) {invalid_gid <- TRUE; gid <- 32767L}
            header[117:123] <- charToRaw(sprintf("%07o", gid))
	}
        header[137:147] <- charToRaw(sprintf("%011o", as.integer(info$mtime)))
        ## size is 0 for directories and for links.
        size <- info$size
        if (info$isdir) {
            header[157L] <- charToRaw("5")
            size <- 0
        } else {
            lnk <- Sys.readlink(f)
            if(is.na(lnk)) lnk <- ""
            header[157L] <- charToRaw(ifelse(nzchar(lnk), "2", "0"))
            if(nzchar(lnk)) {
                if(nchar(lnk, "b") > 100L) {
                    ##  stop("linked path is too long")
                    GNUname(charToRaw(lnk), TRUE)
                    warn1 <- c(warn1, "using GNU extension for long linkname")
                    lnk <- "dummy"
                }
                header[157L + seq_len(nchar(lnk))] <- charToRaw(lnk)
                size <- 0
            }
        }
        if(size >= 8^11) stop("file size is limited to 8GB")
        header[125:135] <- .Call(C_octsize, size)
        ## the next two are what POSIX says, not what GNU tar does.
        header[258:262] <- charToRaw("ustar")
        header[264:265] <- charToRaw("0")
        ## Windows does not have uname, grname
        ##     too long ( > 32 ) uname, grname are truncated (PR#17871)
        for (frag in list(list('uname',  265L),
                          list('grname', 297L))) {
            s <- info[[frag[[1L]]]]
            if(!is.null(s) && !is.na(s)) {
                ns <- nchar(s, "b")
                if (ns > 32L) {
                    warn1 <- c(warn1, sprintf("truncating %d character long '%s'",
                                              ns, frag[[1L]]))
                    ns <- 32L
                }
                i <- seq_len(ns)
                header[frag[[2L]] + i] <- charToRaw(s)[i]
            }
        }
        header[149:156] <- charToRaw(" ")
        checksum <- sum(as.integer(header)) %% 2^24 # 6 bytes
        header[149:154] <- charToRaw(sprintf("%06o", as.integer(checksum)))
        header[155L] <- as.raw(0L)
        writeBin(header, con)
        if(info$isdir || nzchar(lnk)) next
        inf <- file(f, "rb")
        for(i in seq_len(ceiling(info$size/512L))) {
            block <- readBin(inf, "raw", 512L)
            writeBin(block, con)
            if( (n <- length(block)) < 512L) writeBin(raw(512L - n), con)
        }
        close(inf)
    }
    if (invalid_uid)
        warning("invalid uid value replaced by that for user 'nobody'",
                call. = FALSE)
    if (invalid_gid)
        warning("invalid gid value replaced by that for user 'nobody'",
                call. = FALSE)
    ## trailer is two blocks of nuls.
    block <- raw(512L)
    writeBin(block, con)
    writeBin(block, con)
    if(length(warn1)) {
        warn1 <- unique(warn1)
        for (w in warn1) warning(w, domain = NA)
    }
    invisible(0L)
}
