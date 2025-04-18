#  File src/library/base/baseloader.R
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


## should be kept in step with code in  >> R/lazyload.R <<
## (but not everywhere ?!)
.Internal(eval(quote({
..lazyLoad <- function(filebase, envir = parent.frame())
{
    ##
    ## bootstrapping definitions so we can load base
    ##
    glue <- function (..., sep = " ", collapse = NULL)
##      .Internal(paste(list(...), sep, collapse, TRUE))# recycle0=TRUE
        .Internal(paste(list(...), sep, collapse, FALSE))
    readRDS <- function (file) {
        halt <- function (message) .Internal(stop(TRUE, message))
        gzfile <- function (description, open)
            .Internal(gzfile(description, open, "", 6))
        close <- function (con) .Internal(close(con, "rw"))
        if (! is.character(file)) halt("bad file name")
        con <- gzfile(file, "rb")
        on.exit(close(con))
        .Internal(unserializeFromConn(con, baseenv()))
    }
    `parent.env<-` <-
        function (env, value) .Internal(`parent.env<-`(env, value))
    existsInFrame <- function (x, env) .Internal(exists(x, env, "any", FALSE))
    list2env <- function (x, envir) .Internal(list2env(x, envir))
    environment <- function () .Internal(environment(NULL))
    mkenv <- function() .Internal(new.env(TRUE, baseenv(), 29L))

    ##
    ## main body
    ##
    mapfile  <- glue(filebase, "rdx", sep = ".")
    datafile <- glue(filebase, "rdb", sep = ".")
    env <- mkenv()
    map <- readRDS(mapfile)
    vars <- names(map$variables)
    compressed <- map$compressed
    list2env(map$references, env)
    envenv <- mkenv()
    envhook <- function(n) {
        if (existsInFrame(n, envenv))
            envenv[[n]]
        else {
            e <- mkenv()
            envenv[[n]] <- e           # MUST do this immediately
            key <- env[[n]]
            data <- lazyLoadDBfetch(key, datafile, compressed, envhook)
            parent.env(e) <- if(!is.null(data$enclos)) data$enclos else emptyenv()
            list2env(data$bindings, e)
            if (! is.null(data$attributes))
                attributes(e) <- data$attributes
            ## there are no S4 objects in base
            if (! is.null(data$locked) && data$locked)
                .Internal(lockEnvironment(e, FALSE))
            e
        }
    }
    expr <- quote(lazyLoadDBfetch(key, datafile, compressed, envhook))
    this <- environment()
    .Internal(makeLazy(vars, map$variables, expr, this, envir))

    ## reduce memory use
    map <- NULL
    vars <- NULL
    vals <- NULL
    rvars <- NULL
    mapfile <- NULL
    readRDS <- NULL
}

    existsInBase <- function (x)
        .Internal(exists(x, .BaseNamespaceEnv, "any", TRUE))
    glue <- function(..., sep = " ")
        .Internal(paste(list(...), sep, collapse=NULL, FALSE))

    basedb <- glue(.Internal(R.home()), "library", "base", "R",
                   "base", sep= .Platform$file.sep)

    ..lazyLoad(basedb, baseenv())

}), .Internal(new.env(FALSE, baseenv(), 29L)), baseenv()))

## keep in sync with R/zzz.R
as.numeric <- as.double
is.name <- is.symbol


## populate C/Fortran symbols
local({
    routines <- getDLLRegisteredRoutines("base")
    for (i in c("dqrcf", "dqrdc2", "dqrqty", "dqrqy", "dqrrsd", "dqrxb", # qr
                "dtrco")) # .kappa_tri
        assign(paste0(".F_", i), routines[[3]][[i]], envir = .BaseNamespaceEnv)
    for(i in 1:2)
        lapply(routines[[i]],
               function(sym) assign(paste0(".C_", sym$name), sym, envir = .BaseNamespaceEnv))
})

## make sure these two promises are forced to avoid recursive invocation
## of "args" and consequent "promise already under evaluation" error

invisible(force(.ArgsEnv))
invisible(force(.GenericArgsEnv))

## also force these condition system callback promises to avoid
## recursive invocation in some rare situations at start-up
invisible(force(.signalSimpleWarning))
invisible(force(.handleSimpleError))
invisible(force(.tryResumeInterrupt))

local({
    assignWrapped <- function(x, method, home, envir) {
        method <- method                # force evaluation
        home <- home                    # force evaluation
        delayedAssign(x, get(method, envir = home), assign.env = envir)
    }
    methods <- paste0(.S3_methods_table[, 1L], ".",
                      .S3_methods_table[, 2L])
    env <- .BaseNamespaceEnv
    table <- env[[".__S3MethodsTable__."]]
    for(m in methods)
        assignWrapped(m, m, env, table)
})

## Lock .ArgsEnv, .GenericArgsEnv, and their bindings.
## This has to do this here, not in zzz.R, since lazy laoding doesn't
## preserve binding locks (or environment locks for environments in
## base).
lockEnvironment(.ArgsEnv, bindings = TRUE)
lockEnvironment(.GenericArgsEnv, bindings = TRUE)
