require("tools")
Rd2txt_options(underline_titles = FALSE)

# -------------------------------------------------------------------
# prepare_Rd() is OK with a top level \Sexpr that is yet to be rendered

txt <- "
\\name{foo}
\\title{Title}
\\description{Desc.}
\\Sexpr[stage=render,results=rd]{\"\\\\\\details{This is dynamic.}\"}
"

rd <- parse_Rd(con <- textConnection(txt)); close(con)

warn <- NULL
withCallingHandlers(
  rd2 <- tools:::prepare_Rd(rd),
  warning = function(w) { warn <<- w; invokeRestart("muffleWarning") }
)
stopifnot(is.null(warn))
stopifnot("\\Sexpr" %in% tools:::RdTags(rd2))


## \Sexpr[stage=build, results=hide]{ <a dozen "empty" lines> }
tf <- textConnection("RdTeX", "w")
Rd2latex("Rd-Sexpr-hide-empty.Rd", tf, stages="build")
tex <- textConnectionValue(tf); close(tf); rm(tf)
(H2end <- tex[grep("^Hello", tex):length(tex)])
stopifnot((n <- length(H2end)) <= 4, # currently '3'; was 13 in R < 4.2.0
          H2end[-c(1L,n)] == "")     # also had \\AsIs{ .. }  " "  "   "


## checkRd() gives file name and correct line number of \Sexpr[results=rd] chunk
stopifnot(grepl("Rd-Sexpr-warning.Rd:5:",
                print(checkRd("Rd-Sexpr-warning.Rd", stages = "build")),
                fixed = TRUE))

## processRdChunk() gives file name and location of eval error
(msg <- tryCatch(checkRd(file_path_as_absolute("Rd-Sexpr-error.Rd")),
                 error = conditionMessage))
stopifnot(startsWith(msg, "Rd-Sexpr-error.Rd:4-7:"),
          length(checkRd("Rd-Sexpr-error.Rd", stages = NULL)) == 0)
## file name and line numbers were missing in R < 4.2.0


## \doi with hash symbol or Rd specials
rd <- parse_Rd("doi.Rd")
writeLines(out <- capture.output(Rd2txt(rd, stages = "build")))
stopifnot(grepl("10.1000/456#789", out[5], fixed = TRUE),
          grepl("doi.org/10.1000/456%23789", out[5], fixed = TRUE),
          grepl("10.1000/{}", out[7], fixed = TRUE),
          grepl("doi.org/10.1000/%7B%7D", out[7], fixed = TRUE))
## R < 4.2.0 failed to encode the hash and lost {}


## \title and \section name should not end in a period
rd <- parse_Rd(textConnection(r"(
\name{test}
\title{title.}
\description{description}
\section{section.}{nothing}
)"))
stopifnot(identical(endsWith(print(checkRd(rd)), "end in a period"),
                    rep(TRUE, 2)))

## checkRd() with duplicated \name (is documented to fail from prepare_Rd)
assertError(checkRd(parse_Rd(textConnection(r"(
\name{test}\title{test}\name{test2}
)"))), verbose = TRUE)
## no error in R < 4.4.0

## prepared NEWS should check cleanly
NEWS_Rd <- readRDS(file.path(R.home("doc"), "NEWS.rds"))
stopifnot(inherits(NEWS_Rd, "Rd"),
          length(print(checkRd(NEWS_Rd))) == 0L)
## "Must have a \description" in R < 4.4.0, now moved to checkRdContents()

## checkRd() raises some instances of "lost braces"
Sys.setenv("_R_CHECK_RD_NOTE_LOST_BRACES_" = TRUE)
bad <- function (Rd) sum(startsWith(checkRd(Rd), "checkRd: (-1) "))
stopifnot(bad("Rd-braces_ignored.Rd") == 0L,
          bad("Rd-braces_reported.Rd") == 10L)


## "srcref" of usermacro expansion
rd <- parse_Rd(textConnection(r"(\newcommand{\Emph}{\emph{#1}}
\Emph{this}
)"), fragment = TRUE, verbose = TRUE, macros = FALSE)
print(rd) # shows the expansion, not the source
stopifnot(!grepl("\\Emph", paste(as.character(rd), collapse = ""), fixed = TRUE))
print(getSrcref(rd[[4]]), useSource = FALSE) # "chars 2:12 to 2:11"
## Maybe the expansion should not get a "srcref" in the first place?
## (Note that RdTextFilter would need to be updated in this case.)


## An unmatched un-escaped '{' in a comment in \examples{} ... should *NOT* trip up, but does

txt <- r"(\title{Commented left-brace in Example}
\name{ex-comm-brace}
\examples{
  if(1 <= 11) { # if(require("MASS")) \{  << only works when escaped with '\\'
    fractions(355, 112)
  }% if(.)
}
\keyword{misc})"
## these all work fine:

(rd1 <- parse_Rd(con <- textConnection(txt))); close(con)
Rd2ex(rd1)
Rd2txt(rd1)
## etc

## however: When I try the bare "{" instead of  "\{"
txt0 <- sub("\\{", "{", txt, fixed=TRUE)
stopifnot(nchar(txt0) == nchar(txt) - 1)
## This currently gives a warning .. and later problems {-> package checking etc}
rd0 <- parse_Rd(con0 <- textConnection(txt0)); close(con0)
## Warning message:
## In parse_Rd(con0) : <connection>:8: unexpected section header '\keyword'
checkRd(rd0)
Rd2ex(rd0) # shows extra "}" and "{misc}"


## PR#18960: Rd macros \packageTitle, \packageDescription, \packageAuthor, \packageMaintainer
pkg <- tempfile("testpkg")
dir.create(pkg)
dir.create(file.path(pkg, "man", "unix"), recursive = TRUE)
dir.create(file.path(pkg, "inst", "sub"), recursive = TRUE)

write.dcf(file = file.path(pkg, "DESCRIPTION"),
  list(Package = "testpkg",
       Title = "Test Package Title",
       Version = "1.0",
       Description = "Test package description.",
       Author = "Test Author",
       Maintainer = "Test Maintainer <tm@example.com>",
       License = "GPL-2"))

f_man <- file.path(pkg, "man", "foo.Rd")
cat(file = f_man, r"(
\name{foo}
\title{\packageTitle{testpkg}}
\description{\packageDescription{testpkg}}
\author{\packageAuthor{testpkg}}
\section{Maintainer}{\packageMaintainer{testpkg}}
)")

f_man_sub <- file.path(pkg, "man", "unix", "bar.Rd")
cat(file = f_man_sub, r"(
\name{bar}
\title{\packageTitle{testpkg}}
\description{\packageDescription{testpkg}}
\author{\packageAuthor{testpkg}}
)")

f_inst <- file.path(pkg, "inst", "NEWS.Rd")
cat(file = f_inst, r"(
\name{NEWS}
\title{NEWS for \packageTitle{testpkg}}
\description{\packageDescription{testpkg}}
)")

f_inst_sub <- file.path(pkg, "inst", "sub", "baz.Rd")
cat(file = f_inst_sub, r"(
\name{baz}
\title{\packageTitle{testpkg}}
\description{\packageDescription{testpkg}}
)")

## checkRd() on individual Rd files from outside the package directory
stopifnot(exprs = {
    length(checkRd(f_man, stages = "build")) == 0L
    length(checkRd(f_man_sub, stages = "build")) == 0L
    length(checkRd(f_inst, stages = "build")) == 0L
    length(checkRd(f_inst_sub, stages = "build")) == 0L
})

## Rd2txt() expands package macros
out <- capture.output(Rd2txt(f_man, stages = "build", options = list(underline_titles = FALSE)))
stopifnot(exprs = {
    any(grepl("Test Package Title", out, fixed = TRUE))
    any(grepl("Test package description.", out, fixed = TRUE))
    any(grepl("Test Author", out, fixed = TRUE))
    any(grepl("Test Maintainer", out, fixed = TRUE))
})

## Rd_macros_package_dir() fallbacks and override
f_other <- file.path(tempdir(), "other.Rd")
file.create(f_other)
d_fake_man <- file.path(tempdir(), "fake_man_dir", "man")
dir.create(d_fake_man, recursive = TRUE, showWarnings = FALSE)
f_fake_man <- file.path(d_fake_man, "dummy.Rd")
file.create(f_fake_man)

Rd_macros_package_dir <- tools:::Rd_macros_package_dir
processRdChunk_data_store <- tools:::processRdChunk_data_store
stopifnot(exprs = {
    identical(Rd_macros_package_dir(), ".")
    local({
        processRdChunk_data_store(list(Rdfile = f_other))
        on.exit(processRdChunk_data_store(NULL))
        identical(Rd_macros_package_dir(), ".")
    })
    local({
        processRdChunk_data_store(list(Rdfile = f_fake_man))
        on.exit(processRdChunk_data_store(NULL))
        identical(Rd_macros_package_dir(), ".")
    })
    local({
        Sys.setenv("_R_RD_MACROS_PACKAGE_DIR_" = pkg)
        on.exit(Sys.unsetenv("_R_RD_MACROS_PACKAGE_DIR_"))
        identical(Rd_macros_package_dir(), pkg)
    })
})

unlink(pkg, recursive = TRUE)
unlink(f_other)
unlink(dirname(d_fake_man), recursive = TRUE)
