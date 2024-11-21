### tests of strftime (formatting POSIXlt objects).

Sys.setenv(TZ = "Europe/Rome")

dt <- as.POSIXlt("2022-12-11 09:03;04")

ff <- c(LETTERS, letters)
ff <- setdiff(c(LETTERS, letters),
              c("E", "J", "K", "L", "N", "P", "O", "Q",
                "f", "i", "k", "l",  "o", "q",
                "r", # %r is locale- and implementation-dependent.
                "s", "v")
              )

for (f in ff) {
    f <- paste0("%", f)
    cat(sprintf("%s: %s\n", f, format(dt, f)))
}

## 'not in the standards and less widely implemented'
## %P is a glibc extension which we added to IANA tzcode for R. Not in macOS.
for (f in c("P", "k", "l", "s")) {
    f <- paste0("%", f)
    cat(sprintf("%s: %s\n", f, try(format(dt, f), silent = TRUE)))
}

## week numbers
dt2 <- as.POSIXlt(sprintf("%d-01-01 09:03;04", 2015:2018))
cat(format(dt2, "%Y: %U %V %W"), sep = "\n")


## fractional seconds print(<POSIXct>) --> format.POSIXlt() -- PR#17350 (and rdev day #83)
## Original PR#17350 example (Vitalie Spinu):
op <- options(digits.secs = 6, scipen = 20, digits = 15)
## what we'd desire for print()ing etc:
chx <- paste0("2009-08-03 12:01:59", c("", paste0(".",1:3)))
print(chx, width = 40)
xl <- as.POSIXlt(chx)
stopifnot(identical(xl$sec, 59 + 0:3/10)) # POSIXlt keeping full precision (always did)
## (but all arithmetic with POSIX*t currently happens via POSIXct, losing precision)
fxl <- format(xl) # is perfect {with getOption("digits.secs") > 0  !}
stopifnot(identical(sub(".*:59", '', fxl), paste0(".", 0:3)))
x <- as.POSIXct("2009-08-03 12:01:59") + 0:3/10 # using POSIXct looses prec
x. <- structure(x, tzone = "") ## == Vitalie's explicit original ex.
identical(x, x.) # FALSE :  x. contains `tzone = ""`
print(x, width = 40) # now .000000 .099999 2.00000 2.999999 (as digits.secs = 6 !)
fx <- format(x)
stopifnot(identical(fx, format(x.))) # *are* the same (for a while now)
## The %OS and  %OS<d> formats have been fine "always":
fD.OS <- function(d) format(x, format = paste0("%Y-%m-%d %H:%M:%OS", if(d=="_") "" else d))
f.OSss <- vapply(c("_",0:6), fD.OS, character(length(x)))
t(f.OSss) |> print(width=111, quote=FALSE) # shows  'trunc()'  instead of  'round()'
stopifnot(identical(f.OSss[,"_"], f.OSss[,"6"])) # by option digits.secs 
(secDig <- sub(".*:59", '', f.OSss)) ## [,"1"] is  *.0 *.0 *.2 *.2 - "bad" from using trunc() by design
##      ___________   ___            __          __  "factory fresh" default
options(digits.secs = NULL, scipen = 0, digits = 7)
f.OSssD <- vapply(c("_",0:6), fD.OS, character(length(x))) # same call but different "digits.secs" option
## digits = <d> now works "the same":
fdig <- vapply(c("_",0:6), \(d) format(x, digits = if(d != "_") d), character(length(x)))
stopifnot(exprs = {
    nchar(t(secDig)) == c(7L, 0L, 2:7)     # as always
    identical(f.OSssD[, 1], f.OSssD[,"0"]) # "" <--> "0"
    identical(f.OSss [,-1], f.OSssD[, -1]) # only meaning of `empty' "%OS" changes with "digits.secs" option
    identical(fdig, f.OSssD)
})
options(op)
## Number of digits used differed in several cases in R <= 4.4.z
