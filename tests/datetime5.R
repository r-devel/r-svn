### tests of strftime (formatting POSIXlt objects via format.POSIXlt)

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

## recycling *both* {x, format} "heavily"; digits = <n> must influence %OS<empty>; PR#17350
(fmt <- c("", paste0("%H:%M:%OS", c("", 2), " in %Y"),                  # || nasty (but "correct")
          paste0("%Y-%m-%d", c("", paste0(" %H:%M:%OS", c("", 0, 1, 6, 9, 11))))))
weekD <- seq(as.Date("2020-04-01"), by = "weeks", length.out = 5 * length(fmt)) ; joff <- (0:4)*length(fmt)
weekPlt <- as.POSIXlt(weekD, tz = "UTC")
(Lf1 <- split(f1 <- format(weekPlt,        format = fmt),             fmt))
(Lf. <- split(f. <- format(weekPlt + 0.25, format = fmt),             fmt))
(Lf3 <- split(f3 <- format(weekPlt + 0.25, format = fmt, digits = 3), fmt))
stopifnot(f3[2L+joff] == f3[3L+joff],
          grepl("^00:00:00.25 in 202[01]", f3[2L+joff]))
## digits = 3 had no effect on "%OS "
