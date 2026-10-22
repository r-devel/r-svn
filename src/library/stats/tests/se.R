## Tests for se() generic and default method

## Default method for an lm object
fit <- lm(mpg ~ wt + cyl, data = mtcars)

expected <- sqrt(diag(vcov(fit)))
observed <- se(fit)

stopifnot(
    identical(observed, expected),
    identical(names(observed), names(coef(fit)))
)

stopifnot(
    isTRUE(all.equal(
        se(fit),
        summary(fit)$coefficients[, "Std. Error"]
    ))
)

## Default method for a glm object
fit.glm <- glm(am ~ wt, data = mtcars, family = binomial())

stopifnot(
    identical(
        se(fit.glm),
        sqrt(diag(vcov(fit.glm)))
    )
)

stopifnot(
    isTRUE(all.equal(
        se(fit.glm),
        summary(fit.glm)$coefficients[, "Std. Error"]
    ))
)

## Additional arguments are forwarded to vcov()
stopifnot(
    all.equal(
        se(fit.glm, dispersion = 2),
        sqrt(diag(vcov(fit.glm, dispersion = 2)))
    )
)


## Aliased coefficients are retained as NA
dat <- transform(mtcars, wt.copy = wt)
fit.aliased <- lm(mpg ~ wt + wt.copy, data = dat)

expected <- sqrt(diag(vcov(fit.aliased)))
observed <- se(fit.aliased)

stopifnot(
    identical(observed, expected),
    anyNA(observed),
    identical(names(observed), names(coef(fit.aliased)))
)


## The generic dispatches to a class-specific method
se.se_test <- function(object, ...)
    "class-specific method"

object <- structure(list(), class = "se_test")

stopifnot(
    identical(se(object), "class-specific method")
)


## The default method forwards arguments to a custom vcov() method
vcov.se_vcov_test <- function(object, scale = 1, ...)
{
    value <- diag(c(1, 4)) * scale
    dimnames(value) <- list(c("first", "second"),
                            c("first", "second"))
    value
}

object <- structure(list(), class = "se_vcov_test")

stopifnot(
    identical(
        se(object, scale = 9),
        c(first = 3, second = 6)
    )
)
