objective <- function(p){
    out <- -log(p[1] + p[2]) + log(p[2]) + log(1 - p[1] - p[2])
    out
}

gradfun <- function(p){
    out <- c(
        -1/(p[1] + p[2]) - 1/(1 - p[1] - p[2]),
        -1/(p[1] + p[2]) + 1/p[2] - 1/(1 - p[1] - p[2])
    )
    out
}
startp <- c(1/3, 1/3)
small <- 1e-6
CI <- c(small, small, small - 1)
UI <- rbind(
    c(1, 0), # p1>small
    c(0, 1), # p2>small
    c(-1, -1) # p1+p2 < 1-small
)

out <- constrOptim(
    theta = startp,
    f     = objective,
    grad  = gradfun,
    ui    = UI,
    ci    = CI,
    outer.iterations = 100,
    method = "BFGS"
)

# Convergence code 0 (set by optim)
stopifnot(identical(out$convergence, 0L))
