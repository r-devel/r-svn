stopifnot(exprs = {
  !length(convolve(1:5, rep(1, 7), type="filter"))
  !length(convolve(1:5, rep(1, 6), type="filter"))
  all.equal(convolve(1:5, rep(1, 5), type="filter"), 15)
  all.equal(convolve(1:5, rep(1, 4), type="filter"), c(10, 14))
  all.equal(convolve(1:5, rep(1, 3), type="filter"), c(6, 9, 12))
  all.equal(convolve(1:5, rep(1, 2), type="filter"), c(3, 5, 7, 9))
  all.equal(convolve(1:5, rep(1, 1), type="filter"), c(1, 2, 3, 4, 5))
})
