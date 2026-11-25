stopifnot(exprs = {
  all.equal(convolve(1:5, rep(1, 5)), rep(15, 5L))

  all.equal(convolve(1:5, rep(1, 7), type="open"), c(1, 3, 6, 10, 15, 15, 15, 14, 12, 9, 5))
  all.equal(convolve(1:5, rep(1, 6), type="open"), c(1, 3, 6, 10, 15, 15, 14, 12, 9, 5))
  all.equal(convolve(1:5, rep(1, 5), type="open"), c(1, 3, 6, 10, 15, 14, 12, 9, 5))
  all.equal(convolve(1:5, rep(1, 4), type="open"), c(1, 3, 6, 10, 14, 12, 9, 5))
  all.equal(convolve(1:5, rep(1, 3), type="open"), c(1, 3, 6, 9, 12, 9, 5))
  all.equal(convolve(1:5, rep(1, 2), type="open"), c(1, 3, 5, 7, 9, 5))
  all.equal(convolve(1:5, rep(1, 1), type="open"), c(1, 2, 3, 4, 5))

  !length(convolve(1:5, rep(1, 7), type="filter"))
  !length(convolve(1:5, rep(1, 6), type="filter"))
  all.equal(convolve(1:5, rep(1, 5), type="filter"), 15)
  all.equal(convolve(1:5, rep(1, 4), type="filter"), c(10, 14))
  all.equal(convolve(1:5, rep(1, 3), type="filter"), c(6, 9, 12))
  all.equal(convolve(1:5, rep(1, 2), type="filter"), c(3, 5, 7, 9))
  all.equal(convolve(1:5, rep(1, 1), type="filter"), c(1, 2, 3, 4, 5))
})
