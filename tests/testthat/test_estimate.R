
test_that("main", {
  data(faux_pns)
  p1 <- population_estimate(rds$subject,rds$recruiter, rds$degree, nbrs2)
  testthat::expect_equal(round(unlist(p1)), c(1094,1149,1066), ignore_attr = TRUE)
  p2 <- population_estimate_hash(rds$subject,rds$recruiter,
        subj_hash, rds$degree, nbrs_hash,rho=rho)
  testthat::expect_equal(round(unlist(p2)), c(901,887,0), ignore_attr = TRUE)
})
