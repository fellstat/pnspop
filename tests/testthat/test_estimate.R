
test_that("main", {
  data(faux_pns)
  #p1 <- population_estimate(rds$subject,rds$recruiter, rds$degree, nbrs2)
  #testthat::expect_equal(round(unlist(p1)), c(1094,1149,1066), ignore_attr = TRUE)
  p2 <- population_estimate_hash(rds$subject,rds$recruiter,
        subj_hash, rds$degree, nbrs_hash,rho=rho)
  testthat::expect_equal(round(unlist(p2)), c(901,887,0), ignore_attr = TRUE)
})


test_that("bias", {
  #
  # Check mean of estimates is near true value on a configuration graph
  #
  skip_on_cran()
  n <- 5000 #pop size
  hashSize <- 50000
  d <- rpois(n,lambda = 3) + 1
  el <- make_configuration_graph(d)
  sim <- function(){
    rho <- 1 / hashSize
    hash <- floor(runif(n, min = 0, max=hashSize))
    g <- rep(1,n)
    seeds <- 7 # #of seeds
    rds <- samp_rds(el, d, seeds,g,400,FALSE, pr = c(0,.1,.9))
    subj_hash <- hash[rds$subject]
    nbrs <- lapply(1:n, function(i) c(el[el[,1]==i,2],el[el[,2]==i,1]))
    nbrs2 <- nbrs[rds$subject]
    nbrs_hash <- lapply(nbrs2,function(x) hash[x])
    unlist(population_estimate_hash(rds$subject,rds$recruiter,
                                    subj_hash, d[rds$subject], nbrs_hash, rho))
  }

  ll <- list()
  for(i in 1:20){
    ll[[i]] <- sim()
  }
  r <- do.call(rbind, ll)
  mns <- colMeans(r)
  expect_true(all(mns[1:2] < 5500) & all(mns[1:2] > 4500))

})
