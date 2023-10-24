
test_that("main", {
  data(faux_pns)

  #hashes
  pp <- cross_tree_pse(faux_pns$subject,faux_pns$recruiter,
                 faux_pns$subject_hash, faux_pns$degree,
                 faux_pns[paste0("friend_hash",1:11)], rho=.001)
  testthat::expect_equal(round(unlist(pp)), c(1073, 0,    687,  350764), ignore_attr = TRUE)

  pp <- cross_tree_pse(faux_pns$subject,faux_pns$recruiter,
                       faux_pns$subject_hash, faux_pns$degree,
                       faux_pns[paste0("friend_hash",1:11)])
  testthat::expect_equal(floor(1/pp$rho), 1170, ignore_attr = TRUE)
  testthat::expect_equal(floor(pp$estimate), 938, ignore_attr = TRUE)

  pp <- cross_tree_pse(faux_pns$subject,faux_pns$recruiter,
                       faux_pns$subject_hash, faux_pns$degree,
                       faux_pns[paste0("friend_hash",1:11)], small_sample_fraction = FALSE)

  testthat::expect_true(pp$estimate > 925 & pp$estimate < 935)

  faux_pns2 <- faux_pns[200:1,]
  pp <- cross_tree_pse(faux_pns$subject,faux_pns$recruiter,
                       faux_pns$subject_hash, faux_pns$degree,
                       faux_pns[paste0("friend_hash",1:11)], small_sample_fraction = TRUE)
  pp2 <- cross_tree_pse(faux_pns2$subject,faux_pns2$recruiter,
                       faux_pns2$subject_hash, faux_pns2$degree,
                       faux_pns2[paste0("friend_hash",1:11)], small_sample_fraction = TRUE)
  testthat::expect_true(abs(pp$estimate - pp2$estimate ) < .000001)
})


test_that("bias", {
  #
  # Check mean of estimates is near true value on a configuration graph
  #
  skip_on_cran()
  n <- 5000 #pop size
  hashSize <- 5000
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
    unlist(cross_tree_pse(rds$subject,rds$recruiter,
                                    subj_hash, d[rds$subject], nbrs_hash, rho))
  }

  ll <- list()
  for(i in 1:20){
    ll[[i]] <- sim()
  }
  r <- do.call(rbind, ll)
  mns <- colMeans(r)
  testthat::expect_true(mns[1] < 5500 & mns[1] > 4500)

})
