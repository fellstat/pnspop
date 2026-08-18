
test_that("main", {
  data(faux_pns)

  #hashes
  # Expected values updated 2026-08-17 when the sample-match privatization
  # weight was aligned to manuscript section 3.2 (CODE_REVIEW.md 2.1):
  # numerator gains the -n+t correction, denominator subtracts an edge end
  # only for non-seeds. Pre-fix values: 1073, 938, and (925, 935) bounds.
  pp <- cross_tree_pse(faux_pns$subject,faux_pns$recruiter,
                 faux_pns$subject_hash, faux_pns$degree,
                 faux_pns[paste0("friend_hash",1:11)], rho=.001)
  testthat::expect_equal(round(unlist(pp)), c(1056, 0,    687,  350764), ignore_attr = TRUE)

  pp <- cross_tree_pse(faux_pns$subject,faux_pns$recruiter,
                       faux_pns$subject_hash, faux_pns$degree,
                       faux_pns[paste0("friend_hash",1:11)])
  testthat::expect_equal(floor(1/pp$rho), 1170, ignore_attr = TRUE)
  testthat::expect_equal(floor(pp$estimate), 926, ignore_attr = TRUE)

  pp <- cross_tree_pse(faux_pns$subject,faux_pns$recruiter,
                       faux_pns$subject_hash, faux_pns$degree,
                       faux_pns[paste0("friend_hash",1:11)], small_sample_fraction = FALSE)

  testthat::expect_true(pp$estimate > 913 & pp$estimate < 923)

  faux_pns2 <- faux_pns[200:1,]
  pp <- cross_tree_pse(faux_pns$subject,faux_pns$recruiter,
                       faux_pns$subject_hash, faux_pns$degree,
                       faux_pns[paste0("friend_hash",1:11)], small_sample_fraction = TRUE)
  pp2 <- cross_tree_pse(faux_pns2$subject,faux_pns2$recruiter,
                       faux_pns2$subject_hash, faux_pns2$degree,
                       faux_pns2[paste0("friend_hash",1:11)], small_sample_fraction = TRUE)
  testthat::expect_true(abs(pp$estimate - pp2$estimate ) < .000001)
})


test_that("estimated rho with missing hashes is stable", {
  # Pins the post-NA-drop estimated-rho path and match counting in
  # cross_tree_pse. Values captured 2026-08-17 immediately BEFORE the
  # CODE_REVIEW.md 4.3/1.2 refactor (table-based rho count, tabulated
  # cross-tree matches, lapply out-sets), which must be bit-identical.
  data(faux_pns)
  h <- faux_pns$subject_hash
  set.seed(5)
  h[sample.int(nrow(faux_pns), 25)] <- NA
  for (m in c("network", "alter", "sample")) {
    pp <- cross_tree_pse(faux_pns$subject, faux_pns$recruiter, h,
                         faux_pns$degree, faux_pns[paste0("friend_hash", 1:11)],
                         method = m)
    testthat::expect_equal(floor(1 / pp$rho), 1268)
    testthat::expect_equal(
      floor(pp$estimate),
      c(network = 866, alter = 897, sample = 784)[[m]]
    )
    testthat::expect_equal(
      pp$num_matches,
      c(network = 646, alter = 482, sample = 164)[[m]]
    )
  }
})


test_that("overlap_statistics accepts all documented nbrs types", {
  # Values pinned on faux_pns (2026-08-17). List input (the documented
  # type) previously deparsed each element to a "c(...)" string and
  # matched nothing (CODE_REVIEW.md 2.7): data.frame, matrix and list
  # inputs must agree exactly.
  data(faux_pns)
  fh <- paste0("friend_hash", 1:11)
  b <- overlap_statistics(faux_pns$subject_hash, faux_pns[fh])
  testthat::expect_equal(b$neighbors$total_nbrs, 949)
  testthat::expect_equal(b$neighbors$unique_nbrs, 430)
  testthat::expect_equal(b$unique$total_unique_ident, 447)
  testthat::expect_equal(b$naive_crc_estimate$unique_nbrs_sample_overlap, 183)
  testthat::expect_equal(floor(b$naive_crc_estimate$N), 469)

  bm <- overlap_statistics(faux_pns$subject_hash, as.matrix(faux_pns[fh]))
  testthat::expect_identical(b, bm)

  nbl <- lapply(seq_len(nrow(faux_pns)), function(i) unlist(faux_pns[i, fh]))
  bl <- overlap_statistics(faux_pns$subject_hash, nbl)
  testthat::expect_identical(b, bl)
})


test_that("bootstrap_pse contract", {
  # Pins the bootstrap CI machinery (values captured 2026-08-17, seed 101,
  # bit-identical across the 1.2/1.7/1.8/2.8 hardening) and the
  # infinite-point-estimate branch (CODE_REVIEW.md 1.7): the "1 / rho" row
  # must hold 1/rho, and conf_level must reflect the argument.
  skip_on_cran()
  data(faux_pns)
  fh <- paste0("friend_hash", 1:11)
  set.seed(101)
  b <- bootstrap_pse(faux_pns$subject, faux_pns$recruiter, faux_pns$subject_hash,
                     faux_pns$degree, faux_pns[fh], rho = .001,
                     n_bootstrap = 10, progress = FALSE)
  testthat::expect_equal(floor(b$value), c(1055, 1000))
  testthat::expect_true(b$ci_lower_bound[1] < b$value[1],
                        b$value[1] < b$ci_upper_bound[1])
  testthat::expect_equal(b$ci_lower_bound[2], 1000) # rho known: degenerate CI
  testthat::expect_equal(nrow(attr(b, "bootstrap_samples")), 10)

  # Infinite point estimate (alter method, zero cross-tree matches)
  un_nbrs <- as.data.frame(matrix(1000 + 1:120, nrow = 40))
  nr <- suppressWarnings(
    bootstrap_pse(1:40, rep(-1, 40), 1:40, rep(6, 40), un_nbrs, rho = .01,
                  method = "alter", n_bootstrap = 5, conf_level = .9,
                  progress = FALSE))
  testthat::expect_true(is.infinite(nr$value[1]))
  testthat::expect_equal(nr$value[2], 100) # 1/rho, not rho
  testthat::expect_equal(attr(nr, "conf_level"), 0.9)
})


test_that("random missingness does not corrupt estimates", {
  # Guards the NA handling added 2026-08-17 (CODE_REVIEW.md 1.1 + the
  # NA-hash subject drop in one_step_pse/cross_tree_pse).
  #
  # Thresholds were calibrated from 100-rep runs at 15% missingness on
  # faux_pns. The subject-hash bounds for one_step and method="sample"
  # are documented residual-sensitivity CEILINGS, not unbiasedness
  # claims: estimators that rely on matches INTO the sample retain
  # finite-sample sensitivity to dropped subjects (calibrated +11% and
  # +9% respectively; pre-fix one_step was +57%). Alter-nomination
  # missingness is design-equivalent to nomination subsampling and must
  # be near-neutral for every estimator.
  skip_on_cran()
  data(faux_pns)
  fh <- paste0("friend_hash", 1:11)
  est_all <- function(hash, nbrs) {
    c(one_step = one_step_pse(faux_pns$subject, faux_pns$recruiter, hash,
                              faux_pns$degree, nbrs, rho = .001)$estimate,
      sapply(c("network", "alter", "sample"), function(m)
        cross_tree_pse(faux_pns$subject, faux_pns$recruiter, hash,
                       faux_pns$degree, nbrs, rho = .001,
                       method = m)$estimate))
  }
  base <- est_all(faux_pns$subject_hash, faux_pns[fh])

  # Precondition: full-data estimates are finite and distinct
  testthat::expect_true(all(is.finite(base)))
  testthat::expect_gt(sd(base), 0)

  # 15% of subject hashes set to NA
  set.seed(42)
  r_subj <- t(sapply(1:25, function(r) {
    h <- faux_pns$subject_hash
    h[sample.int(nrow(faux_pns), 30)] <- NA
    est_all(h, faux_pns[fh])
  }))
  testthat::expect_true(all(is.finite(r_subj)))
  testthat::expect_true(all(apply(r_subj, 2, sd) > 0)) # not degenerate
  shift <- colMeans(r_subj) / base - 1
  testthat::expect_lt(abs(shift[["network"]]), .05)
  testthat::expect_lt(abs(shift[["alter"]]), .05)
  testthat::expect_gt(shift[["sample"]], -.05)
  testthat::expect_lt(shift[["sample"]], .15)   # known residual ceiling
  testthat::expect_gt(shift[["one_step"]], -.05)
  testthat::expect_lt(shift[["one_step"]], .20) # known residual ceiling

  # 15% of alter nominations set to NA
  set.seed(43)
  r_nbrs <- t(sapply(1:25, function(r) {
    nb <- as.matrix(faux_pns[fh])
    pos <- which(!is.na(nb))
    nb[sample(pos, round(.15 * length(pos)))] <- NA
    est_all(faux_pns$subject_hash, as.data.frame(nb))
  }))
  testthat::expect_true(all(is.finite(r_nbrs)))
  testthat::expect_true(all(apply(r_nbrs, 2, sd) > 0))
  shift_nb <- colMeans(r_nbrs) / base - 1
  for (k in names(shift_nb))
    testthat::expect_lt(abs(shift_nb[[k]]), .05)
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
