
#' @importFrom stats rexp uniroot median na.omit qnorm runif sd
#' @importFrom utils setTxtProgressBar txtProgressBar
#' @importFrom RDS2 ss_fixed_point
NULL

#' Constructs a configuration graph
#' @param d a vector of degrees for each node
#' @return A matrix with 3 columns and approximately sum(d)/2 rows,
#' with each row representing an edge. Edges are from
#' the first column to the second column, with the third column
#' indicating whether the edge is reciprocated (1 -> reciprocated, 0 -> directed).
#' @references
#' Michael S. Molloy and Bruce A. Reed. A critical point for random graphs with a
#' given degree sequence. Random Structures and Algorithms, 6:161–179, 1995.
#' @examples
#' set.seed(1)
#' make_configuration_graph(c(2,3,4,5))
#' @export
make_configuration_graph <- function(d){
  n <- length(d)
  if(sum(d) < 0.5)
    return(matrix(numeric(), nrow=0, ncol=3))
  edges <- list()
  d1 <- d
  while(sum(d1)>0.5){
    l <- sample.int(n,1,prob=d1)
    dt <- d1
    dt[l] <- 0 # exclude l so self-loops are impossible
    if(sum(dt) > 0.5){
      k <- sample.int(n,1,prob=dt)
      v <- TRUE
      d1[k] <- d1[k] - 1
    }else{
      dp <- d
      dp[l] <- 0
      if(sum(dp) < 0.5)
        stop("Cannot construct graph: only one node has positive degree")
      k <- sample.int(n,1,prob=dp)
      v <- FALSE
    }
    d1[l] <- d1[l] - 1
    edges[[length(edges)+1]] <- c(l,k,v)
  }
  el <- do.call(rbind,edges)
  el
}


#' Create an RDS sample from a graph
#' @param el A matrix with three columns, where each row is an edge. Edges are from
#' the first column to the second column, with the third column
#' indicating whether the edge is reciprocated (1 -> reciprocated, 0 -> directed).
#' @param d The degrees of each node
#' @param ns The number of seeds
#' @param g A factor variable for use with biased sampling
#' @param ss The size of the RDS sample
#' @param biased If true, all seeds are drawn from subjects with the highest level of g.
#' @param pr A probability vector for the number of recruits. The first element is the
#' probability that a subject tries to recruit no neighbors. The second element is the probability
#' that a subject tries to recruit 1 neighbor, and so on. Subjects can not recruit more
#' neighbors than they have available.
#' @param seeds seed ids to override other options
#' @param exclude ids to exclude from sampling
#' @details
#' Subjects recruit neighbors at random, with a time delay of rexp(1). Sampling stops
#' when ss is reached. If no recruit is available, a new seed is drawn and 'redraw' is
#' printed to the console.
#' @return
#' A data.frame with columns:
#' 'subject' The index the recruited subject
#' 'recruiter' The index of the recruiter of the subject (-1 if seed)
#' 'time' the time of the subject's recruitment
#' @examples
#' set.seed(1)
#' n <- 1000 #pop size
#' d <- rpois(n,lambda = 3) + 1
#' g <- rep(1,n)
#' el <- make_configuration_graph(d)
#' seeds <- 7 # #of seeds
#' rds <- samp_rds(el, d, seeds,g,200,FALSE, pr = c(0,.1,.9))
#' @export
samp_rds <- function(el, d, ns, g, ss, biased=TRUE,pr = c(0,.1,.9), seeds=NULL, exclude=c()){
  maxR <- length(pr) -1
  n <- length(g)
  if(is.null(seeds)){
    if(biased){
      seeds <- sample.int(n,ns,prob=as.numeric(as.factor(g))-1)
    }else{
      seeds <- sample.int(n,ns, prob = d)
    }
  }
  samp <- seeds
  recr <- rep(-1,ns)
  time <- 0 + (1:ns)/10000000
  rcTime <- rexp(ns)
  t1 <- time
  while(length(samp) < ss){
    subjIndex <- which.min(t1 + rcTime)
    if(length(subjIndex)==0){
      print("redraw")
      candidates <- setdiff(1:n, c(samp, exclude))
      if(length(candidates) == 0)
        stop("No unsampled subjects available for seed redraw.")
      subj <- candidates[sample.int(length(candidates),1)]
      samp <- c(samp,subj)
      recr <- c(recr,-1)
      time <- c(time,max(time+1))
      rcTime <- c(rcTime,rexp(1))
      t1 <- c(t1,max(time+1))
    }else{
      t <- t1[subjIndex] + rcTime[subjIndex]
      t1[subjIndex] <- NA
      subj <- samp[subjIndex]
      nr <- sample(0:maxR,1,replace=FALSE,prob=pr)
      nbrs <- rbind(el[el[,1]==subj,2:3,drop=FALSE],
                    el[el[,2]==subj & el[,3]>0.5,c(1,3),drop=FALSE]
      )
      nbrs <- nbrs[!(nbrs[,1] %in% c(samp, exclude)) & nbrs[,1]!=subj,,drop=FALSE]
      nr <- min(nr,nrow(nbrs))
      if(nr>0){
        s <- sample.int(nrow(nbrs),nr,replace=FALSE)
        s <- s[!duplicated(nbrs[s,1])]
        nr <- length(s)
        samp <- c(samp,nbrs[s,1])
        recr <- c(recr,rep(subj,nr))
        tm <- t + (0:(nr-1)) / 1000000
        time <- c(time,tm)
        t1 <- c(t1,tm)
        rcTime <- c(rcTime,rexp(nr))
      }
    }
  }
  data.frame(subject=samp,recruiter=recr,time=time)
}


#' get the root seed for each subject
#' @param id subject ids
#' @param recruiter.id subject recruiter id
#' @noRd
get_seed <- function(id, recruiter.id){
  sid <- -1
  n <- length(id)
  seed <- rep(NA, n)
  for(j in seq_len(n)){
    if(!is.na(seed[j]))
      next
    chain <- j
    row <- j
    repeat{
      i <- id[row]
      rec.id <- recruiter.id[row]
      if(rec.id == i){
        stop(sprintf("Yikes! The data says that the person with id %s recruited themselves! Please check that the coupon information in the data for that person is correct :-)",i),call.=FALSE)
      }
      if(rec.id == sid){
        seed[chain] <- i
        break
      }
      rec.row <- match(rec.id, id)
      if(is.na(rec.row)){
        stop(sprintf("Recruiter id %s does not match any subject id and is not the seed indicator (-1).", rec.id), call.=FALSE)
      }
      if(rec.row %in% chain){
        stop("Loop found in recruitment tree.")
      }
      if(!is.na(seed[rec.row])){
        seed[chain] <- seed[rec.row]
        break
      }
      chain <- c(chain, rec.row)
      row <- rec.row
    }
  }
  seed
}


#' Estimate population size given privatized (hashed) neighbor identifiers
#' @param subject The integer ids of each subject
#' @param recruiter The integer ids of the recruiter of each subject (-1 for seeds)
#' @param subject_hash The hashed identifier for the subject
#' @param degree The degree of each subject
#' @param nbrs A list, each element indicating the hashed identifier of the neighbors of each subject,
#' or a random subset of those neighbors.
#' @param rho The probability two random individuals have the same hash value.
#' If NULL this is estimated from the number of hash collisions in subject_hash.
#' @return
#' A one-row data.frame with columns:
#' 'estimate': The n_2 population size estimate, adjusted for hashing
#' 'cross_seed_estimate' : The n_3 cross-seed estimate, adjusted for hashing
#' 'rho': The value of rho
#' @references
#' Khan, Bilal; Lee, Hsuan-Wei; Fellows, Ian; Dombrowski, Kirk One-step Estimation of Networked Population Size: Respondent-Driven Capture-Recapture with Anonymity eprint arXiv:1710.03953, 2017
#' @examples
#' set.seed(1)
#' n <- 1000 #pop size
#' hashSize <- 1000
#' rho <- 1 / hashSize
#' hash <- floor(runif(n, min = 0, max=hashSize))
#' d <- rpois(n,lambda = 3) + 1
#' g <- rep(1,n)
#' el <- make_configuration_graph(d)
#' seeds <- 7 # #of seeds
#' rds <- samp_rds(el, d, seeds,g,200,FALSE, pr = c(0,.1,.9))
#' subj_hash <- hash[rds$subject]
#' nbrs <- lapply(1:n, function(i) c(el[el[,1]==i,2],el[el[,2]==i,1]))
#' nbrs2 <- nbrs[rds$subject]
#' nbrs_hash <- lapply(nbrs2,function(x) hash[x])
#' one_step_pse(rds$subject,rds$recruiter,
#'      subj_hash, d[rds$subject], nbrs_hash, rho)
#'
#' #rho estimated from data
#' one_step_pse(rds$subject,rds$recruiter,
#'      subj_hash, d[rds$subject], nbrs_hash)
#' @export
one_step_pse <- function(subject, recruiter, subject_hash, degree, nbrs, rho=NULL){
  if(length(unique(subject)) != length(subject))
    stop("Subject ids must be unique")
  if(anyNA(subject))
    stop("No missing subject identifiers allowed")
  if(anyNA(recruiter))
    stop("No missing recruiter identifiers allowed. Use a value like '-1' for seed subjects")
  n_input <- length(subject)
  if(length(recruiter) != n_input || length(subject_hash) != n_input ||
     length(degree) != n_input)
    stop("subject, recruiter, subject_hash and degree must all have the same length")
  if(anyNA(degree)){
    if(all(is.na(degree)))
      stop("All degrees are missing")
    warning(paste0(sum(is.na(degree)), " missing degrees. Imputing median"))
    degree[is.na(degree)] <- median(degree, na.rm=TRUE)
  }
  if(any(degree <= 0))
    stop("Degrees must be > 0")

  #convert to list if needed
  if(is.data.frame(nbrs) || is.matrix(nbrs)){
    df <- nbrs
    nbrs <- list()
    for(i in 1:nrow(df))
      nbrs[[i]] <- unlist(df[i,])
  }
  if(length(nbrs) != n_input)
    stop("nbrs must have one element (or row) per subject")
  nbrs <- lapply(nbrs, na.omit)

  s2 <- 1:length(subject)
  r2 <- match(recruiter, subject)
  r2[is.na(r2)] <- -1
  subject <- s2
  recruiter <- r2
  seed <- get_seed(subject, recruiter)

  # Drop subjects with missing hashes, preserving the tree membership
  # computed above so recruitment chains survive the removal (mirrors
  # cross_tree_pse). An unmatchable subject left in the sample deflates
  # the observed match count and inflates the estimate.
  if(anyNA(subject_hash)){
    na_hash <- is.na(subject_hash)
    seed <- seed[!na_hash]
    subject <- subject[!na_hash]
    recruiter <- recruiter[!na_hash]
    subject_hash <- subject_hash[!na_hash]
    degree <- degree[!na_hash]
    nbrs <- nbrs[!na_hash]
    s2 <- 1:length(subject)
    r2 <- match(recruiter, subject)
    r2[is.na(r2)] <- -1
    subject <- s2
    recruiter <- r2
  }
  ns <- length(subject)
  if(is.null(rho)){
    # Count hash collisions among the non-missing hashes only: comparing
    # against an NA hash must contribute nothing, not -1.
    obs_hash <- na.omit(subject_hash)
    n_obs <- length(obs_hash)
    if(n_obs < 2)
      stop("Cannot estimate rho: fewer than 2 non-missing subject hashes")
    hash_counts <- table(obs_hash)
    nMatches <- sum(hash_counts * (hash_counts - 1))
    rho <- nMatches / (n_obs*(n_obs-1))
  }
  outSet <- lapply(seq_len(ns), function(j){
    excl <- subject_hash[recruiter == subject[j]]
    if(recruiter[j] != -1)
      excl <- c(excl, subject_hash[match(recruiter[j], subject)])
    nb <- nbrs[[j]]
    for(e in excl){
      i <- which(nb %in% e)
      if(length(i) > 0)
        nb <- nb[-i[1]]
    }
    nb
  })
  dSample <- mean(degree) - 1
  dPopulation <- ns / sum(1/degree)
  matchSet <- lapply(outSet, function(x) x[x %in% subject_hash])
  NTilda <- function(N){
    wts <-  lapply(matchSet, function(x){
      wts1 <- list()
      for(a in x){
        mId <- which(subject_hash == a)
        wts1[[length(wts1) + 1]] <- 1 / (dPopulation * rho * (N - 1) / (degree[mId] - 1) + 1)
      }
      wts1
    })
    wts <- unlist(wts)
    wts[is.nan(wts)] <- 1 # degree[mId] == 1 and rho == 0. There technically shouldn't be a match since subject only knows recruiter
    m <- sum(wts)
    (dSample / dPopulation) * ns * length(unlist(outSet)) / m
  }
  if(NTilda(2) < 2){
    opt <- list(root=0)
  }else if(NTilda(7000000000) > 7000000000){
    opt <- list(root=Inf)
  } else{
    opt <- uniroot(function(N) NTilda(N) - N, interval=c(2, 7000000000))
  }

  # seed (tree membership) was computed before the NA-hash drop above
  seedIds <- unique(seed)
  outSets <- list()
  matchSets <- list()
  for(s in seedIds){
    outSets[[s]] <- lapply(which(seed == s), function(j){
      excl <- subject_hash[recruiter == subject[j]]
      if(recruiter[j] != -1)
        excl <- c(excl, subject_hash[match(recruiter[j], subject)])
      nb <- nbrs[[j]]
      for(e in excl){
        i <- which(nb %in% e)
        if(length(i) > 0)
          nb <- nb[-i[1]]
      }
      nb
    })
    matchSets[[s]] <- lapply(outSets[[s]], function(x) x[x %in% subject_hash[seed != s]])
  }
  NTildaCross <- function(N){
    crossSeedEstimateNum <- 0
    crossSeedEstimateDenom <- 0
    for(s in seedIds){
      outSet <- outSets[[s]]
      matchSet <- matchSets[[s]]
      if(is.null(outSet))
        next
      wts <-  lapply(matchSet, function(x){
        wts1 <- list()
        for(a in x){
          mId <- which(subject_hash == a & seed != s)
          wts1[[length(wts1) + 1]] <- 1 / (dPopulation * rho * (N - 1) / (degree[mId] - 1) + 1)
        }
        wts1
      })
      wts <- unlist(wts)
      wts[is.nan(wts)] <- 1
      m <- sum(wts)
      dSample <- mean(degree[seed != s]) - 1
      dPopulation <- ns / sum(1/degree)
      n_without <- sum(seed != s & !is.na(subject_hash))
      crossSeedEstimateNum <- crossSeedEstimateNum + length(unlist(outSet)) *
        n_without * dSample / dPopulation
      crossSeedEstimateDenom <- crossSeedEstimateDenom + m
    }
    crossSeedEstimateNum / crossSeedEstimateDenom
  }
  if(NTildaCross(2) < 2){
    optCross <- list(root=0)
  }else if(NTildaCross(7000000000) > 7000000000){
    optCross <- list(root=Inf)
  } else{
    optCross <- uniroot(function(N) NTildaCross(N) - N, interval=c(2, 7000000000))
  }
  result <- data.frame(
    estimate=opt$root,
    cross_seed_estimate= optCross$root,
    rho=rho
  )
  result
}





#' Estimate population size given privatized (hashed) neighbor identifiers
#' @param subject The integer ids of each subject
#' @param recruiter The integer ids of the recruiter of each subject (-1 for seeds)
#' @param subject_hash The hashed identifier for the subject
#' @param degree The degree of each subject
#' @param nbrs A list, each element indicating the hashed identifier of the neighbors of each subject,
#' or a random subset of those neighbors.
#' @param rho The probability two random individuals have the same hash value. If NULL this is estimated from the number of hash collisions in subject_hash.
#' @param method combined uses both the sample and the nominated alters for potential matches. "sample" uses only recruited individuals and "alter" uses only their nominated alters.
#' @param small_sample_fraction If TRUE, simplifies estimation by assuming a small sample fraction.
#' @return
#' A data.frame with elements:
#' 'estimate': The population size estimate, adjusted for hashing
#' 'rho': The hash collision probability
#' 'num_matches': The number of identifier matches
#' 'num_poss_matches': The number of identifier match opportunities
#' @examples
#' data(faux_pns)
#' #rho estimated from data
#' cross_tree_pse(faux_pns$subject,faux_pns$recruiter,
#'                faux_pns$subject_hash, faux_pns$degree,
#'                faux_pns[paste0("friend_hash",1:11)])
#'
#'
#' # fixed rho
#' cross_tree_pse(faux_pns$subject,faux_pns$recruiter,
#'                faux_pns$subject_hash, faux_pns$degree,
#'                faux_pns[paste0("friend_hash",1:11)], rho=.001)
#' @export
cross_tree_pse <- function(
  subject,
  recruiter,
  subject_hash,
  degree,
  nbrs,
  rho=NULL,
  method = c("network","alter","sample"),
  small_sample_fraction=TRUE){

  method <- match.arg(method)
  add_alter <- method %in% c("network","alter")
  add_sample <- method %in% c("network","sample")

  if(length(unique(subject)) != length(subject))
    stop("Subject ids must be unique")
  if(anyNA(subject))
    stop("No missing subject identifiers allowed")
  if(anyNA(recruiter))
    stop("No missing recruiter identifiers allowed. Use a value like '-1' for seed subjects")
  n_input <- length(subject)
  if(length(recruiter) != n_input || length(subject_hash) != n_input ||
     length(degree) != n_input)
    stop("subject, recruiter, subject_hash and degree must all have the same length")

  #convert to list if needed
  if(is.data.frame(nbrs) || is.matrix(nbrs)){
    df <- nbrs
    nbrs <- list()
    for(i in 1:nrow(df))
      nbrs[[i]] <- unlist(df[i,])
  }
  if(length(nbrs) != n_input)
    stop("nbrs must have one element (or row) per subject")
  nbrs <- lapply(nbrs, na.omit)

  # Put seeds first
  seed_order <- order(recruiter %in% subject)
  subject <- subject[seed_order]
  recruiter <- recruiter[seed_order]
  subject_hash <- subject_hash[seed_order]
  degree <- degree[seed_order]
  nbrs <- nbrs[seed_order]

  # Reorganize identifiers so they are 1:n, with seeds at the start
  ns <- length(subject)
  s2 <- 1:ns
  r2 <- match(recruiter, subject)
  r2[is.na(r2)] <- -1
  subject <- s2
  recruiter <- r2
  ##
  #
  # subject ids are now reorganized so that they are 1:ns with the seeds at the start.
  # variables are sorted by subject id.
  #
  ##
  seed <- get_seed(subject,recruiter)

  # Handle missing hashes
  if(anyNA(subject_hash)){
    na_hash <- is.na(subject_hash)
    seed <- seed[!na_hash]
    subject <- subject[!na_hash]
    recruiter <- recruiter[!na_hash]
    subject_hash <- subject_hash[!na_hash]
    degree <- degree[!na_hash]
    nbrs <- nbrs[!na_hash]
    ns <- length(subject)
    s2 <- 1:ns
    r2 <- match(recruiter, subject)
    r2[is.na(r2)] <- -1
    subject <- s2
    recruiter <- r2
  }

  if(anyNA(degree)){
    warning(paste0(sum(is.na(degree)), " missing degrees. Imputing median"))
    degree[is.na(degree)] <- median(degree, na.rm=TRUE)
  }
  if(any(degree <= 0)){
    stop("Degrees must be > 0")
  }

  seed_ids <- sort(unique(seed))

  # Calculate hash collision probability if not specified (missing hashes
  # were dropped above, so every pairwise comparison is well defined)
  if(is.null(rho)){
    hash_counts <- table(subject_hash)
    n_matches <- sum(hash_counts * (hash_counts - 1))
    rho <- n_matches / (ns*(ns-1))
  }

  # construct out edge ends, matches and degres of matches
  out_sets <- list()
  match_sets <- list()
  match_degrees <- list()
  match_nonseed <- list()
  o <- rep(0, max(seed_ids))
  for(s in seed_ids){
    out_sets[[s]] <- lapply(which(seed == s), function(j){
      excl <- subject_hash[recruiter == subject[j]]
      if(recruiter[j] != -1)
        excl <- c(excl, subject_hash[match(recruiter[j], subject)])
      nb <- nbrs[[j]]
      for(e in excl){
        i <- which(nb %in% e)
        if(length(i) > 0)
          nb <- nb[-i[1]]
      }
      nb
    })
    match_sets[[s]] <- lapply(out_sets[[s]], function(x) x[x %in% subject_hash[seed != s]])
    match_degrees[[s]] <-  unlist(lapply(match_sets[[s]], function(x){
      deg <- list()
      for(a in x){
        mId <- which(subject_hash == a & seed != s)
        deg[[length(deg) + 1]] <- degree[mId]
      }
      deg
    }))
    # 1 if the matched subject is a non-seed (has a recruiter), 0 for seeds:
    # the I(R_i != -1) in the manuscript's match-weight denominator
    match_nonseed[[s]] <- unlist(lapply(match_sets[[s]], function(x){
      ind <- list()
      for(a in x){
        mId <- which(subject_hash == a & seed != s)
        ind[[length(ind) + 1]] <- as.numeric(recruiter[mId] != -1)
      }
      ind
    }))

    if(length(match_degrees) < s || is.null(match_degrees[[s]]))
      match_degrees[[s]] <- numeric()
    if(length(match_nonseed) < s || is.null(match_nonseed[[s]]))
      match_nonseed[[s]] <- numeric()
    o[s] <- length(unlist(out_sets[[s]]))
  }

  # Cross-tree nomination matches, counted by tabulating each tree's
  # nominations against the pooled nominations (O(total nominations))
  # rather than comparing every pair; match() shares ==-equality
  # semantics so the counts are identical to the pairwise loop.
  cross_net_matches <- rep(0, max(seed_ids))
  o_mc <- rep(0, max(seed_ids))
  all_out <- unlist(out_sets)
  out_vals <- unique(all_out)
  total_count <- tabulate(match(all_out, out_vals), nbins = length(out_vals))
  for(s in seed_ids){
    tree_count <- tabulate(match(unlist(out_sets[[s]]), out_vals),
                           nbins = length(out_vals))
    cross_net_matches[s] <- sum(tree_count * (total_count - tree_count))
    o_mc[s] <- sum(o[-s])
  }

  # Estimating equation
  N_tilda <- function(N){
    if(small_sample_fraction){
      deg_wts <- 1 / degree
    }else{
      # Gile SS weights via RDS2's deterministic fixed-point solver.
      # (RDS::gile.ss.weights was Monte Carlo and consumed the RNG
      # stream, which required saving/restoring .Random.seed here.)
      deg_wts <- RDS2::ss_fixed_point(degree, population_size = max(ns, N))$weights
    }
    d_population <- sum(deg_wts * degree) / sum(deg_wts)
    d_tilda <- sum(deg_wts * degree * degree) / sum(deg_wts * degree)
    expected_matches <- 0
    observed_matches <- 0
    if(add_alter){
      ptrue <- sapply(degree,
                      function(deg) ((rho * (d_population*(N-1) - ns + length(seed_ids))) /
                                       (deg - 1) + 1)^(-1))
      ptrue[degree == 1] <- 0
      p_1 <- sum(deg_wts * degree^2 * ptrue) / sum(deg_wts * degree^2)
      p_2 <- sum(deg_wts * degree * ptrue) / sum(deg_wts * degree)
      phi_value <- p_2 / (1 - p_1 + p_2)
      if(phi_value <= 0) return(N-1)
      expected_matches <- (d_tilda - 1) * sum(o*o_mc) / (N * d_population - (ns - length(seed_ids)))
      observed_matches <- phi_value * sum(cross_net_matches)
      if(sum(cross_net_matches) == 0) return(Inf)
    }
    if(add_sample){
      for(s in seed_ids){
        out_set <- out_sets[[s]]
        match_set <- match_sets[[s]]

        if(is.null(out_set))
          next
        # Manuscript section 3.2 match weight: numerator includes the
        # -n+t correction (as does ptrue above); denominator subtracts an
        # edge end only for non-seeds, I(R_i != -1).
        wts <- 1 / (rho * (d_population * (N - 1) - ns + length(seed_ids)) /
                      (match_degrees[[s]] - match_nonseed[[s]]) + 1)
        wts[is.nan(wts)] <- 1
        m <- sum(wts)

        n_within <- sum(seed == s & !is.na(subject_hash))
        d_sample <- mean(degree[seed == s]) - (n_within - 1) / n_within
        expected_matches <- expected_matches + sum(na.omit(o[-s])) * d_sample * n_within /
          (N * d_population - (ns - length(seed_ids)) )
        observed_matches <- observed_matches + m
      }
    }
    expected_matches - observed_matches
  }

  # Find estimating equation solution
  if(N_tilda(ns) < 0){
    opt <- list(root=ns)
  }else if(N_tilda(7000000000) > 0){
    opt <- list(root=Inf)
  } else{
    opt <- uniroot(function(N) N_tilda(N), interval=c(ns, 7000000000))
  }

  # calculate summary statistics (number of matches out of the number of possible matches)
  nmatch <- 0
  nposs <- 0
  if(add_alter){
    nmatch <- sum(cross_net_matches)
    nposs <- sum(o*o_mc)
  }
  if(add_sample){
    nmatch <- nmatch + length(unlist(match_degrees))
    for(s in seed_ids){
      n_within <- sum(seed == s & !is.na(subject_hash))
      nposs <- nposs + sum(na.omit(o[-s])) * n_within
    }
  }


  result <- data.frame(
    estimate= opt$root,
    rho=rho,
    num_matches = nmatch,
    num_poss_matches = nposs
  )
  result
}


#' Estimate population size given privatized (hashed) neighbor identifiers with bootstrap confidence intervals
#' @param subject The integer ids of each subject
#' @param recruiter The integer ids of the recruiter of each subject (-1 for seeds)
#' @param subject_hash The hashed identifier for the subject
#' @param degree The degree of each subject
#' @param nbrs A list, each element indicating the hashed identifier of the neighbors of each subject,
#' or a random subset of those neighbors.
#' @param rho The probability two random individuals have the same hash value.
#' If NULL this is estimated from the number of hash collisions in subject_hash.
#' @param method combined uses both the sample and the nominated alters for potential matches. "sample" uses only recruited individuals and "alter" uses only their nominated alters.
#' @param small_sample_fraction If TRUE, simplifies estimation by assuming a small sample fraction.
#' @param n_bootstrap The number of bootrap samples used to estimate the confidence intervals
#' @param conf_level The coverage level for the confidence intervals
#' @param progress Show a text progress bar
#' @return
#' A data frame with the population size values, CI lower and CI upper bounds.
#' @examples
#' data(faux_pns)
#'
#' #hashes
#' cross_tree_pse(faux_pns$subject,faux_pns$recruiter,
#'                faux_pns$subject_hash, faux_pns$degree,
#'                faux_pns[paste0("friend_hash",1:11)], rho=.001)
#'
#' # Set n_bootstrap much higher in practice
#' bootstrap_pse(faux_pns$subject,faux_pns$recruiter,
#'                faux_pns$subject_hash, faux_pns$degree,
#'                faux_pns[paste0("friend_hash",1:11)], rho=.001, n_bootstrap=10)
#' @export
bootstrap_pse <- function(
  subject,
  recruiter,
  subject_hash,
  degree,
  nbrs,
  rho=NULL,
  method = c("network","alter","sample"),
  small_sample_fraction = TRUE,
  n_bootstrap=500,
  conf_level = .95,
  progress = TRUE){

  if(is.logical(progress) && progress){
    pb <- txtProgressBar(min = 0, max = n_bootstrap, style = 3)
    progress_function <- function(i){
      setTxtProgressBar(pb, i)
    }
    on.exit(close(pb))
  }else if(!is.logical(progress)){
    progress_function <- progress
    progress <- TRUE
  }
  method <- match.arg(method)
  add_alter <- method %in% c("network","alter")
  add_sample <- method %in% c("network","sample")

  if(length(unique(subject)) != length(subject))
    stop("Subject ids must be unique")
  if(anyNA(subject))
    stop("No missing subject identifiers allowed")
  if(anyNA(recruiter))
    stop("No missing recruiter identifiers allowed. Use a value like '-1' for seed subjects")

  # Get point estimates of population size and rho
  point_estimate <- cross_tree_pse(subject, recruiter, subject_hash, degree, nbrs, rho,
                                   method=method, small_sample_fraction=small_sample_fraction)[1:2]

  # Reorganize identifiers so they are 1:n, with seeds at the start
  ns <- length(subject)
  s2 <- 1:ns
  r2 <- match(recruiter, subject)
  r2[is.na(r2)] <- -1
  subject <- s2
  recruiter <- r2

  #convert to list if needed
  if(is.data.frame(nbrs) || is.matrix(nbrs)){
    df <- nbrs
    nbrs <- list()
    for(i in 1:nrow(df))
      nbrs[[i]] <- unlist(df[i,])
  }
  nbrs <- lapply(nbrs, na.omit)

  if(is.infinite(point_estimate$estimate)){
    warning("Infinite point estimate. Bootstrap not performed.")
    null_result <- structure(
      list(
        name = c("estimate", "1 / rho"),
        value = c(point_estimate$estimate, 1 / point_estimate$rho),
        ci_lower_bound = c(NA, NA),
        ci_upper_bound = c(NA, NA)),
      row.names = c(NA, -2L),
      class = "data.frame",
      bootstrap_samples = matrix(numeric(),0,2),
      conf_level = conf_level)
    return(null_result)
  }
  # Get the number of alter ids recorded excluding recruiter and recrutee
  s2 <- 1:length(subject)
  r2 <- match(recruiter, subject)
  r2[is.na(r2)] <- -1
  seed <- get_seed(s2, r2)

  # Mirror the estimators (CODE_REVIEW.md 2.9): subjects with missing
  # hashes are dropped from the estimation sample, so the bootstrap world
  # -- tree sizes, degree pool and nomination rates -- is built from the
  # retained subjects only. Tree membership is computed above, before the
  # drop, so recruitment chains survive the removal.
  if(anyNA(subject_hash)){
    keep <- !is.na(subject_hash)
    old_ids <- s2[keep]
    seed <- seed[keep]
    subject_hash <- subject_hash[keep]
    degree <- degree[keep]
    nbrs <- nbrs[keep]
    s2 <- seq_along(old_ids)
    r2 <- match(r2[keep], old_ids)
    r2[is.na(r2)] <- -1
  }

  # lapply, never apply: with a uniform number of free alters per subject,
  # apply() returns a matrix and sapply(., length) below silently produces
  # per-cell 1s instead of per-subject counts (CODE_REVIEW.md 1.2)
  free_nbrs <- lapply(s2, function(j){
    excl <- subject_hash[r2 == j]
    if(r2[j] != -1)
      excl <- c(excl, subject_hash[match(r2[j], s2)])
    nb <- nbrs[[j]]
    for(e in excl){
      i <- which(nb %in% e)
      if(length(i) > 0)
        nb <- nb[-i[1]]
    }
    nb
  })
  n_free_nbrs <- sapply(free_nbrs, length)
  tree_sizes <- table(seed)
  tree_seed_ids <- as.numeric(names(tree_sizes))

  # calculate the rate at which alters are nominated
  nfn_by_seed <- sapply(tree_seed_ids, function(sd){
    sum(n_free_nbrs[seed == sd],na.rm=TRUE)
  })
  maxfn_by_seed <- sapply(tree_seed_ids, function(sd){
    nn <- sum(degree[seed == sd],na.rm=TRUE) - 2 * (sum(seed == sd) - 1)
    nfn <- sum(n_free_nbrs[seed == sd],na.rm=TRUE)
    if(nn < nfn){
      nn <- nfn
    }
    nn
  })
  # A tree can have no free edge ends at all (e.g. two degree-1 subjects):
  # 0/0 would give NaN and NA-select nominations downstream; the observed
  # rate of an empty tree is 0 (CODE_REVIEW.md 2.9)
  nom_rate_by_seed <- ifelse(maxfn_by_seed > 0, nfn_by_seed / maxfn_by_seed, 0)

  # generate one bootstrap sample
  bootstrap <- function(n, rho, rho_known){

    id <- 1:n
    ind <- sample.int(length(degree),n,TRUE, prob = ifelse(is.na(degree) | degree <= 0,0, 1 / degree))
    pop_degree <- degree[ind]

    # draw sample
    samp <- pns_sample(pop_degree, tree_sizes)

    # match observed alter nomination rates
    bs_nbrs <- samp$nbrs
    for(i in seq_along(bs_nbrs)){
      alters <- bs_nbrs[[i]]
      # Remove this subject's recruits -- every copy, while preserving
      # duplicate edges to others (setdiff would silently dedupe
      # multi-edges). The recruiter edge is intentionally left in and
      # thinned like any other nomination; cross_tree_pse excludes one
      # recruiter instance from the nominations downstream. The
      # asymmetry (recruits removed here, recruiter handled by the
      # estimator) mirrors how the observed nomination rate was
      # computed from free alters (CODE_REVIEW.md 2.9).
      alters <- alters[!(alters %in% samp$subject[samp$recruiter == samp$subject[i]])]
      alters <- alters[runif(length(alters)) < nom_rate_by_seed[samp$seed[i]]]
      bs_nbrs[[i]] <- alters
    }

    # hash ids
    if(rho == 0){
      hash <- 1:length(pop_degree)
      nbrs_hash <- bs_nbrs
    }else{
      hash <- floor(runif(n, min = 0, max=1 / rho))
      nbrs_hash <- lapply(bs_nbrs, function(x) hash[x])
    }
    subj_hash <- hash[samp$subject]
    degree <- pop_degree[samp$subject]
    rho1 <- if(rho_known) rho else NULL
    unlist(cross_tree_pse(samp$subject, samp$recruiter, subj_hash, degree, nbrs_hash, rho1,
                             method=method,small_sample_fraction=small_sample_fraction)[1:2])
  }

  boots <- matrix(NA, n_bootstrap, 2)
  n_failed <- 0
  for(i in 1:n_bootstrap){
    if(progress)
      progress_function(i)
    b1 <- tryCatch(
      bootstrap(round(point_estimate$estimate), point_estimate$rho, !is.null(rho)),
      error = function(e) NULL)
    if(is.null(b1))
      n_failed <- n_failed + 1
    else
      boots[i,] <- b1
  }
  if(n_failed == n_bootstrap)
    stop("All ", n_bootstrap, " bootstrap replicates failed")
  if(n_failed > 0)
    warning(n_failed, " of ", n_bootstrap,
            " bootstrap replicates failed and were dropped")
  crit <- -qnorm((1-conf_level)/2)

  estimates <- c( log(point_estimate$estimate), point_estimate$rho)

  # robust standard error assuming normal distribution
  sd_norm <- function(x){
    as.vector(diff(quantile(x, c(pnorm(-1),pnorm(1)), na.rm=TRUE))/2)
  }
  sd_vals <- c(sd_norm(log(boots[,1])), sd_norm(boots[,2]))
  lower <- estimates - crit * sd_vals
  upper <- estimates + crit * sd_vals

  # Fall back on quantiles if lots of infinites
  if(!is.finite(lower[1])){
    lower[1] <- log(quantile(boots[,1],(1-conf_level)/2, na.rm=TRUE))
  }
  if(!is.finite(upper[1])){
    upper[1] <- log(quantile(boots[,1], 1-(1-conf_level)/2, na.rm=TRUE))
  }
  # rho is bounded below by zero: clamp before inverting so the 1/rho
  # interval stays ordered and non-negative (upper bound Inf when the
  # rho interval reaches 0)
  if(is.finite(lower[2]) && lower[2] < 0)
    lower[2] <- 0

  result <- data.frame(
    name = c("estimate", "1 / rho"),
    value = estimates,
    ci_lower_bound = lower,
    ci_upper_bound = upper
  )
  result[1,2:4] <- exp(result[1,2:4])
  result[2,2:4] <- 1 / result[2,2:4]
  tmp <- result[2,4]
  result[2,4] <- result[2,3]
  result[2,3] <- tmp
  attr(result,"bootstrap_samples") <- boots
  attr(result, "conf_level") <- conf_level
  result
}

# pop_degree : the degrees of the population
# n_seed : The number recruited by each seed
pns_sample <- function(pop_degree, n_seed){
  n <- sum(n_seed)
  N <- length(pop_degree)

  r <- s <- rep(-1, n)
  alt <- list()
  for(i in 1:N){
    alt[[i]] <- numeric()
  }
  d <- pop_degree
  seed <- rep(0, N)

  # draw seeds; already-sampled subjects have d == 0 and so cannot be redrawn
  j <- 1
  while(j <= length(n_seed)){
    s[j] <- sample.int(N, 1, prob = d)
    alt[[s[j]]] <- sample.int(N, d[s[j]], prob = d)
    for(i in alt[[s[j]]]){
      alt[[i]] <- c(alt[[i]], s[j])
      d[i] <- d[i] - 1
    }
    d[s[j]] <- 0
    n_seed[j] <- n_seed[j] - 1
    seed[s[j]] <- j
    j <- j + 1
  }

  #draw sample from seeds
  while(sum(n_seed) > 0){
    # select a seed tree to recruit from
    sd <- sample.int(length(n_seed), 1, prob=n_seed)
    found <- FALSE
    k <- 1
    while(!found){
      k <- k + 1
      if(k > 5000){
        # No free edge ends: give up on one recruit for this tree. The
        # returned sample will be smaller than requested.
        warning("Free edge end search failed in pns sampling.")
        n_seed[sd] <- n_seed[sd] - 1
        break
      }

      # draw a random individual from the seed tree to be a recruiter
      recr <- sample.int(N, 1, prob=seed==sd)

      # add a free neighbor if recruitment from the tree has been exhausted;
      # account for the new edge on both ends so the neighbor's remaining
      # stub count stays consistent (CODE_REVIEW.md 2.9)
      if(length(setdiff(unlist(alt[seed == sd]), s)) == 0){
        if(sum(d) <= 0)
          stop("No free edge ends remain in the bootstrap population")
        nnbr <- sample.int(N, 1, prob = d)
        alt[[recr]] <- c(alt[[recr]], nnbr)
        alt[[nnbr]] <- c(alt[[nnbr]], recr)
        d[nnbr] <- d[nnbr] - 1
      }

      # the alters of the recruiter who have not been recruited
      fnbr <- setdiff(alt[[recr]], s)

      if(length(fnbr) > 0){
        found <- TRUE

        # follow a random edge from the recruiter to a new subject
        s[j] <- fnbr[sample.int(length(fnbr),1)]
        r[j] <- recr

        # draw alters for the new subject
        nnbr <- sample.int(N, d[s[j]], prob = d)
        alt[[s[j]]] <- c(alt[[s[j]]], nnbr)
        for(i in nnbr){
          alt[[i]] <- c(alt[[i]], s[j])
          d[i] <- d[i] - 1 # one edge for each alter is now accounted for (i.e. known to go to the PNS sample)
        }
        d[s[j]] <- 0 # all edges from subject accounted for
        n_seed[sd] <- n_seed[sd] - 1
        seed[s[j]] <- sd
        j <- j + 1
      }
    }
  }
  r <- r[s != -1]
  s <- s[s != -1]
  list(
    subject = s,
    recruiter = r,
    nbrs = alt[s],
    degree = pop_degree[s],
    seed = seed[s]
  )
}

#' Calculates relevant summary statistics for PNS
#' @param subject_hash The hashed identifier for the subject
#' @param nbrs A list, each element indicating the hashed identifier of the neighbors of each subject,
#' or a random subset of those neighbors.
#' @returns A list with elements:
#'    'unique' - 'unique_nbrs' The total number of unique hash values in the nominations.
#'    'unique' - 'sample_size' The sample size of the PNS
#'    'unique' - 'total_unique_ident' The unique nominations plus the number of subjects whose hashes
#'    do not appear in the nominations. This represents the total number of hashed identities directly
#'    identified by sampling (either through recruitment or nomination) and is thus a hard lower bound
#'    on population size.
#'
#'    'neighbors' - 'total_nbrs' The number of nominations
#'    'neighbors' - 'unique_nbrs' The number of unique nominations
#'
#'    'naive_crc_estimate' - 'unique_nbrs' The number of unique nominations
#'    'naive_crc_estimate' - 'sample_size' The sample size of the PNS
#'    'naive_crc_estimate' - 'unique_nbrs_sample_overlap' The number of nominations that match a hash in the sample
#'    'naive_crc_estimate' - 'N' Treating the sample and the nominations as two independent capture events,
#'    this is the population size estimate. This should be treated as a descriptive statistic and not a true
#'    estimate as the assumption does not hold.
#' @examples
#' data(faux_pns)
#'
#' overlap_statistics(faux_pns$subject, faux_pns[paste0("friend_hash",1:11)])
#' @export
overlap_statistics <- function(subject_hash, nbrs){
  subject_hash <- na.omit(as.character(subject_hash))
  nsamp <- length(subject_hash)
  # A plain list (the documented input type) must be flattened element by
  # element; as.matrix() on a list would deparse each element to a string
  # like "c(\"a\", \"b\")" and nothing would ever match (CODE_REVIEW.md 2.7)
  if(is.list(nbrs) && !is.data.frame(nbrs)){
    nbrs <- unlist(nbrs, use.names = FALSE)
  }else{
    nbrs <- as.matrix(nbrs)
  }
  nbrs <- na.omit(as.character(nbrs))
  unbrs <- unique(nbrs)
  un <- length(unbrs)
  nbrs_in_sample <- sum(unbrs %in% subject_hash)
  unbrs <- unbrs[!(unbrs %in% subject_hash)]
  u <- length(unbrs)
  n <- length(nbrs)


  res <- list(
    unique = data.frame(
      unique_nbrs = un,
      sample_size = nsamp,
      total_unique_ident = u + nsamp
    ),
    neighbors = data.frame(
      total_nbrs = n,
      unique_nbrs = un
    ),
    naive_crc_estimate = data.frame(
      unique_nbrs = un,
      sample_size = nsamp,
      unique_nbrs_sample_overlap = nbrs_in_sample,
      N = un * nsamp / nbrs_in_sample
    )
  )
  res
}

check_packages <- function(pkgs){
  installed_pkgs <- utils::installed.packages() |> rownames()
  miss <- pkgs[!(pkgs %in% installed_pkgs)]
  if(length(miss) == 0)
    return(FALSE)
  if(interactive()){
    cat("The following required pacakges are not installed: ", paste(miss, collapse = ", "))
    install <- readline("Install now (y/n/c)?")
    if(install != "y")
      stop("missing packages")
  }
  utils::install.packages(miss)
  return(TRUE)
}

#' Launches the Shiny PNS Application
#' @param ... Additional parameters to be passed to shiny::runApp
#' @export
shiny_pnspop <- function(...){
  app_dir <- system.file("shinyui", package = "pnspop")
  if (app_dir == "") {
    stop("Could not find example directory. Try re-installing `shinyrecap`.", call. = FALSE)
  }
  check_packages(
    c("shinyWidgets", "shinyhelper", "promises", "future", "ipc", "ggplot2",
      "DT")
  )
  shiny::runApp(app_dir, display.mode = "normal", ...)
}


#' This is example pns data
#'
#' @name faux_pns
#' @docType data
#' @keywords data
#' @aliases rds nbrs_hash nbrs2 rho subj_hash
#' @examples
#' data(faux_pns)
#' #rho estimated from data
#'
#'
#' #hashes
#' cross_tree_pse(faux_pns$subject,faux_pns$recruiter,
#'      faux_pns$subject_hash, faux_pns$degree,
#'      faux_pns[paste0("friend_hash",1:11)],
#'      rho=.001)


#' @importFrom stats pnorm quantile
NULL
