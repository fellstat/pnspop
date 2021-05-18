
#' @importFrom stats rexp uniroot median na.omit qnorm runif sd
#' @importFrom utils setTxtProgressBar txtProgressBar
NULL

#' Constructs a configuration graph
#' @param d a vector of degrees for each node
#' @return A matrix with 3 columns and length(d) rows,
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
  edges <- list()
  d1 <- d
  while(sum(d1)>0.5){
    l <- sample.int(n,1,prob=d1)
    dt <- d1
    dt[l] <- d1[l] - 1
    if(sum(dt) > 0.5){
      k <- sample.int(n,1,prob=dt)
      v <- TRUE
      d1[k] <- d1[k] - 1
    }else{
      k <- sample.int(n,1,prob=d)
      v <- FALSE
    }
    d1[l] <- d1[l] - 1
    edges[[length(edges)+1]] <- c(l,k,v)
  }
  el <- do.call(rbind,edges)
  el
}


#' Create and RDS sample from a graph
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
#' that a subject tries to recruit 1 neightbor, and so on. Subjects can not recruit more
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
  ml <- if(is.factor(g)) max(levels(g)) else max(g)
  if(is.null(seeds)){
    if(biased){
      seeds <- sample.int(n,ns,prob=as.numeric(as.factor(g))-1)
    }else{
      seeds <- sample.int(n,ns, prob = d)
    }
  }
  samp <- c(exclude,seeds)
  recr <- rep(-1,ns)
  time <- 0 + (1:ns)/10000000
  rcTime <- rexp(ns)
  t1 <- time
  while(length(samp) < ss){
    subjIndex <- which.min(t1 + rcTime)
    if(length(subjIndex)==0){
      print("redraw")
      subj <- sample( (1:n)[-samp],1)
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
      nbrs <- nbrs[!(nbrs[,1] %in% samp) & nbrs[,1]!=subj,,drop=FALSE]
      nr <- min(nr,nrow(nbrs))
      if(nr>0){
        s <- sample.int(nrow(nbrs),nr,replace=FALSE)
        s <- s[!duplicated(nbrs[s,1])]
        nr <- length(s)
        samp <- c(samp,nbrs[s,1])
        recr <- c(recr,rep(subj,nr))
        tm <- t + t + (0:(nr-1)) / 1000000
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
get_seed <- function(id, recruiter.id){
  sid <- -1
  get.seed <- function(i, history) {
    row <- match(i, id)
    rec.id <- recruiter.id[row]
    if(rec.id==i){
      stop(sprintf("Yikes! The data says that the person with id %s recruited themselves! Please check that the coupon information in the data for that person is correct :-)",i),call.=FALSE)}
    if(rec.id %in% history){
      stop("Loop found in recruitment tree.")
    }
    if (rec.id == sid) {
      return(i)
    }
    else {
      get.seed(rec.id,history=c(history,i))
    }
  }
  seed <- sapply(id, get.seed,history=c())
  seed
}

# #' Estimate population size given neighbor identifiers
# #' @param subject The integer ids of each subject
# #' @param recruiter The integer ids of the recruiter of each subject (-1 for seeds)
# #' @param degree The degree of each subject
# #' @param nbrs A list, each element indicating the ids of the neighbors of each subject,
# #' or a random subset of those neighbors
# #' @return
# #' A list with elements:
# #' 'no_degree_estimate': The naive n_1 capture re-capture estimate
# #' 'estimate': The n_2 population size estimate
# #' 'cross_seed_estimate' : The n_3 cross-seed estimate
# #' @references
# #' Khan, Bilal; Lee, Hsuan-Wei; Fellows, Ian; Dombrowski, Kirk One-step Estimation of Networked Population Size: Respondent-Driven Capture-Recapture with Anonymity eprint arXiv:1710.03953, 2017
# #' @examples
# #' set.seed(1)
# #' n <- 1000 #pop size
# #' d <- rpois(n,lambda = 3) + 1
# #' g <- rep(1,n)
# #' el <- make_configuration_graph(d)
# #' seeds <- 7 # #of seeds
# #' rds <- samp_rds(el, d, seeds,g,200,FALSE, pr = c(0,.1,.9))
# #' nbrs <- lapply(1:n, function(i) c(el[el[,1]==i,2],el[el[,2]==i,1]))
# #' nbrs2 <- nbrs[rds$subject]
# #' population_estimate(rds$subject,rds$recruiter, d[rds$subject], nbrs2)
# #' @export
# population_estimate <- function(subject, recruiter, degree, nbrs){
#   nbrs2 <- list()
#   nbrs2[subject] <- nbrs
#   nbrs <- nbrs2
#   ns <- length(subject)
#   outSet <- do.call(c, apply(cbind(subject,recruiter), 1, function(x){
#     excl <- c(x[2], subject[recruiter == x[1]])
#     nb <- nbrs[[x[1]]]
#     for(e in excl){
#       i <- which(nb %in% e)
#       if(length(i) > 0)
#         nb <- nb[-i[1]]
#     }
#     nb
#   }))
#   m <- length(outSet[outSet %in% subject])
#   dSample <- mean(degree) - 1
#   dPopulation <- ns / sum(1/degree)
#
#   result <- list(
#     estimate=(dSample / dPopulation) * ns * length(outSet) / m ,
#     no_degree_estimate=ns * length(outSet) / m
#   )
#   seed <- get_seed(subject,recruiter)
#   seedIds <- unique(seed)
#   crossSeedEstimateNum <- 0
#   crossSeedEstimateDenom <- 0
#   for(s in seedIds){
#     outSet <- do.call(c, as.list(apply(cbind(subject[seed == s],recruiter[seed == s]), 1, function(x){
#       excl <- c(x[2], subject[recruiter == x[1]])
#       nb <- nbrs[[x[1]]]
#       for(e in excl){
#         i <- which(nb %in% e)
#         if(length(i) > 0)
#           nb <- nb[-i[1]]
#       }
#       nb
#     })))
#     if(is.null(outSet))
#       next
#     m <- length(outSet[outSet %in% subject[seed != s]])
#     dSample <- mean(degree[seed != s]) - 1
#     dPopulation <- ns / sum(1/degree)
#     crossSeedEstimateNum <- crossSeedEstimateNum + length(outSet) *
#       length(subject[seed != s]) * dSample / dPopulation
#     crossSeedEstimateDenom <- crossSeedEstimateDenom + m
#   }
#   result$cross_seed_estimate <- crossSeedEstimateNum / crossSeedEstimateDenom
#   result
# }


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
#' A list with elements:
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
#' population_estimate_hash(rds$subject,rds$recruiter,
#'      subj_hash, d[rds$subject], nbrs_hash, rho)
#'
#' #rho estimated from data
#' population_estimate_hash(rds$subject,rds$recruiter,
#'      subj_hash, d[rds$subject], nbrs_hash)
#' @export
population_estimate_hash <- function(subject, recruiter, subject_hash, degree, nbrs, rho=NULL){
  if(length(unique(subject)) != length(subject))
    stop("Subject ids must be unique")
  if(anyNA(subject))
    stop("No missing subject identifiers allowed")
  if(anyNA(recruiter))
    stop("No missing recruiter identifiers allowed. Use a value like '-1' for seed subjects")
  if(anyNA(degree)){
    warning(paste0(sum(is.na(degree)), " missing degrees. Imputing median"))
    degree[is.na(degree)] <- median(degree, na.rm=TRUE)
  }
  s2 <- 1:length(subject)
  r2 <- match(recruiter, subject)
  r2[is.na(r2)] <- -1
  subject <- s2
  recruiter <- r2
  #nbrs2 <- list()
  #nbrs2[subject] <- nbrs
  #nbrs <- nbrs2
  ns <- length(na.omit(subject))
  if(is.null(rho)){
    nMatches <- sum(sapply(subject_hash,function(h) sum(subject_hash==h,na.rm=TRUE) - 1))
    rho <- nMatches / (ns*(ns-1))
  }
  outSet <- apply(cbind(subject,recruiter), 1, function(x){
    excl <- subject_hash[recruiter == x[1]]
    if(x[2] != -1)
      excl <- c(excl, subject_hash[match(x[2], subject)])
    nb <- nbrs[[x[1]]]
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
        #for(id in mId){
        #  wts1[[length(wts1) + 1]] <- 1 / (dPopulation * rho * (N - 1) / (degree[id] - 1) + 1)
        #}
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

  seed <- get_seed(subject,recruiter)
  seedIds <- unique(seed)
  outSets <- list()
  matchSets <- list()
  for(s in seedIds){
    outSets[[s]] <- apply(cbind(subject[seed == s],recruiter[seed == s]), 1, function(x){
      excl <- subject_hash[recruiter == x[1]]
      if(x[2] != -1)
        excl <- c(excl, subject_hash[match(x[2], subject)])
      nb <- nbrs[[x[1]]]
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
      #outSet <- apply(cbind(subject[seed == s],recruiter[seed == s]), 1, function(x){
      #  excl <- subject_hash[recruiter == x[1]]
      #  if(x[2] != -1)
      #    excl <- c(excl, subject_hash[match(x[2], subject)])
      #  nb <- nbrs[[x[1]]]
      #  for(e in excl){
      #    i <- which(nb %in% e)
      #    if(length(i) > 0)
      #      nb <- nb[-i[1]]
      #  }
      #  nb
      #})
      if(is.null(outSet))
        next
      #matchSet <- lapply(outSet, function(x) x[x %in% subject_hash[seed != s]])
      wts <-  lapply(matchSet, function(x){
        wts1 <- list()
        for(a in x){
          mId <- which(subject_hash == a & seed != s)
          #for(id in mId){
          #  wts1[[length(wts1) + 1]] <- 1 / (dPopulation * rho * (N - 1) / (degree[id] - 1) + 1)
          #}
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
  result <- list(
    estimate=opt$root,
    cross_seed_estimate= optCross$root,
    rho=rho
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
#' @param n_bootstrap The number of bootrap samples used to estimate the confidence intervals
#' @param conf_level The coverage level for the confidence intervals
#' @param progress Show a text progress bar
#' @return
#' A data frame with the population size values, CI lower and CI upper bounds.
#' @examples
#' data(faux_pns)
#' #rho estimated from data
#'
#'
#' #hashes
#' population_estimate_hash(rds$subject,rds$recruiter,
#'      subj_hash, rds$degree, nbrs_hash, rho)
#' \dontrun{
#' bootstrap_population_estimate(rds$subject,rds$recruiter,
#'   subj_hash, rds$degree, nbrs_hash, rho, 50)
#' }
bootstrap_population_estimate <- function(
  subject,
  recruiter,
  subject_hash,
  degree,
  nbrs,
  rho=NULL,
  n_bootstrap=500,
  conf_level = .95,
  progress = TRUE){

  # Get point estimates of population saize and rho
  point_estimate <- population_estimate_hash(subject, recruiter, subject_hash, degree, nbrs, rho)

  # Get the number of alter ids recorded excluding recruiter and recrutee
  s2 <- 1:length(subject)
  r2 <- match(recruiter, subject)
  r2[is.na(r2)] <- -1
  free_nbrs <- apply(cbind(s2, r2), 1, function(x){
    excl <- subject_hash[r2 == x[1]]
    if(x[2] != -1)
      excl <- c(excl, subject_hash[match(x[2], s2)])
    nb <- nbrs[[x[1]]]
    for(e in excl){
      i <- which(nb %in% e)
      if(length(i) > 0)
        nb <- nb[-i[1]]
    }
    nb
  })
  n_free_nbrs <- sapply(free_nbrs, length)

  #id <- 1:n
  #ind <- sample.int(length(degree),n,TRUE, prob = 1 / degree)
  #pop_degree <- degree[ind]
  #pop_n_alters <- n_free_nbrs[ind]
  #el <- make_configuration_graph(pop_degree)
  #pop_nbrs <- lapply(1:n, function(i) c(el[el[,1]==i,2],el[el[,2]==i,1]))
  # generate one bootstrap sample
  bootstrap <- function(n, rho, rho_known){
    hash_size <- 1 / rho
    seed <- get_seed(subject,recruiter)
    ns <- table(seed)
    sds <- as.numeric(names(ns))
    nsamp <- sum(ns)

    nfn_by_seed <- sapply(sds, function(sd){
      sum(n_free_nbrs[seed == sd])
    })

    id <- 1:n
    ind <- sample.int(length(degree),n,TRUE, prob = 1 / degree)
    pop_degree <- degree[ind]
    pop_n_alters <- n_free_nbrs[ind]
    browser()
    samp <- pns_sample(pop_degree, ns)
    bs_free_nbrs <- apply(cbind(samp$subject, samp$recrutier,1:length(samp$subject)), 1, function(x){
      excl <- c(samp$subject[samp$recrutier == x[1]], x[2])
      nb <- samp$nbrs[[x[3]]]
      nb <- setdiff(nb, excl)
      nb
    })

    tots <- nfn_by_seed
    nout <- sapply(sds, function(sd) sum(sapply(bs_free_nbrs[seed == sd], length)))
    fix <- nout < tots
    tots[!fix] <- round(tots[!fix] *  sum(tots[!fix]) / (sum(tots[!fix]) - sum(tots - nout)))
    for(i in 1:length(sds)){
      fn <- bs_free_nbrs[seed == sds[i]]
      nn <- sapply(fn, length)
      while(sum(nn) > tots[i]){
        j <- sample.int(length(nn), 1, prob=nn)
        fn[[j]] <- fn[[j]][sample.int(nn[j],1)]
        nn[j] <- nn[j] - 1
      }
      bs_free_nbrs[seed == sds[i]] <- fn
    }
    # samp_id <- sample(sample(id, nsamp, prob=pop_degree))
    # recr_id <- c(rep(-1,length(ns)), unlist(lapply(1:length(ns), function(i) rep(samp_id[i], ns[i]-1))))
    #
    # # remove edges observed in the RDS tree
    # pop_degree_free <- pop_degree
    # pop_degree_free[samp_id] <- pop_degree_free[samp_id] - 1
    #
    # bs_nbrs <- lapply(1:nsamp, function(i){
    #   nnbr <- pop_n_alters[samp_id[i]]
    #   s <- if(nnbr > 0) sample(id, size = nnbr, prob = pop_degree_free) else as.numeric(c())
    #   pop_degree_free[s] <<- pop_degree_free[s] - 1
    #   s
    # })
    browser()

    if(rho == 0){
      hash <- id
      nbrs_hash <- bs_nbrs
    }else{
      hash <- floor(runif(n, min = 0, max=hash_size))
      nbrs_hash <- lapply(bs_nbrs, function(x) hash[x])
    }
    subj_hash <- hash[samp_id]
    degree <- pop_degree[samp_id]
    rho1 <- if(rho_known) rho else NULL
    unlist(population_estimate_hash(samp_id, recr_id, subj_hash, degree, nbrs_hash, rho1))
  }

  if(progress)
    pb <- txtProgressBar(min = 0, max = n_bootstrap, style = 3)
  boots <- matrix(NA, n_bootstrap, 3)
  for(i in 1:n_bootstrap){
    if(progress)
      setTxtProgressBar(pb, i)
    b1 <- bootstrap(round(point_estimate$estimate), point_estimate$rho, !is.null(rho))
    b2 <- bootstrap(round(point_estimate$cross_seed_estimate), point_estimate$rho, !is.null(rho))
    boots[i,] <- c(b1[1], b2[2], b2[3])
  }
  if(progress)
    close(pb)

  crit <- -qnorm((1-conf_level)/2)

  estimates <- c( log(point_estimate$estimate), log(point_estimate$cross_seed_estimate), point_estimate$rho)
  sds <- c(sd(log(boots[,1])), sd(log(boots[,2])), sd(boots[,3]))
  lower <- estimates - crit * sds
  upper <- estimates + crit * sds
  result <- data.frame(
    name = c("estimate", "cross_seed_estimate","1 / rho"),
    value = estimates,
    ci_lower_bound = lower,
    ci_upper_bound = upper
  )
  result[1,2:4] <- exp(result[1,2:4])
  result[2,2:4] <- exp(result[2,2:4])
  result[3,2:4] <- 1 / result[3,2:4]
  tmp <- result[3,3]
  result[3,3] <- result[3,4]
  result[3,4] <- tmp
  attr(result,"bootstrap_samples") <- boots
  attr(result, "conf_level") <- conf_level
  result
}

# pop_degree : the degrees of the population
# n_seed : The number recruited by each seed
pns_sample <- function(pop_degree, n_seed){
  p_seed <- n_seed / sum(n_seed)
  n <- sum(n_seed)
  N <- length(pop_degree)

  r <- s <- rep(-1, n)
  alt <- list()
  for(i in 1:N){
    alt[[i]] <- numeric()
  }
  d1 <- d <- pop_degree
  not_sampled <- rep(TRUE, N)
  seed <- rep(0, N)

  #draw seeds
  j <- 1
  while(j <= length(p_seed)){
    s[j] <- sample.int(N, 1, prob = d*not_sampled)
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
    print(sum(n_seed))
    # select a seed tree to recruit from
    sd <- sample.int(length(n_seed), 1, prob=n_seed)
    found <- FALSE
    k <- 1
    while(!found){
      k <- k + 1
      if(k > 500)
        browser()
      # draw a random individual from the seed tree to be a recruiter
      recr <- sample.int(N, 1, prob=seed==sd)

      # the alters of the recruiter who have not been recruited
      fnbr <- setdiff(alt[[recr]], s)

      if(length(fnbr) > 0){
        found <- TRUE

        # follow a random edge from the recruiter to a new subject
        s[j] <- fnbr[sample.int(length(fnbr),1)]
        r[j] <- recr

        if(seed[s[j]] != 0)
          browser()

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
  list(
    subject = s,
    recruiter = r,
    nbrs = alt[s],
    degree = pop_degree[s],
    seed = seed[s]
  )
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
#' # no hashes
#' population_estimate_hash(rds$subject,rds$recruiter, rds$subject, rds$degree, nbrs2, rho=0)
#'
#' #hashes
#' population_estimate_hash(rds$subject,rds$recruiter,
#'      subj_hash, rds$degree, nbrs_hash)
NULL
