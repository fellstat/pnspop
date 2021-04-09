
#' @importFrom stats rexp uniroot
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
samp_rds <- function(el, d, ns, g, ss, biased=TRUE,pr = c(0,.1,.9)){
  maxR <- length(pr) -1
  n <- length(g)
  ml <- if(is.factor(g)) max(levels(g)) else max(g)
  if(biased){
    seeds <- sample.int(n,ns,prob=as.numeric(as.factor(g))-1)
  }else{
    seeds <- sample.int(n,ns, prob = d)
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

#' Estimate population size given neighbor identifiers
#' @param subject The integer ids of each subject
#' @param recruiter The integer ids of the recruiter of each subject
#' @param degree The degree of each subject
#' @param nbrs A list, each element indicating the ids of the neighbors of each subject,
#' or a random subset of those neighbors
#' @return
#' A list with elements:
#' 'no_degree_estimate': The naive n_1 capture re-capture estimate
#' 'estimate': The n_2 population size estimate
#' 'cross_seed_estimate' : The n_3 cross-seed estimate
#' @references
#' Khan, Bilal; Lee, Hsuan-Wei; Fellows, Ian; Dombrowski, Kirk One-step Estimation of Networked Population Size: Respondent-Driven Capture-Recapture with Anonymity eprint arXiv:1710.03953, 2017
#' @examples
#' set.seed(1)
#' n <- 1000 #pop size
#' d <- rpois(n,lambda = 3) + 1
#' g <- rep(1,n)
#' el <- make_configuration_graph(d)
#' seeds <- 7 # #of seeds
#' rds <- samp_rds(el, d, seeds,g,200,FALSE, pr = c(0,.1,.9))
#' nbrs <- lapply(1:n, function(i) c(el[el[,1]==i,2],el[el[,2]==i,1]))
#' nbrs2 <- nbrs[rds$subject]
#' population_estimate(rds$subject,rds$recruiter, d[rds$subject], nbrs2)
#' @export
population_estimate <- function(subject, recruiter, degree, nbrs){
  nbrs2 <- list()
  nbrs2[subject] <- nbrs
  nbrs <- nbrs2
  ns <- length(subject)
  outSet <- do.call(c, apply(cbind(subject,recruiter), 1, function(x){
    excl <- c(x[2], subject[recruiter == x[1]])
    nb <- nbrs[[x[1]]]
    for(e in excl){
      i <- which(nb %in% e)
      if(length(i) > 0)
        nb <- nb[-i[1]]
    }
    nb
  }))
  m <- length(outSet[outSet %in% subject])
  dSample <- mean(degree) - 1
  dPopulation <- ns / sum(1/degree)

  result <- list(
    estimate=(dSample / dPopulation) * ns * length(outSet) / m ,
    no_degree_estimate=ns * length(outSet) / m
  )
  seed <- get_seed(subject,recruiter)
  seedIds <- unique(seed)
  crossSeedEstimateNum <- 0
  crossSeedEstimateDenom <- 0
  for(s in seedIds){
    outSet <- do.call(c, as.list(apply(cbind(subject[seed == s],recruiter[seed == s]), 1, function(x){
      excl <- c(x[2], subject[recruiter == x[1]])
      nb <- nbrs[[x[1]]]
      for(e in excl){
        i <- which(nb %in% e)
        if(length(i) > 0)
          nb <- nb[-i[1]]
      }
      nb
    })))
    if(is.null(outSet))
      next
    m <- length(outSet[outSet %in% subject[seed != s]])
    dSample <- mean(degree[seed != s]) - 1
    dPopulation <- ns / sum(1/degree)
    crossSeedEstimateNum <- crossSeedEstimateNum + length(outSet) *
      length(subject[seed != s]) * dSample / dPopulation
    crossSeedEstimateDenom <- crossSeedEstimateDenom + m
  }
  result$cross_seed_estimate <- crossSeedEstimateNum / crossSeedEstimateDenom
  result
}


#' Estimate population size given privatized (hashed) neighbor identifiers
#' @param subject The integer ids of each subject
#' @param recruiter The integer ids of the recruiter of each subject
#' @param subjectHash The hashed identifier for the subject
#' @param degree The degree of each subject
#' @param nbrs A list, each element indicating the hashed identifier of the neighbors of each subject,
#' or a random subset of those neighbors.
#' @param rho The probability two random individuals have the same hash value.
#' If NULL this is estimated from the number of hash collisions in subjectHash.
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
population_estimate_hash <- function(subject, recruiter, subjectHash, degree, nbrs, rho=NULL){
  nbrs2 <- list()
  nbrs2[subject] <- nbrs
  nbrs <- nbrs2
  ns <- length(subject)
  if(is.null(rho)){
    nMatches <- sum(sapply(subjectHash,function(h) sum(subjectHash==h,na.rm=TRUE) - 1))
    rho <- nMatches / (ns*(ns-1))
  }
  outSet <- apply(cbind(subject,recruiter), 1, function(x){
    excl <- subjectHash[recruiter == x[1]]
    if(x[2] != -1)
      excl <- c(excl, subjectHash[match(x[2], subject)])
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
  matchSet <- lapply(outSet, function(x) x[x %in% subjectHash])
  NTilda <- function(N){
    wts <-  lapply(matchSet, function(x){
      wts1 <- list()
      for(a in x){
        mId <- which(subjectHash == a)
        for(id in mId){
          wts1[[length(wts1) + 1]] <- 1 / (dPopulation * rho * (N - 1) / (degree[id] - 1) + 1)
        }
      }
      unlist(wts1)
    })
    m <- sum(unlist(wts))
    (dSample / dPopulation) * ns * length(unlist(outSet)) / m
  }
  opt <- uniroot(function(N) NTilda(N) - N, interval=c(2, 7000000000))

  seed <- get_seed(subject,recruiter)
  seedIds <- unique(seed)
  NTildaCross <- function(N){
    crossSeedEstimateNum <- 0
    crossSeedEstimateDenom <- 0
    for(s in seedIds){
      outSet <- apply(cbind(subject[seed == s],recruiter[seed == s]), 1, function(x){
        excl <- subjectHash[recruiter == x[1]]
        if(x[2] != -1)
          excl <- c(excl, subjectHash[match(x[2], subject)])
        nb <- nbrs[[x[1]]]
        for(e in excl){
          i <- which(nb %in% e)
          if(length(i) > 0)
            nb <- nb[-i[1]]
        }
        nb
      })
      if(is.null(outSet))
        next
      matchSet <- lapply(outSet, function(x) x[x %in% subjectHash[seed != s]])
      wts <-  lapply(matchSet, function(x){
        wts1 <- list()
        for(a in x){
          mId <- which(subjectHash == a & seed != s)
          for(id in mId){
            wts1[[length(wts1) + 1]] <- 1 / (dPopulation * rho * (N - 1) / (degree[id] - 1) + 1)
          }
        }
        unlist(wts1)
      })
      m <- sum(unlist(wts))
      dSample <- mean(degree[seed != s]) - 1
      dPopulation <- ns / sum(1/degree)
      crossSeedEstimateNum <- crossSeedEstimateNum + length(unlist(outSet)) *
        length(subject[seed != s]) * dSample / dPopulation
      crossSeedEstimateDenom <- crossSeedEstimateDenom + m
    }
    crossSeedEstimateNum / crossSeedEstimateDenom
  }
  optCross <- uniroot(function(N) NTildaCross(N) - N, interval=c(2, 7000000000))
  result <- list(
    estimate=opt$root,
    cross_seed_estimate= optCross$root,
    rho=rho
  )
  result
}
