% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/pnspop.R
\name{one_step_pse}
\alias{one_step_pse}
\title{Estimate population size given privatized (hashed) neighbor identifiers}
\usage{
one_step_pse(subject, recruiter, subject_hash, degree, nbrs, rho = NULL)
}
\arguments{
\item{subject}{The integer ids of each subject}

\item{recruiter}{The integer ids of the recruiter of each subject (-1 for seeds)}

\item{subject_hash}{The hashed identifier for the subject}

\item{degree}{The degree of each subject}

\item{nbrs}{A list, each element indicating the hashed identifier of the neighbors of each subject,
or a random subset of those neighbors.}

\item{rho}{The probability two random individuals have the same hash value.
If NULL this is estimated from the number of hash collisions in subject_hash.}
}
\value{
A list with elements:
'estimate': The n_2 population size estimate, adjusted for hashing
'cross_seed_estimate' : The n_3 cross-seed estimate, adjusted for hashing
'rho': The value of rho
}
\description{
Estimate population size given privatized (hashed) neighbor identifiers
}
\examples{
set.seed(1)
n <- 1000 #pop size
hashSize <- 1000
rho <- 1 / hashSize
hash <- floor(runif(n, min = 0, max=hashSize))
d <- rpois(n,lambda = 3) + 1
g <- rep(1,n)
el <- make_configuration_graph(d)
seeds <- 7 # #of seeds
rds <- samp_rds(el, d, seeds,g,200,FALSE, pr = c(0,.1,.9))
subj_hash <- hash[rds$subject]
nbrs <- lapply(1:n, function(i) c(el[el[,1]==i,2],el[el[,2]==i,1]))
nbrs2 <- nbrs[rds$subject]
nbrs_hash <- lapply(nbrs2,function(x) hash[x])
one_step_pse(rds$subject,rds$recruiter,
     subj_hash, d[rds$subject], nbrs_hash, rho)

#rho estimated from data
one_step_pse(rds$subject,rds$recruiter,
     subj_hash, d[rds$subject], nbrs_hash)
}
\references{
Khan, Bilal; Lee, Hsuan-Wei; Fellows, Ian; Dombrowski, Kirk One-step Estimation of Networked Population Size: Respondent-Driven Capture-Recapture with Anonymity eprint arXiv:1710.03953, 2017
}
