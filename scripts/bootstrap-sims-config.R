library(tidyverse)
n <- 1000 #pop size
hashSize <- 500000
d <- rpois(n,lambda = 3) + 1#rep(2,n)#
el <- make_configuration_graph(d)
nbrs <- lapply(1:n, function(i) c(el[el[,1]==i,2],el[el[,2]==i,1]))
sim <- function(){
  rho <- 1 / hashSize
  hash <- floor(runif(n, min = 0, max=hashSize))
  g <- rep(1,n)
  seeds <- 7 # #of seeds
  rds <- samp_rds(el, d, seeds,g,300,FALSE, pr = c(0,.1,.9))
  subj_hash <- hash[rds$subject]
  nbrs2 <- nbrs[rds$subject]
  nbrs_hash <- lapply(nbrs2,function(x) hash[x])
  bootstrap_population_estimate(
    rds$subject,
    rds$recruiter,
    subj_hash,
    d[rds$subject],
    nbrs_hash,
    rho, 3)
}

lll <- list()
for(s in 1:5){
  print(s)
  ll <- parallel::mclapply(
    1:200,
    function(i){
      df <- sim()
      df$simulation <- i
      df
    },
    mc.cores = 40)
  lll <- append(lll, ll)
}

estimates <- sapply(lll, function(x) x[1:2,2])
estimate_mns <- apply(estimates,1, mean)
log_estimate_vars <- apply(log(estimates),1, var)

bs_log_vars <- sapply(lll, function(x){
  boot <- attr(x,"bootstrap_samples")
  c(var(log(boot[,1])), var(log(boot[,2])))
})
rowMeans(bs_log_vars)

bs_mns <- sapply(lll, function(x){
  boot <- attr(x,"bootstrap_samples")
  c(mean(boot[,1]), mean(boot[,2]))
})
rowMeans(bs_mns)

qplot( bs_log_vars[1,] / log_estimate_vars[1] )



library(tidyverse)
n <- 1000 #pop size
#hashSize <- 500000
d <- rep(2,n)#rpois(n,lambda = 3) + 1#
el <- make_configuration_graph(d)
nbrs <- lapply(1:n, function(i) c(el[el[,1]==i,2],el[el[,2]==i,1]))
sim2 <- function(){
  rho <- 0#1 / hashSize
  hash <- 1:n#floor(runif(n, min = 0, max=hashSize))
  g <- rep(1,n)
  seeds <- 100 # #of seeds
  rds <- samp_rds(el, d, seeds,g,300,FALSE, pr = c(0,.1,.9))
  subj_hash <- hash[rds$subject]
  nbrs2 <- nbrs[rds$subject]
  nbrs_hash <- lapply(nbrs2,function(x) hash[x])
  unlist(population_estimate_hash(
    rds$subject,
    rds$recruiter,
    subj_hash,
    d[rds$subject],
    nbrs_hash,
    rho))
}


rr <- sapply(1:100, function(x) sim2())
rr[is.infinite(rr)] <- NA
rowMeans(rr, na.rm = TRUE)


rr2 <- sapply(1:100, function(x) sim()[1:2,2])

sim()
sim2()


s <- 200
u <- 1600 - 200
m <- 0
while(s >0){
  if(runif(1) < s / (s+u) & s > 1){
    s <- s - 2
    m <- m + 2
  }else{
    s <- s - 1
    u <- u - 1
  }
}

