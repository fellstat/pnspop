library(tidyverse)
library(ggplot2)
sim <- function(){
  rho <- 1 / hashSize
  hash <- floor(runif(n, min = 0, max=hashSize))
  g <- rep(1,n)
  rds <- samp_rds(el, d, seeds,g,samp_size,FALSE, pr = c(0,.1,.9))
  subj_hash <- hash[rds$subject]
  nbrs2 <- nbrs[rds$subject]
  nbrs_hash <- lapply(nbrs2,function(x) hash[x])
  nbrs_hash <- lapply(nbrs_hash, function(x){
    if(length(x) > nom_limit){
      x <- sample(x, nom_limit)
    }
    x
  })
  bootstrap_pse(
    rds$subject,
    rds$recruiter,
    subj_hash,
    d[rds$subject],
    nbrs_hash,
    rho,
    method=method,
    small_sample_fraction=small_sample_fraction,
    n_bootstrap=n_boot)
}

run_sims <- function(){
  lll <- list()
  for(s in 1:5){
    print(s)
    ll <- parallel::mclapply(
      1:100,
      function(i){
        df <- sim()
        df$simulation <- i
        df
      },
      mc.cores = 40)
    lll <- append(lll, ll)
  }
  list(
    params = data.frame(
      n=n,
      sample_fraction=sample_fraction,
      hash_size=hashSize,
      mean_degree = mean_degree,
      nom_limit=nom_limit,
      seeds=seeds,
      n_boot=n_boot,
      method=method,
      small_sample_fraction = small_sample_fraction
    ),
    boot = lll
  )
}


n <- 10000 #pop size
sample_fraction <- .05
hashSize <- 500000
mean_degree <- 10
d <- rpois(n,lambda = mean_degree - 1) + 1#rep(2,n)#
el <- make_configuration_graph(d)
nbrs <- lapply(1:n, function(i) c(el[el[,1]==i,2],el[el[,2]==i,1]))
nom_limit <- 5
seeds <- 10 # #of seeds
samp_size <- round(sample_fraction * n)
n_boot <- 500
method <- "sample"
small_sample_fraction <- TRUE

results <- list()
for(mean_degree in c(5,10,20)){
  d <- rpois(n,lambda = mean_degree-1) + 1#rep(2,n)#
  el <- make_configuration_graph(d)
  nbrs <- lapply(1:n, function(i) c(el[el[,1]==i,2],el[el[,2]==i,1]))
  for(hashSize in c(5000000000, 250000, 50000, 10000)){
    samp_size <- round(sample_fraction * n)
    for(sample_fraction in c(.05,.1)){
      for(method in c("sample","alter","combined")){
        print(data.frame(
          n=n,
          sample_fraction=sample_fraction,
          hash_size=hashSize,
          mean_degree = mean_degree,
          nom_limit=nom_limit,
          seeds=seeds,
          n_boot=n_boot,
          method=method,
          small_sample_fraction = small_sample_fraction
        ))
        results[[length(results) + 1 ]] <- run_sims()
      }
    }
  }
}
# r1 <- readRDS("scripts/results/results1.rds")
# r2 <- readRDS("scripts/results/results2.rds")
# r3 <- readRDS("scripts/results/results3.rds")
# boot_sims <- c(r1,r2,r3)
boot_sims <-results

boot_sims <- lapply(boot_sims, function(x){
  lll <- x[[2]]
  lll <- Filter(function(x) !(is.null(x) | inherits(x,"try-error")),lll)
  estimates <- sapply(lll, function(x) if(is.null(x)) NA else x[1,2])
  estimate_mns <- mean(estimates, na.rm=TRUE)#apply(estimates,1, mean)
  log_estimate_vars <- var(log(estimates), na.rm=TRUE)#apply(log(estimates),1, var)

  bs_log_vars <- sapply(lll, function(x){
    if(is.null(x)) return(NA)
    boot <- attr(x,"bootstrap_samples")
    c(var(log(boot[,1])))
  })
  r <- data.frame(
    estimate_mean = estimate_mns,
    log_estimate_var = log_estimate_vars,
    bs_log_vars = bs_log_vars
  )
  bind_cols(r,x[[1]])
}) %>% bind_rows()

boot_sims <- boot_sims %>%
  mutate(
    Fraction = case_when(
      sample_fraction == .05 ~ "Sample Fraction = 5%",
      sample_fraction == .1 ~ "Sample Fraction = 10%"
    ),
    Hash = factor(
      case_when(
        hash_size == 10000 ~ "Hash Size = 10k",
        hash_size == 50000 ~ "Hash Size = 50k",
        hash_size == 250000 ~ "Hash Size = 250k",
        hash_size > 1000000 ~ "Hash Size = ∞"
      ),
      levels=c("Hash Size = 10k","Hash Size = 50k","Hash Size = 250k","Hash Size = ∞")
    ),
    Estimator = case_when(
      method == "sample" ~ "Sample",
      method == "alter" ~ "Alter",
      method == "combined" ~ "Network"
    )
  )

p <- ggplot(boot_sims) +
  geom_hline(yintercept=0,color="green") +
  stat_summary(aes(
    x=as.factor(mean_degree),
    y = bs_log_vars/log_estimate_var - 1,#log(sqrt(bs_log_vars)) - log(sqrt(log_estimate_var)),
    color=Estimator),
               fun=mean,
               fun.max = function(x) mean(x) + 2*sd(x),
               fun.min = function(x) mean(x) - 2*sd(x),
               position= position_dodge(.3)
  ) +
  scale_y_continuous(labels = scales::percent) +
  facet_grid(Fraction ~ Hash) +
  xlab("Mean Degree") +
  ylab("Bootstrap Variance Estimate Relative Error") +
  theme_bw()
p
png("scripts/results/boot_config_plot.png",width = 500,height = 300)
p
dev.off()






