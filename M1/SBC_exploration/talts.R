# If RStan is not properly installed
# remove.packages("rstan")
# if (file.exists(".RData")) file.remove(".RData")
# install.packages("rstan", repos = "https://cloud.r-project.org/", dependencies = TRUE)

# Include rstan library and some options
library("rstan")
options(mc.cores = parallel::detectCores())
rstan_options(auto_write = TRUE)

# Change working directory as appropriate
setwd('~/Imperial/statml/M1/SBC_exploration/')

linreg_model <- stan_model(file='linreg_model.stan', 
                           model_name = "linreg_model")

num_post_samples <- 100  # L in the paper
num_rank_samples <- 2000 # N in the paper
linreg_sbc_ranks <- parallel::mclapply(1:num_rank_samples, FUN = function(n) {
  S <- seq(from = 0, to = .Machine$integer.max, length.out = num_rank_samples)[n]
  
  # Draw a prior sample
  prior_samples <- list('beta'=rnorm(1, 0, 10), 'alpha'=rnorm(1, 0, 10))
  
  # Draw a simulated data
  num_simulated_data <- 2
  sample_data <- list(N=num_simulated_data,
                      X=seq(0, num_simulated_data - 1),
                      y=seq(0, num_simulated_data - 1) * prior_samples[['beta']] + 
                        prior_samples[['alpha']] + 
                        rnorm(num_simulated_data, 0, 1.2))
  
  # Run a MC for some iterations to get some posterior examples
  trial_num_iterations <- 100
  num_warmup <- 2000
  trial_post <- sampling(linreg_model, data=sample_data, chains = 1, cores = 1, 
                         iter=num_warmup + trial_num_iterations, warmup=num_warmup, seed = S)
  
  # Calculate the effective sample size
  # Taken as the minimum of effective sample size across parameters we care
  n_eff <- min(sapply(names(prior_samples), 
                      FUN = function(p){summary(trial_post)$summary[, 'n_eff'][p]}))
  
  if (n_eff > num_post_samples) {
    # We have enough useful samples, proceed
    post <- trial_post
  } else {
    # Thinning to combat autocorrelation
    num_iterations <- trial_num_iterations * num_post_samples / n_eff
    post <- sampling(linreg_model, data=sample_data, chains = 1, cores = 1, 
                     iter=num_warmup + num_iterations, warmup=num_warmup, seed = S,
                     thin=floor(num_iterations / num_post_samples) )
  }

  # Compute ranks
  ranks <- sapply(names(prior_samples), FUN = function(p){
    posterior_samples <- extract(post)[[p]][1:num_post_samples]
    return(rank(append(posterior_samples, prior_samples[[p]]))[length(posterior_samples) + 1] - 1)
  })
  
  return(ranks)
})

linreg_sbc_ranks <- do.call(rbind, linreg_sbc_ranks)

hist(linreg_sbc_ranks[,1], breaks=100)
hist(linreg_sbc_ranks[,2], breaks=100)

misspecified_prior_ranks<- linreg_sbc_ranks

