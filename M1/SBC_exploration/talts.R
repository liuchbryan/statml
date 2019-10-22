## If RStan is not properly installed
# remove.packages("rstan")
# if (file.exists(".RData")) file.remove(".RData")
# install.packages("rstan", repos = "https://cloud.r-project.org/", dependencies = TRUE)
# install.packages("ggplot2")
# install.packages('reshape2')

## Include rstan library (+some config), and ggplot2
library(rstan)
options(mc.cores = parallel::detectCores())
rstan_options(auto_write = TRUE)
library(ggplot2)
library(reshape2)

# Change working directory as appropriate
setwd('~/Imperial/statml/M1/SBC_exploration/')

# Correctly specified model
#linreg_model <- stan_model(file='linreg_model.stan', 
#                           model_name = "linreg_model")

## Underdispersed prior
#linreg_model <- stan_model(file='linreg_model_underdispersed.stan', 
#                           model_name = "linreg_model_underdispersed")

## Overdispersed prior
linreg_model <- stan_model(file='linreg_model_overdispersed.stan', 
                           model_name = "linreg_model_overdispersed")

## Biased prior
#linreg_model <- stan_model(file='linreg_model_biased.stan', 
#                           model_name = "linreg_model_biased")

### OBTAINING THE RANKS

num_post_samples <- 100  # L in the Talts et al. paper
num_rank_samples <- 10000 # N in the Talts et al. paper

linreg_sbc_ranks <- parallel::mclapply(1:num_rank_samples, FUN = function(n) {
  # A different seed for each MCMC sample
  S <- seq(from = 0, to = .Machine$integer.max, length.out = num_rank_samples)[n]
  
  # Draw a prior sample
  prior_samples <- list('beta'=rnorm(1, 0, 10), 'alpha'=rnorm(1, 0, 10))
  
  # Draw simulated data
  num_simulated_data <- 5
  sample_data <- list(N=num_simulated_data,
                      X=seq(0.0, num_simulated_data - 1) * 1.0,
                      y=seq(0.0, num_simulated_data - 1) * prior_samples[['beta']] + 
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

# Convert ranks into a matrix (one column for each parameter)
linreg_ranks <- do.call(rbind, linreg_sbc_ranks)

### PLOTTING THE RESULTANT HISTOGRAM

# The 99% CI for uniform samples
linreg_ranks_CI <- c(qbinom(0.005, num_rank_samples, 1/(num_post_samples + 1)),
                     qbinom(0.995, num_rank_samples, 1/(num_post_samples + 1)))

# ggplot2 requires a melted DF of ranks for multiple-param plotting to work properly
linreg_ranks_plot <- melt(as.data.frame(linreg_ranks), 
                          variable.name = 'param', value.name = "rank")

# Plot histogram of one parameter - beta in this case
ggplot(linreg_ranks_plot[linreg_ranks_plot$param=='beta',], 
       aes(x=rank)) + 
  geom_ribbon(aes(ymin = linreg_ranks_CI[1], ymax = linreg_ranks_CI[2]), 
              fill = "grey80", alpha=0.8) +
  geom_hline(yintercept= num_rank_samples / (num_post_samples + 1)) + 
  geom_histogram(bins=num_post_samples + 1, 
                 size=0.2, color ='black', fill='white', alpha=0.5) + 
  theme_classic()

# Plot historgram of multiple parameters
ggplot(linreg_ranks_plot, aes(x=rank)) + 
  geom_ribbon(aes(ymin = linreg_ranks_CI[1], ymax = linreg_ranks_CI[2]), 
              fill = "grey80", alpha=0.8) +
  geom_hline(yintercept= num_rank_samples / (num_post_samples + 1)) + 
  geom_histogram(bins=num_post_samples + 1, 
                 size=0.2, color ='black', fill='white', alpha=0.5) + 
  facet_grid(~param) + theme_classic()



