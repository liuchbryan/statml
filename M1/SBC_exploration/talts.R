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

M <- 20
yyy <- parallel::mclapply(1:M, FUN = function(m) {
  S <- seq(from = 0, to = .Machine$integer.max, length.out = M)[m]
  
  # Draw a prior sample
  prior_samples <- list('beta'=rnorm(1, 0, 10), 'alpha'=rnorm(1, 0, 10))
  
  # Draw a simulated data
  num_simulated_data <- 50
  sample_data <- list(N=num_simulated_data,
                      X=seq(0, num_simulated_data - 1),
                      y=seq(0, num_simulated_data - 1) * prior_samples[['beta']] + 
                        prior_samples[['alpha']] + 
                        rnorm(num_simulated_data, 0, 1.2))
  
  # Run a MC for some iterations to get some posterior examples
  post <- sampling(linreg_model, data=sample_data, chains = 1, cores = 1, 
                   iter=3100, warmup=3000, seed = S)
  
  print(summary(post))
  
  # Compute ranks
  ranks <- sapply(names(prior_samples), FUN = function(p){
    posterior_samples <- extract(post)[[p]]
    return(rank(append(posterior_samples, prior_samples[[p]]))[length(posterior_samples) + 1] - 1)
  })
  
  return(ranks)
})

yyy1 <- do.call(rbind, yyy)

hist(yyy1[,1], breaks=100)
hist(yyy1[,2], breaks=100)


