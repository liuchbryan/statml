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

##### SBC Example

sbc_example <- stan_model(file='sbc.stan', model_name='sbc_example')

sbc_result <- sbc(sbc_example, data=list(N=100, a=0.1, b=0.2), M=100)

plot(sbc_result)
print(sbc_result)

###### Linear regression model in Talts et al. (2018), changed to fit rstan::sbc

linreg_model_sbc <- stan_model(file='linreg_model_sbc.stan', 
                               model_name = "linreg_model_sbc")

linreg_model_sbc_result <- sbc(
  linreg_model_sbc, 
  data=list(N=10, X=c(0, 1, 2, 3, 4.0, 5, 6, 7, 8, 9), 
            y=c(-17.1, 10.39, -5.06, -7.94, -37.55, -46.18, 148.75, -46, -47, 39)), 
  M=100)

