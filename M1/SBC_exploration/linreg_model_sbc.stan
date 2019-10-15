data {
  int<lower=1> N;
  vector[N] X;
  vector[N] y;
}

transformed data {
  // Convention 1, one param_ for each param in parameters
  // Generate them in the same way as the model does
  real beta_ = normal_rng(0, 10); 
  real alpha_ = normal_rng(0, 10);
  
  // Convention 2, name of variable does not matter, but it
  // has to match the dimension of that used for realising the
  // prior predictive distribution (i.e. y in this case)
  // Generate them in the same way as the model does
  vector[N] y1;
  for (n in 1:N)
    y1[n] = normal_rng(X[n] * beta_ + alpha_, 1.2);
}

parameters {
  real beta;
  real alpha;
}

model {
  beta ~ normal(0, 10);
  alpha ~ normal(0, 10);
  
  y ~ normal(X * beta + alpha, 1.2);
}

generated quantities {
  vector[2] pars_;                                // Convention 3, name non-negotiable, one space for each param in parameters
  vector[N] y_;                                   // Convention 4, name non-negotiable, same dimension as that defined in Convention 2
  int ranks_[2] = {beta > beta_, alpha > alpha_}; // Convention 5, name non-negotiable, int array w/ one space for each param in parameters, remember to put in the conditions
  
  pars_[1] = beta_;
  pars_[2] = alpha_;
  
  for (n in 1:N)
    y_[n] = y1[n];
}
