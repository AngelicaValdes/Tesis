data {
  int<lower=0> nObs;
  int<lower=0> nvar;
  matrix[nObs,nvar] xdata;
  int ydata[nObs];
}

parameters {
  vector [nvar] beta;
  real beta_0;
}

model {
  
  beta_0 ~ normal(0,10);
  beta ~ normal(0,10);
  
  ydata ~ bernoulli_logit(beta_0+ xdata*beta);
}
generated quantities{
  real loglike;
  
  loglike = bernoulli_logit_lpmf(ydata|beta_0+ xdata*beta);
}

