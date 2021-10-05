data {
  int<lower=0> nObs;
  int<lower=0> nvar;
  int<lower=0> nclients;
  int pos_in[nclients];
  int pos_fin[nclients];
  matrix[nObs,nvar] xdata;
  int ydata[nObs];
  int<lower=0> nRoutes;
  matrix[nObs,nRoutes] routedata;
}

parameters {
  vector [nvar] beta;
  vector [nRoutes] beta_0[nclients];
  real beta_0_mean;
  real beta_0_var;
  
}

model {
  
  beta_0_mean ~ normal(0,10);
  beta_0_var ~ inv_gamma(3,1);
  for (client in 1:nclients){
    beta_0[client] ~ normal(beta_0_mean, sqrt(beta_0_var));
    //beta_0[client] ~ normal(0,10);
    ydata[pos_in[client]:pos_fin[client]] ~ bernoulli_logit(routedata[pos_in[client]:pos_fin[client]]*beta_0[client]+ xdata[pos_in[client]:pos_fin[client]]*beta);
  }
}
generated quantities{
  real loglike;
  vector[nclients] loglikei;
  for (client in 1:nclients){
    loglikei[client] = bernoulli_logit_lpmf(ydata[pos_in[client]:pos_fin[client]]|routedata[pos_in[client]:pos_fin[client]]*beta_0[client]+ xdata[pos_in[client]:pos_fin[client]]*beta);
  }
  loglike=sum(loglikei);
}


