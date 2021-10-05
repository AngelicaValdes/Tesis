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
  vector [nvar-2] beta1[nclients];
  vector<lower=0.01>[2] beta2[nclients];
  vector [nvar] beta_mean;
  vector [nvar] beta_var;
  vector [nRoutes] beta_0[nclients];
  real beta_0_mean;
  real beta_0_var;
  
}

model {
  beta_mean ~ normal(0,10);
  beta_var ~ inv_gamma(3,1);
  beta_0_mean ~ normal(0,10);
  beta_0_var ~ inv_gamma(3,1);
  for (client in 1:nclients){
    beta_0[client] ~ normal(beta_0_mean, sqrt(beta_0_var));
    beta1[client] ~ normal(beta_mean[1:(nvar-2)], sqrt(beta_var[1:(nvar-2)]));
    beta2[client]~lognormal(beta_mean[(nvar-1):nvar],sqrt(beta_var[(nvar-1):nvar]));
    ydata[pos_in[client]:pos_fin[client]] ~ bernoulli_logit(routedata[pos_in[client]:pos_fin[client]]*beta_0[client]+ 
    xdata[pos_in[client]:pos_fin[client],1:(nvar-2)]*beta1[client] +
    xdata[pos_in[client]:pos_fin[client],(nvar-1):nvar]*beta2[client]);
  }
}
generated quantities{
  real loglike;
  vector[nclients] loglikei;
  for (client in 1:nclients){
    loglikei[client] = bernoulli_logit_lpmf(ydata[pos_in[client]:pos_fin[client]]|routedata[pos_in[client]:pos_fin[client]]*beta_0[client]+ 
                                            xdata[pos_in[client]:pos_fin[client],1:(nvar-2)]*beta1[client] +
                                            xdata[pos_in[client]:pos_fin[client],(nvar-1):nvar]*beta2[client]);
  }
  loglike=sum(loglikei);
}

