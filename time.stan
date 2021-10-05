data {
  //int<lower=0> nData;
  int<lower=0> nRoutes;
  int<lower=0> nObs;
  matrix[nObs,nRoutes] y;
  matrix[nObs,nRoutes] obs;
} parameters {
  vector[nRoutes] taur;
  real tau;
  real<lower=0> sigma2;    // standard deviation like rhot
  real<lower=0> xi2;
} model {
  sigma2 ~ inv_gamma(1.05,10);
  xi2 ~ inv_gamma(1.05,3);
  tau ~ normal(0,300);
  for (route in 1:nRoutes)
        taur[route] ~ normal(tau,sqrt(xi2));
  for (period in 1:nObs)
    for (route in 1:nRoutes)
       target += obs[period,route]*normal_lpdf(y[period,route]| taur[route], sqrt(sigma2));
        //if (obs[period,route]==1)
        //y[period,route] ~ normal(thetar[route],sqrt(sigma2));
}
generated quantities {
    real<lower=0> sigma;    // standard deviation like rhot
    real<lower=0> xi;
    sigma = sqrt(sigma2);
    xi = sqrt(xi2);
  
}
