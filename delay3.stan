data {
  int<lower=0> nRoutes;
  int<lower=0> nObs;
  matrix[nObs,nRoutes] y;
  matrix[nObs,nRoutes] obs;
} parameters {
  vector[nRoutes] thetar;
  real<lower=0> sigma2;    // standard deviation like rhot
} model {
  sigma2 ~ inv_gamma(1.05,10);
  for (route in 1:nRoutes)
        thetar[route] ~ normal(0,30);
  for (period in 1:nObs)
    for (route in 1:nRoutes)
       target += obs[period,route]*normal_lpdf(y[period,route]| thetar[route], sqrt(sigma2));
}
generated quantities {
    real<lower=0> sigma;    // standard deviation like rhot
    sigma = sqrt(sigma2); 
}
