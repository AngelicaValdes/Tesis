data {
  //int<lower=0> nData;
  int<lower=0> nRoutes;
  int<lower=0> nObs;
  matrix[nObs,nRoutes] y;
  matrix[nObs,nRoutes] obs;
} parameters {
  vector[nRoutes] thetar;
  real theta;
  real<lower=0> sigma2;    // standard deviation like rhot
  real<lower=0> xi2;
} transformed parameters {
  //matrix[nData,nYdim] mu;
  //for (ydim in 1:nRoutes)
  //      for (period in 1:nPeriods)
  //          mu[task+(resp-1)*nTask,ydim]=
  //          x[task+(resp-1)*nTask,1]*b1[resp,ydim]+
  //          x[task+(resp-1)*nTask,3]*bt[resp,ydim]+
  //          x[task+(resp-1)*nTask,2]*delta[1+(ydim-1)*(nPredictors-2)];
} model {
  sigma2 ~ inv_gamma(1.05,10);
  xi2 ~ inv_gamma(1.05,3);
  theta ~ normal(0,30);
  for (route in 1:nRoutes)
        thetar[route] ~ normal(theta,sqrt(xi2));
  for (period in 1:nObs)
    for (route in 1:nRoutes)
       target += obs[period,route]*normal_lpdf(y[period,route]| thetar[route], sqrt(sigma2));
        //if (obs[period,route]==1)
        //y[period,route] ~ normal(thetar[route],sqrt(sigma2));
}
generated quantities {
    real<lower=0> sigma;    // standard deviation like rhot
    real<lower=0> xi;
    sigma = sqrt(sigma2);
    xi = sqrt(xi2);
//  corr_matrix[nYdim] Omega;
//  cov_matrix[nYdim] CovarError;
//  corr_matrix[nYdim] Sigma;
//  cov_matrix[nYdim] CovarBeta;
//  Omega <- multiply_lower_tri_self_transpose(L_Omega);
//  CovarError <- quad_form_diag(Omega, tau);  
//  Sigma <- multiply_lower_tri_self_transpose(L_Lambda);
//  CovarBeta <- quad_form_diag(Sigma, rho1);  
}
