// Stan Model with missing counts

functions{

  real genLog(int t, real a, real b, real c, real f, int logScale){
    real logV = log(f)+log(a)+log(c)-(c*t)-(f+1)*log( b+exp(-c*t) );
    if (logScale){
      return logV;
    } else {
      return exp(logV);
    }
  }

}

data {
  
  // -----> observed data

  int qk;
  int<lower = 0> nk[qk];
  int<lower = 1> Tk[qk];
  int<lower = 1> Dk[qk];

  int q_k;
  int<lower = 0> n_k[q_k];
  int<lower = 1> T_k[q_k];
  int<lower = 1> D_k[q_k];

  int<lower = 1> T;
  int<lower = 1> D;
  
}

parameters {

  real<lower = 0> a_theta;
  real<lower = 0> b_theta;
  real<lower = 0> c_theta;
  real<lower = 0> f_theta;
  real<lower = 0> a_alpha;
  real<lower = 0> b_alpha;
  real<lower = 0> c_alpha;
  real<lower = 0> f_alpha;
  real b_beta;
  
}

transformed parameters {
  
  matrix<lower = 0>[T, D - 1] lambda;
  vector[T] alpha;
  vector[D - 1] beta;
  real<lower = sum(lambda[T, ])> theta[T];
  vector<lower = 0>[T] psi;

  real b1_theta = log(b_theta);  
  real b1_alpha = log(b_alpha);  

  for(t in 1:T){
    alpha[t] = genLog(t, a_alpha, b_alpha, c_alpha, f_alpha, 1);
  }
  
  for(d in 1:(D - 1)){
    beta[d] = b_beta*(d + 1);   # comeca do 1??? 
  }
  
  for(t in 1:T){
    for(d in 1:(D - 1)){
      lambda[t, d] = exp(alpha[t] + beta[d]);
    }
  }

  for(t in 1:T){
    theta[t] = genLog(t, a_theta, b_theta, c_theta, f_theta, 0);
  }

  for(t in 1:T){
    psi[t] = theta[t] - sum(lambda[t, ]); #?????????
  }
  
}

model {
  
  // -----> likelihood function
  
  for(td in 1:q_k){
    n_k[td] ~ poisson( lambda[ T_k[td], D_k[td] - 1 ] );
  }

  for(t in 1:qk){
    nk[t] ~ poisson(psi[t]);
  }

  // -----> prior distributions

   a_theta ~ gamma(0.1, 0.1);
  b1_theta ~ normal(0, sqrt(20));  // sqrt(1/0.2)
   c_theta ~ gamma(2,9);           //  gamma(2,9)  shape=2, scale=9,
   f_theta ~ gamma(0.01,0.01);
   a_alpha ~ gamma(0.1, 0.1);
  b1_alpha ~ normal(0, sqrt(20));  // sqrt(1/0.2)
   c_alpha ~ gamma(2,9);           //  gamma(2,9)  shape=2, scale=9,
   f_alpha ~ gamma(0.01,0.01);
    b_beta ~ normal(0, 100);
  
}
