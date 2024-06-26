//
// Stan model to evaluated the cases of Covid-19 - Poisson model
// model: static generalized logistic


data {

  //-----------------------------
  // observed data
  int<lower=1> n; // number of observations
  int<lower=0> y[n]; // counts of new case
  //-----------------------------
}


parameters {

  real<lower=1> f;
  real<lower=-30> b1;
  real<lower=0> a;
  real<lower=0> c;


}

transformed parameters{
  
  real<lower=0> b;
  real<lower=0> mu[n];
  b = exp(b1);
  
  for(t in 1:n){
    mu[t] = exp(log(f)+log(a)+log(c)-(c*t)-(f+1)*log( b+exp(-c*t) ) );
  }

}


model {
  //----------------------------
  // likelihood function
    y ~ poisson(mu); // observed model
  //----------------------
   // prior distributions
   a ~ gamma(0.1, 0.1);
   c ~ gamma(2,9);          //  gamma(2,9)  shape=2, scale=9,
   f ~ gamma(0.01,0.01);
  b1 ~ normal(0, sqrt(20));  // sqrt(1/0.2)
}