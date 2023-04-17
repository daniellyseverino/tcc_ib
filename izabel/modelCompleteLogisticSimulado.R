library(rstan)
library(dplyr)
library(INLA)
library(stringr)

source("modelCompleteLogisticFunctions.R")

# -----> Generating data

# -> Fixing main quantities and parameters
T = 35
D = 10
t = seq(1, T)
d = seq(1, D)

# -> Indexes
I = cbind(t = rep(1:T, D), d = rep(1:D, each = T))

# -> alpha_t, delta_d

# beta.d é uma reta decrescente
b.beta = -0.5
beta.d = b.beta*d
ts.plot(beta.d)

a.alpha = 185 # valores iniciais provindos do ajuste aos dados da Dengue
b.alpha = 0.004
c.alpha = 0.34
f.alpha = 1

# alpha.t é o log da curva logística generalizada
alpha.t = log(f.alpha)+log(a.alpha)+log(c.alpha)-(c.alpha*(1:T))-(f.alpha + 1)*log( b.alpha+exp(-c.alpha*(1:T)) ) # logistica com log
ts.plot(alpha.t)

# -> Poisson intensity for n(-k)
lambda = matrix(NA, T, D)
for(t in 1:T){
  for(d in 1:D){
    lambda[t, d] = exp(alpha.t[t] + beta.d[d])
  }
}

# -> Generating counts
set.seed(1)
data = matrix(rpois(T*D, lambda), ncol = D) # data matrix

# -> Poisson intensity for n(k)
ts.plot(data)
lines(rowSums(data))

stanModel = stan_model("modelLogistic.stan")
samp = stanLogistic(stanModel, y = rowSums(data))

ts.plot(samp$a)
ts.plot(samp$b)
ts.plot(samp$c)
ts.plot(samp$f)

a.theta = mean(samp$a) %>% round(4)
b.theta = mean(samp$b) %>% round(4)
c.theta = mean(samp$c) %>% round(4)
f.theta = mean(samp$f) %>% round(4)

a.theta = 275.0895
b.theta = 0.0042
c.theta = 0.3383
f.theta = 1.013

theta.t = exp(log(f.theta)+log(a.theta)+log(c.theta)-(c.theta*(1:T))-(f.theta + 1)*log( b.theta+exp(-c.theta*(1:T)) ) )
ts.plot(theta.t)
points(rowSums(data), pch = 16)

psi = theta.t - rowSums(lambda[, - 1])
ts.plot(psi)
lines(lambda[, 1])

set.seed(1)
data.vector = rpois(T*D, lambda) # data vector

# -> Stablishing missing data
M = matrix(outer(1:T, 0:(D - 1), FUN = "+") > T, ncol = 1)[, 1]
data.vector[M] = NA

data.obs = data.vector[!M]
data.complete = data.frame(n = data.vector, M, I)  %>% filter(!M)

Ik = (data.complete %>% filter(d == 1))[ , 3:4]
nk = (data.complete %>% filter(d == 1))[ , 1]

I_k = (data.complete %>% filter(d != 1))[ , 3:4]
n_k = (data.complete %>% filter(d != 1))[ , 1]

# -----> Estimating model

# -> Data information
initial.data = list(nk = nk, n_k = n_k,
                    T = T, D = D,
                    Tk = Ik[, 1], Dk = Ik[, 2],
                    T_k = I_k[, 1], D_k = I_k[, 2],
                    qk = nrow(Ik), q_k = nrow(I_k))

# -> Stan configuration
warmup = 1000
chains = 1
thin = 1
sample_size = 1000
number_interations = warmup + thin*sample_size

# -> Setting initial parameters

data.matrix = matrix(data.vector, nrow = T)

stanModel = stan_model("modelLogistic.stan")
samp.init.alpha.beta = apply(data.matrix, 2, function(y) stanLogistic(stanModel, y = y))
samp.init.theta = stanLogistic(stanModel, y = rowSums(data.matrix, na.rm = TRUE))

abcf.init = lapply(samp.init.alpha.beta, function(x) c(mean(x$a), mean(x$b), mean(x$c), mean(x$f)) ) %>%
  unlist() %>% matrix(ncol = 4, byrow = TRUE)

alphas.init = matrix(NA, D, T)
b.betas.init = NULL
for(dd in 1:D){
  alphas.init[dd, ] = genLog(t = 1:T, abcf.init[dd , 1], abcf.init[dd , 2], abcf.init[dd , 3], abcf.init[dd , 4], logScale = FALSE)
  if(dd > 1) b.betas.init = c(b.betas.init, log(alphas.init[dd, ]/alphas.init[dd - 1, ]))
}

init.b.beta = mean(b.betas.init)
init.beta = init.b.beta*(1:D)

init.a.alpha = abcf.init[1 , 1]/exp(init.b.beta)
init.b.alpha = abcf.init[1 , 2]
init.c.alpha = abcf.init[1 , 3]
init.f.alpha = abcf.init[1 , 4]

init.alpha.t = alphas.init[1, ]/exp(init.b.beta)

init.a.theta = mean(samp.init.theta$a)
init.b.theta = mean(samp.init.theta$b)
init.c.theta = mean(samp.init.theta$c)
init.f.theta = mean(samp.init.theta$f)

init.theta.t = genLog(t = 1:T, init.a.theta, init.b.theta, init.c.theta, init.f.theta, logScale = FALSE)

# -> Preparing model
teste = stan_model("modelCompleteLogistic.stan")
params = c("lambda",
           "alpha", "a_alpha", "b_alpha", "c_alpha", "f_alpha",
           "beta", "b_beta",
           "theta", "a_theta", "b_theta", "c_theta", "f_theta",
           "psi")

# -> Run!
init = list(list(# lambda = lambda[, -1],
  alpha = init.alpha.t,
  a_alpha = init.a.alpha,
  b_alpha = init.b.alpha,
  c_alpha = init.c.alpha,
  f_alpha = init.f.alpha,
  beta = init.beta[-1],
  b_beta = init.b.beta,
  theta = init.theta.t,
  a_theta = init.a.theta,
  b_theta = init.b.theta,
  c_theta = init.c.theta,
  f_theta = init.f.theta,
  psi = nk
))

output = sampling(teste,
                  data = initial.data,
                  iter = number_interations,
                  warmup = warmup,
                  chains = chains,
                  pars = params,
                  init = init,
                  verbose = FALSE)

# -> Extracting samples
samp = extract(output)
save(samp, file = "sampCompleteLogistic.RData")

# -----> Results


ts.plot(samp$b_beta)
abline(h = b.beta, lwd = 2, col = "red")

par(mfrow = c(2, 4), mar = c(2.5, 2, 0, 0.5))
ts.plot(samp$a_alpha)
abline(h = a.alpha, lwd = 2, col = "red")
ts.plot(samp$b_alpha)
abline(h = b.alpha, lwd = 2, col = "red")
ts.plot(samp$c_alpha)
abline(h = c.alpha, lwd = 2, col = "red")
ts.plot(samp$f_alpha)
abline(h = f.alpha, lwd = 2, col = "red")

ts.plot(samp$a_theta)
abline(h = a.theta, lwd = 2, col = "red")
ts.plot(samp$b_theta)
abline(h = b.theta, lwd = 2, col = "red")
ts.plot(samp$c_theta)
abline(h = c.theta, lwd = 2, col = "red")
ts.plot(samp$f_theta)
abline(h = f.theta, lwd = 2, col = "red")

lambda.mean = matrix(NA, T, D - 1)
for(t in 1:T){
  for(d in 1:(D - 1)){
    lambda.mean[t, d] = mean(samp$lambda[ , t, d])
  }
}

par(mfrow = c(5, 5), mar = c(2.5, 2, 0, 0.5))
for(t in 1:T){
  for(d in 1:(D - 1)){
    ts.plot(samp$lambda[ , t, d])
    abline(h = lambda[t, d + 1], lwd = 2, col = "red")
  }
}

psi.mean = NULL
for(t in 1:T){
  psi.mean[t] = mean(samp$psi[ , t])
}

par(mfrow = c(5, 5), mar = c(2.5, 2, 0, 0.5))
for(t in 1:T){
  ts.plot(samp$psi[ , t])
  abline(h = psi[t], lwd = 2, col = "red")
}
