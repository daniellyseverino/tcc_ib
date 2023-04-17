
library(rstan)
library(dplyr)
library(INLA)
library(stringr)

# simulacao banco de dados =====================================================

# T = current time (today)
# t = time index 
# D = maximum relevant delay
# d = delay index 
# n_t_d = number of events occurred at time t recorded after d units of time
# N_t = sum of n_t_d = total number of events occurred at time t

# valores fixos
T = 35
t = seq(1,T)
D = 10
d = seq(1,D)

# n_t_d ========================================================================

#n_t_d ~ NegBin(lambda_t_d, sigma)
#log(lambda_t_d) = alpha_t + beta_d
#exp(alpha_t) = ( a_alpha * c_alpha * f_alpha * exp(-c_alpha*t)  )/
#  ( (b_alpha + exp(-c_alpha*f))^( f_alpha + 1 ) )
#beta_d = gama * d











