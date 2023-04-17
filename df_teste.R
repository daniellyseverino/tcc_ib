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

# fixando alguns valores -------------------------------------------------------

a = 2
b = 0.05
c = 1.1
d = 10 # delay maximo de 10 dias
f = 7

t = 25 # hoje = semana 25

phi = 0.004
gama = 0.7
sigma = 1.4


# definindo distribuicoes ------------------------------------------------------

teta_t = ( a*c*f*exp(-c*t) ) / ( (b + exp(-c*t))^(f+1) ) 

N_t = rnbinom(n = 35*10, teta_t, phi)
hist(N_t)

# esperanca e variancia de Nt por definicao da binomial negativa
Esp_N_t = teta_t
Var_n_t = teta_t*(1 + (teta_t/phi))

# media e variancia mostrais (valore muito difentes)
mean(N_t)
var(N_t)


# exp(alpha) = ( a*c*f*exp(-c*t) ) / 
#   ( (b + exp(-c*t))^(f+1) )
alpha = log( a*c*f*exp(-c*t) ) - log( (b + exp(-c*t))^(f+1) )

beta = gama*d

#ln(lambda) = alpha + beta
lambda = exp(alpha*beta)

n_t_d = rnbinom(n = 35*10, lambda, sigma)


# estruturando a base de dados -------------------------------------------------

dados <- matrix(
  nrow = 35, ncol = 10
)
