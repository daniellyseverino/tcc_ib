library(rstan)
library(dplyr)
library(INLA)
library(stringr)
library(tidyverse)

dados_dengue1 = readr::read_rds("izabel\\dengueData.RDS")
dados_dengue = dados_dengue1[1:35,]

T = dim(dados_dengue)[1]
D = dim(dados_dengue)[2]

# criando os NA
dados_dengue[outer(1:T, 0:(D - 1), FUN = "+") > T] <- NA

dados_dengue_longo <- dados_dengue %>% 
  mutate(t = rownames(dados_dengue)) %>% 
  pivot_longer(cols = 1:11, names_to = "d", values_to = "n_td") %>% 
  mutate(d = str_remove_all(d,"d"),
         d = as.numeric(d)) %>% 
  group_by(d) %>% 
  mutate(t = rep(1:35)) %>% 
  ungroup() 

# dados completos = linha T-D
dados_dengue_complete <- dados_dengue_longo %>% 
  filter(!is.na(n_td))

# stan ---------------------

# dalay = 0
It_d0 = dados_dengue_complete %>% 
  filter(d == 0) %>% 
  select(t,d) %>% as.data.frame()
nt_d0 = dados_dengue_complete %>% 
  filter(d == 0) %>% 
  select(n_td) %>% as.data.frame()

# dalay > 0
It_d = dados_dengue_complete %>% 
  filter(d > 0) %>% 
  select(t,d) %>% as.data.frame()
nt_d = dados_dengue_complete %>% 
  filter(d > 0) %>% 
  select(n_td) %>% as.data.frame()

# data stan
inicial_data = list(
  nt_d0 = nt_d0$n_td, # n_td com dalay = 0
  nt_d = nt_d$n_td, # n_td com dalay > 0
  T = T, D = D,
  t_d0 = It_d0[,1], # t com dalay = 0 (1 ate 35)
  d0 = It_d0[,2], # d com dalay = 0
  t_d = It_d[,1], # t com dalay > 0
  d = It_d[,2], # d com dalay > 0 (1 ate 10)
  q_d0 = nrow(It_d0), q_d = nrow(It_d)
)

# configuracoes stan
warmup = 1000 # interacoes para treinamento da cadeia (nao vamos usar na inferencia) 
chains = 1 # num de cadeias de Markov
thin = 1 # periodo para salvar amostras
sample_size = 1000
number_interations = warmup + thin*sample_size

stanModel = stan_model("izabel\\modelLogistic.stan")
stanModelf1 = stan_model("modelLogisticf1.stan")

# ajustando modelo y ~poisson para cada coluna (dalay), vamos ter mil estimativas por parametro p cda dalay
samp.init.alpha.beta = apply(dados_dengue, 2, function(y) stanLogistic(stanModel, y = y))
samp.init.alpha.betaf1 = apply(dados_dengue, 2, function(y) stanLogisticf1(stanModelf1, y = y)) # Erro = Stan model 'modelLogisticf1' does not contain samples.

# N_td = soma(n_td), vamos ter mil estimativas por parametro
samp.init.theta = stanLogistic(stanModel, y = rowSums(dados_dengue, na.rm = TRUE))

# media dos mil parametros em cada dalay (cada linha é um d)
abcf.init = lapply(samp.init.alpha.beta, function(x) c(mean(x$a), mean(x$b), mean(x$c), mean(x$f)) ) %>%
  unlist() %>% matrix(ncol = 4, byrow = TRUE)

alphas.init = matrix(NA, D, T)
b.betas.init = NULL
for(d in 1:D){
  alphas.init[d, ] = genLog(t = 1:T, a = abcf.init[d , 1], b = abcf.init[d , 2], c = abcf.init[d , 3], f = abcf.init[d , 4], logScale = FALSE)
  if(d > 1) b.betas.init = c(b.betas.init, log(alphas.init[d, ]/alphas.init[d - 1, ])) #########
}

init.b.beta = mean(b.betas.init)
init.beta = init.b.beta*(1:D)

# valores iniciais dos parametros para n_td
init.a.alpha = abcf.init[1 , 1]/exp(init.b.beta)
init.b.alpha = abcf.init[1 , 2]
init.c.alpha = abcf.init[1 , 3]
init.f.alpha = abcf.init[1 , 4]

# valores iniciais dos parametros para N_td
init.alpha.t = alphas.init[1, ]/exp(init.b.beta)

init.a.theta = mean(samp.init.theta$a)
init.b.theta = mean(samp.init.theta$b)
init.c.theta = mean(samp.init.theta$c)
init.f.theta = mean(samp.init.theta$f)

init.theta.t = genLog(t = 1:T, a = init.a.theta, b = init.b.theta, c = init.c.theta, f = init.f.theta, logScale = FALSE)

# comparando N_td real x estimado
ts.plot(rowSums(dados_dengue, na.rm = TRUE))
lines(init.theta.t, col = "red")
