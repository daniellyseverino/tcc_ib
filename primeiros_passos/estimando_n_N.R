rm(list = ls())

library(rstan)
library(dplyr)
library(INLA)
library(stringr)
library(tidyverse)
library(highcharter)

dados_dengue = readr::read_rds("izabel\\dengueData.RDS")

# considera dalay 0 = dalay 1
# filtrando apenas as primeiras 35 semanas
dados_dengue_sem_d0 = dados_dengue[1:35,]

T = dim(dados_dengue_sem_d0)[1]
D = dim(dados_dengue_sem_d0)[2]

# criando os NAs
dados_dengue_sem_d0[outer(1:T, 0:(D - 1), FUN = "+") > T] <- NA

dados_dengue_longo <- dados_dengue_sem_d0 %>% 
  mutate(t = rownames(dados_dengue_sem_d0)) %>% 
  pivot_longer(cols = 1:D, names_to = "d", values_to = "n_td") %>% 
  mutate(d = str_remove_all(d,"d"),
         d = as.numeric(d),
         d = d + 1) %>% 
  group_by(d) %>% 
  mutate(t = rep(1:T)) %>% 
  ungroup() %>% 
  arrange(d)

dados_dengue_longo_completo = dados_dengue_longo %>% 
  filter(!is.na(n_td)) %>% 
  arrange(d)

# separando daly 1 dos demais
td_dalay_1 = (dados_dengue_longo_completo %>% filter(d == 1)) %>% select(t,d)
n_dalay_1 = (dados_dengue_longo_completo %>% filter(d == 1)) %>% 
  select(n_td) %>% 
  mutate(n_td = as.numeric(n_td))

td_dalay_i = dados_dengue_longo_completo %>% filter(d != 1) %>% select(t,d)
n_dalay_i = dados_dengue_longo_completo %>% filter(d != 1) %>%
  select(n_td) %>% 
  mutate(n_td = as.numeric(n_td))



# stan
modelo_stan =  stan_model("izabel\\modelCompleteLogistic.stan")

dados_stan = list(
  nk = n_dalay_1$n_td, n_k = n_dalay_i$n_td,
  T = T, D = D,
  Tk = td_dalay_1$t, Dk = td_dalay_1$d,
  T_k =  td_dalay_i$t,  D_k =  td_dalay_i$d,
  qk = nrow(n_dalay_1), q_k = nrow(n_dalay_i)
)

warmup = 1000
chains = 1
thin = 1
sample_size = 1000
number_interations = warmup + thin*sample_size

params = c("lambda",
           "alpha", "a_alpha", "b_alpha", "c_alpha", "f_alpha",
           "beta", "b_beta",
           "theta", "a_theta", "b_theta", "c_theta", "f_theta",
           "psi")

# Estimativas sem chute inicial ================================================

output = rstan::sampling(modelo_stan,
                  data = dados_stan,
                  iter = number_interations,
                  warmup = warmup,
                  chains = chains,
                  pars = params,
                  #init = init,
                  verbose = FALSE)

# estimativas = rstan::extract(output)
# saveRDS(estimativas, "modelos/estimativas_n_N.rds", version = 2)

estimativas = readRDS("modelos/estimativas_n_N.rds")

# para verificar convergencia (tem que fazer isso para cada parametro)
ts.plot(estimativas$b_beta)
plot(density(estimativas$b_beta))
acf(estimativas$b_beta)

mean(estimativas$lambda) 
mean(estimativas$theta)


genLog = function(t, a, b, c, f, logScale = TRUE){
  logV = log(f)+log(a)+log(c)-(c*t)-(f+1)*log( b+exp(-c*t) )
  if (logScale){
    return(logV);
  } else {
    return(exp(logV));
  }
}

alfha_t = genLog(
  t = 1:T,
  a = mean(estimativas$a_alpha),
  b = mean(estimativas$b_alpha),
  c = mean(estimativas$c_alpha),
  f = mean(estimativas$c_alpha),
  logScale = T
)

beta_d = apply(estimativas$beta, 2, mean)

lambda_t_d1 = exp(alfha_t+beta_d[1])


lambda = estimativas$lambda %>% as.data.frame()
lambda = apply(lambda2, 2, mean) %>% as.data.frame()
lambda <- lambda %>% 
  mutate(t = rep(1:T, D-1)) %>%
  group_by(t) %>% 
  mutate(d = 1:(D-1))
colnames(lambda)[1] = "lambda_td"

theta = estimativas$theta %>% as.data.frame()
theta = apply(theta, 2, mean) %>% as.data.frame()
theta = theta %>% 
  mutate(t = 1:T) %>% 
  rename("theta_t"=".")

lambda %>% 
  hchart("line", hcaes(x = t, y = lambda_td, group = d))

dados_dengue_longo %>% 
  hchart("line", hcaes(x = t, y = n_td, group = d))

highchart() %>% 
  hc_add_series(type = "line",
                data = lambda %>% filter(d==1) %>% pull(lambda_td)) %>% 
  hc_add_series(type = "line",
                data = dados_dengue_longo %>% filter(d ==1) %>% pull(n_td))

dados_dengue_sem_d0$N_t = rowSums(dados_dengue_sem_d0, na.rm = T)

dados_plot = data.frame(
  N_t = dados_dengue_sem_d0$N_t,
  theta_t = theta$theta_t,
  t = theta$t
) %>% 
  pivot_longer(
    cols = 1:2
  )

dados_plot %>% 
  hchart("line", hcaes(x = t, y = value, group = name)) %>% 
  hc_xAxis(lineColor = "#f7f7f7", gridLineColor = "#f4f4f4") %>% 
  hc_yAxis(lineColor = "#f7f7f7", gridLineColor = "#f4f4f4",
           title = list(text = "Valor")) %>% 
  hc_title(
    text = paste0(
      "<b style='display: block; font-size: 12px;'>",
      "Valor estimado e real do total de casos ao longo do tempo</b>"
    ),
    margin = 20,
    align = "left",
    style = list(useHTML = TRUE)
  )


# Estimativas com chute inicial ================================================

#estimativas_por_delay = readRDS("modelos/estimativas_n_td_por_delay.rds")
abcf_por_dalay = readRDS( "modelos/abcf_por_dalay.rds")
abcf_N_t = readRDS("modelos/abcf_N_t.rds")

a_alpha_inicial = abcf_por_dalay$d1[1]
b_alpha_inicial = abcf_por_dalay$d1[2]
c_alpha_inicial = abcf_por_dalay$d1[3]
f_alpha_inicial = abcf_por_dalay$d1[4]

a_theta_inicial = abcf_N_t[1]
b_theta_inicial = abcf_N_t[2]
c_theta_inicial = abcf_N_t[3]
f_theta_inicial = abcf_N_t[4]

genLog = function(t, a, b, c, f, logScale = TRUE){
  logV = log(f)+log(a)+log(c)-(c*t)-(f+1)*log( b+exp(-c*t) )
  if (logScale){
    return(logV);
  } else {
    return(exp(logV));
  }
}

theta_t_inicial = genLog(t = 1:T, a = a_theta_inicial, 
                         b = b_theta_inicial, 
                         c = c_theta_inicial, 
                         f = f_theta_inicial, logScale = FALSE)

alfha_t_inical = genLog(t = 1:T, a = a_alpha_inicial,
                        b = b_alpha_inicial, 
                        c = c_alpha_inicial, 
                        f = f_theta_inicial, logScale = FALSE)

chute_inicial = list(
  list(
    
    alpha = alfha_t_inical,
    
    a_alpha = a_alpha_inicial,
    b_alpha = b_alpha_inicial,
    c_alpha = c_alpha_inicial,
    f_alpha = f_theta_inicial,
    
    theta = theta_t_inicial,
    
    a_theta = a_theta_inicial,
    b_theta = b_theta_inicial,
    c_theta = c_theta_inicial,
    f_theta = f_theta_inicial,
    
    psi = n_dalay_1$n_td
  )
)

output = sampling(modelo_stan,
                  data = dados_stan,
                  iter = number_interations,
                  warmup = warmup,
                  chains = chains,
                  pars = params,
                  init = chute_inicial,
                  verbose = FALSE)

estimativas_com_chute = rstan::extract(output)

ts.plot(estimativas_com_chute$b_beta)

mean(estimativas_com_chute$lambda) 
mean(estimativas_com_chute$theta)

lambda = estimativas_com_chute$lambda %>% as.data.frame()
lambda = apply(lambda, 2, mean) %>% as.data.frame()
lambda <- lambda %>% 
  mutate(t = rep(1:T, D-1)) %>% 
  group_by(t) %>% 
  mutate(d = 1:(D-1))
colnames(lambda)[1] = "lambda_td"

theta = estimativas_com_chute$theta %>% as.data.frame()
theta = apply(theta, 2, mean) %>% as.data.frame()
theta = theta %>% 
  mutate(t = 1:T) %>% 
  rename("theta_t"=".")

lambda %>% 
  hchart("line", hcaes(x = t, y = lambda_td, group = d))

dados_dengue_longo %>% 
  hchart("line", hcaes(x = t, y = n_td, group = d))

highchart() %>% 
  hc_add_series(type = "line",
                data = lambda %>% filter(d==1) %>% pull(lambda_td)) %>% 
  hc_add_series(type = "line",
                data = dados_dengue_longo %>% filter(d ==1) %>% pull(n_td))

dados_dengue_sem_d0$N_t = rowSums(dados_dengue_sem_d0, na.rm = T)

dados_plot = data.frame(
  N_t = dados_dengue_sem_d0$N_t,
  theta_t = theta$theta_t,
  t = theta$t
) %>% 
  pivot_longer(
    cols = 1:2
  )

dados_plot %>% 
  hchart("line", hcaes(x = t, y = value, group = name)) %>% 
  hc_xAxis(lineColor = "#f7f7f7", gridLineColor = "#f4f4f4") %>% 
  hc_yAxis(lineColor = "#f7f7f7", gridLineColor = "#f4f4f4",
           title = list(text = "Valor")) %>% 
  hc_title(
    text = paste0(
      "<b style='display: block; font-size: 12px;'>",
      "Valor estimado e real do total de casos ao longo do tempo</b>"
    ),
    margin = 20,
    align = "left",
    style = list(useHTML = TRUE)
  )
