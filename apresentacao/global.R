
rm(list = ls())

library(rstan)
library(dplyr)
library(INLA)
library(stringr)
library(tidyverse)
library(highcharter)

# Dados reais ==================================================================

dados_dengue = readr::read_rds("izabel\\dengueData.RDS")

T = dim(dados_dengue)[1]
D = dim(dados_dengue)[2]

dados_dengue_longo <- dados_dengue %>% 
  rownames_to_column(var = "t") %>% 
  pivot_longer(cols = 2:(D+1), names_to = "d", values_to = "n_td") %>% 
  mutate(delay = d,
         delay = factor(delay, levels = c("d0","d1","d2","d3","d4","d5","d6","d7","d8","d9", "d10")),
         d = str_remove_all(d,"d"),
         d = as.numeric(d)) %>% 
  group_by(d) %>% 
  mutate(t = rep(1:T)) %>% 
  ungroup()


formatacao_grafico <- function(plot, titulo, titulo_x, titulo_y){
  
  plot %>% 
    hc_yAxis(title = list(text = titulo_y),
             lineColor = "#f7f7f7", gridLineColor = "#f4f4f4",
             labels = list(format = "{value:,.0f}")) %>%
    hc_xAxis(lineColor = "#f7f7f7", gridLineColor = "#f4f4f4",
             title = list(text = titulo_x)) %>% 
    hc_title(
      text = paste0(
        "<b style='display: block; font-size: 15px;'>",
        titulo, "</b>"
      ),
      margin = 20,
      align = "center",
      style = list(useHTML = TRUE)
    ) %>% 
    hc_colors(c(
      '#2f7ed8', '#0d233a', '#8bbc21', '#910000', '#1aadce',
      '#492970', '#f28f43', '#77a1e5', '#c42525', '#a6c96a'
    )) %>% 
    hc_exporting(
      enabled = TRUE, 
      buttons = list(
        contextButton = list(
          menuItems = list('downloadCSV', 'downloadSVG')
        )
      )
    )
  
}

plot_n_td_completo = dados_dengue_longo %>%
  hchart('line', hcaes(x = t, 
                       y = n_td, 
                       group = delay,
                       d = d), marker = F,
         showInLegend = T) %>% 
  formatacao_grafico(titulo = "Casos Relatados de Dengue ao longo do Tempo por Atraso",
                     titulo_x = "Tempo t",
                     titulo_y = "Casos de Dengue (n_td)") %>% 
  hc_tooltip(
    pointFormat = paste0(
      '<span style="color:{series.color}; font-weight: bold;">Numéro de casos no tempo {point.t} com {point.d} unidade(s) de atraso:</span> <b>{point.y:,.0f}</b>'
    )
  ) 



# filtrando apenas as primeiras 35 semanas (primeira onda) =====================

dados_dengue = dados_dengue[1:35,]

T = dim(dados_dengue)[1]
D = dim(dados_dengue)[2]

# criando os NAs
dados_dengue[outer(1:T, 0:(D - 1), FUN = "+") > T] <- NA

dados_dengue_longo <- dados_dengue %>% 
  rownames_to_column(var = "t") %>% 
  pivot_longer(cols = 2:(D+1), names_to = "d", values_to = "n_td") %>% 
  mutate(delay = d,
         delay = factor(delay, levels = c("d0","d1","d2","d3","d4","d5","d6","d7","d8","d9","d10")),
         d = str_remove_all(d,"d"),
         d = as.numeric(d)) %>% 
  group_by(d) %>% 
  mutate(t = rep(1:T)) %>% 
  ungroup()

plot_n_td = dados_dengue_longo %>%
  hchart('line', hcaes(x = t, 
                       y = n_td, 
                       group = delay,
                       d = d), marker = F,
         showInLegend = T) %>% 
  formatacao_grafico(titulo = "Casos Relatados de Dengue ao longo do Tempo por Atraso",
                     titulo_x = "Tempo t",
                     titulo_y = "Casos de Dengue (n_td)") %>% 
  hc_tooltip(
    pointFormat = paste0(
      '<span style="color:{series.color}; font-weight: bold;">Numéro de casos no tempo {point.t} com {point.d} unidade(s) de atraso:</span> <b>{point.y:,.0f}</b>'
    )
  ) 




plot_estrutura_delay_tempo = dados_dengue_longo %>% 
  hchart('line', hcaes(x = t, 
                       y = log(n_td + 0.05), 
                       group = delay,
                       d = d),marker = F,
         showInLegend = T) %>% 
  formatacao_grafico(titulo = "Estrutura de Atraso ao longo do Tempo",
                     titulo_x = "Tempo t",
                     titulo_y = "Log (Casos de Dengue (n_td) + 0,05)") %>% 
  hc_tooltip(
    pointFormat = paste0(
      '<span style="color:{series.color}; font-weight: bold;">Logaritmo no tempo {point.t} com {point.d} unidade(s) de atraso:</span> <b>{point.y:.2f}</b>'
    )
  ) 



# estimando n_td por delay =====================================================

#modelo_stan =  stan_model("izabel\\modelLogistic.stan")

stanLogistic = function(stanModel, y){
  
  y = y[!is.na(y)]
  n = length(y)
  
  # -> Stan configuration
  warmup = 1000
  chains = 1
  thin = 1
  sample_size = 10000
  number_interations = warmup + thin*sample_size
  
  # -> Preparing model
  
  params = c("mu", "a", "b", "c", "f")
  
  output = sampling(stanModel,
                    data = list(n = n, y = y),
                    iter = number_interations,
                    warmup = warmup,
                    chains = chains,
                    pars = params,
                    verbose = FALSE)
  
  # -> Extracting samples
  rstan::extract(output)
  
}

# para cada tempo do dalay i, vamos ter 35(T) - i estimativas de mu com mil repeticoes
# temos mil estimativas de a,b,c,f

#estimativas = apply(dados_dengue, 2, function(y) stanLogistic(modelo_stan, y = y))
#saveRDS(estimativas, "estimativas/estimativas_n_td_por_delay.rds",version = 2)
estimativas = readRDS("estimativas/estimativas_n_td_por_delay.rds")

# abcf_por_dalay = lapply(estimativas, function(x) c(mean(x$a), mean(x$b), mean(x$c), mean(x$f)) )
# saveRDS(abcf_por_dalay, "estimativas/abcf_por_dalay.rds", version = 2)
abcf_por_dalay = readRDS("estimativas/abcf_por_dalay.rds")

# estimativas_theta = stanLogistic(modelo_stan, y = rowSums(dados_dengue, na.rm = TRUE))
# saveRDS(estimativas_theta, "estimativas/estimativas_theta.rds",version = 2)
estimativas_theta = readRDS("estimativas/estimativas_theta.rds")

genLog = function(t, a, b, c, f, logScale = TRUE){
  logV = log(f)+log(a)+log(c)-(c*t)-(f+1)*log( b+exp(-c*t) )
  if (logScale){
    return(logV);
  } else {
    return(exp(logV));
  }
}

theta_inicial = genLog(t = 1:T,
                       a = mean(estimativas_theta$a),
                       b = mean(estimativas_theta$b),
                       c = mean(estimativas_theta$c),
                       f = mean(estimativas_theta$f),
                       logScale = F)

dados_dengue$N_t = rowSums(dados_dengue, na.rm = T)

plot_N_t_inical = dados_dengue %>% 
  bind_cols(theta_t = theta_inicial) %>% 
  mutate(t = 1:T) %>% 
  pivot_longer(cols = 12:13) %>% 
  hchart('line', hcaes(x = t, 
                       y = value, 
                       group = name),marker = F,
         showInLegend = T) %>% 
  formatacao_grafico(titulo = "Total de Casos reais e estimados de Dengue no Tempo",
                     titulo_x = "Tempo t",
                     titulo_y = "Total de Casos") %>% 
  hc_tooltip(
    pointFormat = paste0(
      '<span style="color:{series.color}; font-weight: bold;">Total de casos no tempo {point.t}:</span> <b>{point.y:,.0f}</b>'
    )
  ) %>% 
  hc_subtitle(
    text = "Estimativa sem incorporar estrutura de Atraso na Notificação"
  )



# graficos estimativas =========================================================

lambda_t_d = list()
for(d in (0+1):(10+1)){
  lambda_t_d[[d]] = genLog(t = 1:T, a = abcf_por_dalay[[d]][1], b = abcf_por_dalay[[d]][2],
                           c = abcf_por_dalay[[d]][3], f = abcf_por_dalay[[d]][4], logScale = F)
  
}

dados_estimados <- as.data.frame(do.call(cbind, lambda_t_d))
colnames(dados_estimados) = paste0("d", 0:10)
dados_estimados <- dados_estimados %>% 
  pivot_longer(cols = (0+1):(10+1),
               names_to = "d") %>% 
  group_by(d) %>% 
  mutate(t = 1:T) %>% 
  ungroup() %>% 
  mutate(delay = factor(d, levels = c("d0", "d1", "d2", "d3", "d4", "d5", "d6", "d7", "d8", "d9", "d10" )),
         d = str_remove(d, "d"))

plot_lambda_td = dados_estimados %>% 
  hchart('line', hcaes(x = t, 
                       y = value, 
                       group = delay,
                       d = d), marker = F,
         showInLegend = T) %>% 
  formatacao_grafico(titulo = "Casos Estimados de Dengue no Tempo por Atraso na Notificação",
                     titulo_x = "Tempo t",
                     titulo_y = "Casos Estimados") %>% 
  hc_tooltip(
    pointFormat = paste0(
      '<span style="color:{series.color}; font-weight: bold;">Casos Estimados no tempo {point.t} com {point.d} unidade(s) de atraso:</span> <b>{point.y:,.0f}</b>'
    )
  ) %>% 
  hc_subtitle(
    text = "Estimativa sem incorporar estrutura de Atraso na Notificação"
  )




plot_estrutura_delay_estimativas = dados_estimados %>% 
  hchart('line', hcaes(x = t,
                       y = log(value+0.05), 
                       group = delay,
                       d = d), marker = F,
         showInLegend = T)%>% 
  formatacao_grafico(titulo = "Estrutura de Atraso no Tempo para os Casos Estimados",
                     titulo_x = "Tempo t",
                     titulo_y = "Log( Valor Estimado (lambda_td) + 0.05 )") %>% 
  hc_tooltip(
    pointFormat = paste0(
      '<span style="color:{series.color}; font-weight: bold;">Logaritmo no tempo {point.t} com {point.d} unidade(s) de atraso:</span> <b>{point.y:.2f}</b>'
    )
  ) %>% 
  hc_subtitle(
    text = "Estimativa sem incorporar estrutura de Atraso na Notificação"
  )

  

# Incorporando estrutura delay =================================================

dados_dengue_longo_completo = dados_dengue_longo %>% 
  mutate(d = d + 1) %>% 
  filter(!is.na(n_td)) %>% 
  arrange(d)

# separando o delay 0 (novo 1)
td_dalay_1 = (dados_dengue_longo_completo %>% filter(d == 1)) %>% select(t,d)
n_dalay_1 = (dados_dengue_longo_completo %>% filter(d == 1)) %>% 
  select(n_td) %>% 
  mutate(n_td = as.numeric(n_td))

td_dalay_k = dados_dengue_longo_completo %>% filter(d != 1) %>% select(t,d)
n_dalay_k = dados_dengue_longo_completo %>% filter(d != 1) %>%
  select(n_td) %>% 
  mutate(n_td = as.numeric(n_td))

dados_stan = list(
  nk = n_dalay_1$n_td, n_k = n_dalay_k$n_td,
  T = T, D = D,
  Tk = td_dalay_1$t, Dk = td_dalay_1$d,
  T_k =  td_dalay_k$t,  D_k =  td_dalay_k$d,
  qk = nrow(n_dalay_1), q_k = nrow(n_dalay_k)
)

#modelo_completo_stan =  stan_model("izabel\\modelCompleteLogistic.stan")

warmup = 1000
chains = 1
thin = 1
sample_size = 10000
number_interations = warmup + thin*sample_size

params = c("lambda",
           "alpha", "a_alpha", "b_alpha", "c_alpha", "f_alpha",
           "beta", "b_beta",
           "theta", "a_theta", "b_theta", "c_theta", "f_theta",
           "psi")

# output_modelo_sem_chute = rstan::sampling(modelo_completo_stan,
#                                           data = dados_stan,
#                                           iter = number_interations,
#                                           warmup = warmup,
#                                           chains = chains,
#                                           pars = params,
#                                           #init = init,
#                                           verbose = FALSE)

# estimativas_sem_chute = rstan::extract(output_modelo_sem_chute)
# saveRDS(estimativas_sem_chute, "estimativas\\estimativas_sem_chute.rds", version = 2)
estimativas_sem_chute = readRDS("estimativas\\estimativas_sem_chute.rds")

theta_t_sem_chute = apply(estimativas_sem_chute$theta, 2, mean)

plot_N_t_sem_chute = dados_dengue %>% 
  bind_cols(theta_t = theta_t_sem_chute) %>% 
  mutate(t = 1:T) %>% 
  pivot_longer(cols = 12:13) %>% 
  hchart('line', hcaes(x = t, 
                       y = value, 
                       group = name), marker = F,
         showInLegend = T) %>% 
  formatacao_grafico(titulo = "Total de Casos reais e estimados de Dengue no Tempo",
                     titulo_x = "Tempo t",
                     titulo_y = "Total de Casos") %>% 
  hc_tooltip(
    pointFormat = paste0(
      '<span style="color:{series.color}; font-weight: bold;">Total de casos no tempo {point.t}:</span> <b>{point.y:,.0f}</b>'
    )
  ) %>% 
  hc_subtitle(
    text = "Estimativa incorporando estrutura de Delay sem chute inicial"
  )




lambda.mean = matrix(NA, T, D - 1)
for(t in 1:T){
  for(d in 1:(D - 1)){
    lambda.mean[t, d] = mean(estimativas_sem_chute$lambda[ , t, d])
  }
}

lambda_td_sem_chute = as.data.frame(lambda.mean)
colnames(lambda_td_sem_chute) = paste0("d",1:10)
lambda_td_sem_chute = lambda_td_sem_chute %>% 
  rownames_to_column(var = "t")

lambda_td_sem_chute_longo = lambda_td_sem_chute %>% 
  pivot_longer(cols = (0+2):D,
               names_to = "delay",
               values_to = "lambda_td") %>% 
  mutate(delay = factor(delay, levels = c("d1", "d2", "d3", "d4", "d5", "d6", "d7", "d8", "d9", "d10" )),
         d = str_remove(delay, "d"))

lambda_estimado_sem_chute = lambda_td_sem_chute_longo %>% 
  hchart('line', hcaes(x = t, 
                       y = lambda_td, 
                       group = delay,
                       d = d), marker = F,
         showInLegend = T) %>% 
  formatacao_grafico(titulo = "Casos Estimados de Dengue no Tempo por tempo de Atraso",
                     titulo_x = "Tempo t",
                     titulo_y = "Casos Estimados") %>% 
  hc_tooltip(
    pointFormat = paste0(
      '<span style="color:{series.color}; font-weight: bold;">Casos Estimados no tempo {point.t} com {point.d} unidade(s) de atraso:</span> <b>{point.y:.2f}</b>'
    )
  ) %>% 
  hc_subtitle(
    text = "Estimativa sem incorporando estrutura de Delay sem chute inicial"
  )

# Incorporando estrutura delay e estimativa inicial ============================


a_alpha_inicial = abcf_por_dalay$d1[1]
b_alpha_inicial = abcf_por_dalay$d1[2]
c_alpha_inicial = abcf_por_dalay$d1[3]
f_alpha_inicial = abcf_por_dalay$d1[4]

a_theta_inicial = mean(estimativas_theta$a)
b_theta_inicial = mean(estimativas_theta$b)
c_theta_inicial = mean(estimativas_theta$c)
f_theta_inicial = mean(estimativas_theta$f)

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

# output_modelo_com_chute = rstan::sampling(modelo_completo_stan,
#                                           data = dados_stan,
#                                           iter = number_interations,
#                                           warmup = warmup,
#                                           chains = chains,
#                                           pars = params,
#                                           init = chute_inicial,
#                                           verbose = FALSE)

# estimativas_com_chute = rstan::extract(output_modelo_com_chute)
# saveRDS(estimativas_com_chute, "estimativas\\estimativas_com_chute.rds", version = 2)
estimativas_com_chute = readRDS("estimativas\\estimativas_com_chute.rds")

theta_t_com_chute = apply(estimativas_com_chute$theta, 2, mean)

plot_N_t_com_chute = dados_dengue %>% 
  bind_cols(theta_t = theta_t_com_chute) %>% 
  mutate(t = 1:T) %>% 
  pivot_longer(cols = 12:13) %>% 
  hchart('line', hcaes(x = t, 
                       y = value, 
                       group = name), marker = F,
         showInLegend = T) %>% 
  formatacao_grafico(titulo = "Total de Casos reais e estimados de Dengue no Tempo",
                     titulo_x = "Tempo t",
                     titulo_y = "Total de Casos") %>% 
  hc_tooltip(
    pointFormat = paste0(
      '<span style="color:{series.color}; font-weight: bold;">Total de casos no tempo {point.t}:</span> <b>{point.y:,.0f}</b>'
    )
  ) %>% 
  hc_subtitle(
    text = "Estimativa incorporando estrutura de Atraso na Notificação"
  )



lambda.mean = matrix(NA, T, D - 1)
for(t in 1:T){
  for(d in 1:(D - 1)){
    lambda.mean[t, d] = mean(estimativas_com_chute$lambda[ , t, d])
  }
}

lambda_td_com_chute = as.data.frame(lambda.mean)
colnames(lambda_td_com_chute) = paste0("d",1:10)
lambda_td_com_chute = lambda_td_com_chute %>% 
  rownames_to_column(var = "t")

lambda_td_com_chute_longo = lambda_td_com_chute %>% 
  pivot_longer(cols = (0+2):D,
               names_to = "delay",
               values_to = "lambda_td") %>% 
  mutate(delay = factor(delay, levels = c("d1", "d2", "d3", "d4", "d5", "d6", "d7", "d8", "d9", "d10" )),
         d = str_remove(delay, "d"))

plot_lamda_com_chute = lambda_td_com_chute_longo %>% 
  hchart('line', hcaes(x = t, 
                       y = lambda_td, 
                       group = delay,
                       d = d), marker = F,
         showInLegend = T) %>% 
  formatacao_grafico(titulo = "Casos Estimados de Dengue no Tempo por tempo de Atraso na Notificação",
                     titulo_x = "Tempo t",
                     titulo_y = "Casos Estimados") %>% 
  hc_tooltip(
    pointFormat = paste0(
      '<span style="color:{series.color}; font-weight: bold;">Casos Estimados no tempo {point.t} com {point.d} unidade(s) de atraso:</span> <b>{point.y:.2f}</b>'
    )
  ) %>% 
  hc_subtitle(
    text = "Estimativa incorporando estrutura de Atraso na Notificação"
  )



dados_comparacao_lambda = lambda_td_com_chute_longo %>% 
  mutate(t = as.numeric(t)) %>% 
  left_join(dados_dengue_longo %>% select(-d), by = c("t", "delay"))

dados_comparacao_lambda_longo = dados_comparacao_lambda %>% 
  pivot_longer(cols = c("lambda_td", "n_td"))


formatar_plot_n_lambda_delayi = function(dados_plot, num_delay){
  
  dados_plot %>% 
    hchart('line', hcaes(x = t, 
                         y = value, 
                         group = name,
                         delay = delay), marker = F,
           showInLegend = T) %>% 
    formatacao_grafico(titulo = paste0("Casos Real e Estimado de Dengue no Tempo - Delay ",num_delay),
                       titulo_x = "Tempo t",
                       titulo_y = "Quantidade de Casos") %>% 
    hc_tooltip(
      pointFormat = paste0(
        '<span style="color:{series.color}; font-weight: bold;">Casos no tempo {point.t} com ',num_delay,' unidade(s) de atraso:</span> <b>{point.y:,.2f}</b>'
      )
    ) %>% 
    hc_subtitle(
      text = "Estimativa incorporando estrutura de Delay com chute inicial"
    )
}


