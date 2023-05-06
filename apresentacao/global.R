
rm(list = ls())

library(rstan)
library(dplyr)
library(INLA)
library(stringr)
library(tidyverse)
library(highcharter)
library(latex2exp)
library(TeachingDemos)
library(coda)
library(kableExtra)
library(htmltools)

# Distribuições ================================================================

distribuicoes = tibble(
  x = seq(0,50,0.01),
  a = dgamma(x, shape = 0.1, rate = 0.1),
  c = dgamma(x, shape = 2, rate = 9),
  f = dgamma(x, shape = 0.01, rate = 0.01)
)

 
plot_a <- distribuicoes %>% 
  hchart('line', hcaes(x = x,
                       y = a),
         marker = F) %>% 
  hc_xAxis(max = 4) %>% 
  hc_yAxis(title = list(text = "f(x)")) %>% 
  hc_title(
    text = paste0(
      "<b style='display: block; font-size: 15px;'>",
      "a ~ Gamma(0.1, 0.1)", "</b>"
    ),
    margin = 20,
    align = "center",
    style = list(useHTML = TRUE)
  )

plot_c <- distribuicoes %>% 
  hchart('line', hcaes(x = x,
                       y = c),
         marker = F) %>% 
  hc_xAxis(max = 1.5) %>% 
  hc_yAxis(title = list(text = "f(x)")) %>% 
  hc_title(
    text = paste0(
      "<b style='display: block; font-size: 15px;'>",
      "c ~ Gamma(2, 9)", "</b>"
    ),
    margin = 20,
    align = "center",
    style = list(useHTML = TRUE)
  )

plot_f <- distribuicoes %>% 
  hchart('line', hcaes(x = x,
                       y = f),
         marker = F) %>% 
  hc_xAxis(max = 2) %>% 
  hc_yAxis(title = list(text = "f(x)")) %>% 
  hc_title(
    text = paste0(
      "<b style='display: block; font-size: 15px;'>",
      "f ~ Gamma(0.01, 0.01)", "</b>"
    ),
    margin = 20,
    align = "center",
    style = list(useHTML = TRUE)
  )


plot_b <- tibble(
  x = seq(-3,3, 0.1),
  b = dnorm(x, mean = 0, sd = sqrt(20))
  ) %>% 
  hchart('line', hcaes(x = x,
                       y = b),
         marker = F) %>% 
  hc_yAxis(title = list(text = "f(x)")) %>% 
  hc_title(
    text = paste0(
      "<b style='display: block; font-size: 15px;'>",
      "b ~ Normal(0, sqrt(20))", "</b>"
    ),
    margin = 20,
    align = "center",
    style = list(useHTML = TRUE)
  )

# Dados reais ==================================================================

dados_dengue = readr::read_rds("izabel\\dengueData.RDS")

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
    ) %>% 
    hc_xAxis(
      plotLines = list(
        list(
          value = 25,
          color = "gray",
          dashStyle = 'longdashdot',
          width = 2,
          label = list(
            text = "Hoje (T = 25)",
            style = list(color = "black", fontWeight = "bold")
          )
        )
      )
    )
  
}



# filtrando apenas as primeiras 35 semanas (primeira onda) =====================

dados_dengue = dados_dengue[1:35,]
dados_dengue_real = dados_dengue
dados_dengue_real$N = rowSums(dados_dengue_real)

T = dim(dados_dengue)[1]
D = dim(dados_dengue)[2]
hoje = 25
#hoje = 20

# criando os NAs
dados_dengue[outer(1:T, 0:(D - 1), FUN = "+") > T] <- NA
dados_dengue_completo = dados_dengue
dados_dengue_completo$N = rowSums(dados_dengue_completo, na.rm = T)


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

dados_dengue_longo_completo <- dados_dengue_completo %>% 
  rownames_to_column(var = "t") %>% 
  pivot_longer(cols = 2:(D+1), names_to = "d", values_to = "n_td") %>% 
  mutate(delay = d,
         delay = factor(delay, levels = c("d0","d1","d2","d3","d4","d5","d6","d7","d8","d9","d10")),
         d = str_remove_all(d,"d"),
         d = as.numeric(d)) %>% 
  group_by(d) %>% 
  mutate(t = rep(1:T)) %>% 
  ungroup()

# plots motivacao ====================================

dados_plot = dados_dengue

for(i in 1:11){
  if(i == 1){
    dados_plot[,i] = dados_plot[,i]
  }else{
    dados_plot[,i] = ifelse(is.na(dados_plot[,i]), 0 , dados_plot[,i]) + 
      ifelse(is.na(dados_plot[,i-1]), 0 , dados_plot[,i-1])
  }
}
colnames(dados_plot)[1:11] = paste0("Atualização em t ", 25:35)

dados_plot[outer(1:T, 0:(D - 1), FUN = "+") > T] <- NA

dados_plot = dados_plot%>% 
  mutate(t = 1:T)

N_t_atualizacao = dados_plot %>% 
  pivot_longer(
    cols = 1:D,
    names_to = "Última Atualização",
    values_to = "Total de Casos de Dengue"
  ) %>% 
  hchart('line', hcaes(x = t, 
                       y = `Total de Casos de Dengue`, 
                       group = `Última Atualização`),
         marker = F) %>% 
  formatacao_grafico(titulo = "Total de Casos Relatados de Dengue ao longo do Tempo por período de Atualização",
                     titulo_x = "Tempo t",
                     titulo_y = "Total de Casos de Dengue (N_t)") %>% 
  hc_tooltip(
    pointFormat = paste0(
      '<span style="color:{series.color}; font-weight: bold;">Numéro total de casos no tempo {point.t} com {series.name} :</span> <b>{point.y:,.0f}</b>'
    )
  )

# tabelas motivacao =====================================



# plots dados completos =================================

plot_N_t_completo = dados_dengue_longo_completo %>% 
  group_by(t) %>% 
  summarise(N = mean(N)) %>% 
  hchart("line", hcaes(x= t, y = N),
         marker = F) %>% 
  formatacao_grafico(titulo = "Total de Casos Relatados de Dengue ao longo do Tempo",
                     titulo_x = "Tempo t",
                     titulo_y = "Total de Casos de Dengue (N_t)") %>% 
  hc_tooltip(
    pointFormat = paste0(
      '<span style="color:{series.color}; font-weight: bold;">Numéro total de casos no tempo {point.t}:</span> <b>{point.y:,.0f}</b>'
    )
  ) %>% 
  hc_colors(c('black'))

plot_n_td_completo = dados_dengue_longo_completo %>%
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

# plots com NA ============================================

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
  formatacao_grafico(titulo = "Influência do Atraso nas Notificações ao longo do Tempo",
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
#saveRDS(estimativas, "estimativas/estimativas_n_td_por_delay_T20.rds",version = 2)
estimativas = readRDS("estimativas/estimativas_n_td_por_delay.rds")

# abcf_por_dalay = lapply(estimativas, function(x) c(mean(x$a), mean(x$b), mean(x$c), mean(x$f)) )
# saveRDS(abcf_por_dalay, "estimativas/abcf_por_dalay.rds", version = 2)
abcf_por_dalay = readRDS("estimativas/abcf_por_dalay.rds")

# estimativas_theta = stanLogistic(modelo_stan, y = rowSums(dados_dengue, na.rm = TRUE))
# saveRDS(estimativas_theta, "estimativas/estimativas_theta.rds",version = 2)
estimativas_theta = readRDS("estimativas/estimativas_theta.rds")

ic_mu_theta = HPDinterval(mcmc(estimativas_theta$mu), prob = 0.99) %>% 
  as.data.frame() %>% 
  mutate(t = 1:T)

library(brms)
HPDinterval(mcmc(dpois(x = 1:T,estimativas_theta$mu[,1])))


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

calcula_mae_e_rmse = function(valores_estimados, valores_reais){
  
  erros = valores_estimados - valores_reais
  rmse = sqrt(mean(erros^2))
  mae = mean(abs(erros))
  
  return(
    paste0("MAE=", round(mae, 2), " RMSE=", round(rmse,2))
  )
  
}


plot_N_t_inical = dados_dengue %>% 
  bind_cols(theta_t = theta_inicial) %>% 
  mutate(t = 1:T) %>% 
  pivot_longer(cols = 12:13) %>% 
  hchart('line', hcaes(x = t, 
                       y = value, 
                       group = name),marker = F,
         showInLegend = T,
         tooltip = list(pointFormat = paste0(
           'Total de casos no tempo {point.t}:</span> <b>{point.y:,.0f}</b><br>')
           )
         ) %>% 
  formatacao_grafico(titulo = "Total de Casos reais e estimados de Dengue no Tempo",
                     titulo_x = "Tempo t",
                     titulo_y = "Total de Casos") %>% 
  hc_add_series(data = ic_mu_theta,
                type = "arearange",
                hcaes(x = t, low = lower, high = upper),
                marker = F,
                color = "#BEBEBE",
                name = "IC 99%",
                tooltip = list(pointFormat = paste0(
                  "IC 99% no tempo {point.t}: <b> = [{point.lower:,.2f}; {point.upper:,.2f}]</b><br>"
                ))
                ) %>% 
  hc_subtitle(
    text = "Estimativa sem incorporar estrutura de Atraso na Notificação"
  ) %>% 
  hc_credits(enabled = TRUE,
             text = calcula_mae_e_rmse(
               valores_reais = dados_dengue$N_t,
               valores_estimados = theta_inicial
             ))



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
  formatacao_grafico(titulo = "Influência do Atraso nas Notificações no Tempo para os Casos Estimados",
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

 
# tabela estimativas ===========================================================

dados_tabela = dados_estimados %>% 
  pivot_wider(values_from = value,
              names_from = d,
              id_cols = t) 
colnames(dados_tabela) = c("t",paste0("d", 0:(D-1)))

tabela = data.frame(
  t = 1:T,
  d0 = paste0(dados_dengue_real$d0," (",round(dados_tabela$d0,0),")"),
  d1 = paste0(dados_dengue_real$d1," (",round(dados_tabela$d1,0),")"),
  d2 = paste0(dados_dengue_real$d2," (",round(dados_tabela$d2,0),")"),
  d3 = paste0(dados_dengue_real$d3," (",round(dados_tabela$d3,0),")"),
  d4 = paste0(dados_dengue_real$d4," (",round(dados_tabela$d4,0),")"),
  d5 = paste0(dados_dengue_real$d5," (",round(dados_tabela$d5,0),")"),
  d6 = paste0(dados_dengue_real$d6," (",round(dados_tabela$d6,0),")"),
  d7 = paste0(dados_dengue_real$d7," (",round(dados_tabela$d7,0),")"),
  d8 = paste0(dados_dengue_real$d8," (",round(dados_tabela$d8,0),")"),
  d9 = paste0(dados_dengue_real$d9," (",round(dados_tabela$d9,0),")"),
  d10 = paste0(dados_dengue_real$d10," (",round(dados_tabela$d10,0),")"),
  N = paste0(dados_dengue_real$N," (",round(theta_inicial,0),")")
)


tabela_estimativas_iniciais = tabela %>% 
  kable(
    align = "c", 
    format = "html",
    escape = F
  ) %>% 
  kable_styling(
    bootstrap_options = c("hover", "condensed", "responsive"),
    font_size = 12
  ) %>% 
  row_spec(0, background = "#e07a5f", color = "#07080E") %>% 
  column_spec(1, background = "#FFFFFF", color = "#07080E", bold = TRUE) %>% 
  row_spec(1:(nrow(tabela)), background = "rgb(235, 235, 245, 0.3)", color = "#07080E")  %>% 
  scroll_box(height = "280px")

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



ic_theta_com_chute = HPDinterval(mcmc(estimativas_com_chute$theta), prob = 0.99) %>% 
  as.data.frame() %>% 
  mutate(t = 1:T)

theta_t_com_chute = apply(estimativas_com_chute$theta, 2, mean)

plot_N_t_com_chute = dados_dengue %>% 
  bind_cols(theta_t = theta_t_com_chute) %>% 
  mutate(t = 1:T) %>% 
  pivot_longer(cols = 12:13) %>% 
  hchart('line', hcaes(x = t, 
                       y = value, 
                       group = name), marker = F,
         showInLegend = T,
         tooltip = list(pointFormat = paste0(
           'Total de casos no tempo {point.t}:</span> <b>{point.y:,.0f}</b><br>')
         )
         ) %>% 
  formatacao_grafico(titulo = "Total de Casos reais e estimados de Dengue no Tempo",
                     titulo_x = "Tempo t",
                     titulo_y = "Total de Casos") %>% 
  hc_add_series(data = ic_theta_com_chute,
                type = "arearange",
                hcaes(x = t, low = lower, high = upper),
                marker = F,
                color = "#BEBEBE",
                name = "IC 99%",
                tooltip = list(pointFormat = paste0(
                  "IC 99% no tempo {point.t}: <b> = [{point.lower:,.2f}; {point.upper:,.2f}]</b><br>"
                ))
  ) %>% 
  hc_subtitle(
    text = "Estimativa incorporando estrutura de Atraso na Notificação"
  ) %>% 
  hc_credits(enabled = TRUE,
             text = calcula_mae_e_rmse(
               valores_reais = dados_dengue$N_t,
               valores_estimados = theta_t_com_chute
             ))


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

ic_lambda_delayi = function(estimativas_lambda, delayi){
  HPDinterval(mcmc(estimativas_lambda[,,delayi]), prob = 0.99)
}

# tabela estimativas com chute =================================================

tabela_com_chute = data.frame(
  t = 1:T,
  d1 = paste0(dados_dengue_real$d1," (",round(lambda_td_com_chute$d1,0),")"),
  d2 = paste0(dados_dengue_real$d2," (",round(lambda_td_com_chute$d2,0),")"),
  d3 = paste0(dados_dengue_real$d3," (",round(lambda_td_com_chute$d3,0),")"),
  d4 = paste0(dados_dengue_real$d4," (",round(lambda_td_com_chute$d4,0),")"),
  d5 = paste0(dados_dengue_real$d5," (",round(lambda_td_com_chute$d5,0),")"),
  d6 = paste0(dados_dengue_real$d6," (",round(lambda_td_com_chute$d6,0),")"),
  d7 = paste0(dados_dengue_real$d7," (",round(lambda_td_com_chute$d7,0),")"),
  d8 = paste0(dados_dengue_real$d8," (",round(lambda_td_com_chute$d8,0),")"),
  d9 = paste0(dados_dengue_real$d9," (",round(lambda_td_com_chute$d9,0),")"),
  d10 = paste0(dados_dengue_real$d10," (",round(lambda_td_com_chute$d10,0),")"),
  N = paste0(dados_dengue_real$N," (",round(theta_t_com_chute,0),")")
)


tabela_estimativas_com_chute = tabela_com_chute %>% 
  kable(
    align = "c", 
    format = "html",
    escape = F
  ) %>% 
  kable_styling(
    bootstrap_options = c("hover", "condensed", "responsive"),
    font_size = 12
  ) %>% 
  row_spec(0, background = "#e07a5f", color = "#07080E") %>% 
  column_spec(1, background = "#FFFFFF", color = "#07080E", bold = TRUE) %>% 
  row_spec(1:(nrow(tabela_com_chute)), background = "rgb(235, 235, 245, 0.3)", color = "#07080E")  %>% 
  scroll_box(height = "280px")




formatar_plot_n_lambda_delayi = function(dados_plot, num_delay, estimativas_lambda){
  
  dados_plot %>% 
    hchart('line', hcaes(x = t, 
                         y = value, 
                         group = name,
                         delay = delay), marker = F,
           showInLegend = T,
           tooltip = list(pointFormat = paste0(
             'Total de casos no tempo {point.t}:</span> <b>{point.y:,.0f}</b><br>')
           )
           ) %>% 
    formatacao_grafico(titulo = paste0("Casos Reais e Estimados de Dengue no Tempo - ",num_delay, ' unidade(s) de Atraso'),
                       titulo_x = "Tempo t",
                       titulo_y = "Quantidade de Casos") %>% 
    hc_add_series(data = ic_lambda_delayi(
      estimativas_lambda = estimativas_lambda, 
      delayi = num_delay) %>% 
        as.data.frame() %>% 
        mutate(t = 1:T),
      type = "arearange",
      hcaes(x = t, low = lower, high = upper),
      marker = F,
      color = "#FA8072",
      name = "IC 99%",
      tooltip = list(pointFormat = paste0(
        "IC 99% no tempo {point.t}: <b> = [{point.lower:,.2f}; {point.upper:,.2f}]</b><br>"
      ))
    ) %>% 
    hc_subtitle(
      text = "Estimativa incorporando estrutura de Atraso na Notificação"
    )  %>% 
    hc_credits(enabled = TRUE,
               text = calcula_mae_e_rmse(
                 valores_reais = dados_plot %>% filter(name == "n_td", !is.na(value)) %>% pull(value),
                 valores_estimados = c(dados_plot %>% filter(name == "lambda_td") %>% pull(value))[1:(T-num_delay)]
               )) %>% 
    hc_colors(c("red", "black")) 
  
}


# comparacao modelos ===========================================================

plot_N_t_modelos = dados_dengue %>% 
  bind_cols(`Estimativa - Modelo 1` = theta_inicial,
            `Estimativa - Modelo 2` = theta_t_com_chute) %>% 
  mutate(t = 1:T) %>% 
  pivot_longer(cols = 12:14) %>%
  hchart('line', hcaes(x = t, 
                       y = value, 
                       group = name),marker = F,
         showInLegend = T,
         tooltip = list(pointFormat = paste0(
           'Total de casos no tempo {point.t}:</span> <b>{point.y:,.0f}</b><br>')
         )
  ) %>% 
  hc_xAxis(
    plotLines = list(
      list(
        value = 25,
        color = "gray",
        dashStyle = 'longdashdot',
        width = 2,
        label = list(
          text = "Hoje (T = 25)",
          style = list(color = "black", fontWeight = "bold")
        )
      )
    )
  ) %>% 
  hc_colors(c("#910000","#1aadce", "black")) %>% 
  hc_yAxis(
    title = list(text = "Total de Casos")
  ) %>% 
  hc_title(
    text = paste0(
      "<b style='display: block; font-size: 15px;'>",
      "Total de Casos de Dengue no Tempo por Modelo",
      "</b>"
    ),
    margin = 20,
    align = "center",
    style = list(useHTML = TRUE)
  ) %>% 
  hc_exporting(
    enabled = TRUE, 
    buttons = list(
      contextButton = list(
        menuItems = list('downloadCSV', 'downloadSVG')
      )
    )
  ) %>% 
  hc_subtitle(
    text = paste0(
      "Modelo 1: Estimativas SEM estrutura de Atraso na Notificação", "<br>",
      "Modelo 2: Estimativas COM estrutura de Atraso na Notificação"
    )
  ) 


dados_comparacao_modelos = lambda_td_com_chute_longo %>% 
  mutate(t = as.numeric(t)) %>% 
  left_join(dados_estimados, by = c("t", "delay", "d")) %>% 
  rename("Estimativa - Modelo 1" = "lambda_td",
         "Estimativa - Modelo 2" = "value") %>% 
  left_join(dados_dengue_longo %>% 
              mutate(d = as.character(d)), by = c("t", "delay", "d")) %>% 
  pivot_longer(cols = c(3,5,6))



formatacao_grafico_compara_modelos = function(dados_plot, num_delay){
  
  dados_plot %>% 
    hchart(
      'line',
      hcaes(x = t, y = value, group = name),marker = F,
      showInLegend = T
    ) %>% 
    hc_xAxis(
      plotLines = list(
        list(
          value = 25,
          color = "gray",
          dashStyle = 'longdashdot',
          width = 2,
          label = list(
            text = "Hoje (T = 25)",
            style = list(color = "black", fontWeight = "bold")
          )
        )
      )
    ) %>% 
    hc_colors(c("#910000","#1aadce", "black")) %>% 
    hc_yAxis(
      title = list(text = "Quantidade de Casos")
    ) %>% 
    hc_subtitle(
      text = paste0(
        "Modelo 1: Estimativas SEM estrutura de Atraso na Notificação", "<br>",
        "Modelo 2: Estimativas COM estrutura de Atraso na Notificação"
      )
    ) %>% 
    hc_title(
      text = paste0(
        "<b style='display: block; font-size: 15px;'>",
        "Casos de Dengue no Tempo por Modelo - ",num_delay,"unidade(s) de Atraso",
        "</b>"
      ),
      margin = 20,
      align = "center",
      style = list(useHTML = TRUE)
    ) %>% 
    hc_exporting(
      enabled = TRUE, 
      buttons = list(
        contextButton = list(
          menuItems = list('downloadCSV', 'downloadSVG')
        )
      )
    ) 
  
}

# convergencia parametros ======================================================

#estimativas_com_chute

#ts.plot(estimativas_com_chute$b_beta)
# ts.plot(estimativas_com_chute$b_beta[1:sample_size])
# plot(estimativas_com_chute$b_beta[1:sample_size], type = "l")
# 
# ts.plot(estimativas_com_chute$lambda[1:sample_size])
# plot(density(estimativas_com_chute$lambda))
# acf(estimativas_com_chute$lambda)
# 
# plot(density(estimativas_com_chute$alpha))
# acf(estimativas_com_chute$alpha)
# 
# plot(density(estimativas_com_chute$a_alpha))
# acf(estimativas_com_chute$a_alpha)
# 
# plot(density(estimativas_com_chute$b_alpha))
# acf(estimativas_com_chute$b_alpha)
# 
# plot(density(estimativas_com_chute$c_alpha))
# acf(estimativas_com_chute$c_alpha)
# 
# plot(density(estimativas_com_chute$f_alpha))
# acf(estimativas_com_chute$f_alpha)
# 
# plot(density(estimativas_com_chute$beta))
# acf(estimativas_com_chute$beta)
# 
# plot(density(estimativas_com_chute$b_beta))
# acf(estimativas_com_chute$b_beta)
# 
# plot(density(estimativas_com_chute$theta))
# acf(estimativas_com_chute$theta)
# 
# plot(density(estimativas_com_chute$a_theta))
# acf(estimativas_com_chute$a_theta)
# 
# plot(density(estimativas_com_chute$b_theta))
# acf(estimativas_com_chute$b_theta)
# 
# plot(density(estimativas_com_chute$c_theta))
# acf(estimativas_com_chute$c_theta)
# 
# plot(density(estimativas_com_chute$f_theta))
# acf(estimativas_com_chute$f_theta)
# 
# plot(density(estimativas_com_chute$psi))
# acf(estimativas_com_chute$psi)
# 
# mean(estimativas$lambda) 
# mean(estimativas$theta)

# Variacao em T (hoje) =========================================================

# estimativas_T15 = readRDS("estimativas/estimativas_n_td_por_delay_T15.rds")
# abcf_por_dalay_T15 = readRDS("estimativas/abcf_por_dalay_T15.rds")
# estimativas_theta_T15 = readRDS("estimativas/estimativas_theta_T15.rds")
# 
# ic_mu_theta_T15 = HPDinterval(mcmc(estimativas_theta_T15$mu), prob = 0.99) %>% 
#   as.data.frame() %>% 
#   mutate(t = 1:T)
# 
# 
# 
# genLog = function(t, a, b, c, f, logScale = TRUE){
#   logV = log(f)+log(a)+log(c)-(c*t)-(f+1)*log( b+exp(-c*t) )
#   if (logScale){
#     return(logV);
#   } else {
#     return(exp(logV));
#   }
# }
# 
# theta_inicial_T15 = genLog(t = 1:T,
#                        a = mean(estimativas_theta_T15$a),
#                        b = mean(estimativas_theta_T15$b),
#                        c = mean(estimativas_theta_T15$c),
#                        f = mean(estimativas_theta_T15$f),
#                        logScale = F)
# 
# dados_dengue$N_t = rowSums(dados_dengue, na.rm = T)
# 
# calcula_mae_e_rmse = function(valores_estimados, valores_reais){
#   
#   erros = valores_estimados - valores_reais
#   rmse = sqrt(mean(erros^2))
#   mae = mean(abs(erros))
#   
#   return(
#     paste0("MAE=", round(mae, 2), " RMSE=", round(rmse,2))
#   )
#   
# }
# 
# 
# plot_N_t_inical = dados_dengue_real %>% 
#   bind_cols(theta_t = theta_inicial_T15) %>% 
#   mutate(t = 1:T) %>% 
#   pivot_longer(cols = 12:13) %>% 
#   hchart('line', hcaes(x = t, 
#                        y = value, 
#                        group = name),marker = F,
#          showInLegend = T,
#          tooltip = list(pointFormat = paste0(
#            'Total de casos no tempo {point.t}:</span> <b>{point.y:,.0f}</b><br>')
#          )
#   ) %>% 
#   formatacao_grafico(titulo = "Total de Casos reais e estimados de Dengue no Tempo",
#                      titulo_x = "Tempo t",
#                      titulo_y = "Total de Casos") %>% 
#   hc_add_series(data = ic_mu_theta,
#                 type = "arearange",
#                 hcaes(x = t, low = lower, high = upper),
#                 marker = F,
#                 color = "#BEBEBE",
#                 name = "IC 99%",
#                 tooltip = list(pointFormat = paste0(
#                   "IC 99% no tempo {point.t}: <b> = [{point.lower:,.2f}; {point.upper:,.2f}]</b><br>"
#                 ))
#   ) %>% 
#   hc_subtitle(
#     text = "Estimativa sem incorporar estrutura de Atraso na Notificação"
#   ) %>% 
#   hc_credits(enabled = TRUE,
#              text = calcula_mae_e_rmse(
#                valores_reais = dados_dengue$N_t,
#                valores_estimados = theta_inicial
#              )) %>% 
#   hc_xAxis(
#     plotLines = list(
#       list(
#         value = 25,
#         color = "gray",
#         dashStyle = 'longdashdot',
#         width = 2,
#         label = list(
#           text = "Hoje (t = 25)",
#           style = list(color = "black", fontWeight = "bold")
#         )
#       )
#     )
#   )
# 



