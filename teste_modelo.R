# Apresentação TCC 2023/01
# Autora: Danielly Santos Sevrino

rm(list = ls())

# Pacotes ======================================================================

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
library(brms)

# Funcoes ======================================================================

formatacao_grafico <- function(plot, titulo, titulo_x, titulo_y, hoje){
  
  plot = plot %>% 
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
  
  if(is.na(hoje)==TRUE){
    plot = plot
  }else{
    plot = plot %>% 
      hc_xAxis(
        plotLines = list(
          list(
            value = hoje,
            color = "gray",
            dashStyle = 'longdashdot',
            width = 2,
            label = list(
              text = paste0("Hoje (T = ", hoje,")"),
              style = list(color = "black", fontWeight = "bold")
            )
          )
        )
      )
  }
  
  return(plot)
  
  
}

estruturar_dados_em_T <- function(dados_dengue, T){
  
  dados_dengue = dados_dengue[1:T,]
  dados_dengue[outer(1:T, 0:(D - 1), FUN = "+") > T] <- NA
  
  dados_dengue_longo = dados_dengue %>% 
    rownames_to_column(var = "t") %>% 
    mutate(t = 1:T) %>% 
    pivot_longer(
      cols = 2:12,
      names_to = "d",
      values_to = "n_td"
    ) %>% 
    mutate(
      atualizacao = ifelse(
        d == "d0", t, ifelse(
          d == "d1", t+1, ifelse(
            d == "d2", t+2, ifelse(
              d == "d3", t+3, ifelse(
                d == "d4",t+4, ifelse(
                  d == "d5",t+5, ifelse(
                    d == "d6",t+6, ifelse(
                      d == "d7", t+7, ifelse(
                        d == "d8", t+8, ifelse(
                          d == "d9",t+9, ifelse(
                            d == "d10",t+10, NA_real_
                          )
                        )
                      )
                    )
                  )
                )
              )
            )
          )
        )
      )
    )
  
  ultima_atualizacao = dados_dengue_longo %>% 
    filter(!is.na(n_td)) %>% 
    group_by(t) %>% 
    summarise(
      ultima_atualizacao = last(atualizacao)
    ) 
  
  dados_dengue_longo = dados_dengue_longo %>% 
    left_join(
      ultima_atualizacao, by = "t"
    )
  
  return(
    list(
      dados_dengue = dados_dengue,
      dados_dengue_longo = dados_dengue_longo
    )
  )
  
}

plot_bandas <- function(plot, inicio, fim){
  plot %>% 
    hc_xAxis(
      plotBands = list(
        color= '#FCFFC5',
        from = inicio,
        to = fim,
        label = list(
          text = paste0("Dados parciais"),
          style = list(color = "#696969", fontWeight = "bold")
        )
      )
    )
}

genLog = function(t, a, b, c, f, logScale = TRUE){
  logV = log(f)+log(a)+log(c)-(c*t)-(f+1)*log( b+exp(-c*t) )
  if (logScale){
    return(logV);
  } else {
    return(exp(logV));
  }
}

calcula_mae_e_rmse = function(valores_estimados, valores_reais){
  
  erros = valores_estimados - valores_reais
  rmse = sqrt(mean(erros^2))
  mae = mean(abs(erros))
  
  return(
    paste0("MAE=", round(mae, 2), " RMSE=", round(rmse,2))
  )
  
}

ic_lambda_delayi = function(estimativas_lambda, delayi){
  HPDinterval(mcmc(estimativas_lambda[,,delayi]), prob = 0.99)
}

formatar_plot_n_lambda_delayi = function(dados_plot, num_delay, estimativas_lambda, hoje){
  
  ic_theta = ic_lambda_delayi(estimativas_lambda = estimativas_lambda,
                              delayi = num_delay) %>% 
    as.data.frame() %>% 
    mutate(t = 1:T)
  
  ic_N_t = gerar_intervalo_N_t(
    theta = dados_plot %>% filter(name=="lambda_td") %>% arrange(t) %>%  pull(value),
    confianca = 0.99
  ) %>% as.data.frame()
  
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
    formatacao_grafico(titulo = paste0("Casos reais e estimados de dengue no tempo - ",num_delay, ' unidade(s) de atraso'),
                       titulo_x = "Tempo t",
                       titulo_y = "Quantidade de casos",
                       hoje = hoje) %>% 
    hc_add_series(data = ic_theta[1:(T-D+1),],
                  type = "arearange",
                  hcaes(x = t, low = lower, high = upper),
                  marker = F,
                  color = "#FA8072",
                  name = "IC 99% theta_t",
                  tooltip = list(pointFormat = paste0(
                    "IC 99% no tempo {point.t}: <b> = [{point.lower:,.2f}; {point.upper:,.2f}]</b><br>"
                  ))
    ) %>% 
    hc_add_series(data = ic_N_t[(T-D+1):T,],
                  type = "arearange",
                  hcaes(x = t, low = lower, high = upper),
                  marker = F,
                  color = "gray",
                  name = "IC 99% N_t",
                  tooltip = list(pointFormat = paste0(
                    "IC 99% no tempo {point.t}: <b> = [{point.lower:,.2f}; {point.upper:,.2f}]</b><br>"
                  ))
    ) %>% 
    hc_subtitle(
      text = "Estimativa com estrutura conjunta de atraso na notificação"
    )  %>% 
    hc_colors(c("red", "black")) 
  
}

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
      title = list(text = "Quantidade de casos")
    ) %>% 
    hc_subtitle(
      text = paste0(
        "Modelo 1: Estimativas com estrutura independente de atraso", "<br>",
        "Modelo 2: Estimativas com estrutura conjunta de atraso"
      )
    ) %>% 
    hc_title(
      text = paste0(
        "<b style='display: block; font-size: 15px;'>",
        "Casos de dengue no tempo por modelo - ",num_delay,"unidade(s) de atraso",
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

gerar_intervalo_N_t = function(theta_t, confianca){
  
  set.seed(5940516)
  N = list()
  lower = list()
  upper = list()
  for (t in 1:length(theta_t)) {
    N[[t]] = rpois(n = 10000, lambda = theta_t[t])
    
    lower[[t]] = HPDinterval(mcmc(N[[t]]), prob = confianca)[1]
    upper[[t]] = HPDinterval(mcmc(N[[t]]), prob = confianca)[2]
  }
  
  lower = unlist(lower)
  upper = unlist(upper)
  
  return(
    ic = cbind(
      lower = lower,
      upper = upper,
      t = 1:length(theta_t)
    )
  )
  
}

# Dados reais ==================================================================

dados_dengue = readr::read_rds("izabel\\dengueData.RDS")

# filtrando apenas as primeiras 35 semanas (primeira onda)
dados_dengue = dados_dengue[1:35,]
dados_dengue_real = dados_dengue
dados_dengue_real$N = rowSums(dados_dengue_real)

T = dim(dados_dengue)[1]
D = dim(dados_dengue)[2]

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

dados_dengue_longo_real <- dados_dengue_real %>% 
  rownames_to_column(var = "t") %>% 
  pivot_longer(cols = 2:(D+1), names_to = "d", values_to = "n_td") %>% 
  mutate(delay = d,
         delay = factor(delay, levels = c("d0","d1","d2","d3","d4","d5","d6","d7","d8","d9","d10")),
         d = str_remove_all(d,"d"),
         d = as.numeric(d)) %>% 
  group_by(d) %>% 
  mutate(t = rep(1:T)) %>% 
  ungroup()


# Estimando n_td por delay =====================================================

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
estimativas = readRDS("estimativas/estimativas_n_td_por_delay_T25.rds")

# abcf_por_dalay = lapply(estimativas, function(x) c(mean(x$a), mean(x$b), mean(x$c), mean(x$f)) )
# saveRDS(abcf_por_dalay, "estimativas/abcf_por_dalay.rds", version = 2)
abcf_por_dalay = readRDS("estimativas/abcf_por_dalay_em_T25.rds")

# estimativas_theta = stanLogistic(modelo_stan, y = rowSums(dados_dengue, na.rm = TRUE))
# saveRDS(estimativas_theta, "estimativas/estimativas_theta.rds",version = 2)
estimativas_theta = readRDS("estimativas/estimativas_theta_em_T25.rds")

ic_mu_theta = HPDinterval(mcmc(estimativas_theta$mu), prob = 0.99) %>% 
  as.data.frame() %>% 
  mutate(t = 1:T)

theta_inicial = genLog(t = 1:T,
                       a = mean(estimativas_theta$a),
                       b = mean(estimativas_theta$b),
                       c = mean(estimativas_theta$c),
                       f = mean(estimativas_theta$f),
                       logScale = F)

ic_inicial_N_t = gerar_intervalo_N_t(theta_t = theta_inicial, confianca = 0.99) %>% 
  as.data.frame()

dados_dengue$N_t = rowSums(dados_dengue, na.rm = T)

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
  formatacao_grafico(titulo = "Total de casos reais e estimados de dengue no tempo",
                     titulo_x = "Tempo t",
                     titulo_y = "Total de casos",
                     hoje = 25) %>% 
  hc_add_series(data = ic_mu_theta[1:(T-D+1),],
                type = "arearange",
                hcaes(x = t, low = lower, high = upper),
                marker = F,
                color = "#FA8072",
                name = "IC 99% theta_t",
                tooltip = list(pointFormat = paste0(
                  "IC 99% no tempo {point.t}: <b> = [{point.lower:,.2f}; {point.upper:,.2f}]</b><br>"
                ))
  ) %>% 
  hc_add_series(data = ic_inicial_N_t[(T-D+1):T,],
                type = "arearange",
                hcaes(x = t, low = lower, high = upper),
                marker = F,
                color = "gray",
                name = "IC 99% N_t",
                tooltip = list(pointFormat = paste0(
                  "IC 99% no tempo {point.t}: <b> = [{point.lower:,.2f}; {point.upper:,.2f}]</b><br>"
                ))
  ) %>% 
  hc_subtitle(
    text = "Estimativa com estrutura de atraso na notificação independente"
  ) %>% 
  hc_colors(c("black", "red"))



# Incorporando estrutura conjunta de delay =====================================

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

modelo_completo_stan =  stan_model("izabel\\modelLogistic2.stan")

warmup = 5000
chains = 1
thin = 1
sample_size = 10000
number_interations = warmup + thin*sample_size

params = c("lambda",
           "alpha", "a_alpha", "b_alpha", "c_alpha", "f_alpha",
           "theta", "a_theta", "b_theta", "c_theta", "f_theta",
           "psi")

a_theta_inicial = mean(estimativas_theta$a) #95.75487
b_theta_inicial = mean(estimativas_theta$b) #0.002144927
c_theta_inicial = mean(estimativas_theta$c) #0.390706
f_theta_inicial = mean(estimativas_theta$f) #1.000605

theta_t_inicial = genLog(t = 1:T, a = a_theta_inicial, 
                         b = b_theta_inicial, 
                         c = c_theta_inicial, 
                         f = f_theta_inicial, logScale = FALSE)

# abcf_por_dalay = list(
#   d0 = c(43.37062359,  0.04110974 , 0.25988601,  1.93579865),
#   d1 = c(17.579709914,  0.001539704,  0.375394225,  1.009165071),
#   d2 = c(15.543233917,  0.002681017,  0.349974288,  1.007444746),
#   d3 = c(16.665236027,  0.005709287,  0.314062615,  1.023590627),
#   d4 = c(13.308564544,  0.009719619,  0.289207972,  1.050499141),
#   d5 = c(8.090284394, 0.005526861, 0.291367178, 1.042248264),
#   d6 = c(0.5001923573, 0.0004408973, 0.4805044915, 1.0134160103),
#   d7 = c(0.1865507972, 0.0001903261, 0.5346711332, 1.0141887227),
#   d8 = c(4.465459e-02, 8.932224e-05, 6.187396e-01, 1.026824e+00),
#   d9 = c(0.107585673, 0.000310951, 0.495876242, 1.138525422),
#   d10 = c(4.9711283, 0.3033697, 0.2389501, 2.9223285)
# )

a_alpha_inicial = list()
b_alpha_inicial = list()
c_alpha_inicial = list()
f_alpha_inicial =list()
alfha_t_d_inicial = list()
for (d in 1:(D-1)) {
  
  a_alpha_inicial[[d]] = abcf_por_dalay[[d]][1]
  b_alpha_inicial[[d]] = abcf_por_dalay[[d]][2]
  c_alpha_inicial[[d]] = abcf_por_dalay[[d]][3]
  f_alpha_inicial[[d]] = abcf_por_dalay[[d]][4]
  
  
  alfha_t_d_inicial[[d]] = genLog(t = 1:T, a = abcf_por_dalay[[d]][1],
                                  b = abcf_por_dalay[[d]][2], 
                                  c = abcf_por_dalay[[d]][3], 
                                  f = abcf_por_dalay[[d]][4], logScale = TRUE)
}

alfha_t_d_inicial = matrix(unlist(alfha_t_d_inicial), ncol = 10)
a_alpha_inicial = unlist(a_alpha_inicial)
b_alpha_inicial = unlist(b_alpha_inicial)
c_alpha_inicial = unlist(c_alpha_inicial)
f_alpha_inicial = unlist(f_alpha_inicial)



chute_inicial = list(
  list(
    
    # alpha = alfha_t_d_inicial,
    # 
    # a_alpha = a_alpha_inicial,
    # b_alpha = b_alpha_inicial,
    # c_alpha = c_alpha_inicial,
    # f_alpha = f_alpha_inicial,
    
    theta = theta_t_inicial,

    a_theta = a_theta_inicial,
    b_theta = b_theta_inicial,
    c_theta = c_theta_inicial,
    f_theta = f_theta_inicial#,
    
    #psi = n_dalay_1$n_td
  )
)



output_modelo_com_chute = rstan::sampling(modelo_completo_stan,
                                          data = dados_stan,
                                          iter = number_interations,
                                          warmup = warmup,
                                          chains = chains,
                                          pars = params,
                                          init = chute_inicial,
                                          verbose = FALSE)



estimativas_com_chute = rstan::extract(output_modelo_com_chute)
saveRDS(estimativas_com_chute, "estimativas\\estimativas_estrutura_independente.rds", version = 2)
#estimativas_com_chute = readRDS("estimativas\\estimativas_com_chute_em_T25.rds")


ic_theta_com_chute = HPDinterval(mcmc(estimativas_com_chute$theta), prob = 0.99) %>% 
  as.data.frame() %>% 
  mutate(t = 1:T)

theta_t_com_chute = apply(estimativas_com_chute$theta, 2, mean)

ic_N_t_com_chute = gerar_intervalo_N_t(theta_t = theta_t_com_chute, confianca = 0.99) %>%
  as.data.frame()

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
  formatacao_grafico(titulo = "Total de casos reais e estimados de dengue no tempo",
                     titulo_x = "Tempo t",
                     titulo_y = "Total de casos",
                     hoje = 25) %>% 
  hc_add_series(data = ic_theta_com_chute[1:(T-D+1), ],
                type = "arearange",
                hcaes(x = t, low = lower, high = upper),
                marker = F,
                color = "#FA8072",
                name = "IC 99% theta_t",
                tooltip = list(pointFormat = paste0(
                  "IC 99% no tempo {point.t}: <b> = [{point.lower:,.2f}; {point.upper:,.2f}]</b><br>"
                ))
  ) %>% 
  hc_add_series(data = ic_N_t_com_chute[(T-D+1):T, ],
                type = "arearange",
                hcaes(x = t, low = lower, high = upper),
                marker = F,
                color = "gray",
                name = "IC 99% N_t",
                tooltip = list(pointFormat = paste0(
                  "IC 99% no tempo {point.t}: <b> = [{point.lower:,.2f}; {point.upper:,.2f}]</b><br>"
                ))
  ) %>% 
  hc_subtitle(
    text = "Estimativa com estrutura conjunta de atraso na notificação"
  ) %>% 
  hc_colors(c("black", "red"))


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
  formatacao_grafico(titulo = "Casos estimados de dengue no tempo por tempo de atraso na notificação",
                     titulo_x = "Tempo t",
                     titulo_y = "Casos estimados",
                     hoje = 25) %>% 
  hc_tooltip(
    pointFormat = paste0(
      '<span style="color:{series.color}; font-weight: bold;">Casos estimados no tempo {point.t} com {point.d} unidade(s) de atraso:</span> <b>{point.y:.2f}</b>'
    )
  ) %>% 
  hc_subtitle(
    text = "Estimativa com estrutura conjunta de atraso na notificação"
  )

dados_comparacao_lambda = lambda_td_com_chute_longo %>% 
  mutate(t = as.numeric(t)) %>% 
  left_join(dados_dengue_longo %>% select(-d), by = c("t", "delay"))

dados_comparacao_lambda_longo = dados_comparacao_lambda %>% 
  pivot_longer(cols = c("lambda_td", "n_td"))
