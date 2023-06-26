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
library(tinytex)

# Distribuições a priori =======================================================

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
  ) %>% 
  hc_colors("black")

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
  ) %>% 
  hc_colors("black")

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
  ) %>% 
  hc_colors("black")

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
  ) %>% 
  hc_colors("black")

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
              text = paste0("T = ", hoje),
              style = list(color = "black", fontWeight = "bold")
            )
          )
        )
      )
  }
  
  return(plot)
  
  
}

formatacao_grafico_por_delay <- function(plot, titulo, titulo_x, titulo_y, hoje, num_delay){
  
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
              text = paste0("t=25-", num_delay),
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
        color= '#BFBFBF',
        from = inicio,
        to = fim#,
        # label = list(
        #   text = paste0("Dados parciais"),
        #   style = list(color = "#696969", fontWeight = "bold")
        # )
      )
    )
}

plot_bandas_futuro <- function(plot, inicio, fim){
  plot %>% 
    hc_xAxis(
      plotBands = list(
        color= '#7F7F7F',
        from = inicio,
        to = fim#,
        # label = list(
        #   text = paste0("Dados parciais"),
        #   style = list(color = "#696969", fontWeight = "bold")
        # )
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
  HPDinterval(mcmc(estimativas_lambda[,,delayi]), prob = 0.95)
}

formatar_plot_n_lambda_delayi = function(dados_plot, num_delay, estimativas_lambda, hoje){
  
  ic_theta = ic_lambda_delayi(estimativas_lambda = estimativas_lambda,
                              delayi = num_delay) %>% 
    as.data.frame() %>% 
    mutate(t = 1:35)
  
  ic_N_t = gerar_intervalo_N_t(
    theta = dados_plot %>% filter(name == 'lambda_td') %>%  arrange(t) %>% pull(value),
    confianca = 0.95
  ) %>% as.data.frame()
  
  ponto_de_corte = 25 - num_delay
  
  dados_plot %>% 
    mutate(
      name = case_when(name == 'lambda_td' ~ 'lambda',
                       name == 'n_td' ~ 'n')) %>% 
    hchart('line', hcaes(x = t, 
                         y = value, 
                         group = name,
                         delay = delay), marker = F,
           showInLegend = T,
           tooltip = list(pointFormat = paste0(
             'Total de casos no tempo {point.t}:</span> <b>{point.y:,.0f}</b><br>')
           )
    ) %>% 
    formatacao_grafico_por_delay(titulo = paste0("Casos de dengue no tempo com ",num_delay, ' semana(s) de atraso'),
                       titulo_x = "t",
                       titulo_y = "n",
                       hoje = hoje-num_delay,
                       num_delay = num_delay) %>% 
    hc_add_series(data = ic_theta[1:ponto_de_corte,],
      type = "arearange",
      hcaes(x = t, low = lower, high = upper),
      marker = F,
      color = "#FA8072",
      name = "IC 95% lambda | n",
      tooltip = list(pointFormat = paste0(
        "IC 95% no tempo {point.t}: <b> = [{point.lower:,.2f}; {point.upper:,.2f}]</b><br>"
      ))
    ) %>% 
    hc_add_series(data = ic_N_t[ponto_de_corte:35,],
                  type = "arearange",
                  hcaes(x = t, low = lower, high = upper),
                  marker = F,
                  color = "gray",
                  name = "IC 95% n | lambda",
                  tooltip = list(pointFormat = paste0(
                    "IC 95% no tempo {point.t}: <b> = [{point.lower:,.2f}; {point.upper:,.2f}]</b><br>"
                  ))
    ) %>% 
    # hc_subtitle(
    #   text = "Estimativa com estrutura conjunta de atraso na notificação"
    # )  %>% 
    hc_colors(c("red", "black")) 
  
}

formatacao_grafico_compara_modelos = function(dados_plot, num_delay){
  
  ponto_de_corte = 25 - num_delay
  
  dados_plot %>% 
    filter(name != "N") %>% 
    mutate(
      name = case_when(name == 'MI' ~ 'lambda | n - MI',
                       name == 'MC' ~ 'lambda | n - MC',
                       name == 'n_td' ~ 'n')
    ) %>% 
    hchart(
      'line',
      hcaes(x = t, y = value, group = name),marker = F,
      showInLegend = T
    ) %>% 
    hc_add_series(data = dados_plot,
                  hcaes(x = t, y = n_td),
                  type = 'line',
                  name = 'n',
                  marker = F) %>% 
    hc_xAxis(
      plotLines = list(
        list(
          value = ponto_de_corte,
          color = "gray",
          dashStyle = 'longdashdot',
          width = 2,
          label = list(
            text = paste0("t=25-", num_delay),
            style = list(color = "black", fontWeight = "bold")
          )
        )
      )
    ) %>% 
    hc_colors(c("#910000","#1aadce", "black")) %>% 
    hc_yAxis(
      title = list(text = "n")
    ) %>% 
    # hc_subtitle(
    #   text = paste0(
    #     "MI: Estimativas com estrutura independente de atraso", "<br>",
    #     "MC: Estimativas com estrutura conjunta de atraso"
    #   )
    # ) %>% 
    hc_title(
      text = paste0(
        "<b style='display: block; font-size: 15px;'>",
        "Casos de dengue no tempo com ",num_delay," semana(s) de atraso por modelo",
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
    hc_tooltip(
      pointFormat = paste0(
        '<span style="color:{series.color}; font-weight: bold;">{series.name} no tempo {point.t}:</span> <b>{point.y:,.0f}</b>'
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

dados_dengue = readr::read_rds("dados/dengueData.RDS")

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

# Graficos Motivacao ===========================================================

dados_plot = dados_dengue

for(i in 1:11){
  if(i == 1){
    dados_plot[,i] = dados_plot[,i]
  }else{
    dados_plot[,i] = ifelse(is.na(dados_plot[,i]), 0 , dados_plot[,i]) + 
      ifelse(is.na(dados_plot[,i-1]), 0 , dados_plot[,i-1])
  }
}
dados_plot[outer(1:nrow(dados_plot), 0:(D - 1), FUN = "+") > nrow(dados_plot)] <- NA
colnames(dados_plot) = c("Atualização em T", paste0("Atualização em T + ", 1:10))
dados_plot = dados_plot %>% 
  mutate(t = 1:nrow(dados_plot))

N_t_atualizacao = dados_plot %>% 
  pivot_longer(
    cols = 1:D,
    names_to = "Última Atualização",
    values_to = "Total de Casos de Dengue"
  ) %>% 
  mutate(
    `Última Atualização` = factor(
      `Última Atualização`,
      levels = c("Atualização em T", "Atualização em T + 1", "Atualização em T + 2",
                 "Atualização em T + 3", "Atualização em T + 4", "Atualização em T + 5",
                 "Atualização em T + 6", "Atualização em T + 7", "Atualização em T + 8",
                 "Atualização em T + 9", "Atualização em T + 10")
    )
  ) %>% 
  hchart('line', hcaes(x = t, 
                       y = `Total de Casos de Dengue`, 
                       group = `Última Atualização`),
         marker = F) %>% 
  formatacao_grafico(titulo = "Total de casos de dengue ao longo do tempo por período de atualização",
                     titulo_x = "t",
                     titulo_y = "N",
                     hoje = NA) %>% 
  hc_tooltip(
    pointFormat = paste0(
      '<span style="color:{series.color}; font-weight: bold;">Numéro total de casos no tempo {point.t} com {series.name} :</span> <b>{point.y:,.0f}</b>'
    )
  ) #%>% 
  # plot_bandas(inicio = 16, fim = 25) %>% 
  # plot_bandas_futuro(inicio = 26, fim = 35)

# Graficos dados reais =========================================================


dados_dengue_parcial = readr::read_rds("dados/dengueData.RDS")
dados_dengue_parcial = dados_dengue_parcial[1:25,]
T = dim(dados_dengue_parcial)[1]
D = dim(dados_dengue_parcial)[2]
dados_dengue_parcial[outer(1:T, 0:(D - 1), FUN = "+") > T] <- NA
dados_dengue_parcial$N = rowSums(dados_dengue_parcial, na.rm = T)


plot_N_t_completo = dados_dengue_real %>% 
  mutate(t = 1:35) %>% 
  hchart('line', hcaes(x = t, y = N),
         marker = F,
         name = 'N real',
         showInLegend = T) %>% 
  hc_add_series(
    type = 'line',
    data = dados_dengue_parcial %>%
      mutate(t = 1:25) %>% 
      filter(t > 15 & t < 26),
    hcaes(x = t, y = N),
    marker = F,
    showInLegend = T,
    name = 'N parcial'
  ) %>% 
  hc_add_series(
    type = 'line',
    data = dados_dengue_real %>%
      mutate(t = 1:35) %>% 
      filter(t > 25),
    hcaes(x = t, y = N),
    marker = F,
    showInLegend = T,
    name = 'N futuro'
  ) %>% 
  hc_colors(c('black','#BFBFBF', '#7F7F7F')) %>% 
  hc_tooltip(
    pointFormat = paste0(
      '<span style="color:{series.color}; font-weight: bold;">{series.name} no tempo {point.t}:</span> <b>{point.y:,.0f}</b>'
    )
  ) %>% 
  hc_yAxis(lineColor = "#f7f7f7", gridLineColor = "#f4f4f4",
           labels = list(format = "{value:,.0f}")) %>% 
  hc_yAxis(lineColor = "#f7f7f7", gridLineColor = "#f4f4f4") %>% 
  hc_title(
    text = paste0(
      "<b style='display: block; font-size: 15px;'>",
      'Número total de casos de dengue ao longo do tempo', "</b>"
    ),
    margin = 20,
    align = "center",
    style = list(useHTML = TRUE)
  )

# plot_N_t_completo = dados_dengue_longo_completo %>% 
#   group_by(t) %>% 
#   summarise(N = mean(N)) %>% 
#   hchart("line", hcaes(x= t, y = N),
#          marker = F) %>% 
#   formatacao_grafico(titulo = "Total de casos relatados de dengue ao longo do tempo",
#                      titulo_x = "Tempo t",
#                      titulo_y = "Total de casos de dengue (N_t)",
#                      hoje = NA) %>% 
#   hc_tooltip(
#     pointFormat = paste0(
#       '<span style="color:{series.color}; font-weight: bold;">Numéro total de casos no tempo {point.t}:</span> <b>{point.y:,.0f}</b>'
#     )
#   ) %>% 
#   hc_colors(c('black')) %>% 
#   plot_bandas(inicio = 25, fim = 35)


plot_n_td_completo = dados_dengue_longo_completo %>%
  hchart('line', hcaes(x = t, 
                       y = n_td, 
                       group = delay,
                       d = d), marker = F,
         showInLegend = T) %>% 
  formatacao_grafico(titulo = "Casos relatados de dengue ao longo do tempo por atraso",
                     titulo_x = "Tempo t",
                     titulo_y = "Casos de dengue (n_td)",
                     hoje = NA) %>% 
  hc_tooltip(
    pointFormat = paste0(
      '<span style="color:{series.color}; font-weight: bold;">Numéro de casos no tempo {point.t} com {point.d} unidade(s) de atraso:</span> <b>{point.y:,.0f}</b>'
    )
  ) %>% 
  plot_bandas(inicio = 25, fim = 35)

# Influencia do atraso =========================================================

plot_estrutura_delay_tempo = dados_dengue_longo %>% 
  hchart('line', hcaes(x = t, 
                       y = log(n_td + 0.05), 
                       group = d,
                       d = d),marker = F,
         showInLegend = T) %>% 
  formatacao_grafico(titulo = "Influência do atraso nas notificações ao longo do tempo",
                     titulo_x = "t",
                     titulo_y = "Log (n + 0,05)",
                     hoje = NA) %>% 
  hc_tooltip(
    pointFormat = paste0(
      '<span style="color:{series.color}; font-weight: bold;">Logaritmo no tempo {point.t} com {point.d} unidade(s) de atraso:</span> <b>{point.y:.2f}</b>'
    )
  )  %>% 
  #plot_bandas(inicio = 16, fim = 25) %>% 
  hc_legend(title = list(text = 'd')) #%>% 
  #plot_bandas_futuro(inicio = 26, fim = 35)


# Dados para modelagem em T = 25 ===============================================

dados_dengue = readr::read_rds("dados/dengueData.RDS")
# filtrando apenas as primeiras 35 semanas (primeira onda)
dados_dengue = dados_dengue[1:25,]

T = 25
D = dim(dados_dengue)[2]

# criando os NAs
dados_dengue[outer(1:T, 0:(D - 1), FUN = "+") > T] <- NA
dados_NA = matrix(nrow = 35-T, ncol = D) %>% as.data.frame()
colnames(dados_NA) = paste0("d",0:(D-1))
dados_dengue = rbind(dados_dengue, dados_NA)


dados_dengue_longo <- dados_dengue %>% 
  rownames_to_column(var = "t") %>% 
  pivot_longer(cols = 2:(D+1), names_to = "d", values_to = "n_td") %>% 
  mutate(delay = d,
         delay = factor(delay, levels = c("d0","d1","d2","d3","d4","d5","d6","d7","d8","d9","d10")),
         d = str_remove_all(d,"d"),
         d = as.numeric(d)) %>% 
  group_by(d) %>% 
  mutate(t = rep(1:(T + (35-T)))) %>% 
  ungroup()

dados_dengue$N_t = rowSums(dados_dengue, na.rm = T)

# Estimativas independentes por delay ==========================================
# Estimativas por coluna para d = 0, ..., D

#modelo_stan =  stan_model("stan/modelLogisticbyDelay.stan")

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
# temos 10 mil estimativas de a,b,c,f

# estimativas_por_delay = apply(dados_dengue, 2, function(y) stanLogistic(modelo_stan, y = y))
# saveRDS(estimativas_por_delay, "estimativas/poisson/estimativas_por_delay.rds",version = 2)
estimativas_por_delay = readRDS("estimativas/poisson/estimativas_por_delay.rds")

# abcf_por_dalay = lapply(estimativas_por_delay, function(x) c(mean(x$a), mean(x$b), mean(x$c), mean(x$f)) )
# saveRDS(abcf_por_dalay, "estimativas/poisson/abcf_por_dalay.rds", version = 2)
abcf_por_dalay = readRDS("estimativas/poisson/abcf_por_dalay.rds")

# estimativas_theta_isolado = stanLogistic(modelo_stan, y = rowSums(dados_dengue, na.rm = TRUE))
# saveRDS(estimativas_theta_isolado, "estimativas/poisson/estimativas_theta_isolado.rds",version = 2)
estimativas_theta_isolado = readRDS("estimativas/poisson/estimativas_theta_isolado.rds")

# ts.plot(estimativas_theta_isolado$a)
# ts.plot(estimativas_theta_isolado$b)
# ts.plot(estimativas_theta_isolado$c)
# ts.plot(estimativas_theta_isolado$f)

# Estimativas com estrutura independente =======================================

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
  T = 35, D = D,
  Tk = td_dalay_1$t, Dk = td_dalay_1$d,
  T_k =  td_dalay_k$t,  D_k =  td_dalay_k$d,
  qk = nrow(n_dalay_1), q_k = nrow(n_dalay_k)
)

#modelo_stan_indep =  stan_model("stan/modelLogistic.stan")

warmup = 5000
chains = 1
thin = 1
sample_size = 10000
number_interations = warmup + thin*sample_size

params = c("lambda",#"n_k"
           "alpha", "a_alpha", "b_alpha", "c_alpha", "f_alpha",
           "theta", "a_theta", "b_theta", "c_theta", "f_theta",
           "psi")

a_theta_inicial = mean(estimativas_theta_isolado$a) #95.78993
b_theta_inicial = mean(estimativas_theta_isolado$b) #0.002145523
c_theta_inicial = mean(estimativas_theta_isolado$c) #0.3906966
f_theta_inicial = mean(estimativas_theta_isolado$f) #1.0006

theta_t_inicial = genLog(t = 1:35, a = a_theta_inicial, 
                         b = b_theta_inicial, 
                         c = c_theta_inicial, 
                         f = f_theta_inicial, logScale = FALSE)

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
  
  
  alfha_t_d_inicial[[d]] = genLog(t = 1:35, a = abcf_por_dalay[[d]][1],
                                  b = abcf_por_dalay[[d]][2], 
                                  c = abcf_por_dalay[[d]][3], 
                                  f = abcf_por_dalay[[d]][4], logScale = TRUE)
}

alfha_t_d_inicial = matrix(unlist(alfha_t_d_inicial), ncol = 10)
a_alpha_inicial = unlist(a_alpha_inicial)
b_alpha_inicial = unlist(b_alpha_inicial)
c_alpha_inicial = unlist(c_alpha_inicial)
f_alpha_inicial = unlist(f_alpha_inicial)


parametros_iniciais = list(
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
    f_theta = f_theta_inicial,
    
    psi = n_dalay_1$n_td
  )
)

# output_modelo_indep = rstan::sampling(modelo_stan_indep,
#                                       data = dados_stan,
#                                       iter = number_interations,
#                                       warmup = warmup,
#                                       chains = chains,
#                                       pars = params,
#                                       init = parametros_iniciais,
#                                       verbose = FALSE)
# 
# estimativas_indep = rstan::extract(output_modelo_indep)
# saveRDS(estimativas_indep, "estimativas/poisson/estimativas_estrutura_independente.rds", version = 2)
estimativas_indep = readRDS("estimativas/poisson/estimativas_estrutura_independente.rds")

# ts.plot(estimativas_indep$a_theta)
# ts.plot(estimativas_indep$b_theta)
# ts.plot(estimativas_indep$c_theta)
# ts.plot(estimativas_indep$f_theta)
# acf(estimativas_indep$a_theta)
# acf(estimativas_indep$b_theta)
# acf(estimativas_indep$c_theta)
# acf(estimativas_indep$f_theta)

# Graficos estrutura Independente ==============================================

ic_theta_indep = HPDinterval(mcmc(estimativas_indep$theta), prob = 0.95) %>% 
  as.data.frame() %>% 
  mutate(t = 1:35)

theta_inicial_indep = apply(estimativas_indep$theta, 2, mean)

ic_N_t_indep = gerar_intervalo_N_t(theta_t = theta_inicial_indep, confianca = 0.95) %>% 
  as.data.frame()

plot_N_t_indep = dados_dengue_real %>% 
  bind_cols(`theta | N` = theta_inicial_indep) %>% 
  mutate(t = 1:35) %>% 
  pivot_longer(cols = 12:13) %>% 
  hchart('line', hcaes(x = t, 
                       y = value, 
                       group = name),marker = F,
         showInLegend = T,
         tooltip = list(pointFormat = paste0(
           'Total de casos no tempo {point.t}:</span> <b>{point.y:,.2f}</b><br>')
           )
         ) %>% 
  formatacao_grafico(titulo = "Total de casos de dengue ao longo do tempo",
                     titulo_x = "t",
                     titulo_y = "N",
                     hoje = 25) %>% 
  hc_add_series(data = ic_theta_indep[1:15,],
                type = "arearange",
                hcaes(x = t, low = lower, high = upper),
                marker = F,
                color = "#FA8072",
                name = "IC 95% theta | N",
                tooltip = list(pointFormat = paste0(
                  "IC 95% no tempo {point.t}: <b> = [{point.lower:,.2f}; {point.upper:,.2f}]</b><br>"
                ))
  ) %>% 
  hc_add_series(data = ic_N_t_indep[15:35,],
                type = "arearange",
                hcaes(x = t, low = lower, high = upper),
                marker = F,
                color = "gray",
                name = "IC 95% N | theta",
                tooltip = list(pointFormat = paste0(
                  "IC 95% no tempo {point.t}: <b> = [{point.lower:,.2f}; {point.upper:,.2f}]</b><br>"
                ))
                ) %>% 
  hc_subtitle(
    text = "Estimativas com estrutura de atraso na notificação independente"
  ) %>% 
  hc_colors(c("black", "red"))


lambda.mean = matrix(NA, 35, D - 1)
for(t in 1:35){
  for(d in 1:(D - 1)){
    lambda.mean[t, d] = mean(estimativas_indep$lambda[ , t, d])
  }
}

lambda_td_indep = as.data.frame(lambda.mean)
colnames(lambda_td_indep) = paste0("d",1:10)
lambda_td_indep = lambda_td_indep %>% 
rownames_to_column(var = "t")

lambda_td_indep_longo = lambda_td_indep %>% 
  pivot_longer(cols = (0+2):D,
               names_to = "delay",
               values_to = "lambda_td") %>% 
  mutate(delay = factor(delay, levels = c("d1", "d2", "d3", "d4", "d5", "d6", "d7", "d8", "d9", "d10" )),
         d = str_remove(delay, "d") %>% as.numeric())

plot_lamda_indep = lambda_td_indep_longo %>% 
  hchart('line', hcaes(x = t, 
                       y = lambda_td, 
                       group = d,
                       d = d), marker = F,
         showInLegend = T) %>% 
  formatacao_grafico(titulo = "Casos estimados de dengue no tempo por atraso na notificação",
                     titulo_x = "t",
                     titulo_y = 'lambda | n',
                     hoje = 25) %>%
  hc_tooltip(
    pointFormat = paste0(
      '<span style="color:{series.color}; font-weight: bold;">Casos estimados no tempo {point.t} com {point.d} unidade(s) de atraso:</span> <b>{point.y:.2f}</b>'
    )
  ) %>% 
  hc_subtitle(
    text = "Estimativas com estrutura de atraso na notificação independente"
  ) %>% 
  hc_legend(
    title = list(text = 'd')
  )


plot_estrutura_delay_estimativas = lambda_td_indep_longo %>% 
  hchart('line', hcaes(x = t,
                       y = log(lambda_td+0.05), 
                       group = d,
                       d = d), marker = F,
         showInLegend = T) %>% 
  formatacao_grafico(titulo = "Influência do atraso nas notificações no tempo para os casos estimados",
                     titulo_x = "t",
                     titulo_y = "Log( lambda | n + 0.05 )",
                     hoje = 25) %>% 
  hc_tooltip(
    pointFormat = paste0(
      '<span style="color:{series.color}; font-weight: bold;">Logaritmo no tempo {point.t} com {point.d} unidade(s) de atraso:</span> <b>{point.y:.2f}</b>'
    )
  ) %>% 
  hc_subtitle(
    text = "Estimativas com estrutura de atraso na notificação independente"
  ) %>% 
  hc_legend(
    title = list(text = 'd')
  )

 
# Tabela com estimativas independentes =========================================

dados_tabela = lambda_td_indep

tabela = data.frame(
  t = 1:35,
  #d0 = paste0(dados_dengue_real$d0," (",round(dados_tabela$d0,0),")"),
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
  N = paste0(dados_dengue_real$N," (",round(theta_inicial_indep,0),")")
)


tabela_estimativas_indep = tabela[25:35,] %>% 
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
  row_spec(1:(nrow(tabela[25:35,])), background = "rgb(235, 235, 245, 0.3)", color = "#07080E")  %>% 
  scroll_box(height = "280px")

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
  T = 35, D = D,
  Tk = td_dalay_1$t, Dk = td_dalay_1$d,
  T_k =  td_dalay_k$t,  D_k =  td_dalay_k$d,
  qk = nrow(n_dalay_1), q_k = nrow(n_dalay_k)
)

#modelo_completo_stan =  stan_model("stan/modelCompleteLogistic.stan")

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


# Estimativas iniciais para os parametros (d = 0)
a_alpha_inicial = abcf_por_dalay$d1[1] #17.59739
b_alpha_inicial = abcf_por_dalay$d1[2] #0.001542128
c_alpha_inicial = abcf_por_dalay$d1[3] #0.3753187
f_alpha_inicial = abcf_por_dalay$d1[4] #1.009277

a_theta_inicial = mean(estimativas_theta_isolado$a) #95.78993
b_theta_inicial = mean(estimativas_theta_isolado$b) #0.002145523
c_theta_inicial = mean(estimativas_theta_isolado$c) #0.3906966
f_theta_inicial = mean(estimativas_theta_isolado$f) #1.0006

theta_t_inicial = genLog(t = 1:35, a = a_theta_inicial, 
                         b = b_theta_inicial, 
                         c = c_theta_inicial, 
                         f = f_theta_inicial, logScale = FALSE)

alfha_t_inical = genLog(t = 1:35, a = a_alpha_inicial,
                        b = b_alpha_inicial, 
                        c = c_alpha_inicial, 
                        f = f_theta_inicial, logScale = FALSE)

valores_iniciais = list(
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

# output_modelo_conj = rstan::sampling(modelo_completo_stan,
#                                      data = dados_stan,
#                                      iter = number_interations,
#                                      warmup = warmup,
#                                      chains = chains,
#                                      pars = params,
#                                      init = valores_iniciais,
#                                      verbose = FALSE)
# 
# estimativas_conjuntas = rstan::extract(output_modelo_conj)
# saveRDS(estimativas_conjuntas, "estimativas/poisson/estimativas_estrutura_conjunta.rds", version = 2)
estimativas_conjuntas = readRDS("estimativas/poisson/estimativas_estrutura_conjunta.rds")


# Graficos estimativas estrutura conjunta ======================================

ic_theta_conj = HPDinterval(mcmc(estimativas_conjuntas$theta), prob = 0.95) %>% 
  as.data.frame() %>% 
  mutate(t = 1:35)

theta_t_conj = apply(estimativas_conjuntas$theta, 2, mean)

ic_N_t_conj = gerar_intervalo_N_t(theta_t = theta_t_conj, confianca = 0.95) %>%
  as.data.frame()

plot_N_t_conj = dados_dengue_real %>% 
  bind_cols(`theta | N` = theta_t_conj) %>% 
  mutate(t = 1:35) %>% 
  pivot_longer(cols = 12:13) %>% 
  hchart('line', hcaes(x = t, 
                       y = value, 
                       group = name), marker = F,
         showInLegend = T,
         tooltip = list(pointFormat = paste0(
           'Total de casos no tempo {point.t}:</span> <b>{point.y:,.0f}</b><br>')
         )
         ) %>% 
  formatacao_grafico(titulo = "Total de casos de dengue ao longo do tempo",
                     titulo_x = "t",
                     titulo_y = "N",
                     hoje = 25) %>% 
  hc_add_series(data = ic_theta_conj[1:15, ],
                type = "arearange",
                hcaes(x = t, low = lower, high = upper),
                marker = F,
                color = "#FA8072",
                name = "IC 95% theta | N",
                tooltip = list(pointFormat = paste0(
                  "IC 95% no tempo {point.t}: <b> = [{point.lower:,.2f}; {point.upper:,.2f}]</b><br>"
                ))
  ) %>% 
  hc_add_series(data = ic_N_t_conj[15:35, ],
                type = "arearange",
                hcaes(x = t, low = lower, high = upper),
                marker = F,
                color = "gray",
                name = "IC 95% N | theta",
                tooltip = list(pointFormat = paste0(
                  "IC 95% no tempo {point.t}: <b> = [{point.lower:,.2f}; {point.upper:,.2f}]</b><br>"
                ))
  ) %>% 
  hc_subtitle(
    text = "Estimativas com estrutura conjunta de atraso na notificação"
  ) %>% 
  hc_colors(c("black", "red"))


lambda.mean = matrix(NA, 35, D - 1)
for(t in 1:35){
  for(d in 1:(D - 1)){
    lambda.mean[t, d] = mean(estimativas_conjuntas$lambda[ , t, d])
  }
}

lambda_td_conj = as.data.frame(lambda.mean)
colnames(lambda_td_conj) = paste0("d",1:10)
lambda_td_conj = lambda_td_conj %>% 
  rownames_to_column(var = "t")

lambda_td_conj_longo = lambda_td_conj %>% 
  pivot_longer(cols = (0+2):D,
               names_to = "delay",
               values_to = "lambda_td") %>% 
  mutate(delay = factor(delay, levels = c("d1", "d2", "d3", "d4", "d5", "d6", "d7", "d8", "d9", "d10" )),
         d = str_remove(delay, "d"))

plot_lamda_conj = lambda_td_conj_longo %>% 
  hchart('line', hcaes(x = t, 
                       y = lambda_td, 
                       group = d,
                       d = d), marker = F,
         showInLegend = T) %>% 
  formatacao_grafico(titulo = "Casos estimados de dengue no tempo por atraso na notificação",
                     titulo_x = "t",
                     titulo_y = "n",
                     hoje = 25) %>% 
  hc_tooltip(
    pointFormat = paste0(
      '<span style="color:{series.color}; font-weight: bold;">Casos estimados no tempo {point.t} com {point.d} unidade(s) de atraso:</span> <b>{point.y:.2f}</b>'
    )
  ) %>% 
  hc_subtitle(
    text = "Estimativas com estrutura conjunta de atraso na notificação"
  )

dados_comparacao_lambda = lambda_td_conj_longo %>% 
  mutate(t = as.numeric(t)) %>% 
  left_join(dados_dengue_longo_real %>% select(-d), by = c("t", "delay"))

dados_comparacao_lambda_longo = dados_comparacao_lambda %>% 
  pivot_longer(cols = c("lambda_td", "n_td"))

# Tabela de estimativas estrutura conjunta =====================================

tabela_conj = data.frame(
  t = 1:35,
  d1 = paste0(dados_dengue_real$d1," (",round(lambda_td_conj$d1,0),")"),
  d2 = paste0(dados_dengue_real$d2," (",round(lambda_td_conj$d2,0),")"),
  d3 = paste0(dados_dengue_real$d3," (",round(lambda_td_conj$d3,0),")"),
  d4 = paste0(dados_dengue_real$d4," (",round(lambda_td_conj$d4,0),")"),
  d5 = paste0(dados_dengue_real$d5," (",round(lambda_td_conj$d5,0),")"),
  d6 = paste0(dados_dengue_real$d6," (",round(lambda_td_conj$d6,0),")"),
  d7 = paste0(dados_dengue_real$d7," (",round(lambda_td_conj$d7,0),")"),
  d8 = paste0(dados_dengue_real$d8," (",round(lambda_td_conj$d8,0),")"),
  d9 = paste0(dados_dengue_real$d9," (",round(lambda_td_conj$d9,0),")"),
  d10 = paste0(dados_dengue_real$d10," (",round(lambda_td_conj$d10,0),")"),
  N = paste0(dados_dengue_real$N," (",round(theta_t_conj,0),")")
)


tabela_estimativas_conj = tabela_conj[25:35,] %>% 
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
  row_spec(1:(nrow(tabela_conj[25:35,])), background = "rgb(235, 235, 245, 0.3)", color = "#07080E")  %>% 
  scroll_box(height = "280px")

# Comparacao modelos Poisson ===================================================

plot_N_t_modelos = dados_dengue_real %>% 
  bind_cols(`theta | N - MI`  = theta_inicial_indep,
            `theta | N - MC` = theta_t_conj) %>% 
  mutate(t = 1:35) %>% 
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
          text = "T = 25",
          style = list(color = "black", fontWeight = "bold")
        )
      )
    )
  ) %>% 
  hc_colors(c("black","#910000","#1aadce")) %>% 
  hc_yAxis(
    title = list(text = "N"),
    labels = list(format = "{value:,.0f}")
  ) %>% 
  hc_title(
    text = paste0(
      "<b style='display: block; font-size: 15px;'>",
      "Total de casos de dengue no tempo por modelo",
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


dados_comparacao_modelos = lambda_td_conj_longo %>% 
  left_join(lambda_td_indep_longo %>% 
              rename('value' = 'lambda_td') %>% 
              mutate(d = as.character(d)), 
            by = c("t", "delay", "d")) %>%
  mutate(t = as.numeric(t)) %>% 
  rename("MI" = "lambda_td",
         "MC" = "value") %>% 
  left_join(dados_dengue_longo_real %>% 
              mutate(d = as.character(d)), by = c("t", "delay", "d")) %>% 
  pivot_longer(cols = c(3,5,6))

desvio_N = sd(dados_dengue_real$N)/2
cobertura_indep = ic_theta_indep %>% 
  mutate(N = dados_dengue_real$N,
         valor_no_ic = ifelse(N >= lower-30 & N <= upper+30, 1, 0))
cobertura_conj = ic_theta_conj %>% 
  mutate(N = dados_dengue_real$N,
         valor_no_ic = ifelse(N >= lower-30 & N <= upper+30, 1, 0))

pct_ind = sum(cobertura_indep$valor_no_ic)/35
pct_conj = sum(cobertura_conj$valor_no_ic)/35


cobertura_indep = ic_N_t_indep %>% 
  mutate(N = dados_dengue_real$N,
         valor_no_ic = ifelse(N >= lower-15 & N <= upper+15, 1, 0)) %>% 
  filter(t > 15)
cobertura_conj = ic_N_t_conj %>% 
  mutate(N = dados_dengue_real$N,
         valor_no_ic = ifelse(N >= lower-15 & N <= upper+15, 1, 0)) %>% 
  filter(t > 15)

pct_ind = sum(cobertura_indep$valor_no_ic)/20
pct_conj = sum(cobertura_conj$valor_no_ic)/20


# Erros modelos ================================================================

media_N_t_conj = (ic_N_t_conj$lower + ic_N_t_conj$upper)/2
media_N_t_ind = (ic_N_t_indep$lower + ic_N_t_indep$upper)/2

# N nowcasting
erro_previsao_ind = calcula_mae_e_rmse(valores_estimados = media_N_t_ind[16:25],
                                       valores_reais = dados_dengue_real$N[16:25])
erro_previsao_conj = calcula_mae_e_rmse(valores_estimados = media_N_t_conj[16:25],
                                        valores_reais = dados_dengue_real$N[16:25])
# N forecasting
erro_previsao_ind = calcula_mae_e_rmse(valores_estimados = media_N_t_ind[26:35],
                                 valores_reais = dados_dengue_real$N[26:35])
erro_previsao_conj = calcula_mae_e_rmse(valores_estimados = media_N_t_conj[26:35],
                                       valores_reais = dados_dengue_real$N[26:35])

# n nowcasting

lambda_T25 = lambda_td_indep[1:25,-1]
lambda_T25 = lambda_T25[outer(1:25, 0:(D - 2), FUN = "+") > 25]
n_T25 = dados_dengue_real[1:25,2:11]
n_T25 = n_T25[outer(1:25, 0:(D - 2), FUN = "+") > 25]

erro_prev_n = calcula_mae_e_rmse(valores_estimados = lambda_T25,
                                 valores_reais = n_T25)

lambda_T25 = lambda_td_conj[1:25,-1]
lambda_T25 = lambda_T25[outer(1:25, 0:(D - 2), FUN = "+") > 25]
n_T25 = dados_dengue_real[1:25,2:11]
n_T25 = n_T25[outer(1:25, 0:(D - 2), FUN = "+") > 25]

erro_prev_n = calcula_mae_e_rmse(valores_estimados = lambda_T25,
                   valores_reais = n_T25)

# n forcasting

lambda_T25 = as.vector(unlist(lambda_td_indep[25:35,] %>% select(-t)))
n_T25 = as.vector(unlist(dados_dengue_real[25:35,2:11]))

erro_prev_conj_n = calcula_mae_e_rmse(valores_estimados = lambda_T25,
                                      valores_reais = n_T25)


lambda_T25 = as.vector(unlist(lambda_td_conj[25:35,] %>% select(-t)))
n_T25 = as.vector(unlist(dados_dengue_real[25:35,2:11]))

erro_prev_conj_n = calcula_mae_e_rmse(valores_estimados = lambda_T25,
                                      valores_reais = n_T25)

# N estimacao
erro = calcula_mae_e_rmse(valores_estimados = media_N_t_ind[1:15],
                          valores_reais = dados_dengue_real$N[1:15])

erro = calcula_mae_e_rmse(valores_estimados = media_N_t_conj[1:15],
                          valores_reais = dados_dengue_real$N[1:15])

# n estimacao

lambda_parcial = lambda_td_indep[1:25,-1]
lambda_parcial[outer(1:25, 0:(D - 2), FUN = "+") > 25] = NA
lambda_T15 = as.vector(unlist(lambda_parcial))
lambda_T15 = lambda_T15[!is.na(lambda_T15)]

n = dados_dengue_real[1:25,2:11]
n[outer(1:25, 0:(D - 2), FUN = "+") > 25] = NA
n_T15 = as.vector(unlist(n))
n_T15 = n_T15[!is.na(n_T15)]

erro_prev_conj_n = calcula_mae_e_rmse(valores_estimados = lambda_T15,
                                      valores_reais = n_T15)


lambda_parcial = lambda_td_conj[1:25,-1]
lambda_parcial[outer(1:25, 0:(D - 2), FUN = "+") > 25] = NA
lambda_T15 = as.vector(unlist(lambda_parcial))
lambda_T15 = lambda_T15[!is.na(lambda_T15)]

n = dados_dengue_real[1:25,2:11]
n[outer(1:25, 0:(D - 2), FUN = "+") > 25] = NA
n_T15 = as.vector(unlist(n))
n_T15 = n_T15[!is.na(n_T15)]

erro_prev_conj_n = calcula_mae_e_rmse(valores_estimados = lambda_T15,
                                      valores_reais = n_T15)



# Convergencia dos parametros ==================================================

plot_a_theta = data.frame(
    a_theta = estimativas_conjuntas$a_theta[1:1000],
    Time = 1:1000
  ) %>% 
  hchart("line", hcaes(y = a_theta, x = Time)) %>% 
  hc_colors("gray")

plot_c_theta = data.frame(
  c_theta = estimativas_conjuntas$c_theta[1:1000],
  Time = 1:1000
) %>% 
  hchart("line", hcaes(y = c_theta, x = Time)) %>% 
  hc_colors("gray")

plot_f_theta = data.frame(
  f_theta = estimativas_conjuntas$f_theta[1:1000],
  Time = 1:1000
) %>% 
  hchart("line", hcaes(y = f_theta, x = Time)) %>% 
  hc_colors("gray")

plot_a_alpha = data.frame(
  a_alpha = estimativas_conjuntas$a_alpha[1:1000],
  Time = 1:1000
) %>% 
  hchart("line", hcaes(y = a_alpha, x = Time)) %>% 
  hc_colors("gray")

plot_c_alpha = data.frame(
  c_alpha = estimativas_conjuntas$c_alpha[1:1000],
  Time = 1:1000
) %>% 
  hchart("line", hcaes(y = c_alpha, x = Time)) %>% 
  hc_colors("gray")

plot_f_alpha = data.frame(
  f_alpha = estimativas_conjuntas$f_alpha[1:1000],
  Time = 1:1000
) %>% 
  hchart("line", hcaes(y = f_alpha, x = Time)) %>% 
  hc_colors("gray")


# Modelo para vários T =========================================================

dados_dengue = readr::read_rds("dados/dengueData.RDS")
# filtrando apenas as primeiras 35 semanas (primeira onda)
dados_dengue = dados_dengue[1:10,]

T = 10
D = dim(dados_dengue)[2]

# criando os NAs
dados_dengue[outer(1:T, 0:(D - 1), FUN = "+") > T] <- NA
dados_NA = matrix(nrow = 35-T, ncol = D) %>% as.data.frame()
colnames(dados_NA) = paste0("d",0:(D-1))
dados_dengue = rbind(dados_dengue, dados_NA)


dados_dengue_longo <- dados_dengue %>% 
  rownames_to_column(var = "t") %>% 
  pivot_longer(cols = 2:(D+1), names_to = "d", values_to = "n_td") %>% 
  mutate(delay = d,
         delay = factor(delay, levels = c("d0","d1","d2","d3","d4","d5","d6","d7","d8","d9","d10")),
         d = str_remove_all(d,"d"),
         d = as.numeric(d)) %>% 
  group_by(d) %>% 
  mutate(t = rep(1:(T + (35-T)))) %>% 
  ungroup()

dados_dengue$N_t = rowSums(dados_dengue, na.rm = T)

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
  T = 35, D = D,
  Tk = td_dalay_1$t, Dk = td_dalay_1$d,
  T_k =  td_dalay_k$t,  D_k =  td_dalay_k$d,
  qk = nrow(n_dalay_1), q_k = nrow(n_dalay_k)
)

#modelo_completo_stan =  stan_model("stan/modelCompleteLogistic.stan")

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


# Estimativas iniciais para os parametros (d = 0)
a_alpha_inicial = abcf_por_dalay$d1[1] #17.59739
b_alpha_inicial = abcf_por_dalay$d1[2] #0.001542128
c_alpha_inicial = abcf_por_dalay$d1[3] #0.3753187
f_alpha_inicial = abcf_por_dalay$d1[4] #1.009277

a_theta_inicial = mean(estimativas_theta_isolado$a) #95.78993
b_theta_inicial = mean(estimativas_theta_isolado$b) #0.002145523
c_theta_inicial = mean(estimativas_theta_isolado$c) #0.3906966
f_theta_inicial = mean(estimativas_theta_isolado$f) #1.0006

theta_t_inicial = genLog(t = 1:35, a = a_theta_inicial, 
                         b = b_theta_inicial, 
                         c = c_theta_inicial, 
                         f = f_theta_inicial, logScale = FALSE)

alfha_t_inical = genLog(t = 1:35, a = a_alpha_inicial,
                        b = b_alpha_inicial, 
                        c = c_alpha_inicial, 
                        f = f_theta_inicial, logScale = FALSE)

valores_iniciais = list(
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

# output_modelo_conj = rstan::sampling(modelo_completo_stan,
#                                      data = dados_stan,
#                                      iter = number_interations,
#                                      warmup = warmup,
#                                      chains = chains,
#                                      pars = params,
#                                      init = valores_iniciais,
#                                      verbose = FALSE)
# 
# estimativas_conjuntas_T = rstan::extract(output_modelo_conj)
# saveRDS(estimativas_conjuntas_T, "estimativas/poisson/estimativas_estrutura_conjunta_T10.rds", version = 2)

# Graficos varios T ============================================================

#dados_dengue = readr::read_rds("dados/dengueData.RDS")
dados_dengue_real = readr::read_rds("dados/dengueData.RDS")[1:35,]
dados_dengue_real[outer(1:35, 0:(D - 1), FUN = "+") > 35] <- NA
dados_dengue_real$N = rowSums(dados_dengue_real, na.rm = T)

# # filtrando apenas as primeiras T semanas (primeira onda)
# dados_dengue = dados_dengue[1:20,]
# 
# T = dim(dados_dengue)[1]
# D = dim(dados_dengue)[2]
# 
# # criando os NAs
# dados_dengue[outer(1:T, 0:(D - 1), FUN = "+") > T] <- NA
# 
# dados_dengue_longo <- dados_dengue %>% 
#   rownames_to_column(var = "t") %>% 
#   pivot_longer(cols = 2:(D+1), names_to = "d", values_to = "n_td") %>% 
#   mutate(delay = d,
#          delay = factor(delay, levels = c("d0","d1","d2","d3","d4","d5","d6","d7","d8","d9","d10")),
#          d = str_remove_all(d,"d"),
#          d = as.numeric(d)) %>% 
#   group_by(d) %>% 
#   mutate(t = rep(1:T)) %>% 
#   ungroup()
# 
# dados_dengue$N = rowSums(dados_dengue, na.rm = T)
# 
# 
# 
# dados_dengue_longo_completo = dados_dengue_longo %>% 
#   mutate(d = d + 1) %>% 
#   filter(!is.na(n_td)) %>% 
#   arrange(d)
# 
# # separando o delay 0 (novo 1)
# td_dalay_1 = (dados_dengue_longo_completo %>% filter(d == 1)) %>% select(t,d)
# n_dalay_1 = (dados_dengue_longo_completo %>% filter(d == 1)) %>% 
#   select(n_td) %>% 
#   mutate(n_td = as.numeric(n_td))
# 
# td_dalay_k = dados_dengue_longo_completo %>% filter(d != 1) %>% select(t,d)
# n_dalay_k = dados_dengue_longo_completo %>% filter(d != 1) %>%
#   select(n_td) %>% 
#   mutate(n_td = as.numeric(n_td))
# 
# dados_stan = list(
#   nk = n_dalay_1$n_td, n_k = n_dalay_k$n_td,
#   T = 35, D = D,
#   Tk = td_dalay_1$t, Dk = td_dalay_1$d,
#   T_k =  td_dalay_k$t,  D_k =  td_dalay_k$d,
#   qk = nrow(n_dalay_1), q_k = nrow(n_dalay_k)
# )
# 
# modelo_completo_stan =  stan_model("stan/modelCompleteLogistic.stan")
# 
# warmup = 1000
# chains = 1
# thin = 1
# sample_size = 10000
# number_interations = warmup + thin*sample_size
# 
# params = c("lambda",
#            "alpha", "a_alpha", "b_alpha", "c_alpha", "f_alpha",
#            "beta", "b_beta",
#            "theta", "a_theta", "b_theta", "c_theta", "f_theta",
#            "psi")
# 
# 
# # Estimativas iniciais para os parametros (d = 0)
# a_alpha_inicial = abcf_por_dalay$d1[1] #17.59739
# b_alpha_inicial = abcf_por_dalay$d1[2] #0.001542128
# c_alpha_inicial = abcf_por_dalay$d1[3] #0.3753187
# f_alpha_inicial = abcf_por_dalay$d1[4] #1.009277
# 
# a_theta_inicial = mean(estimativas_theta_isolado$a) #95.78993
# b_theta_inicial = mean(estimativas_theta_isolado$b) #0.002145523
# c_theta_inicial = mean(estimativas_theta_isolado$c) #0.3906966
# f_theta_inicial = mean(estimativas_theta_isolado$f) #1.0006
# 
# theta_t_inicial = genLog(t = 1:35, a = a_theta_inicial, 
#                          b = b_theta_inicial, 
#                          c = c_theta_inicial, 
#                          f = f_theta_inicial, logScale = FALSE)
# 
# alfha_t_inical = genLog(t = 1:35, a = a_alpha_inicial,
#                         b = b_alpha_inicial, 
#                         c = c_alpha_inicial, 
#                         f = f_theta_inicial, logScale = FALSE)
# 
# valores_iniciais = list(
#   list(
#     
#     alpha = alfha_t_inical,
#     
#     a_alpha = a_alpha_inicial,
#     b_alpha = b_alpha_inicial,
#     c_alpha = c_alpha_inicial,
#     f_alpha = f_theta_inicial,
#     
#     theta = theta_t_inicial,
#     
#     a_theta = a_theta_inicial,
#     b_theta = b_theta_inicial,
#     c_theta = c_theta_inicial,
#     f_theta = f_theta_inicial,
#     
#     psi = n_dalay_1$n_td
#   )
# )
# 
# output_modelo_conj_T = rstan::sampling(modelo_completo_stan,
#                                      data = dados_stan,
#                                      iter = number_interations,
#                                      warmup = warmup,
#                                      chains = chains,
#                                      pars = params,
#                                      #init = valores_iniciais,
#                                      verbose = FALSE)
# 
# estimativas_conjuntas_T = rstan::extract(output_modelo_conj_T)
# saveRDS(estimativas_conjuntas_T, "estimativas/poisson/estimativas_estrutura_conjunta_T20_sem_valores_iniciais.rds", version = 2)


# T = 20 -----------

estimativas_conjuntas_T20 = readRDS("estimativas/poisson/estimativas_estrutura_conjunta_T20.rds")

ic_theta_conj_T20 = HPDinterval(mcmc(estimativas_conjuntas_T20$theta), prob = 0.95) %>% 
  as.data.frame() %>% 
  mutate(t = 1:35)

theta_t_conj_T20 = apply(estimativas_conjuntas_T20$theta, 2, mean)

ic_N_t_conj_T20 = gerar_intervalo_N_t(theta_t = theta_t_conj_T20, confianca = 0.95) %>%
  as.data.frame()

plot_N_t_conj_T20 = dados_dengue_real %>% 
  bind_cols(theta_t = theta_t_conj_T20) %>% 
  mutate(t = 1:35) %>% 
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
                     hoje = 20) %>% 
  hc_add_series(data = ic_theta_conj_T20[1:20, ],
                type = "arearange",
                hcaes(x = t, low = lower, high = upper),
                marker = F,
                color = "#FA8072",
                name = "IC 95% theta_t",
                tooltip = list(pointFormat = paste0(
                  "IC 95% no tempo {point.t}: <b> = [{point.lower:,.2f}; {point.upper:,.2f}]</b><br>"
                ))
  ) %>% 
  hc_add_series(data = ic_N_t_conj_T20[20:35, ],
                type = "arearange",
                hcaes(x = t, low = lower, high = upper),
                marker = F,
                color = "gray",
                name = "IC 95% N_t",
                tooltip = list(pointFormat = paste0(
                  "IC 95% no tempo {point.t}: <b> = [{point.lower:,.2f}; {point.upper:,.2f}]</b><br>"
                ))
  ) %>% 
  hc_subtitle(
    text = "Estimativa com estrutura conjunta de atraso na notificação"
  ) %>% 
  hc_colors(c("black", "red"))



# sem paramentros iniciais 

estimativas_conjuntas_T20_spi = readRDS("estimativas/poisson/estimativas_estrutura_conjunta_T20_sem_valores_iniciais.rds")

ic_theta_conj_T20_spi = HPDinterval(mcmc(estimativas_conjuntas_T20_spi$theta), prob = 0.95) %>% 
  as.data.frame() %>% 
  mutate(t = 1:35)

theta_t_conj_T20_spi = apply(estimativas_conjuntas_T20_spi$theta, 2, mean)

ic_N_t_conj_T20_spi = gerar_intervalo_N_t(theta_t = theta_t_conj_T20_spi, confianca = 0.95) %>%
  as.data.frame()

plot_N_t_conj_T20_spi = dados_dengue_real %>% 
  bind_cols(theta_t = theta_t_conj_T20_spi) %>% 
  mutate(t = 1:35) %>% 
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
                     hoje = 20) %>% 
  hc_add_series(data = ic_theta_conj_T20_spi[1:20, ],
                type = "arearange",
                hcaes(x = t, low = lower, high = upper),
                marker = F,
                color = "#FA8072",
                name = "IC 95% theta_t",
                tooltip = list(pointFormat = paste0(
                  "IC 95% no tempo {point.t}: <b> = [{point.lower:,.2f}; {point.upper:,.2f}]</b><br>"
                ))
  ) %>% 
  hc_add_series(data = ic_N_t_conj_T20_spi[20:35, ],
                type = "arearange",
                hcaes(x = t, low = lower, high = upper),
                marker = F,
                color = "gray",
                name = "IC 95% N_t",
                tooltip = list(pointFormat = paste0(
                  "IC 95% no tempo {point.t}: <b> = [{point.lower:,.2f}; {point.upper:,.2f}]</b><br>"
                ))
  ) %>% 
  hc_subtitle(
    text = "Estimativa com estrutura conjunta de atraso na notificação"
  ) %>% 
  hc_colors(c("black", "red"))


# T = 15 ------------

estimativas_conjuntas_T15 = readRDS("estimativas/poisson/estimativas_estrutura_conjunta_T15.rds")

ic_theta_conj_T15 = HPDinterval(mcmc(estimativas_conjuntas_T15$theta), prob = 0.95) %>% 
  as.data.frame() %>% 
  mutate(t = 1:35)

theta_t_conj_T15 = apply(estimativas_conjuntas_T15$theta, 2, mean)

ic_N_t_conj_T15 = gerar_intervalo_N_t(theta_t = theta_t_conj_T15, confianca = 0.95) %>%
  as.data.frame()

plot_N_t_conj_T15 = dados_dengue_real %>% 
  bind_cols(theta_t = theta_t_conj_T15) %>% 
  mutate(t = 1:35) %>% 
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
                     hoje = 15) %>% 
  hc_add_series(data = ic_theta_conj_T15[1:15, ],
                type = "arearange",
                hcaes(x = t, low = lower, high = upper),
                marker = F,
                color = "#FA8072",
                name = "IC 95% theta_t",
                tooltip = list(pointFormat = paste0(
                  "IC 95% no tempo {point.t}: <b> = [{point.lower:,.2f}; {point.upper:,.2f}]</b><br>"
                ))
  ) %>% 
  hc_add_series(data = ic_N_t_conj_T15[15:35, ],
                type = "arearange",
                hcaes(x = t, low = lower, high = upper),
                marker = F,
                color = "gray",
                name = "IC 95% N_t",
                tooltip = list(pointFormat = paste0(
                  "IC 95% no tempo {point.t}: <b> = [{point.lower:,.2f}; {point.upper:,.2f}]</b><br>"
                ))
  ) %>% 
  hc_subtitle(
    text = "Estimativa com estrutura conjunta de atraso na notificação"
  ) %>% 
  hc_colors(c("black", "red"))



# sem parametros iniciais

estimativas_conjuntas_T15_spi = readRDS("estimativas/poisson/estimativas_estrutura_conjunta_T15_sem_valores_iniciais.rds")

ic_theta_conj_T15_spi = HPDinterval(mcmc(estimativas_conjuntas_T15_spi$theta), prob = 0.95) %>% 
  as.data.frame() %>% 
  mutate(t = 1:35)

theta_t_conj_T15_spi = apply(estimativas_conjuntas_T15_spi$theta, 2, mean)

ic_N_t_conj_T15_spi = gerar_intervalo_N_t(theta_t = theta_t_conj_T15_spi, confianca = 0.95) %>%
  as.data.frame()

plot_N_t_conj_T15_spi = dados_dengue_real %>% 
  bind_cols(theta_t = theta_t_conj_T15_spi) %>% 
  mutate(t = 1:35) %>% 
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
                     hoje = 15) %>% 
  hc_add_series(data = ic_theta_conj_T15_spi[1:15, ],
                type = "arearange",
                hcaes(x = t, low = lower, high = upper),
                marker = F,
                color = "#FA8072",
                name = "IC 95% theta_t",
                tooltip = list(pointFormat = paste0(
                  "IC 95% no tempo {point.t}: <b> = [{point.lower:,.2f}; {point.upper:,.2f}]</b><br>"
                ))
  ) %>% 
  hc_add_series(data = ic_N_t_conj_T15_spi[15:35, ],
                type = "arearange",
                hcaes(x = t, low = lower, high = upper),
                marker = F,
                color = "gray",
                name = "IC 95% N_t",
                tooltip = list(pointFormat = paste0(
                  "IC 95% no tempo {point.t}: <b> = [{point.lower:,.2f}; {point.upper:,.2f}]</b><br>"
                ))
  ) %>% 
  hc_subtitle(
    text = "Estimativa com estrutura conjunta de atraso na notificação"
  ) %>% 
  hc_colors(c("black", "red"))

# T = 10 ------------

estimativas_conjuntas_T10 = readRDS("estimativas/poisson/estimativas_estrutura_conjunta_T10.rds")

ic_theta_conj_T10 = HPDinterval(mcmc(estimativas_conjuntas_T10$theta), prob = 0.95) %>% 
  as.data.frame() %>% 
  mutate(t = 1:35)

theta_t_conj_T10 = apply(estimativas_conjuntas_T10$theta, 2, mean)

ic_N_t_conj_T10 = gerar_intervalo_N_t(theta_t = theta_t_conj_T10, confianca = 0.95) %>%
  as.data.frame()

plot_N_t_conj_T10 = dados_dengue_real %>% 
  bind_cols(theta_t = theta_t_conj_T10) %>% 
  mutate(t = 1:35) %>% 
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
                     hoje = 10) %>% 
  hc_add_series(data = ic_theta_conj_T10[1:10, ],
                type = "arearange",
                hcaes(x = t, low = lower, high = upper),
                marker = F,
                color = "#FA8072",
                name = "IC 95% theta_t",
                tooltip = list(pointFormat = paste0(
                  "IC 95% no tempo {point.t}: <b> = [{point.lower:,.2f}; {point.upper:,.2f}]</b><br>"
                ))
  ) %>% 
  hc_add_series(data = ic_N_t_conj_T10[10:35, ],
                type = "arearange",
                hcaes(x = t, low = lower, high = upper),
                marker = F,
                color = "gray",
                name = "IC 95% N_t",
                tooltip = list(pointFormat = paste0(
                  "IC 95% no tempo {point.t}: <b> = [{point.lower:,.2f}; {point.upper:,.2f}]</b><br>"
                ))
  ) %>% 
  hc_subtitle(
    text = "Estimativa com estrutura conjunta de atraso na notificação"
  ) %>% 
  hc_colors(c("black", "red"))



# sem parametros iniciais 

estimativas_conjuntas_T10_spi = readRDS("estimativas/poisson/estimativas_estrutura_conjunta_T10_sem_valores_iniciais.rds")

ic_theta_conj_T10_spi = HPDinterval(mcmc(estimativas_conjuntas_T10_spi$theta), prob = 0.95) %>% 
  as.data.frame() %>% 
  mutate(t = 1:35)

theta_t_conj_T10_spi = apply(estimativas_conjuntas_T10_spi$theta, 2, mean)

ic_N_t_conj_T10_spi = gerar_intervalo_N_t(theta_t = theta_t_conj_T10_spi, confianca = 0.95) %>%
  as.data.frame()

plot_N_t_conj_T10_spi = dados_dengue_real %>% 
  bind_cols(theta_t = theta_t_conj_T10_spi) %>% 
  mutate(t = 1:35) %>% 
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
                     hoje = 10) %>% 
  hc_add_series(data = ic_theta_conj_T10_spi[1:10, ],
                type = "arearange",
                hcaes(x = t, low = lower, high = upper),
                marker = F,
                color = "#FA8072",
                name = "IC 95% theta_t",
                tooltip = list(pointFormat = paste0(
                  "IC 95% no tempo {point.t}: <b> = [{point.lower:,.2f}; {point.upper:,.2f}]</b><br>"
                ))
  ) %>% 
  hc_add_series(data = ic_N_t_conj_T10_spi[10:35, ],
                type = "arearange",
                hcaes(x = t, low = lower, high = upper),
                marker = F,
                color = "gray",
                name = "IC 95% N_t",
                tooltip = list(pointFormat = paste0(
                  "IC 95% no tempo {point.t}: <b> = [{point.lower:,.2f}; {point.upper:,.2f}]</b><br>"
                ))
  ) %>% 
  hc_subtitle(
    text = "Estimativa com estrutura conjunta de atraso na notificação"
  ) %>% 
  hc_colors(c("black", "red"))