library(rstan)
library(dplyr)
library(INLA)
library(stringr)
library(tidyverse)
library(highcharter)

dados_dengue = readr::read_rds("izabel\\dengueData.RDS")

# considerar dalay 0 = delay 1
# dados removendo n_td com dalay = 0
# tirei a coluna d0 mas poderia ser qualquer outra
# nao seria melhor para o modelo se tirasse a coluna com menos obs? (d10) ------
dados_dengue_sem_d0 = dados_dengue

T = dim(dados_dengue_sem_d0)[1]
D = dim(dados_dengue_sem_d0)[2]

dados_dengue_longo <- dados_dengue_sem_d0 %>% 
  mutate(t = rownames(dados_dengue_sem_d0)) %>% 
  pivot_longer(cols = 1:10, names_to = "d", values_to = "n_td") %>% 
  mutate(d = str_remove_all(d,"d"),
         d = as.numeric(d)) %>% 
  group_by(d) %>% 
  mutate(t = rep(1:T)) %>% 
  ungroup()

dados_dengue_longo %>%
  hchart('line', hcaes(x = t, y = n_td, group = d))

dados_dengue_longo %>% 
  hchart('line', hcaes(x = t, y = log(n_td+0.05), group = d))

# filtrando apenas as primeiras 35 semanas

dados_dengue_sem_d0 = dados_dengue[1:35,]

T = dim(dados_dengue_sem_d0)[1]
D = dim(dados_dengue_sem_d0)[2]

# criando os NAs
dados_dengue_sem_d0[outer(1:T, 0:(D - 1), FUN = "+") > T] <- NA

dados_dengue_longo <- dados_dengue_sem_d0 %>% 
  mutate(t = rownames(dados_dengue_sem_d0)) %>% 
  pivot_longer(cols = 1:10, names_to = "d", values_to = "n_td") %>% 
  mutate(d = str_remove_all(d,"d"),
         d = as.numeric(d),
         d = d+1) %>% 
  group_by(d) %>% 
  mutate(t = rep(1:T)) %>% 
  ungroup()

dados_dengue_longo %>% 
  hchart('line', hcaes(x = t, y = n_td, group = d))

dados_dengue_longo %>% 
  filter(t>28) %>% 
  hchart('line', hcaes(x = t, y = n_td, group = d))

dados_dengue_longo %>% 
  hchart('line', hcaes(x = t, y = log(n_td+0.05), group = d))

# stan
modelo_stan =  stan_model("izabel\\modelLogistic.stan")

stanLogistic = function(stanModel, y){
  
  y = y[!is.na(y)]
  n = length(y)
  
  # -> Stan configuration
  warmup = 1000
  chains = 1
  thin = 1
  sample_size = 1000
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
# oq é lp__ ????????????????????????????????? -----------
# dalay2 = stanLogistic(modelo_stan, y = dados_dengue_sem_d0[,2])
# saveRDS(dalay2, "modelos/n_td_dalay2.rds", version = 2)

dalay2 = readRDS("modelos/n_td_dalay2.rds")

mu_dalay2 = apply(dalay2$mu,2, mean)
data.frame(mu = mu_dalay2,
           t = 1:length(mu_dalay2)) %>%  
  hchart("line", hcaes(y = mu, x = t), name= "mu d2")

dados_plot = dados_dengue_longo %>% 
  filter(d==2) %>% 
  mutate(
    lambda_td = c(mu_dalay2,NA)
  ) %>% 
  pivot_longer(cols = 3:4)

dados_plot %>% 
  hchart('line', hcaes(x = t, y = value, group = name)) %>% 
  hc_yAxis(title = list(text = "Valor - Delay 2"),
           lineColor = "#f7f7f7", gridLineColor = "#f4f4f4") %>%
  hc_xAxis(lineColor = "#f7f7f7", gridLineColor = "#f4f4f4") %>% 
  hc_title(
    text = paste0(
      "<b style='display: block; font-size: 12px;'>",
      "Valor real e estimado de casos por tempo no Delay 2 </b>"
    ),
    margin = 20,
    align = "left",
    style = list(useHTML = TRUE)
  )



# estimativas = apply(dados_dengue_sem_d0, 2, function(y) stanLogistic(modelo_stan, y = y))
# saveRDS(estimativas, "modelos/estimativas_n_td_por_delay.rds",version = 2)

estimativas = readRDS("modelos/estimativas_n_td_por_delay.rds")
abcf_por_dalay = lapply(estimativas, function(x) c(mean(x$a), mean(x$b), mean(x$c), mean(x$f)) )

saveRDS(abcf_por_dalay, "modelos/abcf_por_dalay.rds", version = 2)

genLog = function(t, a, b, c, f, logScale = TRUE){
  logV = log(f)+log(a)+log(c)-(c*t)-(f+1)*log( b+exp(-c*t) )
  if (logScale){
    return(logV);
  } else {
    return(exp(logV));
  }
}

n_t = list()
for(d in 1:10){
  n_t[[d]] = genLog(t = 1:T, a = abcf_por_dalay[[d]][1], b = abcf_por_dalay[[d]][2],
                  c = abcf_por_dalay[[d]][3], f = abcf_por_dalay[[d]][4], logScale = F)
  
}

dados_estimados <-  as.data.frame(do.call(cbind, n_t))
colnames(dados_estimados) = paste0("d", 1:10)
dados_estimados <- dados_estimados %>% 
  pivot_longer(cols = 1:10,
               names_to = "d") %>% 
  group_by(d) %>% 
  mutate(t = 1:T) %>% 
  ungroup() %>% 
  mutate(d = factor(d, levels = c("d1", "d2", "d3", "d4", "d5", "d6", "d7", "d8", "d9", "d10" )))

dados_estimados %>% 
  hchart('line', hcaes(x = t, y = value, group = d))%>% 
  hc_tooltip(pointFormat = "Estimativa no delay {series.name} e tempo {point.t}: <b>{point.y:.2f}</b>") %>% 
  hc_xAxis(lineColor = "#f7f7f7", gridLineColor = "#f4f4f4") %>% 
  hc_yAxis(lineColor = "#f7f7f7", gridLineColor = "#f4f4f4",
           title = list(text = "Valor Estimado")) %>% 
  hc_title(
    text = paste0(
      "<b style='display: block; font-size: 12px;'>",
      "Valor estimado de casos por tempo e delay</b>"
    ),
    margin = 20,
    align = "left",
    style = list(useHTML = TRUE)
  )

dados_estimados %>% 
  hchart('line', hcaes(x = t, y = log(value+0.05), group = d))%>% 
  hc_xAxis(lineColor = "#f7f7f7", gridLineColor = "#f4f4f4") %>% 
  hc_yAxis(lineColor = "#f7f7f7", gridLineColor = "#f4f4f4",
           title = list(text = "Log( Valor Estimado+0.05 )")) %>% 
  hc_title(
    text = paste0(
      "<b style='display: block; font-size: 12px;'>",
      "Verificando suposição de linearidade do Delay</b>"
    ),
    margin = 20,
    align = "left",
    style = list(useHTML = TRUE)
  )

# Estimando N_t ================================================================

#estimativa_N_t = stanLogistic(modelo_stan, y = rowSums(dados_dengue_sem_d0, na.rm = TRUE))
#saveRDS(estimativa_N_t, "modelos/estimativa_N_t.rds", version = 2)
estimativa_N_t = readRDS("modelos/estimativa_N_t.rds")

abcf_N_t = c(
  mean(estimativa_N_t$a),
  mean(estimativa_N_t$b),
  mean(estimativa_N_t$c),
  mean(estimativa_N_t$f)
)
saveRDS(abcf_N_t, "modelos/abcf_N_t.rds", version = 2)



