
dados_dengue = readr::read_rds("izabel\\dengueData.RDS")

dados_dengue = dados_dengue[1:35,]
dados_dengue_real = dados_dengue
dados_dengue_real$N = rowSums(dados_dengue_real)




modelo_stan =  stan_model("izabel\\modelLogistic.stan")
modelo_completo_stan =  stan_model("izabel\\modelCompleteLogistic.stan")

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

genLog = function(t, a, b, c, f, logScale = TRUE){
  logV = log(f)+log(a)+log(c)-(c*t)-(f+1)*log( b+exp(-c*t) )
  if (logScale){
    return(logV);
  } else {
    return(exp(logV));
  }
}

estimativa_hoje = function(dados_dengue, hoje = 15){
  
  T = dim(dados_dengue)[1]
  D = dim(dados_dengue)[2]
  
  # criando os NAs
  dados_dengue[outer(1:T, 0:(D - 1), FUN = "+") > hoje] <- NA
  
  estimativas = apply(dados_dengue, 2, function(y) stanLogistic(modelo_stan, y = y))
  abcf_por_dalay = lapply(estimativas, function(x) c(mean(x$a), mean(x$b), mean(x$c), mean(x$f)) )
  estimativas_theta = stanLogistic(modelo_stan, y = rowSums(dados_dengue, na.rm = TRUE))
  
  
  theta_inicial = genLog(t = 1:T,
                         a = mean(estimativas_theta$a),
                         b = mean(estimativas_theta$b),
                         c = mean(estimativas_theta$c),
                         f = mean(estimativas_theta$f),
                         logScale = F)
  
  dados_dengue$N_t = rowSums(dados_dengue, na.rm = T)
  
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
    T = hoje, D = D,
    Tk = td_dalay_1$t, Dk = td_dalay_1$d,
    T_k =  td_dalay_k$t,  D_k =  td_dalay_k$d,
    qk = nrow(n_dalay_1), q_k = nrow(n_dalay_k)
  )
  
  
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
  
  a_alpha_inicial = abcf_por_dalay$d1[1]
  b_alpha_inicial = abcf_por_dalay$d1[2]
  c_alpha_inicial = abcf_por_dalay$d1[3]
  f_alpha_inicial = abcf_por_dalay$d1[4]
  
  a_theta_inicial = mean(estimativas_theta$a)
  b_theta_inicial = mean(estimativas_theta$b)
  c_theta_inicial = mean(estimativas_theta$c)
  f_theta_inicial = mean(estimativas_theta$f)
  
  theta_t_inicial = genLog(t = 1:hoje, a = a_theta_inicial, 
                           b = b_theta_inicial, 
                           c = c_theta_inicial, 
                           f = f_theta_inicial, logScale = FALSE)
  
  alfha_t_inical = genLog(t = 1:hoje, a = a_alpha_inicial,
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
  
  output_modelo_com_chute = rstan::sampling(modelo_completo_stan,
                                            data = dados_stan,
                                            iter = number_interations,
                                            warmup = warmup,
                                            chains = chains,
                                            pars = params,
                                            init = chute_inicial,
                                            verbose = FALSE)
  
  estimativas_com_chute = rstan::extract(output_modelo_com_chute)
  
  
}

saveRDS(estimativas, "estimativas/estimativas_n_td_por_delay_T15.rds",version = 2)
saveRDS(abcf_por_dalay, "estimativas/abcf_por_dalay_T15.rds", version = 2)
saveRDS(estimativas_theta, "estimativas/estimativas_theta_T15.rds",version = 2)
saveRDS(estimativas_com_chute, "estimativas\\estimativas_com_chute_T15.rds", version = 2)


