setwd("C:\\Users\\gi_pa\\OneDrive\\�rea de Trabalho\\bayesiana\\aula")

library(rstan)
library(coda)

###########(An�lise Regress�o Normal)  
########### Modelo Y = beta0 + beta1*X + epsilon 
##########  epsilon ~ Normal (0,sigma^2)
##########  Y ~ Normal (mu,sigma^2)
########### E(Y) = mu e Var(Y) = sigma^2
##########  mu = beta0 + beta1*X

set.seed(1)  #fixando a semente
n = 100  #tamanho da amostra 
beta0 = 0.5  
beta1 = 1
sigma2 = 1
X = rnorm(n,mean=0,sd = 1) #covari�vel 
mu = beta0 + beta1*X
epsilon = rnorm(n,0,sqrt(sigma2)) ##erro 
Y = mu + epsilon #vari�vel resposta 
#plot(X,Y)
#lines(X,mu,type="l")



###Breve Abordagem Frequentista 

ajuste = glm(Y~X,family=gaussian(link="identity"))
summary(ajuste)

ajuste$coefficients
ajuste$residuals
ajuste$fitted.values

#estimativas
estf_beta0 = ajuste$coefficients[1]
estf_beta1 = ajuste$coefficients[2]
estf_sigma2 = 0.926894

Y_hat = estf_beta0 + estf_beta1*X
res = Y_hat - Y 
plot(X,Y)
lines(X,Y_hat,col="blue",type="l")
lines(X,mu,col="red",type="l")

#objetos para o c�lculo do ic de sigma2 
phi = 1/estf_sigma2
KK = n/(2*phi^2)
z = 1.96

#Intervalo de Confian�a 
ic = confint(ajuste)
icf_beta0 = ic[1,]
icf_beta1 = ic[2,]
icf_sigma2 = c(estf_sigma2-z*sqrt(KK^-1), estf_sigma2+z*sqrt(KK^-1))


########Abordagem Bayesiana  

data_stan = list(n=length(Y),x=X,y=Y)
warmup = 1500
chains = 1
#thin = 3
sample_size = 4000
number_iterations = warmup + sample_size

params = c("beta0","beta1","sigma2")
init = list()
init[[1]] = list(beta0=0,beta1=0,sigma2=1)
#init[[2]] = list(beta0=10,beta1=10,sigma2=10) #caso de 2 cadeias 

model = "reg.stan"
mod = stan_model(model)
output = sampling(mod,data=data_stan,iter=number_iterations,warmup=warmup,
                  chains=chains,pars=params,init=init,verbose=FALSE)

summary(output)

chains = rstan::extract(output)


###################################caso para 2 cadeias(ignorar se estiver trabalhando com 1 cadeia) 
cadeias = output@sim$samples
cadeia1 = cadeias[[1]]
c1_beta0 = cadeia1$beta0
c1_beta1 = cadeia1$beta1
c1_sigma2 = cadeia1$sigma2

cadeia2 = cadeias[[2]]
c2_beta0 = cadeia2$beta0
c2_beta1 = cadeia2$beta1
c2_sigma2 = cadeia2$sigma2

plot(c1_beta0,type="l")
lines(c2_beta0,type="l",col="red")

plot(c1_beta1,type="l")
lines(c2_beta1,type="l",col="red")

plot(c1_sigma2,type="l")
lines(c2_sigma2,type="l",col="red")

mean(c1_beta0)
mean(c2_beta0)

mean(c1_beta1)
mean(c2_beta1)

mean(c1_sigma2)
mean(c2_sigma2)


#############################################################

s_beta0 = chains$beta0
s_beta1 = chains$beta1
s_sigma2 = chains$sigma2

plot(s_beta0,type="l")
abline(h=0.5,col="red")
plot(s_beta1,type="l")
abline(h=1,col="red")
plot(s_sigma2,type="l")
abline(h=1,col="red")

hist(s_beta0)
hist(s_beta1)
hist(s_sigma2)

##Intervalo HPD
#hpd_beta0 = HPDinterval(as.mcmc(c(chains$beta0)), prob=0.95)
#hpd_beta1 = HPDinterval(as.mcmc(c(chains$beta1)), prob=0.95)
#hpd_sigma2 = HPDinterval(as.mcmc(c(chains$sigma2)), prob=0.95)

icb_beta0 = quantile(s_beta0, c(0.025, 0.975))
icb_beta1 = quantile(s_beta1, c(0.025, 0.975))
icb_sigma2 = quantile(s_sigma2, c(0.025, 0.975))

estb_beta0 = mean(chains$beta0)
estb_beta1 = mean(chains$beta1)
estb_sigma2 = mean(chains$sigma2)


##Compara��o beta0 
beta0_frequentista = c(estf_beta0,icf_beta0)
beta0_bayesiano = c(estb_beta0,icb_beta0)

tab_comp = rbind(beta0_frequentista,beta0_bayesiano)
colnames(tab_comp) = c("Estimativa Pontual","IC_inf","IC_sup")
tab_comp

##Compara��o beta1

beta1_frequentista = c(estf_beta1,icf_beta1)
beta1_bayesiano = c(estb_beta1,icb_beta1)


tab_comp = rbind(beta1_frequentista,beta1_bayesiano)
colnames(tab_comp) = c("Estimativa Pontual","IC_inf","IC_sup")
tab_comp

##Compara��o sigma2 

sigma2_frequentista = c(estf_sigma2,icf_sigma2)
sigma2_bayesiano = c(estb_sigma2,icb_sigma2)


tab_comp = rbind(sigma2_frequentista,sigma2_bayesiano)
colnames(tab_comp) = c("Estimativa Pontual","IC_inf","IC_sup")
tab_comp



###########(An�lise Regress�o Log�stica)  
########### Modelo log(mu/1-mu) = beta0 + beta1*X  ou logit(mu) = beta0 + beta1*X
##########  Y ~ Bernoulli (mu)
########### E(Y) = mu e Var(Y) = mu(1-mu)
########### term1 = exp(beta0 + beta1*X)
##########  mu = term1/(1+term1)

set.seed(1)
n = 100 
beta0 = 0.5  
beta1 = 1
X = rnorm(n,mean=0,sd = 1)
term1 = exp(beta0 + beta1*X)
mu = term1/(1+term1)
Y = rbinom(n,1,mu) 
#plot(X,Y)
#lines(X,mu,type="l")


###Breve Abordagem Frequentista 

ajuste = glm(Y ~ X,family=binomial(link="logit"))
summary(ajuste) 

ajuste$coefficients
ajuste$residuals
ajuste$fitted.values

#estimativas 
estf_beta0 = ajuste$coefficients[1]
estf_beta1 = ajuste$coefficients[2]

Y_hat = exp(estf_beta0 + estf_beta1*X)/(1+exp(estf_beta0 + estf_beta1*X))
plot(X,Y)
lines(X,Y_hat,col="blue",type="l")

#intervalo de confian�a 
ic = confint(ajuste)
icf_beta0 = ic[1,]
icf_beta1 = ic[2,]


########Abordagem bayesiana  

data_stan = list(n=length(Y),x=X,y=Y)
warmup = 1500
chains = 1
#thin = 3
sample_size = 4000
number_interations = warmup + sample_size

params = c("beta0","beta1")
init = list()
init[[1]] = list(beta0=0,beta1=0)

model = "log.stan"
mod = stan_model(model)
output = sampling(mod,data=data_stan,iter=number_interations,warmup=warmup,
                  chains=chains,pars=params,init=init,verbose=FALSE)
summary(output)

chains = rstan::extract(output)
s_beta0 = chains$beta0
s_beta1 = chains$beta1

hist(s_beta0)
hist(s_beta1)

plot(s_beta0,type="l")
plot(s_beta1,type="l")

#hpd_beta0 = HPDinterval(as.mcmc(c(chains$beta0)), prob=0.95)
#hpd_beta1 = HPDinterval(as.mcmc(c(chains$beta1)), prob=0.95)

icb_beta0 = quantile(s_beta0, c(0.025, 0.975))
icb_beta1 = quantile(s_beta1, c(0.025, 0.975))

estb_beta0 = mean(chains$beta0)
estb_beta1 = mean(chains$beta1)


#est.beta0 = c(mean(chains$beta0), median(chains$beta0), sd(chains$beta0),hpd_beta0,q_beta0)
#est.beta1 = c(mean(chains$beta1), median(chains$beta1), sd(chains$beta1),hpd_beta1,q_beta1)
#est.sigma = c(mean(chains$sigma), median(chains$sigma), sd(chains$sigma),hpd_sigma,q_sigma)
#tab = rbind(est.beta0, est.beta1,est.sigma)
#colnames(tab) = c("m�dia","mediana","desvio-padr�o","HPD_inf","HPD_sup","IC_inf","IC_sup")
#tab

##Compara��o beta0 

beta0_frequentista = c(estf_beta0,icf_beta0)
beta0_bayesiano = c(estb_beta0,icb_beta0)


tab_comp = rbind(beta0_frequentista,beta0_bayesiano)
colnames(tab_comp) = c("Estimativa Pontual","IC_inf","IC_sup")
tab_comp

##Compara��o beta1

beta1_frequentista = c(estf_beta1,icf_beta1)
beta1_bayesiano = c(estb_beta1,icb_beta1)


tab_comp = rbind(beta1_frequentista,beta1_bayesiano)
colnames(tab_comp) = c("Estimativa Pontual","IC_inf","IC_sup")
tab_comp


#######################################An�lise Dados Reais 

#Utilizaremos agora os dados referentes a um estudo de caso-controle realizado
#no Setor de Anatomia e Patologia do Hospital Heli�polis em S�o Paulo, no
#per�odo de 1970 a 1982 (Paula e Tuder, 1986) (ver arquivo canc3.dat).
#Um total de 175 pacientes com processo infecioso pulmonar atendido no
#hospital no per�odo acima foi classificado segundo as seguintes vari�veis: Y,
#tipo de tumor (1: maligno, 0: benigno); IDADE, idade em anos; SEXO (0:
#masculino, 1: feminino); HL, intensidade da c�lula histi�citos-linf�citos (1:                                                                                                                                                   ausente, 2: discreta, 3: moderada, 4: intensa) e FF, intensidade da c�lula
#fibrose-frouxa (1: ausente, 2: discreta, 3: moderada, 4: intensa).

canc3.dat = scan("canc3.dat.txt", what=list(tipo=0, idade=0, sexo=0,
                                            hl=0, ff=0))
attach(canc3.dat)
Y = tipo 
X = idade 

#######Frequentista 

ajuste = glm( Y ~ X,
              family=binomial(link = "logit"))
summary(ajuste)


#estimativas
estf_beta0 = ajuste$coefficients[1]
estf_beta1 = ajuste$coefficients[2]

#intervalo de confian�a 
ic = confint(ajuste)
icf_beta0 = ic[1,]
icf_beta1 = ic[2,]


######################Bayesiano 

data_stan = list(n=length(Y),x=X,y=Y)
warmup = 1500
chains = 1
#thin = 3
sample_size = 4000
number_interations = warmup + sample_size

params = c("beta0","beta1")
init = list()
init[[1]] = list(beta0=0,beta1=0)

model = "log.stan"
mod = stan_model(model)
output = sampling(mod,data=data_stan,iter=number_interations,warmup=warmup,
                  chains=chains,pars=params,init=init,verbose=FALSE)
summary(output)

chains = rstan::extract(output)
s_beta0 = chains$beta0
s_beta1 = chains$beta1

hist(s_beta0)
hist(s_beta1)

hpd_beta0 = HPDinterval(as.mcmc(c(chains$beta0)), prob=0.95)
hpd_beta1 = HPDinterval(as.mcmc(c(chains$beta1)), prob=0.95)

icb_beta0 = quantile(s_beta0, c(0.025, 0.975))
icb_beta1 = quantile(s_beta1, c(0.025, 0.975))

estb_beta0 = mean(chains$beta0)
estb_beta1 = mean(chains$beta1)


##Compara��o beta0 

beta0_frequentista = c(estf_beta0,icf_beta0)
beta0_bayesiano = c(estb_beta0,icb_beta0)


tab_comp = rbind(beta0_frequentista,beta0_bayesiano)
colnames(tab_comp) = c("Estimativa Pontual","IC_inf","IC_sup")
tab_comp

##Compara��o beta1

beta1_frequentista = c(estf_beta1,icf_beta1)
beta1_bayesiano = c(estb_beta1,icb_beta1)


tab_comp = rbind(beta1_frequentista,beta1_bayesiano)
colnames(tab_comp) = c("Estimativa Pontual","IC_inf","IC_sup")
tab_comp