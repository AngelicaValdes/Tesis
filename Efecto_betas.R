dotR <- file.path(Sys.getenv("HOME"), ".R")
if (!file.exists(dotR)) 
  dir.create(dotR)
M <- file.path(dotR, "Makevars")
if (!file.exists(M)) {
  file.create(M)
  cat("\nCXXFLAGS=-O3 -Wno-unused-variable -Wno-unused-function", 
      file = M, sep = "\n", append = TRUE)
  cat('Sys.setenv(BINPREF = "C:/Rtools/mingw_$(WIN)/bin/")',
      file = file.path(Sys.getenv("HOME"), ".Rprofile"), 
      sep = "\n", append = TRUE)
  cat("\nCXXFLAGS += -Wno-ignored-attributes -Wno-deprecated-declarations", 
      file = M, sep = "\n", append = TRUE)
}
setwd("H:/Dropbox/yan shang/delays angelica")
#setwd("C:/Users/Angie/Dropbox/delays angelica")
library(readxl)
library(dplyr)
library("rstan")
rstan_options(auto_write = TRUE)
options(mc.cores = parallel::detectCores())

#guardar los datos de beta, beta_0
load("H:/Dropbox/yan shang/delays angelica/modelo_6_2.RData")
beta_0 <- summary(reg_fit2, "beta_0", probs = NULL)$summary
beta <- summary(reg_fit2, "beta", probs = NULL)$summary

#se cargan los datos necesarios
#load('H:/Dropbox/yan shang/delays angelica/mydata_server_14.RData')
#BD3<-BD
load('H:/Dropbox/yan shang/delays angelica/mydata_server_3.RData')
BD2 <- read_excel("Base_modelo_15.xlsx" , guess_max = 50000)
BD2$tau_1<-BD2$tau_1/24
BD2$tau_2<-BD2$tau_2/24
BD2$tau_3<-BD2$tau_3/24
BD2$tau_4<-BD2$tau_4/24
BD2$demand_1<-BD2$demand_1/100
BD2$demand_2<-BD2$demand_2/100
BD2$demand_3<-BD2$demand_3/100
BD2$demand_4<-BD2$demand_4/100

#se ordenan todos por cliente
BD <-BD[order(BD$child_id),]
BD2 <-BD2[order(BD2$client),]
#BD3 <-BD3[order(BD3$child_id),]
clientes<-clientes[order(clientes$child_id),]

#Pronosticar nuevo theta luego de sumar la desviacion estandar por cliente y por ruta

#modelo de pronostico a utilizar
mod<-stan_model("delay2.stan")

#inicializar variables para guardar theta,sigma y xi
#N es la suma de las rutas para todos los clientes y ya estaba calculado en d_p_s_s_12.RData

efecto<-data.frame("cliente"=rep(0,N),"ruta"=rep(0,N),"sd"=rep(0,N),"theta"=rep(0,N),"theta_mod"=rep(0,N),"sigma"=rep(0,N),"sigma_mod"=rep(0,N),"xi"=rep(0,N),"xi_mod"=rep(0,N),"prob"=rep(0,N), "prob_mod"=rep(0,N), "diferencia"=rep(0,N))

r<-1
x<-1
pos_r<-1
for (j in clientes$child_id[1:677]){
  cliente<-subset(BD,child_id==j)
  cliente2<-subset(BD2,client==j)
  #cliente3<-subset(BD3,child_id==j)
  #desviación estandar por ruta
  
  #sd_1<-sd(cliente3$time_hours[which(cliente3$route_id==1)])
  sd_1<-sd(cliente$difference[which(cliente$route_id==1)])
  #sd_2<-sd(cliente3$time_hours[which(cliente3$route_id==2)])
  #sd_3<-sd(cliente3$time_hours[which(cliente3$route_id==3)])
  #sd_4<-sd(cliente3$time_hours[which(cliente3$route_id==4)])
  cliente$diff1<-cliente$diff1+sd_1
  #cliente$diff2<-cliente$diff2+sd_2
  #cliente$diff3<-cliente$diff3+sd_3
  #cliente$diff4<-cliente$diff4+sd_4
  cliente<-cliente[order(cliente$Periodo_final, cliente$route_id),]
  
  #inicializacion de variables
  h<-length(cliente$Periodo_final)
  thetar<-data.frame(theta1=param[4],theta2=param[4],theta3=param[4],theta4=param[4])
  
  #algoritmo para obtener pronostico
  nRoutes <- cliente$child_route_num[1]
  nObs <- nrow(cliente[1:h,])
  obs <- as.matrix(cliente[1:h,6:(5+nRoutes)], nrow = nrow(cliente[1:h,]))
  y <- as.matrix(cliente[1:h,10:(9+nRoutes)], nrow = nrow(cliente[1:h,]))
  
  stan.data = list(nRoutes = nRoutes, nObs = nObs, obs = obs, y = y)
  reg_fit <- sampling(mod, data=stan.data, iter=50000*nObs^(-0.5), chains=2, save_warmup = FALSE)
  
  summary <- summary(reg_fit, pars = c("theta","sigma","xi","thetar"), probs = c(0.025,0.5,0.975))$summary
  
  if (nRoutes >= 1){thetar$theta1<-summary[4]}
  if (nRoutes >= 2){thetar$theta2<-summary[5]}
  if (nRoutes >= 3){thetar$theta3<-summary[6]}
  if (nRoutes >= 4){thetar$theta4<-summary[7]}
  
  efecto$cliente[pos_r:(pos_r+nRoutes-1)]<-cliente2$client[1]
  efecto$ruta[pos_r:(pos_r+nRoutes-1)]<-c(1:nRoutes)
  efecto$theta[pos_r:(pos_r+nRoutes-1)]<-cliente2[nperiod,32:(31+nRoutes)] #en la columna 32 empiezan los theta_1,2,3,4
  efecto$sigma[pos_r:(pos_r+nRoutes-1)]<-cliente2[nperiod,36:(35+nRoutes)] # en la columna 36 empiezan los sigma_1,2,3,4
  efecto$xi[pos_r:(pos_r+nRoutes-1)]<-cliente2[nperiod,40:(39+nRoutes)] #en la columna 40 empiezan los xi_1,2,3,4
  efecto$theta_mod[pos_r:(pos_r+nRoutes-1)]<-thetar[1,1:nRoutes] 
  efecto$sigma_mod[pos_r:(pos_r+nRoutes-1)]<-summary[2]
  efecto$xi_mod[pos_r:(pos_r+nRoutes-1)]<-summary[3]
  
  cliente2$theta_dist_1<-cliente2$theta_1/cliente2$distance_1
  beta_x_1<-exp(beta_0[r]+sum(cliente2[nperiod,c(12,36,40,44,48,52,56,60,64)]*beta[x:(x+8)]))
  x_mod_1<-c(cliente2$weight_kg_1[nperiod],summary[2],summary[3],cliente2$tau_1[nperiod],
             cliente2$sigma_1[nperiod],cliente2$xi_1[nperiod],cliente2$demand_1[nperiod],
             cliente2$first_half_week_1[nperiod],thetar[1,1]/cliente2$distance_1[nperiod])
  beta_x_mod_1<-exp(beta_0[r]+sum(x_mod_1*beta[x:(x+8)]))
  efecto$prob[pos_r]<-beta_x_1/(1+beta_x_1)
  efecto$prob_mod[pos_r]<-beta_x_mod_1/(1+beta_x_mod_1)
  efecto$sd[pos_r]<-sd_1
  
  if (nRoutes >= 2){
    cliente2$theta_dist_2<-cliente2$theta_2/cliente2$distance_2
    beta_x_2<-exp(beta_0[r+1]+sum(cliente2[nperiod,c(13,37,41,45,49,53,57,61,65)]*beta[x:(x+8)]))
    x_mod_2<-c(cliente2$weight_kg_2[nperiod],summary[2],summary[3],cliente2$tau_2[nperiod],
               cliente2$sigma_2[nperiod],cliente2$xi_2[nperiod],cliente2$demand_2[nperiod],
               cliente2$first_half_week_2[nperiod],thetar[1,2]/cliente2$distance_2[nperiod])
    beta_x_mod_2<-exp(beta_0[r+1]+sum(x_mod_2*beta[x:(x+8)]))
    efecto$prob[pos_r+1]<-beta_x_2/(1+beta_x_2)
    efecto$prob_mod[pos_r+1]<-beta_x_mod_2/(1+beta_x_mod_2)
    #efecto$sd[pos_r+1]<-sd_2
  }
  
  if (nRoutes >= 3){
    cliente2$theta_dist_3<-cliente2$theta_3/cliente2$distance_3
    beta_x_3<-exp(beta_0[r+2]+sum(cliente2[nperiod,c(14,38,42,46,50,54,58,62,66)]*beta[x:(x+8)]))
    x_mod_3<-c(cliente2$weight_kg_3[nperiod],summary[2],summary[3],cliente2$tau_3[nperiod],
               cliente2$sigma_3[nperiod],cliente2$xi_3[nperiod],cliente2$demand_3[nperiod],
               cliente2$first_half_week_3[nperiod],thetar[1,3]/cliente2$distance_3[nperiod])
    beta_x_mod_3<-exp(beta_0[r+2]+sum(x_mod_3*beta[x:(x+8)]))
    efecto$prob[pos_r+2]<-beta_x_3/(1+beta_x_3)
    efecto$prob_mod[pos_r+2]<-beta_x_mod_3/(1+beta_x_mod_3)
    #efecto$sd[pos_r+2]<-sd_3
  }
  
  if (nRoutes >= 4){
    cliente2$theta_dist_4<-cliente2$theta_4/cliente2$distance_4
    beta_x_4<-exp(beta_0[r+3]+sum(cliente2[nperiod,c(15,39,43,47,51,55,59,63,67)]*beta[x:(x+8)]))
    x_mod_4<-c(cliente2$weight_kg_4[nperiod],summary[2],summary[3],cliente2$tau_4[nperiod],
               cliente2$sigma_4[nperiod],cliente2$xi_4[nperiod],cliente2$demand_4[nperiod],
               cliente2$first_half_week_4[nperiod],thetar[1,4]/cliente2$distance_4[nperiod])
    beta_x_mod_4<-exp(beta_0[r+3]+sum(x_mod_4*beta[x:(x+8)]))
    efecto$prob[pos_r+3]<-beta_x_3/(1+beta_x_3)
    efecto$prob_mod[pos_r+3]<-beta_x_mod_4/(1+beta_x_mod_4)
    #efecto$sd[pos_r+3]<-sd_4
  }
  
  r<-r+4 #4 es el numero de rutas
  x<-x+9 #9 es el numero de variables
  pos_r<-pos_r+nRoutes
}

#pasar las columnas de theta, theta_mod, sigma y xi de clase lista a numero
theta<-rep(0,N)
theta_mod<-rep(0,N)
sigma<-rep(0,N)
xi<-rep(0,N)
for(i in 1:N){
  theta[i]<-efecto$theta[[i]]
  theta_mod[i]<-efecto$theta_mod[[i]]
  sigma[i]<-efecto$sigma[[i]]
  xi[i]<-efecto$xi[[i]]
}
efecto$theta<-theta
efecto$theta_mod<-theta_mod
efecto$sigma<-sigma
efecto$xi<-xi

save.image("jerarquico_theta_desv_theta_ruta1.RData")
library(writexl)
write_xlsx(efecto, "H:/Dropbox/yan shang/delays angelica/jerarquico_theta_desv_theta_ruta1.xlsx")

