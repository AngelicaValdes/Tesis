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
load('H:/Dropbox/yan shang/delays angelica/mydata_server_3.RData')
BD3<-BD
load('H:/Dropbox/yan shang/delays angelica/mydata_server_14.RData')
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
BD3 <-BD3[order(BD3$child_id),]
clientes<-clientes[order(clientes$child_id),]

#Pronosticar nuevo theta luego de sumar la desviacion estandar por cliente y por ruta

#modelo de pronostico a utilizar
mod<-stan_model("time.stan")

#inicializar variables para guardar theta,sigma y xi
#N es la suma de las rutas para todos los clientes y ya estaba calculado en d_p_s_s_12.RData

efecto<-data.frame("cliente"=rep(0,N),"ruta"=rep(0,N),"sd"=rep(0,N),"tau"=rep(0,N),"tau_mod"=rep(0,N),"sigma"=rep(0,N),"sigma_mod"=rep(0,N),"xi"=rep(0,N),"xi_mod"=rep(0,N),"prob"=rep(0,N), "prob_mod"=rep(0,N), "diferencia"=rep(0,N))

r<-1
x<-1
pos_r<-1
for (j in clientes$child_id[1:677]){
  cliente<-subset(BD,child_id==j)
  cliente2<-subset(BD2,client==j)
  cliente3<-subset(BD3,child_id==j)
  #desviación estandar por ruta
  sd_1<-sd(cliente3$difference[which(cliente3$route_id==1)])
  #sd_1<-sd(cliente$time_hours[which(cliente$route_id==1)])
  #sd_2<-sd(cliente3$difference[which(cliente3$route_id==2)])
  #sd_3<-sd(cliente3$difference[which(cliente3$route_id==3)])
  #sd_4<-sd(cliente3$difference[which(cliente3$route_id==4)])
  cliente$time1<-cliente$time1+sd_1
  #cliente$time2<-cliente$time2+sd_2
  #cliente$time3<-cliente$time3+sd_3
  #cliente$time4<-cliente$time4+sd_4
  cliente<-cliente[order(cliente$Periodo_final, cliente$route_id),]
  
  #inicializacion de variables
  h<-length(cliente$Periodo_final)
  #h<-max(which(cliente$Periodo_final<=100))
  taur<-data.frame(tau1=param[4],tau2=param[4],tau3=param[4],tau4=param[4])
  
  #algoritmo para obtener pronostico
  nRoutes <- cliente$child_route_num[1]
  nObs <- nrow(cliente[1:h,])
  obs <- as.matrix(cliente[1:h,6:(5+nRoutes)], nrow = nrow(cliente[1:h,]))
  y <- as.matrix(cliente[1:h,10:(9+nRoutes)], nrow = nrow(cliente[1:h,]))
  
  stan.data = list(nRoutes = nRoutes, nObs = nObs, obs = obs, y = y)
  reg_fit <- sampling(mod, data=stan.data, iter=50000*nObs^(-0.5), chains=2, save_warmup = FALSE)
  
  summary <- summary(reg_fit, pars = c("tau","sigma","xi","taur"), probs = c(0.025,0.5,0.975))$summary
  
  if (nRoutes >= 1){taur$tau1<-summary[4]}
  if (nRoutes >= 2){taur$tau2<-summary[5]}
  if (nRoutes >= 3){taur$tau3<-summary[6]}
  if (nRoutes >= 4){taur$tau4<-summary[7]}
  
  #nperiod<-which(cliente2$period==100)
  efecto$cliente[pos_r:(pos_r+nRoutes-1)]<-cliente2$client[1]
  efecto$ruta[pos_r:(pos_r+nRoutes-1)]<-c(1:nRoutes)
  efecto$tau[pos_r:(pos_r+nRoutes-1)]<-cliente2[nperiod,44:(43+nRoutes)] #en la columna 44 empiezan los tau_1,2,3,4
  efecto$sigma[pos_r:(pos_r+nRoutes-1)]<-cliente2[nperiod,48:(47+nRoutes)] # en la columna 48 empiezan los sigma_1,2,3,4
  efecto$xi[pos_r:(pos_r+nRoutes-1)]<-cliente2[nperiod,52:(51+nRoutes)] #en la columna 52 empiezan los xi_1,2,3,4
  efecto$tau_mod[pos_r:(pos_r+nRoutes-1)]<-taur[1,1:nRoutes] 
  efecto$sigma_mod[pos_r:(pos_r+nRoutes-1)]<-summary[2]
  efecto$xi_mod[pos_r:(pos_r+nRoutes-1)]<-summary[3]
  
  cliente2$theta_dist_1<-cliente2$theta_1/cliente2$distance_1
  beta_x_1<-exp(beta_0[r]+sum(cliente2[nperiod,c(12,36,40,44,48,52,56,60,64)]*beta[x:(x+8)]))
  x_mod_1<-c(cliente2$weight_kg_1[nperiod],cliente2$sigma_theta_1[nperiod],cliente2$xi_theta_1[nperiod],taur[1,1]/24,
             summary[2],summary[3],cliente2$demand_1[nperiod],
             cliente2$first_half_week_1[nperiod],cliente2$theta_dist_1[nperiod])
  beta_x_mod_1<-exp(beta_0[r]+sum(x_mod_1*beta[x:(x+8)]))
  efecto$prob[pos_r]<-beta_x_1/(1+beta_x_1)
  efecto$prob_mod[pos_r]<-beta_x_mod_1/(1+beta_x_mod_1)
  efecto$sd[pos_r]<-sd_1
  
  if (nRoutes >= 2){
    cliente2$theta_dist_2<-cliente2$theta_2/cliente2$distance_2
    beta_x_2<-exp(beta_0[r+1]+sum(cliente2[nperiod,c(13,37,41,45,49,53,57,61,65)]*beta[x:(x+8)]))
    x_mod_2<-c(cliente2$weight_kg_2[nperiod],cliente2$sigma_theta_2[nperiod],cliente2$xi_theta_2[nperiod],taur[1,2]/24,
               summary[2],summary[3],cliente2$demand_2[nperiod],
               cliente2$first_half_week_2[nperiod],cliente2$theta_dist_2[nperiod])
    beta_x_mod_2<-exp(beta_0[r+1]+sum(x_mod_2*beta[x:(x+8)]))
    efecto$prob[pos_r+1]<-beta_x_2/(1+beta_x_2)
    efecto$prob_mod[pos_r+1]<-beta_x_mod_2/(1+beta_x_mod_2)
    #efecto$sd[pos_r+1]<-sd_2
  }
  
  if (nRoutes >= 3){
    cliente2$theta_dist_3<-cliente2$theta_3/cliente2$distance_3
    beta_x_3<-exp(beta_0[r+2]+sum(cliente2[nperiod,c(14,38,42,46,50,54,58,62,66)]*beta[x:(x+8)]))
    x_mod_3<-c(cliente2$weight_kg_3[nperiod],cliente2$sigma_theta_3[nperiod],cliente2$xi_theta_3[nperiod],taur[1,3]/24,
               summary[2],summary[3],cliente2$demand_3[nperiod],
               cliente2$first_half_week_3[nperiod],cliente2$theta_dist_3[nperiod])
    beta_x_mod_3<-exp(beta_0[r+2]+sum(x_mod_3*beta[x:(x+8)]))
    efecto$prob[pos_r+2]<-beta_x_3/(1+beta_x_3)
    efecto$prob_mod[pos_r+2]<-beta_x_mod_3/(1+beta_x_mod_3)
    #efecto$sd[pos_r+2]<-sd_3
  }
  
  if (nRoutes >= 4){
    cliente2$theta_dist_4<-cliente2$theta_4/cliente2$distance_4
    beta_x_4<-exp(beta_0[r+3]+sum(cliente2[nperiod,c(15,39,43,47,51,55,59,63,67)]*beta[x:(x+8)]))
    x_mod_4<-c(cliente2$weight_kg_4[nperiod],cliente2$sigma_theta_4[nperiod],cliente2$xi_theta_4[nperiod],taur[1,4]/24,
               summary[2],summary[3],cliente2$demand_4[nperiod],
               cliente2$first_half_week_4[nperiod],cliente2$theta_dist_4[nperiod])
    beta_x_mod_4<-exp(beta_0[r+3]+sum(x_mod_4*beta[x:(x+8)]))
    efecto$prob[pos_r+3]<-beta_x_3/(1+beta_x_3)
    efecto$prob_mod[pos_r+3]<-beta_x_mod_4/(1+beta_x_mod_4)
    #efecto$sd[pos_r+3]<-sd_4
  }
  
  r<-r+4 #4 es el numero de rutas
  x<-x+9 #9 es el numero de variables
  pos_r<-pos_r+nRoutes
}

#pasar las columnas de tau, tau_mod, sigma y xi de clase lista a numero
tau<-rep(0,N)
tau_mod<-rep(0,N)
sigma<-rep(0,N)
xi<-rep(0,N)
for(i in 1:N){
  tau[i]<-efecto$tau[[i]]
  tau_mod[i]<-efecto$tau_mod[[i]]
  sigma[i]<-efecto$sigma[[i]]
  xi[i]<-efecto$xi[[i]]
}
efecto$tau<-tau
efecto$tau_mod<-tau_mod/24
efecto$sigma<-sigma
efecto$xi<-xi

save.image("jerarquico_tau_desv_theta_ruta1.RData")
library(writexl)
write_xlsx(efecto, "H:/Dropbox/yan shang/delays angelica/jerarquico_tau_desv_theta_ruta1.xlsx")
