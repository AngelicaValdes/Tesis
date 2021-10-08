dotR <- file.path(Sys.getenv("HOME"), ".R")
if (!file.exists(dotR)) 
  dir.create(dotR)
M <- file.path(dotR, "Makevars")
if (!file.exists(M)) 
  file.create(M)
cat("\nCXXFLAGS=-O3 -Wno-unused-variable -Wno-unused-function", 
    file = M, sep = "\n", append = TRUE)
cat('Sys.setenv(BINPREF = "C:/Rtools/mingw_$(WIN)/bin/")',
    file = file.path(Sys.getenv("HOME"), ".Rprofile"), 
    sep = "\n", append = TRUE)
cat("\nCXXFLAGS += -Wno-ignored-attributes -Wno-deprecated-declarations", 
    file = M, sep = "\n", append = TRUE)

#Setear directorio

library("rstan")
rstan_options(auto_write = TRUE)
options(mc.cores = parallel::detectCores())

load('/mydata_server_13.RData')
mod<-stan_model("time2.stan")
for (j in clientes$child_id[(count_clientes+1):(677)]){
  
  cliente<-subset(BD,child_id==j)
  cliente<-cliente[order(cliente$Periodo_final, cliente$route_id),]
  
  #inicializacion de variables
  h<-length(cliente$Periodo_final)
  sigma<- rep(param[2],h)
  mean_rhat<-rep(0,h)
  max_rhat<-rep(0,h)
  se_mean<-rep(0,h)
  taur<-data.frame(tau1=c(rep(param[4],h)),tau2=c(rep(param[4],h)),tau3=c(rep(param[4],h)),tau4=c(rep(param[4],h)))
  
  #algoritmo para obtener pronostico
  for (i in 1:h){
    nRoutes <- cliente$child_route_num[1]
    nObs <- nrow(cliente[1:i,])
    obs <- as.matrix(cliente[1:i,6:(5+nRoutes)], nrow = nrow(cliente[1:i,]))
    y <- as.matrix(cliente[1:i,10:(9+nRoutes)], nrow = nrow(cliente[1:i,]))
    
    stan.data = list(nRoutes = nRoutes, nObs = nObs, obs = obs, y = y)
    reg_fit <- sampling(mod, data=stan.data, iter=50000*nObs^(-0.5), chains=2, save_warmup = FALSE)
    
    summary <- summary(reg_fit, pars = c("sigma","taur"), probs = c(0.025,0.5,0.975))$summary
    
    sigma[i]<-summary[1]
    mean_rhat[i]<-mean(summary[,8])
    max_rhat[i]<-max(summary[,8])
    se_mean[i]<-summary[1,2]
    if (nRoutes >= 1){taur$tau1[i]<-summary[2]}
    if (nRoutes >= 2){taur$tau2[i]<-summary[3]}
    if (nRoutes >= 3){taur$tau3[i]<-summary[4]}
    if (nRoutes >= 4){taur$tau4[i]<-summary[5]}
    files <- list.files(tempdir(), full.names = T, pattern = "^file")
    file.remove(files)
  }
  #Introducir el valor de "diferencia" en la columna correspondiente
  for(i in 1:h){
    d<-cliente$Periodo_final[i]
    if(cliente$route_id[i]==1){
      Base_pronostico$time_1[d+f]<-cliente$time_hours[i]
    }
    if(cliente$route_id[i]==2){
      Base_pronostico$time_2[d+f]<-cliente$time_hours[i]
    }
    if(cliente$route_id[i]==3){
      Base_pronostico$time_3[d+f]<-cliente$time_hours[i]
    }
    if(cliente$route_id[i]==4){
      Base_pronostico$time_4[d+f]<-cliente$time_hours[i]
    }
  }
  
  #datos para el cliente sin periodos repetidos
  cliente$Periodos_repetidos<-1
  cliente$Time_prom<-rep(NA,h)
  p<-rep(1,h)
  for (i in 1:(h-1)){
    if(cliente$Periodo_final[i]==cliente$Periodo_final[i+1] & cliente$route_id[i]==cliente$route_id[i+1]){
      p[i+1]<-p[i]+1
      cliente$Periodos_repetidos[i+1]<-cliente$Periodos_repetidos[i]+1
      cliente$Time_prom[i+1]<-mean(cliente$time_hoours[(i+2-p[i+1]):(i+1)])
      cliente$Periodo_final[i]<-0
      sigma[i]<-NA
      mean_rhat[i]<-NA
      max_rhat[i]<-NA
      se_mean[i]<-NA
      taur[i,]<-NA
    }
  }
  cliente<-subset(cliente,Periodo_final>0)
  sigma<-sigma[is.na(sigma)==F]
  mean_rhat<-mean_rhat[is.na(mean_rhat)==F]
  max_rhat<-max_rhat[is.na(max_rhat)==F]
  se_mean<-se_mean[is.na(se_mean)==F]
  taur<-subset(taur,is.na(tau1)==FALSE)
  
  #algoritmo para introducir los datos en las variables
  h<-length(cliente$Periodo_final)
  #datos para primer periodo
  Base_pronostico$sigma[1+f]<-param[2]
  Base_pronostico$tau_1[1+f]<-param[4]
  if(cliente$child_route_num[1]>=2){Base_pronostico$tau_2[1+f]<-param[4]}
  if(cliente$child_route_num[1]>=3){Base_pronostico$tau_3[1+f]<-param[4]}
  if(cliente$child_route_num[1]>=4){Base_pronostico$tau_4[1+f]<-param[4]}
  
  #datos para pronostico
  for(i in 1:h){
    d<-cliente$Periodo_final[i]
    if(cliente$child_route_num[1]>=1){
      Base_pronostico$tau_1[d+1+f]<-taur$tau1[i]
    }
    if(cliente$child_route_num[1]>=2){
      Base_pronostico$tau_2[d+1+f]<-taur$tau2[i]
    }
    if(cliente$child_route_num[1]>=3){
      Base_pronostico$tau_3[d+1+f]<-taur$tau3[i]
    }
    if(cliente$child_route_num[1]>=4){
      Base_pronostico$tau_4[d+1+f]<-taur$tau4[i]
    }
    if(is.na(cliente$Time_prom[i])==FALSE){
      if(cliente$route_id[i]==1){
        Base_pronostico$time_1[d+f]<-cliente$Time_prom[i]
      }
      if(cliente$route_id[i]==2){
        Base_pronostico$time_2[d+f]<-cliente$Time_prom[i]
      }
      if(cliente$route_id[i]==3){
        Base_pronostico$time_3[d+f]<-cliente$Time_prom[i]
      }
      if(cliente$route_id[i]==4){
        Base_pronostico$time_4[d+f]<-cliente$Time_prom[i]
      }
    }
    Base_pronostico$sigma[d+1+f]<-sigma[i]
    Base_pronostico$mean_rhat[d+1+f]<-mean_rhat[i]
    Base_pronostico$max_rhat[d+1+f]<-max_rhat[i]
    Base_pronostico$se_mean[d+1+f]<-se_mean[i]
    Base_pronostico$route[d+f]<-cliente$route_id[i]
    Base_pronostico$count_period[d+f]<-cliente$Periodos_repetidos[i]
    Base_pronostico$route_num[d+f]<-cliente$child_route_num[i]
  }
  for(i in 2:(max(BD$Periodo_final)+1)){
    if(is.na(Base_pronostico$sigma[i+f]))Base_pronostico$sigma[i+f]<-Base_pronostico$sigma[i-1+f]
    for (r in 1:mean(BD$child_route_num[BD$child_id==j])){
      if(r==1){
        if(is.na(Base_pronostico$tau_1[i+f]))Base_pronostico$tau_1[i+f]<-Base_pronostico$tau_1[i-1+f]
      }
      if(r==2){
        if(is.na(Base_pronostico$tau_2[i+f]))Base_pronostico$tau_2[i+f]<-Base_pronostico$tau_2[i-1+f]
      }
      if(r==3){
        if(is.na(Base_pronostico$tau_3[i+f]))Base_pronostico$tau_3[i+f]<-Base_pronostico$tau_3[i-1+f]
      }
      if(r==4){
        if(is.na(Base_pronostico$tau_4[i+f]))Base_pronostico$tau_4[i+f]<-Base_pronostico$tau_4[i-1+f]
      }
    }
  } 
  f<-f+max(BD$Periodo_final)+1
  count_clientes<-count_clientes+1
  print("CLIENTE NUMERO:")
  print(count_clientes)
  unlink('/mydata_server_13.RData', recursive = TRUE)
  save.image(file = '/mydata_server_13.RData')
  rm(list = ls())
  gc()
  load('/mydata_server_13.RData')
  files <- list.files("\\AppData\\Local\\Temp\\2", full.names = T, pattern = "Rtmp")
  unlink(files, recursive = TRUE)
}





