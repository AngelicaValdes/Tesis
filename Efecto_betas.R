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
    library(readxl)
    library(dplyr)
    library("rstan")
    rstan_options(auto_write = TRUE)
    options(mc.cores = parallel::detectCores())
    
    #guardar los datos de beta, beta_0
    load("/modelo_6_5.RData") #jerarquico
    load("/modelo_12_2.RData") #independiente
    load("/modelo_18.RData") #naive
    beta_0 <- summary(reg_fit2, "beta_0", probs = NULL)$summary
    beta1 <- summary(reg_fit2, "beta1", probs = NULL)$summary
    beta2 <- summary(reg_fit2, "beta2", probs = NULL)$summary
    
    #se cargan los datos necesarios
    #Para datos de aprendizaje jerarquico
    load('/mydata_server_3.RData')
    BD2 <- read_excel("Base_modelo_15.xlsx" , guess_max = 50000)
    #Para datos de aprendizaje independiente
    #load('/mydata_server_5.RData')
    #BD2 <- read_excel("Base_modelo_14.xlsx" , guess_max = 50000)
    #Para datos de método Naïve
    #load('/mydata_server_9.RData')
    #BD2 <- read_excel("Base_modelo_13.xlsx" , guess_max = 50000)
    
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
    clientes<-clientes[order(clientes$child_id),]
    
    #Pronosticar nuevo theta luego de sumar la desviacion estandar por cliente y por ruta
    
    #modelo de pronostico a utilizar
    mod<-stan_model("delay2.stan") #para aprendizaje jerarquico
    mod<-stan_model("delay3.stan") #para aprendizaje independiente
    
    #inicializar variables para guardar theta,sigma y xi
    #N es la suma de las rutas para todos los clientes y ya estaba calculado en modelo_6_5.RData
    
    efecto<-data.frame("cliente"=rep(0,N),"ruta"=rep(0,N),"sd"=rep(0,N),"theta"=rep(0,N),"theta_mod"=rep(0,N),"sigma"=rep(0,N),"sigma_mod"=rep(0,N),"xi"=rep(0,N),"xi_mod"=rep(0,N),"prob"=rep(0,N), "prob_mod"=rep(0,N), "diferencia"=rep(0,N))
    
    r<-1
    x<-1
    w<-1
    pos_r<-1
    for (j in clientes$child_id[1:677]){
      cliente<-subset(BD,child_id==j)
      cliente2<-subset(BD2,client==j)
      #desviación estandar por ruta
      
      sd_1<-sd(cliente$difference[which(cliente$route_id==1)])
      sd_2<-sd(cliente$difference[which(cliente$route_id==2)])
      sd_3<-sd(cliente$difference[which(cliente$route_id==3)])
      sd_4<-sd(cliente$difference[which(cliente$route_id==4)])
      cliente$diff1<-cliente$diff1+sd_1
      cliente$diff2<-cliente$diff2+sd_2
      cliente$diff3<-cliente$diff3+sd_3
      cliente$diff4<-cliente$diff4+sd_4
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
      
      #para datos de modelo jerarquico
      summary <- summary(reg_fit, pars = c("theta","sigma","xi","thetar"), probs = c(0.025,0.5,0.975))$summary
      
      if (nRoutes >= 1){thetar$theta1<-summary[4]}
      if (nRoutes >= 2){thetar$theta2<-summary[5]}
      if (nRoutes >= 3){thetar$theta3<-summary[6]}
      if (nRoutes >= 4){thetar$theta4<-summary[7]}
      
      #para datos de modelo independiente
      #summary <- summary(reg_fit, pars = c("sigma","thetar"), probs = c(0.025,0.5,0.975))$summary
  
      #if (nRoutes >= 1){thetar$theta1<-summary[2]}
      #if (nRoutes >= 2){thetar$theta2<-summary[3]}
      #if (nRoutes >= 3){thetar$theta3<-summary[4]}
      #if (nRoutes >= 4){thetar$theta4<-summary[5]}
      
      #para datos de metodo naive
      #if (nRoutes >= 1){thetar$theta1<-cliente$diff1[max(which(cliente$obs1==1))]}
      #if (nRoutes >= 2){thetar$theta2<-cliente$diff2[max(which(cliente$obs2==1))]}
      #if (nRoutes >= 3){thetar$theta3<-cliente$diff3[max(which(cliente$obs3==1))]}
      #if (nRoutes >= 4){thetar$theta4<-cliente$diff4[max(which(cliente$obs4==1))]}
      
      efecto$cliente[pos_r:(pos_r+nRoutes-1)]<-cliente2$client[1]
      efecto$ruta[pos_r:(pos_r+nRoutes-1)]<-c(1:nRoutes)
      efecto$theta[pos_r:(pos_r+nRoutes-1)]<-cliente2[nperiod,32:(31+nRoutes)] #en la columna 30 empiezan los theta_1,2,3,4 del modelo independiente y naive
      efecto$sigma[pos_r:(pos_r+nRoutes-1)]<-cliente2[nperiod,36:(35+nRoutes)] # en la columna 34 empiezan los sigma_1,2,3,4 del modelo independiente y para el metodo naive no aplica
      efecto$xi[pos_r:(pos_r+nRoutes-1)]<-cliente2[nperiod,40:(39+nRoutes)] #esto no aplica para modelo independiente
      efecto$theta_mod[pos_r:(pos_r+nRoutes-1)]<-thetar[1,1:nRoutes] 
      efecto$sigma_mod[pos_r:(pos_r+nRoutes-1)]<-summary[2] #esto no aplica para método naive
      efecto$xi_mod[pos_r:(pos_r+nRoutes-1)]<-summary[3] #esto no aplica para modelo independiente
      
      cliente2$theta_dist_1<-cliente2$theta_1/cliente2$distance_1
      #Modelo de aprendizaje jerarquico
      beta_x_1<-exp(beta_0[r]+sum(cliente2[nperiod,c(12,36,40,48,52,56,60)]*beta1[x:(x+6)])+
                  sum(cliente2[nperiod,c(44,64)]*beta2[w:(w+1)]*-1))
      #Modelo de aprendizaje independiente
      #beta_x_1<-exp(beta_0[r]+sum(cliente2[nperiod,c(10,34,42,46,50)]*beta1[x:(x+4)])+
                  sum(cliente2[nperiod,c(38,54)]*beta2[w:(w+1)]*-1))
      #Modelo de metodo naive
      #beta_x_1<-exp(beta_0[r]+sum(cliente2[nperiod,c(10,38,42)]*beta1[x:(x+2)])+
                  sum(cliente2[nperiod,c(34,46)]*beta2[w:(w+1)]*-1))
      #para datos de aprendizaje independiente se sacan los xi y para metodo naive se sacan tambien los sigma
      x_mod_1<-c(cliente2$weight_kg_1[nperiod],summary[2],summary[3],
                 cliente2$sigma_1[nperiod],cliente2$xi_1[nperiod],cliente2$demand_1[nperiod],
                 cliente2$first_half_week_1[nperiod],cliente2$tau_1[nperiod],thetar[1,1]/cliente2$distance_1[nperiod])
      #Para datos de aprendizaje jerarquico
      beta_x_mod_1<-exp(beta_0[r]+sum(x_mod_1[1:7]*beta1[x:(x+6)])+sum(x_mod_1[8:9]*beta2[w:(w+1)]*-1))
      #Para datos de aprendizaje independiente
      #beta_x_mod_1<-exp(beta_0[r]+sum(x_mod_1[1:5]*beta1[x:(x+6)])+sum(x_mod_1[6:7]*beta2[w:(w+1)]*-1))
      #Para datos de metodo naive
      #beta_x_mod_1<-exp(beta_0[r]+sum(x_mod_1[1:3]*beta1[x:(x+6)])+sum(x_mod_1[4:5]*beta2[w:(w+1)]*-1))
      efecto$prob[pos_r]<-beta_x_1/(1+beta_x_1)
      efecto$prob_mod[pos_r]<-beta_x_mod_1/(1+beta_x_mod_1)
      efecto$sd[pos_r]<-sd_1
      
      if (nRoutes >= 2){
        cliente2$theta_dist_2<-cliente2$theta_2/cliente2$distance_2
        #Modelo de aprendizaje jerarquico
        beta_x_2<-exp(beta_0[r+1]+sum(cliente2[nperiod,c(13,37,41,49,53,57,61)]*beta1[x:(x+6)])+
                      sum(cliente2[nperiod,c(45,65)]*beta2[w:(w+1)]*-1))
        #Modelo de aprendizaje independiente
        #beta_x_2<-exp(beta_0[r+1]+sum(cliente2[nperiod,c(11,35,43,47,51)]*beta1[x:(x+4)])+
                      sum(cliente2[nperiod,c(39,55)]*beta2[w:(w+1)]*-1))
        #Modelo de metodo naive
        #beta_x_2<-exp(beta_0[r+1]+sum(cliente2[nperiod,c(11,39,43)]*beta1[x:(x+2)])+
                      sum(cliente2[nperiod,c(35,47)]*beta2[w:(w+1)]*-1))
        #para datos de aprendizaje independiente se sacan los xi y para metodo naive se sacan tambien los sigma
        x_mod_2<-c(cliente2$weight_kg_1[nperiod],summary[2],summary[3],
                     cliente2$sigma_2[nperiod],cliente2$xi_2[nperiod],cliente2$demand_1[nperiod],
                     cliente2$first_half_week_1[nperiod],cliente2$tau_2[nperiod],thetar[1,2]/cliente2$distance_2[nperiod])
        #Para datos de aprendizaje jerarquico
        beta_x_mod_2<-exp(beta_0[r+1]+sum(x_mod_2[1:7]*beta1[x:(x+6)])+sum(x_mod_2[8:9]*beta2[w:(w+1)]*-1))
        #Para datos de aprendizaje independiente
        #beta_x_mod_2<-exp(beta_0[r+1]+sum(x_mod_2[1:5]*beta1[x:(x+6)])+sum(x_mod_2[6:7]*beta2[w:(w+1)]*-1))
        #Para datos de metodo naive
        #beta_x_mod_2<-exp(beta_0[r+1]+sum(x_mod_2[1:3]*beta1[x:(x+6)])+sum(x_mod_2[4:5]*beta2[w:(w+1)]*-1))
        efecto$prob[pos_r+1]<-beta_x_2/(1+beta_x_2)
        efecto$prob_mod[pos_r+1]<-beta_x_mod_2/(1+beta_x_mod_2)
        #efecto$sd[pos_r+1]<-sd_2
      }
      
      if (nRoutes >= 3){
        cliente2$theta_dist_3<-cliente2$theta_3/cliente2$distance_3
        #Modelo de aprendizaje jerarquico
        beta_x_3<-exp(beta_0[r+2]+sum(cliente2[nperiod,c(14,38,42,50,54,58,62)]*beta1[x:(x+6)])+
                      sum(cliente2[nperiod,c(46,66)]*beta2[w:(w+1)]*-1))
        #Modelo de aprendizaje independiente
        #beta_x_3<-exp(beta_0[r+2]+sum(cliente2[nperiod,c(12,36,44,48,52)]*beta1[x:(x+4)])+
                      sum(cliente2[nperiod,c(40,56)]*beta2[w:(w+1)]*-1))
        #Modelo de metodo naive
        #beta_x_3<-exp(beta_0[r+2]+sum(cliente2[nperiod,c(12,40,44)]*beta1[x:(x+2)])+
                      sum(cliente2[nperiod,c(36,48)]*beta2[w:(w+1)]*-1))
        #para datos de aprendizaje independiente se sacan los xi y para metodo naive se sacan tambien los sigma
        x_mod_3<-c(cliente2$weight_kg_1[nperiod],summary[2],summary[3],
                     cliente2$sigma_3[nperiod],cliente2$xi_3[nperiod],cliente2$demand_1[nperiod],
                     cliente2$first_half_week_1[nperiod],cliente2$tau_3[nperiod],thetar[1,3]/cliente2$distance_3[nperiod])
        #Para datos de aprendizaje jerarquico
        beta_x_mod_3<-exp(beta_0[r+2]+sum(x_mod_3[1:7]*beta1[x:(x+6)])+sum(x_mod_3[8:9]*beta2[w:(w+1)]*-1))
        #Para datos de aprendizaje independiente
        #beta_x_mod_3<-exp(beta_0[r+2]+sum(x_mod_3[1:5]*beta1[x:(x+6)])+sum(x_mod_3[6:7]*beta2[w:(w+1)]*-1))
        #Para datos de metodo naive
        #beta_x_mod_3<-exp(beta_0[r+2]+sum(x_mod_3[1:3]*beta1[x:(x+6)])+sum(x_mod_3[4:5]*beta2[w:(w+1)]*-1))
        efecto$prob[pos_r+2]<-beta_x_3/(1+beta_x_3)
        efecto$prob_mod[pos_r+2]<-beta_x_mod_3/(1+beta_x_mod_3)
        #efecto$sd[pos_r+2]<-sd_3
      }
      
      if (nRoutes >= 4){
        cliente2$theta_dist_4<-cliente2$theta_4/cliente2$distance_4
        #Modelo de aprendizaje jerarquico
        beta_x_4<-exp(beta_0[r+3]+sum(cliente2[nperiod,c(15,39,43,51,55,59,63)]*beta1[x:(x+6)])+
                      sum(cliente2[nperiod,c(47,67)]*beta2[w:(w+1)]*-1))
        #Modelo de aprendizaje independiente
        #beta_x_4<-exp(beta_0[r+3]+sum(cliente2[nperiod,c(13,37,45,49,53)]*beta1[x:(x+4)])+
                      sum(cliente2[nperiod,c(41,57)]*beta2[w:(w+1)]*-1))
        #Modelo de metodo naive
        #beta_x_4<-exp(beta_0[r+3]+sum(cliente2[nperiod,c(13,41,45)]*beta1[x:(x+2)])+
                      sum(cliente2[nperiod,c(37,49)]*beta2[w:(w+1)]*-1))
        #para datos de aprendizaje independiente se sacan los xi y para metodo naive se sacan tambien los sigma
        x_mod_4<-c(cliente2$weight_kg_1[nperiod],summary[2],summary[3],
                     cliente2$sigma_4[nperiod],cliente2$xi_4[nperiod],cliente2$demand_1[nperiod],
                     cliente2$first_half_week_1[nperiod],cliente2$tau_4[nperiod],thetar[1,4]/cliente2$distance_4[nperiod])
        #Para datos de aprendizaje jerarquico
        beta_x_mod_4<-exp(beta_0[r+3]+sum(x_mod_4[1:7]*beta1[x:(x+6)])+sum(x_mod_4[8:9]*beta2[w:(w+1)]*-1))
        #Para datos de aprendizaje independiente
        #beta_x_mod_4<-exp(beta_0[r+3]+sum(x_mod_4[1:5]*beta1[x:(x+6)])+sum(x_mod_4[6:7]*beta2[w:(w+1)]*-1))
        #Para datos de metodo naive
        #beta_x_mod_4<-exp(beta_0[r+3]+sum(x_mod_4[1:3]*beta1[x:(x+6)])+sum(x_mod_4[4:5]*beta2[w:(w+1)]*-1))
        efecto$prob[pos_r+3]<-beta_x_4/(1+beta_x_4)
        efecto$prob_mod[pos_r+3]<-beta_x_mod_4/(1+beta_x_mod_4)
        #efecto$sd[pos_r+3]<-sd_4
      }
      
      r<-r+4 #4 es el numero de rutas
      x<-x+7 #+7 para jerarquico, +5 para independiente y +3 para naive
      w<-w+2
      pos_r<-pos_r+nRoutes
    }
    
    #pasar las columnas de theta, theta_mod, sigma y xi de clase lista a numero, xi no aplica para datos independientes y xi ni sigma para datos de metodo naive
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
    
    library(writexl)
    write_xlsx(efecto, "/jerarquico_theta_desv_theta.xlsx")
