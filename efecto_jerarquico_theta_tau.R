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
    load("H:/Dropbox/yan shang/delays angelica/modelo_6_5.RData")
    beta_0 <- summary(reg_fit2, "beta_0", probs = NULL)$summary
    beta1 <- summary(reg_fit2, "beta1", probs = NULL)$summary
    beta2 <- summary(reg_fit2, "beta2", probs = NULL)$summary
    #se cargan los datos necesarios
    
    BD2<- read_excel("Base_modelo_15.xlsx" , guess_max = 50000)
    BD2$tau_1<-BD2$tau_1/24
    BD2$tau_2<-BD2$tau_2/24
    BD2$tau_3<-BD2$tau_3/24
    BD2$tau_4<-BD2$tau_4/24
    BD2$demand_1<-BD2$demand_1/100
    BD2$demand_2<-BD2$demand_2/100
    BD2$demand_3<-BD2$demand_3/100
    BD2$demand_4<-BD2$demand_4/100
    DB1<-read_excel("jerarquico_theta_desv_theta.xlsx" , guess_max = 50000)
    DB2<-read_excel("jerarquico_tau_desv_theta.xlsx" , guess_max = 50000)
    
    #vector con clientes unicos
    clientes<-distinct(BD2,client)
    
    #se ordenan todos por cliente
    
    BD2 <-BD2[order(BD2$client),]
    clientes<-clientes[order(clientes$client),]
    
    #inicializar variables para guardar theta,sigma y xi
    #N es la suma de las rutas para todos los clientes y ya estaba calculado en modelo_12.RData
    
    efecto<-data.frame("cliente"=rep(0,N),"ruta"=rep(0,N),"sd"=rep(0,N),"prob"=rep(0,N), "prob_mod"=rep(0,N), "diferencia"=rep(0,N))
    
    r<-1
    x<-1
    w<-1
    pos_r<-1
    for (j in clientes$client[1:677]){
    
      cliente2<-subset(BD2,client==j) #datos que no cambian
      theta<-subset(DB1,cliente==j) #datos theta modificados
      tau<-subset(DB2,cliente==j) #datos tau modificado
      #ordenarlos por ruta
      theta <-theta[order(theta$ruta),]
      tau <-tau[order(tau$ruta),]
      
      nRoutes <- cliente2$route_num_1[1]
      efecto$cliente[pos_r:(pos_r+nRoutes-1)]<-cliente2$client[1]
      efecto$ruta[pos_r:(pos_r+nRoutes-1)]<-c(1:nRoutes)
      
      
      #calculo efecto
      cliente2$theta_dist_1<-cliente2$theta_1/cliente2$distance_1
      beta_x_1<-exp(beta_0[r]+sum(cliente2[nperiod,c(12,36,40,48,52,56,60)]*beta1[x:(x+6)])+
                      sum(cliente2[nperiod,c(44,64)]*beta2[w:(w+1)]*-1))
      x_mod_1<-c(cliente2$weight_kg_1[nperiod],theta$sigma_mod[1],theta$xi_mod[1],
                 tau$sigma_mod[1],tau$xi_mod[1],cliente2$demand_1[nperiod],
                 cliente2$first_half_week_1[nperiod],tau$tau_mod[1],theta$theta_mod[1]/cliente2$distance_1[nperiod])
      beta_x_mod_1<-exp(beta_0[r]+sum(x_mod_1[1:7]*beta1[x:(x+6)])+sum(x_mod_1[8:9]*beta2[w:(w+1)]*-1))
      efecto$prob[pos_r]<-beta_x_1/(1+beta_x_1)
      efecto$prob_mod[pos_r]<-beta_x_mod_1/(1+beta_x_mod_1)
      efecto$sd[pos_r]<-theta$sd[1]
      
      if (nRoutes >= 2){
        cliente2$theta_dist_2<-cliente2$theta_2/cliente2$distance_2
        beta_x_2<-exp(beta_0[r+1]+sum(cliente2[nperiod,c(13,37,41,49,53,57,61)]*beta1[x:(x+6)])+
                        sum(cliente2[nperiod,c(45,65)]*beta2[w:(w+1)]*-1))
        x_mod_2<-c(cliente2$weight_kg_2[nperiod],theta$sigma_mod[2],theta$xi_mod[2],
                   tau$sigma_mod[2],tau$xi_mod[2],cliente2$demand_2[nperiod],
                   cliente2$first_half_week_2[nperiod],tau$tau_mod[2],theta$theta_mod[2]/cliente2$distance_2[nperiod])
        beta_x_mod_2<-exp(beta_0[r+1]+sum(x_mod_2[1:7]*beta1[x:(x+6)])+sum(x_mod_2[8:9]*beta2[w:(w+1)]*-1))
        efecto$prob[pos_r+1]<-beta_x_2/(1+beta_x_2)
        efecto$prob_mod[pos_r+1]<-beta_x_mod_2/(1+beta_x_mod_2)
        efecto$sd[pos_r+1]<-theta$sd[2]
      }
      
      if (nRoutes >= 3){
        cliente2$theta_dist_3<-cliente2$theta_3/cliente2$distance_3
        beta_x_3<-exp(beta_0[r+2]+sum(cliente2[nperiod,c(14,38,42,50,54,58,62)]*beta1[x:(x+6)])+
                        sum(cliente2[nperiod,c(46,66)]*beta2[w:(w+1)]*-1))
        x_mod_3<-c(cliente2$weight_kg_3[nperiod],theta$sigma_mod[3],theta$xi_mod[3],
                   tau$sigma_mod[3],tau$xi_mod[3],cliente2$demand_3[nperiod],
                   cliente2$first_half_week_3[nperiod],tau$tau_mod[3],theta$theta_mod[3]/cliente2$distance_3[nperiod])
        beta_x_mod_3<-exp(beta_0[r+2]+sum(x_mod_3[1:7]*beta1[x:(x+6)])+sum(x_mod_3[8:9]*beta2[w:(w+1)]*-1))
        efecto$prob[pos_r+2]<-beta_x_3/(1+beta_x_3)
        efecto$prob_mod[pos_r+2]<-beta_x_mod_3/(1+beta_x_mod_3)
        efecto$sd[pos_r+2]<-theta$sd[3]
      }
      
      if (nRoutes >= 4){
        cliente2$theta_dist_4<-cliente2$theta_4/cliente2$distance_4
        beta_x_4<-exp(beta_0[r+3]+sum(cliente2[nperiod,c(15,39,43,51,55,59,63)]*beta1[x:(x+6)])+
                        sum(cliente2[nperiod,c(47,67)]*beta2[w:(w+1)]*-1))
        x_mod_4<-c(cliente2$weight_kg_4[nperiod],theta$sigma_mod[4],theta$xi_mod[4],
                   tau$sigma_mod[4],tau$xi_mod[4],cliente2$demand_4[nperiod],
                   cliente2$first_half_week_4[nperiod],tau$tau_mod[4],theta$theta_mod[4]/cliente2$distance_4[nperiod])
        beta_x_mod_4<-exp(beta_0[r+3]+sum(x_mod_4[1:7]*beta1[x:(x+6)])+sum(x_mod_4[8:9]*beta2[w:(w+1)]*-1))
        efecto$prob[pos_r+3]<-beta_x_4/(1+beta_x_4)
        efecto$prob_mod[pos_r+3]<-beta_x_mod_4/(1+beta_x_mod_4)
        efecto$sd[pos_r+3]<-theta$sd[4]
      }
      
      r<-r+4 #4 es el numero de rutas
      x<-x+7 #11 es el numero de variables
      w<-w+2
      pos_r<-pos_r+nRoutes
    }
    
    library(writexl)
    write_xlsx(efecto, "H:/Dropbox/yan shang/delays angelica/jerarquico_theta_tau_desv_theta.xlsx")
