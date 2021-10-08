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
        
        #Setear directorio
     
        library(dplyr)
        library("rstan")
        rstan_options(auto_write = TRUE)
        options(mc.cores = parallel::detectCores())
        
        library(readxl)
        BD <- read_excel("Base_modelo_15.xlsx", guess_max = 50000) #Modelo jerarquico
        BD <- read_excel("Base_modelo_14.xlsx", guess_max = 50000) #Modelo independiente
        BD <- read_excel("Base_modelo_13.xlsx", guess_max = 50000) #Modelo naive
        BD<-BD[order(BD$client),]
        BD$tau_1<-BD$tau_1/24
        BD$tau_2<-BD$tau_2/24
        BD$tau_3<-BD$tau_3/24
        BD$tau_4<-BD$tau_4/24
        BD$demand_1<-BD$demand_1/100
        BD$demand_2<-BD$demand_2/100
        BD$demand_3<-BD$demand_3/100
        BD$demand_4<-BD$demand_4/100
        BD$theta_dist_1<-BD$theta_1/BD$distance_1
        BD$theta_dist_2<-BD$theta_2/BD$distance_2
        BD$theta_dist_3<-BD$theta_3/BD$distance_3
        BD$theta_dist_4<-BD$theta_4/BD$distance_4
        BD$route_num<-BD$route_num_1
        
        ############## Modelo Jerarquico ############################
        #se quita la variable order_frequency de la base
        BD<-BD[,-c(20:23)]
        #se quitan las variables distance, route_num y domestic para modelo con beta 0 por ruta
        BD<-BD[,-c(16:27)]
        #se quita la variable theta
        #BD<-BD[,-c(28:31)]
        BD<-BD[,-c(16:19)] #luego de sacar distance, route_num y domestic
        #############################################################
        ############## Modelo independiente y naive #################
        #se quita la variable order_frequency de la base
        #BD<-BD[,-c(18:21)]
        #se quitan las variables distance, route_num y domestic para modelo con beta 0 por ruta
        #BD<-BD[,-c(14:25)]
        #se quita la variable theta
        #BD<-BD[,-c(26:29)]
        #BD<-BD[,-c(14:17)] #luego de quitar distance, route_num y domestic
        ##################################################################
        clients<-distinct(BD,client)
        nclients <- length(clients$client)
        nperiod <- max(BD$period)-BD$period[1]+1
        nroute <- BD$route_num[order(BD$period,BD$client)][1:nclients] #vector con numero de rutas que usa el cliente del 1 al 677
        N<-sum(nroute)
        nObs<-N*nperiod
        nvar <- 9 #varia segun la cantidad de variables que tenga el modelo
        #por ej: el naive solo usa 5 variables para el modelo heterogeneo por ruta
        nRoutes<-4 #no varia
        
        #reordenar la base para que tau y theta_dist esten de las ultimas en para correr el modelo con lognormal
        BD<-BD[,c(1:23,28:43,24:27,44:47)] #Modelo jerarquico
        BD<-BD[,c(1:17,22:33,18:21,34:37)] #Modelo independiente
        BD<-BD[,c(1:13,18:25,14:17,26:29)] #Modelo naive
        
        #cambiar el signo de tau y theta_dist
        BD$tau_1<-BD$tau_1*-1
        BD$tau_2<-BD$tau_2*-1
        BD$tau_3<-BD$tau_3*-1
        BD$tau_4<-BD$tau_4*-1
        BD$theta_dist_1<-BD$theta_dist_1*-1
        BD$theta_dist_2<-BD$theta_dist_2*-1
        BD$theta_dist_3<-BD$theta_dist_3*-1
        BD$theta_dist_4<-BD$theta_dist_4*-1
        
        routedata<-data.frame("route1"=rep(0,nObs),"route2"=rep(0,nObs),"route3"=rep(0,nObs),"route4"=rep(0,nObs))
        
        #las variables de xdata se deben modificar segun sean las necesarias para cada modelo
        #por ej: el modelo heterogeneo por ruta con los datos del metodo naive solo tendra las variables: weight, demand, first_half_week,tau y theta_dist
        xdata<-data.frame("weight"=1:nObs,"sigma_theta"=1:nObs, "xi_theta"=1:nObs,
                          "sigma"=1:nObs, "xi"=1:nObs, "demand"=1:nObs,"first_half_week"=1:nObs,
                          "tau"=1:nObs, "theta_dist"=1:nObs)
        
        c<-0
        d<-0
        for(client in 1:nclients){
          v<-0
            for (var in 1:nvar){
              for (route in 1:nroute[client]){
                f<-80*route
                xdata[(1+f-80+d):(f+d),var]<-BD[(1+c):(80+c),11+route+v]
                routedata[(1+f-80+d):(f+d),route]<-1
              }
              v<-v+4
            }
          d<-d+80*nroute[client]
          c<-c+80
        }
        
        ydata<-data.frame("obs"=1:nObs)
        c<-0
        d<-0
        for(client in 1:nclients){
          for (route in 1:nroute[client]){
            f<-80*route
            ydata[(1+f-80+d):(f+d),1]<-BD[(1+c):(80+c),7+route]
          }
          d<-d+80*nroute[client]
          c<-c+80
        }
        
        xdata<-as.matrix(xdata)
        ydata<-ydata[,1]
        routedata<-as.matrix(routedata)
        
        ##### para correr modelo heterogeneo
        d<-0
        pos_in<-1:nclients
        pos_fin<-1:nclients
        for(client in 1:nclients){
          f<-80*nroute[client]
          pos_in[client]<-(1+f-80*nroute[client]+d)
          pos_fin[client]<-(f+d)
          d<-d+80*nroute[client]
        }
        
        initf1 <- function() {
          list(beta_mean = array(3, dim = (nvar)),beta_var = array(1, dim = (nvar)), beta_0_mean=0,beta_0_var=1)
        }
        ############# Modelo Homogeneo ############################
        
        stan.data = list(nObs = nObs, nvar = nvar, xdata = xdata, ydata=ydata)
        reg_fit <- stan(file= 'model4_2.stan', data=stan.data, iter=1200, chains=1)
        summary <- summary(reg_fit, pars = c("beta","beta_0","loglike"), probs = c(0.025,0.05,0.5,0.95,0.975))$summary
        
        lldraws <- extract(reg_fit,'loglike')
        ll=lldraws$loglike
        minll=min(ll)
        lladj=(ll-minll)
        ladj=exp(-lladj)
        loghm=minll-log(mean(ladj))
        
        save.image("modelo_1.RData")
        
        ############# Modelo Homogeneo hibrido ##########################
        initf1 <- function() {
          list(beta_0_mean=0,beta_0_var=1)
        }
        
        stan.data = list(nObs = nObs, nvar = nvar, nclients = nclients, nRoutes=nRoutes, pos_in = pos_in, pos_fin = pos_fin, xdata = xdata, ydata=ydata, routedata=routedata)
        reg_fit2 <- stan(file= 'model4_5.stan', data=stan.data, init = initf1, iter=6000, chains=1)
        summary2 <- summary(reg_fit2, pars = c("beta","beta_0_mean","beta_0_var","loglike"), probs = c(0.025,0.05,0.5,0.95,0.975))$summary
        
        lldraws <- extract(reg_fit2,'loglike')
        ll=lldraws$loglike
        minll=min(ll)
        lladj=(ll-minll)
        ladj=exp(-lladj)
        loghm=minll-log(mean(ladj))
        
        save.image("modelo_5_2.RData")
        
        ############# Modelo Heterogeneo con beta_0 por ruta ##########################
        
        stan.data = list(nObs = nObs, nvar = nvar, nclients = nclients, nRoutes=nRoutes, pos_in = pos_in, pos_fin = pos_fin, xdata = xdata, ydata=ydata, routedata=routedata)
        reg_fit2 <- stan(file= 'model4_4_3.stan', data=stan.data, init = initf1, iter=6000, chains=1)
        summary2 <- summary(reg_fit2, pars = c("beta_mean","beta_0_mean","beta_var","beta_0_var","loglike"), probs = c(0.025,0.05,0.5,0.95,0.975))$summary
        
        lldraws <- extract(reg_fit2,'loglike')
        ll=lldraws$loglike
        minll=min(ll)
        lladj=(ll-minll)
        ladj=exp(-lladj)
        loghm=minll-log(mean(ladj))
        
        save.image("modelo_6_5.RData")
