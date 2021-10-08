        library(readxl)
        load("mydata_server_3.RData")
        BD2 <- read_excel("Datos final.xlsx", sheet = "Datos")
        
        library(lubridate)
        BD2<-BD2[order(BD2$start_time),]
        p=1
        for (i in 1:length(BD2$start_time)){
          BD2$Periodo_inicial[i]<-p
          if(yday(BD2$start_time[i+1])-yday(BD2$start_time[i])==1 && wday(BD2$start_time[i])==1) p=p+1
          if(yday(BD2$start_time[i+1])-yday(BD2$start_time[i])==1 && wday(BD2$start_time[i])==4) p=p+1
          if(BD2$WEIGHT_KG[i]==0) BD2$WEIGHT_KG[i]<-mean(BD2$WEIGHT_KG[which(BD2$child_id==BD2$child_id[i] & BD2$WEIGHT_KG!=0)])
        }
        
        ##### inicializacion de variables ####
        
        Base_pronostico$obs_1<-0
        Base_pronostico$obs_2<-0
        Base_pronostico$obs_3<-0
        Base_pronostico$obs_4<-0
        Base_pronostico$weight_kg_1<-NA
        Base_pronostico$weight_kg_2<-NA
        Base_pronostico$weight_kg_3<-NA
        Base_pronostico$weight_kg_4<-NA
        Base_pronostico$distance_1<-NA
        Base_pronostico$distance_2<-NA
        Base_pronostico$distance_3<-NA
        Base_pronostico$distance_4<-NA
        Base_pronostico$order_frequency_1<-NA
        Base_pronostico$order_frequency_2<-NA
        Base_pronostico$order_frequency_3<-NA
        Base_pronostico$order_frequency_4<-NA
        Base_pronostico$route_num_1<-NA
        Base_pronostico$route_num_2<-NA
        Base_pronostico$route_num_3<-NA
        Base_pronostico$route_num_4<-NA
        Base_pronostico$domestic_1<-NA
        Base_pronostico$domestic_2<-NA
        Base_pronostico$domestic_3<-NA
        Base_pronostico$domestic_4<-NA
        
        ##### crear variable obs y demas ####
        f<-0
        for (j in clientes$child_id){
          
          cliente<-subset(BD2,child_id==j)
          cliente<-cliente[order(cliente$Periodo_inicial, cliente$route_id),]
          
          #Introducir un 1 si hay un envio para el periodo
          h<-length(cliente$Periodo_inicial)
          
          for(i in 2:h){
            d<-cliente$Periodo_inicial[i]
            g<-d+f
            Base_pronostico$weight_kg_1[g]<-cliente$WEIGHT_KG[i]/1000
            if(cliente$route_id[i]==1){
              Base_pronostico$obs_1[g]<-1
              Base_pronostico$distance_1[g]<-cliente$distance[i]/1000
              Base_pronostico$domestic_1[g]<-cliente$distancia_corta[i]
            }
            if(cliente$route_id[i]==2){
              Base_pronostico$obs_2[g]<-1
              Base_pronostico$distance_2[g]<-cliente$distance[i]/1000
              Base_pronostico$domestic_2[g]<-cliente$distancia_corta[i]
            }
            if(cliente$route_id[i]==3){
              Base_pronostico$obs_3[g]<-1
              Base_pronostico$distance_3[g]<-cliente$distance[i]/1000
              Base_pronostico$domestic_3[g]<-cliente$distancia_corta[i]
            }
            if(cliente$route_id[i]==4){
              Base_pronostico$obs_4[g]<-1
              Base_pronostico$distance_4[g]<-cliente$distance[i]/1000
              Base_pronostico$domestic_4[g]<-cliente$distancia_corta[i]
            }
          }
          
          for(i in 2:(max(Base_pronostico$period))){
            g<-i+f
            Base_pronostico$route_num_1[g]<-max(cliente$child_route_num)
            
            if(is.na(Base_pronostico$weight_kg_1[g]) || Base_pronostico$weight_kg_1[g]==0)Base_pronostico$weight_kg_1[g]<-Base_pronostico$weight_kg_1[g-1]
            if(is.na(Base_pronostico$distance_1[g]))Base_pronostico$distance_1[g]<-mean(cliente$distance[which(cliente$route_id==1)])/1000
            if(is.na(Base_pronostico$distance_2[g]))Base_pronostico$distance_2[g]<-mean(cliente$distance[which(cliente$route_id==2)])/1000
            if(is.na(Base_pronostico$distance_3[g]))Base_pronostico$distance_3[g]<-mean(cliente$distance[which(cliente$route_id==3)])/1000
            if(is.na(Base_pronostico$distance_4[g]))Base_pronostico$distance_4[g]<-mean(cliente$distance[which(cliente$route_id==4)])/1000
            if(is.na(Base_pronostico$domestic_1[g]))Base_pronostico$domestic_1[g]<-Base_pronostico$domestic_1[g-1]
            if(is.na(Base_pronostico$domestic_2[g]))Base_pronostico$domestic_2[g]<-Base_pronostico$domestic_2[g-1]
            if(is.na(Base_pronostico$domestic_3[g]))Base_pronostico$domestic_3[g]<-Base_pronostico$domestic_3[g-1]
            if(is.na(Base_pronostico$domestic_4[g]))Base_pronostico$domestic_4[g]<-Base_pronostico$domestic_4[g-1]
          }
          Base_pronostico$order_frequency_1[(1+f):(max(Base_pronostico$period)+f)]<-mean(Base_pronostico$obs_1[(1+f):(25+f)])
          Base_pronostico$order_frequency_2[(1+f):(max(Base_pronostico$period)+f)]<-mean(Base_pronostico$obs_2[(1+f):(25+f)])
          Base_pronostico$order_frequency_3[(1+f):(max(Base_pronostico$period)+f)]<-mean(Base_pronostico$obs_3[(1+f):(25+f)])
          Base_pronostico$order_frequency_4[(1+f):(max(Base_pronostico$period)+f)]<-mean(Base_pronostico$obs_4[(1+f):(25+f)])
          
          f<-f+max(Base_pronostico$period)
        }
        
        Base_pronostico$weight_kg_2<-Base_pronostico$weight_kg_1
        Base_pronostico$weight_kg_3<-Base_pronostico$weight_kg_1
        Base_pronostico$weight_kg_4<-Base_pronostico$weight_kg_1
        Base_pronostico$route_num_2<-Base_pronostico$route_num_1
        Base_pronostico$route_num_3<-Base_pronostico$route_num_1
        Base_pronostico$route_num_4<-Base_pronostico$route_num_1
        #Para correr modelo con datos del método naive se omiten los sigma_theta y xi_theta
        Base_pronostico$sigma_theta_1<-Base_pronostico$sigma
        Base_pronostico$sigma_theta_2<-Base_pronostico$sigma
        Base_pronostico$sigma_theta_3<-Base_pronostico$sigma
        Base_pronostico$sigma_theta_4<-Base_pronostico$sigma
        #Para correr modelo con datos del aprendizaje indepentdiente se omiten los xi_theta
        Base_pronostico$xi_theta_1<-Base_pronostico$xi
        Base_pronostico$xi_theta_2<-Base_pronostico$xi
        Base_pronostico$xi_theta_3<-Base_pronostico$xi
        Base_pronostico$xi_theta_4<-Base_pronostico$xi
        
        #Jerárquico: quitar las columnas de mean rhat, max rhat,diff, sigma y xi
        Base_pronostico<-Base_pronostico[,-c(5:11,13:15)]
        #Independiente y naive: quitar las columnas de mean rhat, max rhat,theta,diff, sigma y xi
        Base_pronostico<-Base_pronostico[,-c(5:15)]
        
        Base_pronostico_3<-Base_pronostico
        load("mydata_server_14.RData") #para modelo Jerárquico
        #load("mydata_server_13.RData") #para modelo Independiente
        #load("mydata_server_10.RData") #para modelo Naive
        Base_pronostico_3<-Base_pronostico_3[order(Base_pronostico_3$client, Base_pronostico_3$period),]
        Base_pronostico<-Base_pronostico[order(Base_pronostico$client, Base_pronostico$period),]
        
        Base_pronostico_3$tau<-Base_pronostico$tau
        Base_pronostico_3$tau_1<-Base_pronostico$tau_1
        Base_pronostico_3$tau_2<-Base_pronostico$tau_2
        Base_pronostico_3$tau_3<-Base_pronostico$tau_3
        Base_pronostico_3$tau_4<-Base_pronostico$tau_4
        #Para correr modelo con datos del método naive se omiten los sigma_theta y xi_theta
        Base_pronostico_3$sigma_1<-Base_pronostico$sigma
        Base_pronostico_3$sigma_2<-Base_pronostico$sigma
        Base_pronostico_3$sigma_3<-Base_pronostico$sigma
        Base_pronostico_3$sigma_4<-Base_pronostico$sigma
        #Para correr modelo con datos del aprendizaje independiente se omiten los xi_theta
        Base_pronostico_3$xi_1<-Base_pronostico$xi
        Base_pronostico_3$xi_2<-Base_pronostico$xi
        Base_pronostico_3$xi_3<-Base_pronostico$xi
        Base_pronostico_3$xi_4<-Base_pronostico$xi
        
        ##### quitar los primeros 25 periodos
        Base_pronostico_2<-Base_pronostico_3
        Base_pronostico_2<-Base_pronostico_2[order(Base_pronostico_2$period),]
        Base_pronostico_2<-Base_pronostico_2[min(which(Base_pronostico_2$period>25)):nrow(Base_pronostico_2),]
        
        #Variable de demanda
        Base_pronostico_2$obs<-Base_pronostico_2$obs_1 + Base_pronostico_2$obs_2 + Base_pronostico_2$obs_3 + Base_pronostico_2$obs_4
        Base_pronostico_2$demand_1<-NA
        for(i in 1:nrow(Base_pronostico_2)){
          if(Base_pronostico_2$obs[i]>1){Base_pronostico_2$obs[i]<-1}
          Base_pronostico_2$demand_1[i]<-sum(Base_pronostico_2$obs[which(Base_pronostico_2$period==Base_pronostico_2$period[i])])-Base_pronostico_2$obs[i]
        }
        
        Base_pronostico_2$demand_2<-Base_pronostico_2$demand_1
        Base_pronostico_2$demand_3<-Base_pronostico_2$demand_1
        Base_pronostico_2$demand_4<-Base_pronostico_2$demand_1
        Base_pronostico_2<-Base_pronostico_2[order(Base_pronostico_2$client),]
        Base_pronostico_2$first_half_week_1<-rep(c(0,1),length(Base_pronostico_2$demand_1)/2)
        Base_pronostico_2$first_half_week_2<-rep(c(0,1),length(Base_pronostico_2$demand_1)/2)
        Base_pronostico_2$first_half_week_3<-rep(c(0,1),length(Base_pronostico_2$demand_1)/2)
        Base_pronostico_2$first_half_week_4<-rep(c(0,1),length(Base_pronostico_2$demand_1)/2)
        
        
        #ordenar las columnas (client,route_num,period,route,theta,tau,obs,obs1:obs4;weight,distance, order_frequency,route_num,domestic,theta,sigma_theta,xi_theta,tau,sigma,xi,demand,first_half_week)
        Base_pronostico_2<-Base_pronostico_2[,c(1:5,42,55,10:33,6:9,34:41,43:54,56:63)] #Modelo Jerarquico
        #Base_pronostico_2<-Base_pronostico_2[,c(1:4,45,9:32,5:8,33:44,46:53)] #Modelo Independiente
        #Base_pronostico_2<-Base_pronostico_2[,c(1:4,37,9:32,5:8,33:36,38:45)] #Modelo Naive
        
        library(writexl)
        #Modelo jerárquico
        write_xlsx(Base_pronostico_2, "/Base_modelo_15.xlsx")
        save.image("my_base_modelo_9.RData")
        
        #Modelo Independiente
        #write_xlsx(Base_pronostico_2, "/Base_modelo_14.xlsx")
        #save.image("my_base_modelo_8.RData")
        
        #Modelo Naive
        write_xlsx(Base_pronostico_2, "/Base_modelo_13.xlsx")
        save.image("my_base_modelo_7.RData")
