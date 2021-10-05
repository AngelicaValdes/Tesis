library(mfx) #efectos marginales de logit
library(stargazer) #tablas bonitas
library(readxl)
setwd("C:/Users/Angie/Dropbox/delays angelica")
load("mysession_server_6.RData")
Base_pronostico$period<-Base_pronostico$period+1
BD2 <- read_excel("Datos.xlsx")

library(lubridate)
BD2<-BD2[order(BD2$start_time),]
p=1
for (i in 1:length(BD2$start_time)){
  BD2$Periodo_inicial[i]<-p
  if(yday(BD2$start_time[i+1])-yday(BD2$start_time[i])==1 && wday(BD2$start_time[i])==1) p=p+1
  if(yday(BD2$start_time[i+1])-yday(BD2$start_time[i])==1 && wday(BD2$start_time[i])==4) p=p+1
}

##### inicializacion de variables ####

Base_pronostico$obs_1<-0
Base_pronostico$obs_2<-0
Base_pronostico$obs_3<-0
Base_pronostico$obs_4<-0
Base_pronostico$obs_5<-0
Base_pronostico$obs_6<-0
Base_pronostico$weight_kg_1<-NA
Base_pronostico$weight_kg_2<-NA
Base_pronostico$weight_kg_3<-NA
Base_pronostico$weight_kg_4<-NA
Base_pronostico$weight_kg_5<-NA
Base_pronostico$weight_kg_6<-NA
Base_pronostico$distance_1<-NA
Base_pronostico$distance_2<-NA
Base_pronostico$distance_3<-NA
Base_pronostico$distance_4<-NA
Base_pronostico$distance_5<-NA
Base_pronostico$distance_6<-NA
Base_pronostico$domestic_1<-NA
Base_pronostico$domestic_2<-NA
Base_pronostico$domestic_3<-NA
Base_pronostico$domestic_4<-NA
Base_pronostico$domestic_5<-NA
Base_pronostico$domestic_6<-NA
Base_pronostico$first_half_week_1<-NA
Base_pronostico$first_half_week_2<-NA
Base_pronostico$first_half_week_3<-NA
Base_pronostico$first_half_week_4<-NA
Base_pronostico$first_half_week_5<-NA
Base_pronostico$first_half_week_6<-NA
Base_pronostico$order_frequency_1<-NA
Base_pronostico$order_frequency_2<-NA
Base_pronostico$order_frequency_3<-NA
Base_pronostico$order_frequency_4<-NA
Base_pronostico$order_frequency_5<-NA
Base_pronostico$order_frequency_6<-NA

##### crear variable obs y demas ####
f<-0
for (j in clientes$child_id){
  
  cliente<-subset(BD2,child_id==j)
  cliente<-cliente[order(cliente$Periodo_inicial, cliente$route_id),]
  
  #Introducir un 1 si hay un envio para el periodo
  h<-length(cliente$Periodo_inicial)
  
  for(i in 2:h){
    d<-cliente$Periodo_inicial[i]
    g<-d+f-1
    Base_pronostico$weight_kg_1[g]<-cliente$WEIGHT_KG[i]
    Base_pronostico$distance_1[g]<-cliente$distance[i]/1000
    ifelse(cliente$from_country[i]==cliente$to_country[i] , Base_pronostico$domestic_1[g]<-1 , Base_pronostico$domestic_1[g]<-0)
    if(cliente$route_id[i]==1){
      Base_pronostico$obs_1[g]<-1
    }
    if(cliente$route_id[i]==2){
      Base_pronostico$obs_2[g]<-1
    }
    if(cliente$route_id[i]==3){
      Base_pronostico$obs_3[g]<-1
    }
    if(cliente$route_id[i]==4){
      Base_pronostico$obs_4[g]<-1
    }
    if(cliente$route_id[i]==5){
      Base_pronostico$obs_5[g]<-1
    }
    if(cliente$route_id[i]==6){
      Base_pronostico$obs_6[g]<-1
    }
  }
  
  for(i in 15:(max(Base_pronostico$period))){
    g<-i+f-1
    cliente2<-subset(cliente, WEIGHT_KG>0)
    Base_pronostico$route_num[g]<-max(cliente$child_route_num)
    if(is.na(Base_pronostico$weight_kg_1[g]) || Base_pronostico$weight_kg_1[g]==0)Base_pronostico$weight_kg_1[g]<-Base_pronostico$weight_kg_1[g-1]
    if(is.na(Base_pronostico$domestic_1[g]))Base_pronostico$domestic_1[g]<-Base_pronostico$domestic_1[g-1]
    if(is.na(Base_pronostico$distance_1[g]))Base_pronostico$distance_1[g]<-mean(cliente$distance[1:max(which(cliente$Periodo_inicial<=Base_pronostico$period[g]))])/1000
    ifelse(Base_pronostico$period[g]%%2 == 0 , Base_pronostico$first_half_week_1[g]<-0, Base_pronostico$first_half_week_1[g]<-1)
  }
  Base_pronostico$order_frequency_1[(1+f):(max(Base_pronostico$period)+f-1)]<-mean(Base_pronostico$obs_1[(1+f):(25+f)])
  Base_pronostico$order_frequency_2[(1+f):(max(Base_pronostico$period)+f-1)]<-mean(Base_pronostico$obs_2[(1+f):(25+f)])
  Base_pronostico$order_frequency_3[(1+f):(max(Base_pronostico$period)+f-1)]<-mean(Base_pronostico$obs_3[(1+f):(25+f)])
  Base_pronostico$order_frequency_4[(1+f):(max(Base_pronostico$period)+f-1)]<-mean(Base_pronostico$obs_4[(1+f):(25+f)])
  Base_pronostico$order_frequency_5[(1+f):(max(Base_pronostico$period)+f-1)]<-mean(Base_pronostico$obs_5[(1+f):(25+f)])
  Base_pronostico$order_frequency_6[(1+f):(max(Base_pronostico$period)+f-1)]<-mean(Base_pronostico$obs_6[(1+f):(25+f)])
  
  f<-f+max(Base_pronostico$period)-1
}

Base_pronostico$weight_kg_2<-Base_pronostico$weight_kg_1
Base_pronostico$weight_kg_3<-Base_pronostico$weight_kg_1
Base_pronostico$weight_kg_4<-Base_pronostico$weight_kg_1
Base_pronostico$weight_kg_5<-Base_pronostico$weight_kg_1
Base_pronostico$weight_kg_6<-Base_pronostico$weight_kg_1
Base_pronostico$distance_2<-Base_pronostico$distance_1
Base_pronostico$distance_3<-Base_pronostico$distance_1
Base_pronostico$distance_4<-Base_pronostico$distance_1
Base_pronostico$distance_5<-Base_pronostico$distance_1
Base_pronostico$distance_6<-Base_pronostico$distance_1
Base_pronostico$domestic_2<-Base_pronostico$domestic_1
Base_pronostico$domestic_3<-Base_pronostico$domestic_1
Base_pronostico$domestic_4<-Base_pronostico$domestic_1
Base_pronostico$domestic_5<-Base_pronostico$domestic_1
Base_pronostico$domestic_6<-Base_pronostico$domestic_1
Base_pronostico$first_half_week_2<-Base_pronostico$first_half_week_1
Base_pronostico$first_half_week_3<-Base_pronostico$first_half_week_1
Base_pronostico$first_half_week_4<-Base_pronostico$first_half_week_1
Base_pronostico$first_half_week_5<-Base_pronostico$first_half_week_1
Base_pronostico$first_half_week_6<-Base_pronostico$first_half_week_1
Base_pronostico$sigma_1<-Base_pronostico$sigma
Base_pronostico$sigma_2<-Base_pronostico$sigma
Base_pronostico$sigma_3<-Base_pronostico$sigma
Base_pronostico$sigma_4<-Base_pronostico$sigma
Base_pronostico$sigma_5<-Base_pronostico$sigma
Base_pronostico$sigma_6<-Base_pronostico$sigma
Base_pronostico$xi_1<-Base_pronostico$xi
Base_pronostico$xi_2<-Base_pronostico$xi
Base_pronostico$xi_3<-Base_pronostico$xi
Base_pronostico$xi_4<-Base_pronostico$xi
Base_pronostico$xi_5<-Base_pronostico$xi
Base_pronostico$xi_6<-Base_pronostico$xi


##### quitar los primeros 25 periodos
Base_pronostico_2<-Base_pronostico
Base_pronostico_2<-Base_pronostico_2[order(Base_pronostico_2$period),]
Base_pronostico_2<-Base_pronostico_2[17401:75400,]

##### variable de delay (theta_X_d) y early (theta_X_e) para pronostico ######

for (i in 1:nrow(Base_pronostico_2)){
  ifelse(Base_pronostico_2$theta_1[i]>0 , Base_pronostico_2$theta_1_d[i]<-Base_pronostico_2$theta_1[i] , Base_pronostico_2$theta_1_d[i]<-0)
  ifelse(Base_pronostico_2$theta_2[i]>0 , Base_pronostico_2$theta_2_d[i]<-Base_pronostico_2$theta_2[i] , Base_pronostico_2$theta_2_d[i]<-0)
  if(is.na(Base_pronostico_2$theta_3[i])){Base_pronostico_2$theta_3[i]<-100}
  ifelse(Base_pronostico_2$theta_3[i]>0 , Base_pronostico_2$theta_3_d[i]<-Base_pronostico_2$theta_3[i] , Base_pronostico_2$theta_3_d[i]<-0)
  if(is.na(Base_pronostico_2$theta_4[i])){Base_pronostico_2$theta_4[i]<-100}
  ifelse(Base_pronostico_2$theta_4[i]>0 , Base_pronostico_2$theta_4_d[i]<-Base_pronostico_2$theta_4[i] , Base_pronostico_2$theta_4_d[i]<-0)
  if(is.na(Base_pronostico_2$theta_5[i])){Base_pronostico_2$theta_5[i]<-100}
  ifelse(Base_pronostico_2$theta_5[i]>0 , Base_pronostico_2$theta_5_d[i]<-Base_pronostico_2$theta_5[i] , Base_pronostico_2$theta_5_d[i]<-0)
  if(is.na(Base_pronostico_2$theta_6[i])){Base_pronostico_2$theta_6[i]<-100}
  ifelse(Base_pronostico_2$theta_6[i]>0 , Base_pronostico_2$theta_6_d[i]<-Base_pronostico_2$theta_6[i] , Base_pronostico_2$theta_6_d[i]<-0)
  ifelse(Base_pronostico_2$theta_1[i]<0 , Base_pronostico_2$theta_1_e[i]<-Base_pronostico_2$theta_1[i]*-1 , Base_pronostico_2$theta_1_e[i]<-0)
  ifelse(Base_pronostico_2$theta_2[i]<0 , Base_pronostico_2$theta_2_e[i]<-Base_pronostico_2$theta_2[i]*-1 , Base_pronostico_2$theta_2_e[i]<-0)
  ifelse(Base_pronostico_2$theta_3[i]<0 , Base_pronostico_2$theta_3_e[i]<-Base_pronostico_2$theta_3[i]*-1 , Base_pronostico_2$theta_3_e[i]<-0)
  ifelse(Base_pronostico_2$theta_4[i]<0 , Base_pronostico_2$theta_4_e[i]<-Base_pronostico_2$theta_4[i]*-1 , Base_pronostico_2$theta_4_e[i]<-0)
  ifelse(Base_pronostico_2$theta_5[i]<0 , Base_pronostico_2$theta_5_e[i]<-Base_pronostico_2$theta_5[i]*-1 , Base_pronostico_2$theta_5_e[i]<-0)
  ifelse(Base_pronostico_2$theta_6[i]<0 , Base_pronostico_2$theta_6_e[i]<-Base_pronostico_2$theta_6[i]*-1 , Base_pronostico_2$theta_6_e[i]<-0)
}

for (i in 1:nrow(Base_pronostico_2)){
  if(Base_pronostico_2$theta_3_d[i]>90){
    Base_pronostico_2$theta_3_d[i]<-NA
    Base_pronostico_2$theta_3_e[i]<-NA
  }
  if(Base_pronostico_2$theta_4_d[i]>90){
    Base_pronostico_2$theta_4_d[i]<-NA
    Base_pronostico_2$theta_4_e[i]<-NA
  }
  if(Base_pronostico_2$theta_5_d[i]>90){
    Base_pronostico_2$theta_5_d[i]<-NA
    Base_pronostico_2$theta_5_e[i]<-NA
  }
  if(Base_pronostico_2$theta_6_d[i]>90){
    Base_pronostico_2$theta_6_d[i]<-NA
    Base_pronostico_2$theta_6_e[i]<-NA
  }
}

Base_pronostico_2<-Base_pronostico_2[,-c(5:17)]
library(writexl)
write_xlsx(Base_pronostico_2, "C:/Users/Angie/Dropbox/delays angelica/Base_modelo.xlsx")

save.image("my_base_modelo.RData")