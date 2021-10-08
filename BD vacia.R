#Cargar base de datos
library(readxl)
BD <- read_excel("Datos final.xlsx", sheet = "Datos")

#Creacion de periodo final
library(lubridate)
BD<-BD[order(BD$final_time),]
p=2
for (i in 1:length(BD$final_time)){
  BD$Periodo_final[i]<-p
  if(yday(BD$final_time[i+1])-yday(BD$final_time[i])==1 && wday(BD$final_time[i])==1) p=p+1
  if(yday(BD$final_time[i+1])-yday(BD$final_time[i])==1 && wday(BD$final_time[i])==4) p=p+1
}
BD$Periodo_final[1]<-1
BD<-BD[order(BD$outlier),]
BD<-BD[1:(min(which(BD$outlier>0))-1),]

#Dejamos solo las columnas que nos interesan (child_id, route_id, child_route_num, difference, Periodo_final )
BD<-BD[,c(14,17,18,22,29)]

#Anadimos columnas de diferencia por ruta y observacion por ruta
BD$obs1<-0
BD$obs2<-0
BD$obs3<-0
BD$obs4<-0
BD$diff1<-0
BD$diff2<-0
BD$diff3<-0
BD$diff4<-0

for(i in 1:length(BD$child_id)){
  if(BD$route_id[i]==1){
    BD$diff1[i]<-BD$difference[i]
    BD$obs1[i]<-1
  }
  if(BD$route_id[i]==2){
    BD$diff2[i]<-BD$difference[i]
    BD$obs2[i]<-1
  }
  if(BD$route_id[i]==3){
    BD$diff3[i]<-BD$difference[i]
    BD$obs3[i]<-1
  }
  if(BD$route_id[i]==4){
    BD$diff4[i]<-BD$difference[i]
    BD$obs4[i]<-1
  }
}
#vector con clientes unicos
library(dplyr)
clientes<-distinct(BD,child_id)

#parametros iniciales para theta, sigma, xi y thetar
#library(invgamma)
param1<-c(1.05,10)
param2<-c(1.05,3)
param<-c(0,sqrt(param1[2]/(param1[1]-1)),sqrt(param2[2]/(param2[1]-1)),0)

#construccion de tabla vacia
Base_pronostico<-data.frame(client=0,route_num=0,period=0,route=0,count_period=0,diff_1=0,diff_2=0,diff_3=0,diff_4=0,mean_rhat=0, max_rhat=0,theta=0,se_mean=0,sigma=0, xi=0,theta_1=0,theta_2=0,theta_3=0,theta_4=0)
for (j in clientes$child_id){
  l<-max(BD$Periodo_final)+1
  Base_pronostico<-rbind(Base_pronostico,list(rep(j,l),rep(NA,l),(1:l),rep(NA,l),rep(NA,l),rep(NA,l),rep(NA,l),rep(NA,l),rep(NA,l),rep(NA,l),rep(NA,l),rep(NA,l),rep(NA,l),rep(NA,l),rep(NA,l),rep(NA,l),rep(NA,l),rep(NA,l),rep(NA,l)))
}
Base_pronostico<-Base_pronostico[-1,]

f<-0
count_clientes<- 0

save.image(file = 'mydata_server_9.RData')
