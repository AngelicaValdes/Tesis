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
setwd("H:/Dropbox/yan shang/delays angelica")
#setwd("C:/Users/Angie/Dropbox/delays angelica")
library(dplyr)
library("rstan")
rstan_options(auto_write = TRUE)
options(mc.cores = parallel::detectCores())

library(readxl)
#BD <- read_excel("Base_modelo_2.xlsx")
BD <- read_excel("Base_modelo_prueba.xlsx")
BD<-BD[order(BD$client),]
BD$weight_kg_1<-BD$weight_kg_1/1000
BD$weight_kg_2<-BD$weight_kg_2/1000
BD$weight_kg_3<-BD$weight_kg_3/1000
BD$weight_kg_4<-BD$weight_kg_4/1000
BD$weight_kg_5<-BD$weight_kg_5/1000
BD$weight_kg_6<-BD$weight_kg_6/1000

clients<-distinct(BD,client)
nclients <- length(clients$client)
nperiod <- max(BD$period)-BD$period[1]+1
nroute <- BD$route_num[order(BD$period,BD$client)][1:nclients] #vector con numero de rutas que usa el cliente del 1 al 750 
pos <-c(0:(nclients-1))
for(client in 2:nclients){pos[client]=pos[client-1]+nroute[client-1]}
nvar <- 9
N<-sum(nroute)


obs<-as.matrix(BD[1:nperiod,11:(10+nroute[1])], nrow = nrow(BD[1:nperiod,]))
c<-nperiod
for (i in 2:nclients){
  #11 corresponde al primer numero de la columna con las obs de las rutas
  h<-nroute[i]
  a<-as.matrix(BD[(1+c):(nperiod+c),11:(10+h)], nrow = nrow(BD[(1+c):(nperiod+c),]))
  obs<-cbind(obs,a)
  c<-c+nperiod
}

j<-0
b<-as.matrix(BD[1:nperiod,(17+j):(16+nroute[1]+j)], nrow = nrow(BD[1:nperiod,]))
indep<-list(b,b,b,b,b,b,b,b,b)
for (v in 1:nvar){
  f<-0
  b<-as.matrix(BD[1:nperiod,(17+j):(16+nroute[1]+j)], nrow = nrow(BD[1:nperiod,]))
  for (c in 2:nclients){
    f<-f+nperiod
    h<-nroute[c]
    a<-as.matrix(BD[(1+f):(nperiod+f),(17+j):(16+h+j)], nrow = nrow(BD[(1+f):(nperiod+f),]))
    b<-cbind(b,a)
  }
  j<-j+max(nroute)
  indep[[v]]<-b
}

initf1 <- function() {
  list(beta_0_var = 3, beta_var = array(3, dim = nvar))
}

#Con todos los clientes

stan.data = list(nclients = nclients, nperiod = nperiod, nroute = nroute,pos = pos, nvar = nvar, N = N, obs = obs, indep = indep)
reg_fit <- stan(file= 'model1_6.stan', data=stan.data, init = initf1, iter=1000, chains=1, save_warmup = FALSE)
summary <- summary(reg_fit, pars = c("beta_0_mean","beta_0_var","beta_var","beta_mean","beta_0","beta","m","estimated1","estimated2"), probs = c(0.025,0.5,0.975))$summary

save.image("datos_prueba_segunda_simulacion.RData")

