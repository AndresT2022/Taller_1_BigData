install.packages("recipes")
library(tidyverse)
library(mosaic)
library(recipes)
library(openxlsx)
require("fabricatr")
require("stargazer") 
require("boot")

## Data Cleaning: Se toma la base se seleccionan 28 potenciales variables explicativas. La limpieza consiste en
#a la vez elegir elementos de la muestra que cumplan con la condicional de estar ocupados, ser mayores de 18 años y vivir en Bogota (usar el dominio BOGOTA)

data <- read.xlsx("C:\\Users\\DELL\\OneDrive - Universidad de los Andes\\MECA 2022_2023\\BIGDATA\\TALLERES\\Taller_1\\Database.xlsx",sheet="Sheet1")
data_clean <- select(data, "directorio","secuencia_p","orden","clase",
                     "dominio","mes","estrato1","sex","age", "p6210", "maxEducLevel","regSalud","cotPension",
                     "sizeFirm","oficio","wap","ocu","dsi","pea","inac",	"totalHoursWorked","formal",
                     "informal","cuentaPropia","microEmpresa","college","y_total_m","y_total_m_ha") 
data_clean_ocu  <- subset(data_clean,data_clean$ocu == 1 & data_clean$age >= 18 & 
                            data_clean$dominio == "BOGOTA" ) %>% drop_na()
  
data_clean_ocu$agesqr <- data_clean_ocu$age^2

View(data_clean_ocu) 

## Perfil ingreso-edad: Se toma una regresión lineal que evalue la relación entre el ingreso mensual total con la edad
#y se adapta al ajuste de elevar al cuadrado la edad para encontrar un punto de inflexión.

reg1 <- lm(y_total_m~age, data_clean_ocu)

reg2 <- lm(y_total_m~age+ agesqr, data_clean_ocu) 



data_clean_ocu$yhat_reg1<-predict(reg1)
data_clean_ocu$yhat_reg2<-predict(reg2)

stargazer(reg1,reg2,type="text") 

plot(data_clean_ocu$age,data_clean_ocu$yhat_reg2)

eta_mod2.fn <-function(data_clean_ocu,index,age=mean(data_clean_ocu$age),age_sqr=mean(data_clean_ocu$agesqr))
  {
  f<-lm(y_total_m~age+age_sqr, subset = index)
  coefs<-f$coefficients
  b2<-coefs[2]
  b3<-coefs[3]

  income<-b2+b3
  return(income)}

results <- boot(data=data_clean_ocu, eta_mod2.fn,R=1000)
results




View(data)

set.seed(10101) 
sample1<-rbinom(n=100,size=1,p=.51) 
sum(sample1) 

samples<-do(10000) * sum(rbinom(n=100,size=1,p=.51))
samples<- samples %>% mutate(prop=sum/100)
plot(hist(samples)) 
require("boot")

boot(data,statistic,R)



#R= Replicaciones 

eta.fin<- function(data,index{ 
  coef(lm(consumption - price + income, data=data, subset=index))}) 
  
  
  
  
  eta.mod2.fin <. function(data,index,
                           price.bar=mean(gas$price),
                           income.bar=mean(gas$income){
                             f<-lm(consumption - income + price ´price2pprince.icnome,data,subet=index)
                             
                           }
                        
