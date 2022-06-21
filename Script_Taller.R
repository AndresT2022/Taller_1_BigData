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

#data <- read.xlsx("C:\\Users\\DELL\\OneDrive - Universidad de los Andes\\MECA 2022_2023\\BIGDATA\\TALLERES\\Taller_1\\Database.xlsx",sheet="Sheet1")
data <- read.xlsx("D:\\Documents\\Andres\\ANDES\\2.5\\Taller_1_BigData\\Database.xlsx",sheet="Sheet1")



data$Escol <- with(data, ifelse(
  maxEducLevel == 1, 0, ifelse(
    maxEducLevel == 2, 2, ifelse(
      maxEducLevel == 3, 6, ifelse(
        maxEducLevel == 4, 7, ifelse(
          maxEducLevel== 5, 12, ifelse(
            maxEducLevel == 6, 13, ifelse(
              maxEducLevel == 7, 19, "Estudien vagos"
            )
          )
        ))))))

data_clean <- select(data, "directorio","secuencia_p","orden","clase",
                     "dominio","mes","estrato1","sex","age", "p6210", "maxEducLevel","regSalud","cotPension",
                     "sizeFirm","oficio","wap","ocu","dsi","pea","inac",	"totalHoursWorked","formal",
                     "informal","cuentaPropia","microEmpresa","college","Escol","y_total_m","y_total_m_ha") 
data_clean_ocu  <- subset(data_clean,data_clean$ocu == 1 & data_clean$age >= 18 & data_clean$age <= 62  & 
                            data_clean$dominio == "BOGOTA" ) %>% drop_na()
  
data_clean_ocu$agesqr <- data_clean_ocu$age^2

# Experiencia
data_clean_ocu$exp <- data_clean_ocu$age-5- as.numeric(data_clean_ocu$Escol)
data_clean_ocu$exp <- with(data_clean_ocu, ifelse(
  data_clean_ocu$exp < 0,0,data_clean_ocu$exp
  ))

View(data_clean_ocu) 

## Perfil ingreso-edad: Se toma una regresión lineal que evalue la relación entre el ingreso mensual total con la edad
#y se adapta al ajuste de elevar al cuadrado la edad para encontrar un punto de inflexión.

reg1 <- lm(y_total_m~age, data_clean_ocu)

reg2 <- lm(y_total_m~age+ agesqr, data_clean_ocu) 



data_clean_ocu$yhat_reg1<-predict(reg1)
data_clean_ocu$yhat_reg2<-predict(reg2)

stargazer(reg1,reg2,type="text") 

plot(data_clean_ocu$age,data_clean_ocu$yhat_reg2)

eta_mod2.fn <-function(data_clean_ocu,index,
                       age_bar=mean(data_clean_ocu$age),
                       agesqr_bar=mean(data_clean_ocu$agesqr))
  {
  
  f<-lm(y_total_m~age+ agesqr,data_clean_ocu ,  subset = index)
  coefs<-f$coefficients
  b2<-coefs[2]
  b3<-coefs[3]

  income<-b2*age_bar+b3*agesqr_bar
  return(income)}




f<-lm(y_total_m~age+ agesqr,data_clean_ocu)
coefs<-f$coefficients
b1<-coefs[1]
b2<-coefs[2]
b3<-coefs[3]
b1
b2
b3

results <- boot(data=data_clean_ocu, eta_mod2.fn,R=1000)
results

ic <- boot.ci(results,conf = 0.95, type="norm")
ic



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
                        
