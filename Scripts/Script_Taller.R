#install.packages("recipes") 
rm(list=ls()) 
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


data$LnIng <- log(data$y_total_m)
data$Escol <- with(data, ifelse(
  maxEducLevel == 1, 0, ifelse(
    maxEducLevel == 2, 2, ifelse(
      maxEducLevel == 3, 6, ifelse(
        maxEducLevel == 4, 7, ifelse(
          maxEducLevel== 5, 12, ifelse(
            maxEducLevel == 6, 13, ifelse(
              maxEducLevel == 7, 19, "No data"
            )
          )
        ))))))

data_clean <- select(data, "directorio","secuencia_p","orden","clase",
                     "dominio","mes","estrato1","sex","age", "p6210", "maxEducLevel","regSalud","cotPension",
                     "sizeFirm","oficio","wap","ocu","dsi","pea","inac",	"totalHoursWorked","formal",
                     "informal","cuentaPropia","microEmpresa","college","Escol","y_total_m","y_total_m_ha","LnIng") 
data_clean_ocu  <- subset(data_clean,data_clean$ocu == 1 & data_clean$age >= 18 & data_clean$age <= 80  & 
                            data_clean$dominio == "BOGOTA" ) %>% drop_na()
  
data_clean_ocu$agesqr <- data_clean_ocu$age^2

# Experiencia
data_clean_ocu$exp <- data_clean_ocu$age-5- as.numeric(data_clean_ocu$Escol)
data_clean_ocu$exp <- with(data_clean_ocu, ifelse(
  data_clean_ocu$exp < 0,0,data_clean_ocu$exp
  ))


## Perfil ingreso-edad: Se toma una regresión lineal que evalue la relación entre el ingreso mensual total con la edad
#y se adapta al ajuste de elevar al cuadrado la edad para encontrar un punto de inflexión.

reg1 <- lm(y_total_m~age, data_clean_ocu)

reg2 <- lm(y_total_m~age+ agesqr, data_clean_ocu) 



data_clean_ocu$yhat_reg1<-predict(reg1)
data_clean_ocu$yhat_reg2<-predict(reg2)

stargazer(reg1,reg2,type="text") 

plot(data_clean_ocu$age,data_clean_ocu$yhat_reg2)
ggplot(data_clean_ocu,aes(x=age))+
  geom_line(aes(y=yhat_reg2),colour="red")+
  theme_classic()+
  facet_grid()+
  ggtitle("Modelo Ingreso ajustado por edad")+
  theme(plot.title = element_text(hjust = 0.5))+
  labs(x="Edad", y="Ingreso")


eta_mod2.fn <-function(data_clean_ocu,index,
                       age_bar=mean(data_clean_ocu$age),
                       agesqr_bar=mean(data_clean_ocu$agesqr))
  {
  
  f<-lm(y_total_m~age+ agesqr,data_clean_ocu ,  subset = index)
  coefs<-f$coefficients
  b2<-coefs[2]
  b3<-coefs[3]

  age<- -b2/(2*b3)
  return(age)}



results <- boot(data=data_clean_ocu, eta_mod2.fn,R=1000)
results

ic <- boot.ci(results,conf = 0.95, type="norm")
ic

### Si quisieramos observar efectos por género 


data_clean_ocu_fem <- data_clean_ocu
data_clean_ocu_fem$female <- with(data_clean_ocu_fem, ifelse(
  sex == 1, 0, 1)) 
data_clean_ocu_fem<- subset(data_clean_ocu_fem, data_clean_ocu_fem$female==1)
data_clean_ocu_male<- subset(data_clean_ocu, data_clean_ocu$sex==1)
data_clean_ocu_male$male <- with(data_clean_ocu_male, ifelse(
  sex == 1, 0, 1)) 

reg_gap <- lm( LnIng~ age+ agesqr+female  , data=data_clean_ocu_fem)
reg_ingreso<- lm( LnIng~ age+ agesqr  , data=data_clean_ocu_male)
reg_ing_fem <- lm( y_total_m~age+ agesqr+female  , data=data_clean_ocu_fem)
reg_ing_male <- lm( y_total_m~age+ agesqr+male  , data=data_clean_ocu_male)

data_clean_ocu_fem$yhat_reg_gap<-predict(reg_gap)
data_clean_ocu_fem$yhat_reg_fem<-predict(reg_ing_fem)
data_clean_ocu_male$yhat_reg_male<-predict(reg_ing_male)
data_clean_ocu_male$yhat_reg_gapmale<-predict(reg_ingreso)

stargazer(reg_gap,reg_ing_fem,reg_ing_male,type="text") 

plot(data_clean_ocu_fem$age,data_clean_ocu_fem$yhat_reg_gap)
plot(data_clean_ocu_fem$age,data_clean_ocu_fem$yhat_reg_fem)
plot(data_clean_ocu_male$age,data_clean_ocu_male$yhat_reg_male)

ggplot(data_clean_ocu_fem,aes(x=age))+
  geom_line(aes(y=yhat_reg_gap,colour="Femenino"), size=1)+
  geom_line(data=data_clean_ocu_male,aes(y=yhat_reg_gapmale,colour="Masculino"), size=1)+
  theme_classic()+
  facet_grid()+
  ggtitle("Modelo Ingreso analizado por género")+
  theme(plot.title = element_text(hjust = 0.5))+
  labs(x="Edad", y="Ingreso")+
  scale_color_manual(name = "Género", values = c("Masculino" = "red", "Femenino" = "violet"))


eta_modfem.fn <-function(data_clean_ocu_fem,index,
                       age_barfem=mean(data_clean_ocu_fem$age),
                       agesqr_barfem=mean(data_clean_ocu_fem$agesqr))
{
  
  fem<-lm(y_total_m~age+ agesqr,data_clean_ocu_fem ,  subset = index)
  coefs_fem<-fem$coefficients
  b2_fem<-coefs_fem[2]
  b3_fem<-coefs_fem[3]
  
  age_fem<- -b2_fem/(2*b3_fem)
  return(age_fem)}



results_fem <- boot(data=data_clean_ocu_fem, eta_modfem.fn,R=1000)
results_fem

ic_fem <- boot.ci(results_fem,conf = 0.95, type="norm")
ic_fem 


eta_modmale.fn <-function(data_clean_ocu_male,index,
                        age_barfem=mean(data_clean_ocu_male$age),
                        agesqr_barfem=mean(data_clean_ocu_male$agesqr))
{
  
  male<-lm(y_total_m~age+ agesqr,data_clean_ocu_male ,  subset = index)
  coefs_male<-male$coefficients
  b2_male<-coefs_male[2]
  b3_male<-coefs_male[3]
  
  age_male<- -b2_male/(2*b3_male)
  return(age_male)}



results_male <- boot(data=data_clean_ocu_male, eta_modmale.fn,R=1000)
results_male

ic_male <- boot.ci(results_male,conf = 0.95, type="norm")
ic_male


# Bootstrap
library(ISLR2)
set.seed(10101)
count(data_clean_ocu)
count(data_clean_ocu*.7)
data_clean_ocu 
training<-sample(13519, 9463)
test<-

lm.fit0<-lm(y_total_m~1, subset = training)
attach(data_clean_ocu)
mean((y_total_m - predict(lm.fit0, data_clean_ocu))[-training]^2)

lm.fit<- lm(y_total_m~age, data=data_clean_ocu, subset = training)
#mean squared error
attach(data_clean_ocu)
mean((y_total_m - predict(lm.fit, data_clean_ocu))[-training]^2)

lm.fit1<- lm(y_total_m~Escol, data=data_clean_ocu, subset = training)
attach(data_clean_ocu)
mean((y_total_m - predict(lm.fit1, data_clean_ocu))[-training]^2)

lm.fit2<- lm(y_total_m~age+agesqr, data=data_clean_ocu, subset = training)
attach(data_clean_ocu)
mean((y_total_m - predict(lm.fit2, data_clean_ocu))[-training]^2)

lm.fit3<- lm(y_total_m~age+agesqr+Escol, data=data_clean_ocu, subset = training)
attach(data_clean_ocu)
mean((y_total_m - predict(lm.fit3, data_clean_ocu))[-training]^2)

lm.fit4<- lm(y_total_m~poly(Escol, 2), data=data_clean_ocu, subset = training)
attach(data_clean_ocu)
mean((y_total_m - predict(lm.fit4, data_clean_ocu))[-training]^2)

lm.fit5<- lm(y_total_m~poly(Escol, 3), data=data_clean_ocu, subset = training)
attach(data_clean_ocu)
mean((y_total_m - predict(lm.fit5, data_clean_ocu))[-training]^2)

lm.fit6<- lm(y_total_m~exp, data=data_clean_ocu, subset = training)
attach(data_clean_ocu)
mean((y_total_m - predict(lm.fit6, data_clean_ocu))[-training]^2)

lm.fit7<- lm(y_total_m~poly(exp, 2), data=data_clean_ocu, subset = training)
attach(data_clean_ocu)
mean((y_total_m - predict(lm.fit7, data_clean_ocu))[-training]^2)

lm.fit8<- lm(y_total_m~Escol+exp+poly(exp, 2)+sex, data=data_clean_ocu, subset = training)
attach(data_clean_ocu)
mean((y_total_m - predict(lm.fit8, data_clean_ocu))[-training]^2)

lm.fit9<- lm(y_total_m~Escol+exp+poly(exp, 2)+sex+age, data=data_clean_ocu, subset = training)
attach(data_clean_ocu)
mean((y_total_m - predict(lm.fit9, data_clean_ocu))[-training]^2)



                        
