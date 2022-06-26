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

#Punto 5.a
#Selección muestra de entrenamiento y prueba
library(ISLR2)
set.seed(10101)
as.numeric(data_clean_ocu$Escol)
id_train <- sample(1:nrow(data_clean_ocu),size = 0.7*nrow(data_clean_ocu), replace = F)
BD_train<-data_clean_ocu[id_train,]
BD_test<-data_clean_ocu[-id_train,]
#Numeral a.1 a a.4:
#Modelos entrenamiento
model_1<-lm(y_total_m~1,data = BD_train)
model_2<-lm(y_total_m~age,data = BD_train)
model_3<-lm(y_total_m~Escol,data = BD_train)
model_4<-lm(y_total_m~age+agesqr,data = BD_train)
model_5<-lm(y_total_m~age+agesqr+Escol,data = BD_train)
model_6<-lm(y_total_m~exp,data = BD_train)
model_7<-lm(y_total_m~poly(exp, 2),data = BD_train)
model_8<-lm(y_total_m~Escol+poly(exp, 2)+sex,data = BD_train)
model_9<-lm(y_total_m~Escol+poly(exp, 2)+sex+age,data = BD_train)
#Modelos fuera de muestra
BD_test$model_1<-predict(model_1,newdata = BD_test)
BD_test$model_2<-predict(model_2,newdata = BD_test)
BD_test$model_3<-predict(model_3,newdata = BD_test)
BD_test$model_4<-predict(model_4,newdata = BD_test)
BD_test$model_5<-predict(model_5,newdata = BD_test)
BD_test$model_6<-predict(model_6,newdata = BD_test)
BD_test$model_7<-predict(model_7,newdata = BD_test)
BD_test$model_8<-predict(model_8,newdata = BD_test)
BD_test$model_9<-predict(model_9,newdata = BD_test)
#MSE
with(BD_test,mean((y_total_m-model_1)^2))
with(BD_test,mean((y_total_m-model_2)^2))
with(BD_test,mean((y_total_m-model_3)^2))
with(BD_test,mean((y_total_m-model_4)^2))
with(BD_test,mean((y_total_m-model_5)^2))
with(BD_test,mean((y_total_m-model_6)^2))
with(BD_test,mean((y_total_m-model_7)^2))
with(BD_test,mean((y_total_m-model_8)^2))
with(BD_test,mean((y_total_m-model_9)^2))
#Numeral a.5:
# ni idea

#Numeral b. K-fold cross-validation.
install.packages("caret")
library(dplyr)
library(caret)
#modelos
model_1CV<-train(y_total_m~.,
                 data =  data_clean_ocu,
                 trControl=trainControl(method = "cv",number = 5),
                 method="null")

model_1CV<-train(y_total_m~1,
                 data =  data_clean_ocu,
                 trControl=trainControl(method = "cv",number = 5),
                 method="null")
model_2CV<-train(y_total_m~age,
                 data =  data_clean_ocu,
                 trControl=trainControl(method = "cv",number = 5),
                 method="lm")
model_3CV<-train(y_total_m~Escol,
                 data =  data_clean_ocu,
                 trControl=trainControl(method = "cv",number = 5),
                 method="lm")
model_4CV<-train(y_total_m~age+agesqr,
                 data =  data_clean_ocu,
                 trControl=trainControl(method = "cv",number = 5),
                 method="lm")
model_5CV<-train(y_total_m~age+agesqr+Escol,
                 data =  data_clean_ocu,
                 trControl=trainControl(method = "cv",number = 5),
                 method="lm")
model_6CV<-train(y_total_m~exp,
                 data =  data_clean_ocu,
                 trControl=trainControl(method = "cv",number = 5),
                 method="lm")
model_7CV<-train(y_total_m~poly(exp, 2),
                 data =  data_clean_ocu,
                 trControl=trainControl(method = "cv",number = 5),
                 method="lm")
model_8CV<-train(y_total_m~Escol+poly(exp, 2)+sex,
                 data =  data_clean_ocu,
                 trControl=trainControl(method = "cv",number = 5),
                 method="lm")
model_9CV<-train(y_total_m~Escol+poly(exp, 2)+sex+age,
                 data =  data_clean_ocu,
                 trControl=trainControl(method = "cv",number = 5),
                 method="lm")

#Numeral c. LOOCV
dim(data_clean_ocu)#Hay 13504 observaciones

#Falta validar ciclo que funcione bien @@@@@@@@@@@@@@@@@@@@@@@@@
for (i in 1:13504) {
  #Establecer BD
  BD_train_LOOCV<-data_clean_ocu[-c(i),]
  dim(BD_train_LOOCV)
  BD_test_LOOCV<-data_clean_ocu[c(i),]
  dim(BD_test_LOOCV)
  #Modelo entrenamiento
  model_9LOOCV<-lm(y_total_m~Escol+poly(exp, 2)+sex+age,data = BD_train_LOOCV)
  #Modelo fuera de muestra
  BD_test_LOOCV$model_9LOOCV<-predict(model_9LOOCV,newdata = BD_test_LOOCV)
  #MSE
  with(BD_test_LOOCV,mean((y_total_m-model_9LOOCV)^2))
  # falta almacenar el MSE en una variable @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@q
  }

