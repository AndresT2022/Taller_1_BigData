#install.packages("recipes") 
#install.packages('fastDummies')
#install.packages("popgraph")
#install.packages("lfe")
rm(list=ls()) 
library(tidyverse)
library(mosaic)
library(recipes)
library(openxlsx)
require("fabricatr")
require("stargazer") 
require("boot")
require("fastDummies")
require("dplyr")
library(pacman)
library(lfe)

#Carga Base de datos previamente organizada
#data <- read.xlsx("C:\\Users\\DELL\\OneDrive - Universidad de los Andes\\MECA 2022_2023\\BIGDATA\\TALLERES\\Taller_1\\Database.xlsx",sheet="Sheet1")
data <- read.xlsx("D:\\Documents\\Andres\\ANDES\\2.5\\Taller_1_BigData\\Database.xlsx",sheet="Sheet1")

#Creacion de variables
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
#Limpieza de datos
## Data Cleaning: Se toma la base se seleccionan 28 potenciales variables explicativas. La limpieza consiste en
#a la vez elegir elementos de la muestra que cumplan con la condicional de estar ocupados, ser mayores de 18 años y vivir en Bogota (usar el dominio BOGOTA)

data_clean <- select(data, "directorio","secuencia_p","orden","clase",
                     "dominio","mes","estrato1","sex","age", "p6210", "maxEducLevel","regSalud","cotPension",
                     "sizeFirm","oficio","wap","ocu","dsi","pea","inac",	"totalHoursWorked","formal",
                     "informal","cuentaPropia","microEmpresa","college","Escol","y_total_m","y_total_m_ha","LnIng","relab") 
data_clean_ocu  <- subset(data_clean,data_clean$ocu == 1 & data_clean$age >= 18 & data_clean$age <= 80  & 
                            data_clean$dominio == "BOGOTA" ) %>% drop_na()

data_clean_ocu$agesqr <- data_clean_ocu$age^2

# Creación variable Experiencia
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

## Estimar regresion con variables de control 

data_clean_ocu_fem_dum <- data_clean_ocu
data_clean_ocu_fem_dum$female <- with(data_clean_ocu_fem_dum, ifelse(
  sex == 1, 0, 1)) 
data_clean_ocu_fem_dum<- subset(data_clean_ocu_fem_dum, data_clean_ocu_fem_dum$female==1)
data_clean_ocu_male_dum<- subset(data_clean_ocu, data_clean_ocu$sex==1)
data_clean_ocu_male_dum$male <- with(data_clean_ocu_male_dum, ifelse(
  sex == 1, 0, 1)) 

#data_clean_ocu_fem_dum <- dummy_cols(data_clean_ocu_fem_dum, select_columns = 'relab')
#data_clean_ocu_male_dum <- dummy_cols(data_clean_ocu_male_dum, select_columns = 'relab')

data_clean_ocu_fem_dum$female_asalariado <- with(data_clean_ocu_fem_dum, ifelse(
  relab == 5 ,1, 0)) 
#data_clean_ocu_fem_dum$female_empleador <- with(data_clean_ocu_fem_dum, ifelse(
#  oficio == 9 ,1, 0)) 

data_clean_ocu_male_dum$male_asalariado <- with(data_clean_ocu_male_dum, ifelse(
  relab == 5 ,1, 0)) 
#data_clean_ocu_male_dum$male_empleador <- with(data_clean_ocu_male_dum, ifelse(
#  oficio == 9 ,1, 0)) 

reg_gap_oficio <- lm(LnIng~  age + agesqr+ female + oficio , data=data_clean_ocu_fem_dum)
reg_ingreso_oficio <- lm(LnIng~  age + agesqr + oficio , data=data_clean_ocu_male_dum)

data_clean_ocu_fem_dum$yhat_reg_gapoficio <- predict(reg_gap_oficio)
data_clean_ocu_male_dum$yhat_reg_gapmaleoficio <- predict(reg_ingreso_oficio)

#plot(data_clean_ocu_fem_dum$age,data_clean_ocu_fem_dum$yhat_reg_gapoficio)

#plot(data_clean_ocu_male_dum$age,data_clean_ocu_male_dum$yhat_reg_gapmaleoficio)

stargazer(reg_gap_oficio,type="text") 


data_oficio <- ggplot(data_clean_ocu_fem_dum,aes(x=age))+
                geom_point(aes(y=yhat_reg_gapoficio,colour="Femenino"), size=1)+
                geom_point(data=data_clean_ocu_male_dum,aes(y=yhat_reg_gapmaleoficio,colour="Masculino"), size=1)+
                theme_classic()+
                facet_grid()+
                ggtitle("Modelo Ingreso analizado por género y oficio")+
                theme(plot.title = element_text(hjust = 0.5))+
                labs(x="Edad", y="Ingreso")+
                scale_color_manual(name = "Género", values = c("Masculino" = "blue", "Femenino" = "purple"))

data_oficio

to_matrix <- function(the_df, vars) {
  # Create a matrix from variables in var
  new_mat <- the_df %>%
    # Select the columns given in 'vars'
    select_(.dots = vars) %>%
    # Convert to matrix
    as.matrix()
  # Return 'new_mat'
  return(new_mat)
}

resid_ols <- function(data, y_var, X_vars, intercept = TRUE) {
  # Require the 'dplyr' package
  require(dplyr)
  # Create the y matrix
  y <- to_matrix(the_df = data, vars = y_var)
  # Create the X matrix
  X <- to_matrix(the_df = data, vars = X_vars)
  # If 'intercept' is TRUE, then add a column of ones
  if (intercept == T) {
    # Bind a column of ones to X
    X <- cbind(1, X)
    # Name the new column "intercept"
    colnames(X) <- c("intercept", X_vars)
  }
  # Calculate the sample size, n
  n <- nrow(X)
  # Calculate the residuals
  resids <- (diag(n) - X %*% solve(t(X) %*% X) %*% t(X)) %*% y
  # Return 'resids'
  return(resids)
}

b_ols <- function(data, y_var, X_vars, intercept = TRUE) {
  # Require the 'dplyr' package
  require(dplyr)
  # Create the y matrix
  y <- to_matrix(the_df = data, vars = y_var)
  # Create the X matrix
  X <- to_matrix(the_df = data, vars = X_vars)
  # If 'intercept' is TRUE, then add a column of ones
  if (intercept == T) {
    # Bind a column of ones to X
    X <- cbind(1, X)
    # Name the new column "intercept"
    colnames(X) <- c("intercept", X_vars)
  }
  # Calculate beta hat
  beta_hat <- solve(t(X) %*% X) %*% t(X) %*% y
  # Return beta_hat
  return(beta_hat)
}

# Steps 1 and 2: Residualize 'LnIng' on 'relab' and an intercept
e_yx <- resid_ols(data = data_clean_ocu_fem_dum, y_var = "LnIng",
                  X_vars = "oficio", intercept = T)
# Steps 3 and 4: Residualize 'agesqr' on 'relab' and an intercept
e_xx <- resid_ols(data = data_clean_ocu_fem_dum, y_var = "agesqr",
                  X_vars = "oficio", intercept = T)
# Combine the two sets of residuals into a data.frame
e_df <- data.frame(e_yx = e_yx[,1], e_xx = e_xx[,1])
# Step 5: Regress e_yx on e_xx without an intercept
b_ols(data = e_df, y_var = "e_yx",
      X_vars = "e_xx", intercept = F)

b_ols(data = data_clean_ocu_fem_dum, y_var = "LnIng", X_vars = c("age", "agesqr","oficio"))
felm(LnIng~ age+ agesqr+ oficio, data=data_clean_ocu_fem_dum) %>% summary()

#Punto 5 Numeral a.1 a a.4:
#Punto 5.a
#Modelos entrenamiento
#Selección muestra de entrenamiento y prueba
library(ISLR2)
set.seed(10101)
data_clean_ocu$Escol[1]

class(data_clean_ocu$Escol[1])

id_train <- sample(1:nrow(data_clean_ocu),size = 0.7*nrow(data_clean_ocu), replace = F)
BD_train<-data_clean_ocu[id_train,]
BD_test<-data_clean_ocu[-id_train,]
model_1<-lm(LnIng~1,data = BD_train)
model_2<-lm(LnIng~age,data = BD_train)
model_3<-lm(LnIng~Escol,data = BD_train)
model_4<-lm(LnIng~age+agesqr,data = BD_train)
model_5<-lm(LnIng~age+agesqr+Escol,data = BD_train)
model_6<-lm(LnIng~exp,data = BD_train)
model_7<-lm(LnIng~poly(exp, 2),data = BD_train)
model_8<-lm(LnIng~Escol+poly(exp, 2)+sex,data = BD_train)
model_9<-lm(LnIng~Escol+poly(exp, 2)+sex+age,data = BD_train)
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
mse01<-with(BD_test,mean((LnIng-model_1)^2))
mse02<-with(BD_test,mean((LnIng-model_2)^2))
mse03<-with(BD_test,mean((LnIng-model_3)^2))
mse04<-with(BD_test,mean((LnIng-model_4)^2))
mse05<-with(BD_test,mean((LnIng-model_5)^2))
mse06<-with(BD_test,mean((LnIng-model_6)^2))
mse07<-with(BD_test,mean((LnIng-model_7)^2))
mse08<-with(BD_test,mean((LnIng-model_8)^2))
mse09<-with(BD_test,mean((LnIng-model_9)^2))
#Grafica MSE
vmse1<-c(mse01,mse02,mse03,mse04,mse05,mse06,mse07,mse08,mse09)
graf2<-ggplot(mapping = aes(x=1:9, y=vmse1))+
  geom_line(color="blue")+
  xlab("Modelos")+
  ylab("MSE")+
  ggtitle("Resumen MSE")+
theme(plot.title = element_text(hjust = 0.5))+
  scale_x_continuous(breaks = seq(1,9,1))
graf2

#Numeral a.V:
ujs<-c()
hjs<-c()
alphas <- c()
for (j in 1:nrow(BD_test)) {
  uj <- model_9$residual[j]
  hj <- lm.influence(model_9)$hat[j]
  alpha <- uj/(1-hj)
  alphas <- c(alphas, alpha)
  ujs <- c(ujs, uj)
  hjs <- c(hjs, hj)
}

alpha
alphas
ggplot(BD_test, aes(x=alphas, y=LnIng))+
  geom_point(color="red")+
  theme_classic()

#Punto 5b. K-fold cross-validation.
install.packages("caret")
library(dplyr)
library(caret)
#modelos
model_1CV<-train(LnIng~.,
                 data =  data_clean_ocu,
                 trControl=trainControl(method = "cv",number = 5),
                 method="null")

model_1CV<-train(LnIng~1,
                 data =  data_clean_ocu,
                 trControl=trainControl(method = "cv",number = 5),
                 method="null")
model_2CV<-train(LnIng~age,
                 data =  data_clean_ocu,
                 trControl=trainControl(method = "cv",number = 5),
                 method="lm")
model_3CV<-train(LnIng~Escol,
                 data =  data_clean_ocu,
                 trControl=trainControl(method = "cv",number = 5),
                 method="lm")
model_4CV<-train(LnIng~age+agesqr,
                 data =  data_clean_ocu,
                 trControl=trainControl(method = "cv",number = 5),
                 method="lm")
model_5CV<-train(LnIng~age+agesqr+Escol,
                 data =  data_clean_ocu,
                 trControl=trainControl(method = "cv",number = 5),
                 method="lm")
model_6CV<-train(LnIng~exp,
                 data =  data_clean_ocu,
                 trControl=trainControl(method = "cv",number = 5),
                 method="lm")
model_7CV<-train(LnIng~poly(exp, 2),
                 data =  data_clean_ocu,
                 trControl=trainControl(method = "cv",number = 5),
                 method="lm")
model_8CV<-train(LnIng~Escol+poly(exp, 2)+sex,
                 data =  data_clean_ocu,
                 trControl=trainControl(method = "cv",number = 5),
                 method="lm")
model_9CV<-train(LnIng~Escol+poly(exp, 2)+sex+age,
                 data =  data_clean_ocu,
                 trControl=trainControl(method = "cv",number = 5),
                 method="lm")
model_2CV
model_3CV
model_4CV
model_5CV
model_6CV
model_7CV
model_8CV
model_9CV

#Numeral c. LOOCV
data_clean_ocu$MSE_LOOCV <- 1
for (i in 1:nrow(data_clean_ocu)) {
  #Establecer BD
  BD_train_LOOCV<-data_clean_ocu[-c(i),]
  dim(BD_train_LOOCV)
  BD_test_LOOCV<-data_clean_ocu[c(i),]
  dim(BD_test_LOOCV)
  #Modelo entrenamiento
  model_9LOOCV<-lm(LnIng~Escol+poly(exp, 2)+sex+age,data = BD_train_LOOCV)
  #Modelo fuera de muestra
  BD_test_LOOCV$model_9LOOCV<-predict(model_9LOOCV,newdata = BD_test_LOOCV)
  #MSE
  data_clean_ocu$MSE_LOOCV[i]<-with(BD_test_LOOCV,mean((y_total_m-model_9LOOCV)^2))
  }
mean(data_clean_ocu$MSE_LOOCV)

