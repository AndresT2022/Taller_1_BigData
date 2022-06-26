require(pacman)

# usar la función p_load de pacman para instalar/llamar las librerías de la clase
p_load(rio) # Librería para importar datos 
p_load(tidyverse) # Librería para limpiar datos
p_load(e1071) # Tiene la función para calcular skewness
p_load(EnvStats) # Transformación Box-Cox
p_load(tidymodels) # Modelos ML
p_load(ggplot2) # Librería para visualizar datos
p_load(scales) # Formato de los ejes en las gráficas
p_load(ggpubr) # Combinar gráficas
p_load(knitr) # Tablas dentro de Rmarkdown
p_load(kableExtra) # Tablas dentro de Rmarkdown


install.packages("recipes")

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

data <- read.xlsx("C:\\Users\\DELL\\OneDrive - Universidad de los Andes\\MECA 2022_2023\\BIGDATA\\TALLERES\\Taller_1\\Database.xlsx", sheet = "Sheet1")

data$Escol <- with(data, ifelse(
  maxEducLevel == 1, 0, ifelse(
    maxEducLevel == 2, 2, ifelse(
      maxEducLevel == 3, 6, ifelse(
        maxEducLevel == 4, 7, ifelse(
          maxEducLevel == 5, 12, ifelse(
            maxEducLevel == 6, 13, ifelse(
              maxEducLevel == 7, 19, "Estudien vagos"
            )
          )
        )
      )
    )
  )
))

summary(data$Escol)
data_clean <- select(
  data, "Escol", "directorio", "secuencia_p", "orden", "clase",
  "dominio", "mes", "estrato1", "sex", "age", "p6210", "maxEducLevel", "regSalud", "cotPension",
  "sizeFirm", "oficio", "wap", "ocu", "dsi", "pea", "inac", "totalHoursWorked", "formal",
  "informal", "cuentaPropia", "microEmpresa", "college", "y_total_m"
)
data_clean_ocu <- subset(data_clean, data_clean$ocu == 1 & data_clean$age >= 18 &
  data_clean$dominio == "BOGOTA") %>% drop_na()
# age square
data_clean_ocu$agesqr <- data_clean_ocu$age^2



# Escolaridad 
# Ninguno 2, Basica primaria 7, basica secundaria 13, tecnico sin titulo 13
# tecnico con titulo 14, tecnologico sin 14, tecnologico con 16, universitario sin 16,
# universitario con 21, postgrado 

# Convertir Escol a numeric
data_clean_ocu <- data_clean_ocu %>% mutate(Escol=as.numeric(Escol))
data_clean_ocu <- data_clean_ocu %>% mutate(age=as.numeric(age))

data_clean_ocu$exp <- data_clean_ocu$age-5- as.numeric(data_clean_ocu$Escol)
data_clean_ocu$exp <- with(data_clean_ocu, ifelse(
  data_clean_ocu$exp < 0,0,data_clean_ocu$exp
))


cantidad_na <- sapply(data_clean_ocu, function(x) sum(is.na(x)))
cantidad_na <- data.frame(cantidad_na)
porcentaje_na <- cantidad_na/nrow(data_clean_ocu)
porcentaje_na


## + geometry
ggplot(data = data_clean_ocu , mapping = aes(x = exp , y = y_total_m)) +
  geom_point(col = "red" , size = 0.5)
ggplot(data = data_clean_ocu , mapping = aes(x = age , y = y_total_m)) +
  geom_point(col = "red" , size = 0.5)
ggplot(data = data_clean_ocu, mapping = aes(x=exp)) +
  geom_histogram(binwidth = 1)

ggplot(data_clean_ocu, aes(x= Escol)) + geom_bar(width=0.5, colour="red", fill="skyblue") +
  geom_text(aes(label=..count..), stat='count',position=position_dodge(0.9), vjust=-0.5,  size=5.0)


## GIH<-data.frame(age=runif(30,18,80))
#GIH<- GIH %>% mutate(age2=age^2,
 #                    income=rnorm(30,mean=12+0.06*age-0.001*age2))                
#reg_1<-lm(income~age+age2,GIH)
#ggplot(GIH , mapping = aes(x = age , y = predict(reg_1))) +
#  geom_point(col = "red" , size = 0.5)


# Bootstrap
library(ISLR2)
set.seed(10101)

data_clean_ocu 
training<-sample(1:nrow(data_clean_ocu), size=0.7*nrow(data_clean_ocu))


lm.fit0<-lm(y_total_m~1, subset = training)
attach(data_clean_ocu)
mse1<-mean((y_total_m - predict(lm.fit0, data_clean_ocu))[-training]^2)

lm.fit<- lm(y_total_m~age, data=data_clean_ocu, subset = training)
#mean squared error
attach(data_clean_ocu)
mse2<-mean((y_total_m - predict(lm.fit, data_clean_ocu))[-training]^2)

lm.fit1<- lm(y_total_m~Escol, data=data_clean_ocu, subset = training)
attach(data_clean_ocu)
mse3<-mean((y_total_m - predict(lm.fit1, data_clean_ocu))[-training]^2)

lm.fit2<- lm(y_total_m~age+agesqr, data=data_clean_ocu, subset = training)
attach(data_clean_ocu)
mse4<-mean((y_total_m - predict(lm.fit2, data_clean_ocu))[-training]^2)

lm.fit3<- lm(y_total_m~age+agesqr+Escol, data=data_clean_ocu, subset = training)
attach(data_clean_ocu)
mse5<-mean((y_total_m - predict(lm.fit3, data_clean_ocu))[-training]^2)

lm.fit4<- lm(y_total_m~poly(Escol, 2), data=data_clean_ocu, subset = training)
attach(data_clean_ocu)
mse6<-mean((y_total_m - predict(lm.fit4, data_clean_ocu))[-training]^2)

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
mse7<-mean((y_total_m - predict(lm.fit9, data_clean_ocu))[-training]^2)

lm.fit10<- lm(y_total_m~poly(Escol, 3)+exp+poly(exp, 4)+sex+age+age:sex+sex:exp+sex:Escol, data=data_clean_ocu, subset = training)
attach(data_clean_ocu)
mse8<-mean((y_total_m - predict(lm.fit10, data_clean_ocu))[-training]^2)

vecmse<-c(mse1,mse2,mse3,mse4,mse5,mse6,mse7,mse8)

graf1<-ggplot(mapping=aes(x=1:8, y=vecmse))+
  geom_line()

mse01<-with(BD_test,mean((y_total_m-model_1)^2))
mse02<-with(BD_test,mean((y_total_m-model_2)^2))
mse03<-with(BD_test,mean((y_total_m-model_3)^2))
mse04<-with(BD_test,mean((y_total_m-model_4)^2))
mse05<-with(BD_test,mean((y_total_m-model_5)^2))
mse06<-with(BD_test,mean((y_total_m-model_6)^2))
mse07<-with(BD_test,mean((y_total_m-model_7)^2))
mse08<-with(BD_test,mean((y_total_m-model_8)^2))
mse09<-with(BD_test,mean((y_total_m-model_9)^2))

vmse1<-c(mse01,mse02,mse03,mse04,mse05,mse06,mse07,mse08,mse09)

graf2<-ggplot(mapping = aes(x=1:9, y=vmse1))+
  geom_line()

graf2

#---- k_fold

set.seed(10101)
cv.error.5 <- rep(0, 5)
for (i in 1:5) {
  glm.fit <- glm(y_total_m~poly(exp, i), data = data_clean_ocu )
  cv.error.5[i] <- cv.glm (data_clean_ocu, glm.fit, K = 5)$delta[1]
  }
cv.error.5

ggplot(mapping = aes(x=cv.error.5, y=1:5))+
  geom_line()


glm.fit1 <- glm(y_total_m~Escol+exp+poly(exp, 2)+sex+age, data = data_clean_ocu)
cv.error <- cv.glm(data_clean_ocu, glm.fit1)$delta[1]
cv.error
glm.fit1

