Taller_1_BigData
Grupo 2

Introducción

Este trabajo toma la Gran Encuesta de Ingresos y Hogares - GEIH de 2018, realizada por el DANE.
Con este taller se busca dar explicación a partir de la teoría económica y utilizando análisis básico y
herramientas de machine learning para poder captar el concepto de no buscar el mejor ajuste sino la
capacidad del modelo de replicarse y predecir en otros conjuntos muestrales con el menor error posible.

El enfoque de este taller es aplicar la idea anterior, de permitir una mayor variabilidad que tener
un modelo que se ajuste y tenga una alta significancia estadística.

Para aplicar lo anteriormente descrito, se busca analizar varios modelos que expliquen la variable 
dependiente ingreso en función de las variables disponibles en la GEIH, dicho análisis se realizará
por medio de los conceptos vistos en clase.

Desarrollo

1. Obtención y limpieza de datos
   Se obtiene la base de datos GIEH contenida en la página https://ignaciomsarmiento.github.io/GEIH2018sample/. 
   Para extraer esta información se utilizan técnicas de web-scraping. La página no contiene restricciones de acceso para ningún robot.
   Al momento de acceder a las tablas se encuentra que la base de datos no está contenida dentro de la pagína y el elementos aparece vacio 
   cuando se hace la extracción. Sin embargo, la base de datos se encuentra disponible contenida en una página alterna. Es necesario entonces
   identificar el xpath a esa página para extrear los datos.
 ### mi_pagina <- "https://ignaciomsarmiento.github.io/GEIH2018_sample/"
### browseURL(mi_pagina)
### mi_html<-read_html(mi_pagina) 
### mi_html %>% html_node(xpath = '/html/body/div/div/div[2]/div/table') %>% html_text()
### Se identifica problema, el elemento está vacio

Al encontrar la página de donde se extraen los datos es posible hacer el siguiente loop

data <- data.frame()
for (i in 1:10){           
  x=paste0("https://ignaciomsarmiento.github.io/GEIH2018_sample/pages/geih_page_",i,".html")
  print(x)
  tabla <- read_html(x)%>% html_table() %>% .[[1]]
  data <- rbind.data.frame(data,tabla)
}
   
   La limpieza de la base de datos requierió filtar por la edad y estado de ocupación de los participantes, para anilizar únicamente las personas mayores
   de 18 años y estado de ocupación verdadero y habitantes de Bogotá. De esta forma se trabaja con una muestra de habitantes de Bogotá 
   mayores de 18 años considerados ocupados laboralmente. 
   
2. Estimación de modelo y perfiles
   
   Perfil ingreso-edad: Se toma una regresión lineal que evalue la relación entre el ingreso mensual total con la edad
   y se adapta al ajuste de elevar al cuadrado la edad para encontrar un punto de inflexión.

   
   Pequeña conclusión
   
3. Existencias de brechas

   Pequeño párrafo de lo que se hizo
   
   Pequeña conclusión

4. Predicción y comparación de modelos
   
   Se evalúan diferentes modelos utilizando Cross-validation, LOOCV y K-Fold





 
