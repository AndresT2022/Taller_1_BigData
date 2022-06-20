#install.packages("rvest") 
install.packages("stringr")
install.packages("ggvis")
install.packages("knitr")
install.packages("jsonlite")

rm(list=ls())
library(stringr)
library(plyr)
library(ggvis)
library(knitr)
library(rvest)
library(tidyverse)
library(glue)
library(dplyr)
library(jsonlite)
library("writexl")

# mi_pagina <- "https://ignaciomsarmiento.github.io/GEIH2018_sample/"
# browseURL(mi_pagina)
# mi_html<-read_html(mi_pagina) 
# mi_html %>% html_node(xpath = '/html/body/div/div/div[2]/div/table') %>% html_text()
## se identifica problema, el elemento está vacio

data <- data.frame()
for (i in 1:10){           
  x=paste0("https://ignaciomsarmiento.github.io/GEIH2018_sample/pages/geih_page_",i,".html")
  print(x)
  tabla <- read_html(x)%>% html_table() %>% .[[1]]
  data <- rbind.data.frame(data,tabla)
}

write_xlsx(data,"C:\\Users\\DELL\\OneDrive - Universidad de los Andes\\MECA 2022_2023\\BIGDATA\\TALLERES\\Taller_1\\Database.xlsx")

 

