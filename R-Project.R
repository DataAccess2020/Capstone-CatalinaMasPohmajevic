library(httr)
library(jsonlite)
library(tidyverse)
install.packages("ckanr")
library(ckanr)
library(rvest)
library(RCurl)


#Roadmap

#1. SELECT INFORMATION FROM BUENOS AIRES, MILAN AND LONDON
#2. ORGANIZE & TIDY UP THE INFORMATION
#3. TRANSLATE THE INFORMATION? 
#4. GROUP CRIMES BY CATEGORIES & COMAPRE --> VISUALIZATIONS
#5. COMPARE CRIMES IN TIME FRAMES  --> VISUALIZATIONS
#6. CRIME RATE / INHABITANTS
#7. CONCLUSIONS

#DOWNLOAD CRIMES IN BUENOS AIRES

url <- paste0("https://data.buenosaires.gob.ar/api/3/action/",
              "package_show?",
              "id=delitos")

page <- GET(url) # API request
status_code(page) # # Check that the call is successful

datalist <- fromJSON(url) 
data <- datalist$result$resources$url
(data) 

datasets <- as.vector(unlist(data))
(datasets)

Security_BA<- read_csv(data[[4]]) 

download.file("https://cdn.buenosaires.gob.ar/datosabiertos/datasets/ministerio-de-justicia-y-seguridad/delitos/delitos_2019.csv", "Baires.csv")

Security_BA <- read_csv("Baires.csv")

#SEE PROBLEMS

Problems <- problems(Security_BA)

Security_BA
Security_BA%>% 
  as.data.frame(Security_BA)
head(Security_BA)


        