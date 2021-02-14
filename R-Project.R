library(httr)
library(jsonlite)
library(tidyverse)
library(ckanr)
library(rvest)
library(RCurl)
library(readr)
library(rio)

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

Dataset <- read_csv(data[[4]])

write.csv(Dataset, ".csv").

download.file("https://cdn.buenosaires.gob.ar/datosabiertos/datasets/ministerio-de-justicia-y-seguridad/delitos/delitos_2019.csv", "Baires.csv")


write.csv(Dataset, "Baires.csv")


#SEE PROBLEMS
#SEE PROBLEMS

Dataset <- read_csv("Baires.csv")

Dataset$row <- 1:nrow(Dataset)

Problems <- problems(Dataset)

Problems$row <- as.numeric(Problems$row)
Dataset$row <- as.numeric(Dataset$row)

Dataset <- Dataset %>% 
mutate(subtipo_delito = ifelse (row %in% Problems$row, Problems$actual, "NA"))

head(Dataset)

#TIMEFRAME

Security_BA <- Dataset %>% 
  mutate(timeframe = ifelse (franja_horaria >= "7", "Morning",
                             ifelse (franja_horaria <= "18", "Afternoon", 
                                     ifelse(franja_horaria > "18" & franja_horaria < "23", "Evening", 
                                           "Night"))))
(Security_BA$timeframe)
         
#Date format --> data belongs to 2019, therefore the column year is nonsense

is.character(Security_BA$fecha)

Security_BA$fecha <- as.Date.character(Security_BA$fecha, format = c("%d-%m-%Y"))

Security_BA <- Security_BA %>%
  separate(fecha, sep="-", into = c("year", "month", "day"))


write.csv(Security_BA, "Security_Baires.csv")


library(lubridate)

#Visualization 1 | Distributon

library(ggplot2)

Distribution <- Security_BA %>%
  group_by(month, timeframe, comuna, barrio) %>% 
  count()

Distribution %>% 
  ggplot(aes(x = month, y = n))+
  geom_col(position="dodge")+
  facet_wrap(~ timeframe)+
  theme_bw()


Distribution %>% 
  ggplot(aes(x = month, y = n, fill = comuna, color = comuna))+
  geom_boxplot()
theme_bw()

Security_BA %>% 
  group_by(franja_horaria, comuna, month) %>% 
  count() %>% 
ggplot(aes(x = n, y = franja_horaria))+
geom_col(position = "stack")+
  facet_wrap()
  theme_bw()

  
  #Number of crimes x comuna
  
  Security_BA %>% 
    group_by(comuna, barrio) %>% 
    count()

# Type of crimes x comuna
  
  Security_BA %>% 
    group_by(tipo_delito, month, comuna, barrio) %>% 
    count()%>%
    ggplot(aes(x = month, y = n, fill=tipo_delito, color=tipo_delito))+
    geom_col(position="stack")+
   facet_wrap(~ comuna) 
    theme_bw()


#map visualization
    
  install.packages("maptools")
  
library(maptools)
install.packages("mapsapi")
library(mapsapi)
install.packages("openstreetmaps")

ggmap()

  #Type of crimes x comuna x timeframe 
  
  Security_BA %>% 
    group_by(comuna, tipo_delito, timeframe) %>% 
    count()
  
  
 argentina <- c(left = -125, bottom = 25.75, right = -67, top = 49)
  get_stamenmap(argentina, zoom = 5, maptype = "toner-lite") %>% ggmap() 

  place <- "Ciudad Autónoma de Buenos Aires"
  get_googlemap(place, zoom=10)
  
  
#IMPORT INFORMATION MILANO
  
url2 <- ("https://dati.comune.milano.it/api/3/action/datastore_search?q=2019&resource_id=8b03b9f2-f2d7-4408-b439-bc6efc093cff")
page2 <- GET(url2) # API request
status_code(page2) # # Check that the call is successful
datalist <- fromJSON(url2) 
Milano_2019 <- datalist$result$records
view(Milano_2019) 
as.data.frame(Milano_2019)

translateR::translate(Milano_2019, Milano_2019$Reati_denunciati_tipologia,  )

translateR::translate()
translateR::getMicrosoftLanguages()