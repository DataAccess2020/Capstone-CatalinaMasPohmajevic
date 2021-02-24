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

Problems <- problems(Dataset)

write.csv(Dataset, "Baires.csv")

write.csv(Problems, "Problems.csv")

#SEE PROBLEMS
#SEE PROBLEMS

Dataset <- read_csv("Baires.csv")

Dataset <- Dataset[,-1] 
Dataset <- Dataset[,-6]  

Dataset$row <- 1:nrow(Dataset)

Problems$row <- as.numeric(Problems$row)
Dataset$X1 <- as.numeric(Dataset$X1)

Dataset <- Dataset %>% 
  mutate(subtipo_delito = ifelse (row %in% Problems$row, Problems$actual, "NA"))

head(Dataset)

Dataset$subtipo_delito <- gsub('[[:digit:]]+', 'NA', Dataset$subtipo_delito)




#TIMEFRAME

Security_BA <- Dataset %>% 
  mutate(timeframe = ifelse (franja_horaria >= "7", "Morning",
                             ifelse (franja_horaria <= "18", "Afternoon", 
                                     ifelse(franja_horaria > "18" & franja_horaria < "23", "Evening", 
                                            "Night"))))
(Security_BA$timeframe)

Security_BA$franja_horaria <- as.numeric(Security_BA$franja_horaria)


Security_BA <- Dataset %>% 
  mutate(timeframe = ifelse (franja_horaria >= "7", "Morning",
                             ifelse (franja_horaria >= "13" & franja_horaria < "18", "Afternoon", 
                                     ifelse(franja_horaria >= "18" & franja_horaria < "23", "Evening", 
                                            "Night"))))
(Security_BA$timeframe)
(Security_BA$timeframe)


#Date format --> data belongs to 2019, therefore the column year is nonsense
library(lubridate)


is.character(Security_BA$fecha)

Security_BA$fecha <- as.Date.character(Security_BA$fecha, format = c("%d-%m-%Y"))

Security_BA <- Security_BA %>%
  separate(fecha, sep="-", into = c("year", "month", "day"))


#Visualization 1 | Distributon

library(ggplot2)

Distribution <- Security_BA %>%
  group_by(month, timeframe, comuna, barrio) %>% 
  count()


 Security_BA %>%
  group_by(month) %>% 
  count() %>% 
  ggplot(aes(x = month, y = n))+
  geom_col()+
  theme_bw()


Distribution <- as.data.frame(Distribution)

Distribution 


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
  group_by(timeframe, comuna, month) %>% 
  count()%>% 
  ggplot(aes(x = timeframe, y = n))+
  geom_col(position = "dodge")+
theme_bw()


Security_BA %>% 
  group_by(tipo_delito, month, comuna) %>% 
  count() %>% 
  ggplot(aes(x = month, y = n, color = comune))+
  geom_col(position="dodge")+
  facet_wrap(~ tipo_delito)
theme_bw()

Security_BA %>% 
  group_by(timeframe, comuna, month) %>% 
  count()%>% 
  ggplot(aes(x = timeframe, y = n))+
  geom_col(position = "dodge")+
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


Security_BA %>% 
  group_by(tipo_delito, month, comuna, barrio) %>% 
  count()%>%
  ggplot(aes(x = month, y = n, color = tipo_delito, fill = tipo_delito))+
  geom_col(position="stack")+
  facet_wrap(~ comuna) +
scale_x_discrete(labels = labelsmonths, guide = guide_axis(angle = 90))
theme_bw()

labelsmonths <- c("January", "February", "March", "April", "May", "June", "July", "August", "September", "October", "November", "December")

Security_BA %>% 
  group_by(tipo_delito, subtipo_delito) %>% 
  count()%>%
  ggplot(aes(x = tipo_delito, y = n, color = subtipo_delito, fill = subtipo_delito))+
  geom_col(position="dodge")+
theme_bw()

#Timeframe

Graph_timeframe <-  Security_BA %>%
  group_by(timeframe, month, comuna) %>% 
  count()

TimeFrame1 <- Graph_timeframe %>% 
  ggplot(aes(x = month, y = n, fill = timeframe, color = timeframe))+
  geom_col(position="dodge")+
  facet_wrap(~ timeframe)+
  theme_bw()

#TYPE OF CRIME PER COMUNA

Security_BA %>% 
  group_by(tipo_delito, month, comuna) %>% 
  count() %>% 
  ggplot()+
  geom_col(aes(x = month, y = n, fill = tipo_delito, color = tipo_delito), position = "dodge")+
  theme(legend.position = "top")


#comuna mas peligrosa

Security_BA %>%
  group_by(comuna) %>% 
  count() %>% 
  ggplot()+
  geom_col(aes(x = comuna, y = n), position = "dodge")+
  theme(legend.position = "top")

Comuna1 <- Security_BA %>% 
  filter(comuna == "1")%>% 
  group_by(tipo_delito, month) %>% 
  count() %>% 
  ggplot(aes(x = month , y = n, color = tipo_delito, fill = tipo_delito))+
  geom_col(position = "stack")+
  scale_x_discrete(labels = labelsmonths, "Comuna 1", guide = guide_axis(angle = 90))+
      theme(legend.position = "none")

Comuna3 <- Security_BA %>% 
  filter(comuna == "3")%>% 
  group_by(tipo_delito, month) %>% 
  count() %>% 
  ggplot(aes(x = month , y = n, color = tipo_delito, fill = tipo_delito))+
  geom_col(position = "stack")+
  scale_x_discrete(labels = labelsmonths, "Comuna 3",  guide = guide_axis(angle = 90))+
    theme(legend.position = "none")

Comuna4 <- Security_BA %>% 
  filter(comuna == "4")%>% 
  group_by(tipo_delito, month) %>% 
  count() %>% 
  ggplot(aes(x = month , y = n, color = tipo_delito, fill = tipo_delito))+
  geom_col(position = "stack")+
  scale_y_discrete(lab = "Number of Crime Records")+
  scale_x_discrete(labels = labelsmonths, "Comuna 4", guide = guide_axis(angle = 90))+
theme(legend.position = "right",  legend.title = element_text (size=8), legend.text=element_text(size=8))

(Comuna1 | Comuna3 | Comuna4)

library(ggplot2)
library(patchwork)

#Milano

url2 <- ("https://dati.comune.milano.it/api/3/action/datastore_search?q=2019&resource_id=8b03b9f2-f2d7-4408-b439-bc6efc093cff")

page2 <- GET(url2) # API request
status_code(page2) # # Check that the call is successful

datalist <- fromJSON(url2) 
Milano_2019 <- datalist$result$records
view(Milano_2019) 
as.data.frame(Milano_2019)
view(Milano_2019)
head(Milano_2019)
download.file("https://dati.comune.milano.it/api/3/action/datastore_search?q=2019&resource_id=8b03b9f2-f2d7-4408-b439-bc6efc093cff", "Milano.csv")


names(Milano_2019)[3] <- "Crimes Denounced"
head(Milano_2019)
names(Milano_2019)[4] <- "Count"

#DELETE 56 AND MISSING OBSERVATION 

#MILANO

Milan_2019 <- Milano_2019 %>% 
  slice(-c(56))
Milan_2019 = na.omit(Milan_2019)


#turn count into a numeric vector

Milan_2019$Count <- as.numeric(Milan_2019$Count)

(Milan_2019$`Crimes Denounced`)

Milan_2019$`Crimes Denounced` <- str_trim(Milan_2019$`Crimes Denounced`)

library(stringr)

Milan_2019 <- Milan_2019 %>% 
  mutate(crime_category = ifelse(grepl("Omicidi", `Crimes Denounced`), "Omicidi",
                                 ifelse(grepl("Furti", `Crimes Denounced`), "Furti",
                                        ifelse(grepl("furti", `Crimes Denounced`), "Furti",
                                               ifelse(grepl( "Rapine", `Crimes Denounced`),"Rapine",
                                                      ifelse(grepl( "Incendi", `Crimes Denounced`),"Incendi",
                                                             `Crimes Denounced`))))))


Milan_2019$Count <- as.numeric(Milan_2019$Count)

  
summary(Milan_2019$Count)

hist(Milan_2019$Count)

Milan_2019$Count <- as.numeric(Milan_2019$Count)
                stringsAsFactors = FALSE


Milan_2019 %>% 
  group_by(Count_Recoded) %>% 
  ggplot(aes(x = crime_category, y = Count))+
  geom_col(position = "dodge")+
  coord_flip()+
  facet_wrap(~ Count_Recoded, scales = "free_x")
theme_bw()



facet_wrap(~ crime_category, scales = "free_x")+
  
  
  
  
  
  
  
  
  
  
  #IF I ADD INFO BUENOS AIRES 18


Graph_TimeFrame2 <-  Security_BA18 %>%
  group_by(timeframe, month, comuna) %>% 
  count()


TimeFrame2 <- Graph_TimeFrame2 %>% 
  ggplot(aes(x = month, y = n, fill = timeframe, color = timeframe))+
  geom_col(position="dodge")+
  facet_wrap(~ Timeframe)+
  theme_bw()

(TimeFrame1 | TimeFrame2)















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