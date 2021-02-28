library(httr)
library(jsonlite)
library(ckanr)
library(rvest)
library(RCurl)
library(readr)
library(tidyverse)
library(dplyr)
library(rio)
library(stringr)
library(ggplot2)
library(patchwork)
library(leaflet)
library(maps)
library(leaflet.extras)
library(leaflet)
library(leafsync)
library(rmdfiltr)
library(devtools)
library(magrittr)

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

BA17 <- read_csv(data[[2]])
BA18 <- read_csv(data[[3]])
BA19 <- read_csv(data[[4]])
#SEE PROBLEMS
problems19 <- problems(BA19)

write.csv(BA17, "Baires17.csv")
write.csv(BA18, "Baires18.csv")
write.csv(BA19, "Baires19.csv")
write.csv(problems19, "Problems19.csv")

BA17 <- read.csv("Baires17.csv")
BA18 <- read.csv("Baires18.csv")
BA19 <- read.csv("Baires19.csv")

Crimes_BA19 <- BA19 %>% 
  select(BA19$id, fecha, franja_horaria, tipo_delito, comuna, barrio)
  select(id, fecha, franja_horaria, tipo_delito, comuna, barrio) 

Crimes_BA18 <- BA18 %>% 
  select(id, fecha, franja_horaria, tipo_delito, comuna, barrio) 

Crimes_BA17 <- BA17 %>% 
  select(id, fecha, franja_horaria, tipo_delito, comuna, barrio)


#TIMEFRAME

Crimes_BA19 <- as_tibble(Crimes_BA19)

Crimes_BA19 <- Crimes_BA19 %>% 
  mutate(timeframe = ifelse (franja_horaria >= 7 & franja_horaria <= 13, "Morning",
                             ifelse (franja_horaria > 13 & franja_horaria <= 18, "Afternoon", 
                                     ifelse(franja_horaria > 18 & franja_horaria < 22, "Evening", "Night"))))

Crimes_BA18 <- Crimes_BA18 %>% 
  mutate(timeframe = ifelse (franja_horaria >= 7 & franja_horaria <= 13, "Morning",
                             ifelse (franja_horaria > 13 & franja_horaria <= 18, "Afternoon", 
                                     ifelse(franja_horaria > 18 & franja_horaria < 22, "Evening", "Night"))))

is.numeric(Crimes_BA17$franja_horaria) 

Crimes_BA17$franja_horaria <- as.numeric(Crimes_BA17$franja_horaria)

Crimes_BA17 <- Crimes_BA17 %>% 
  mutate(timeframe = ifelse (franja_horaria >= 7 & franja_horaria <= 13, "Morning",
                             ifelse (franja_horaria > 13 & franja_horaria <= 18, "Afternoon", 
                                     ifelse(franja_horaria > 18 & franja_horaria < 22, "Evening", "Night"))))


#Date Format

library(lubridate)


is.character(Security_BA$fecha)
Crimes_BA17$fecha <- lubridate::as_date(Crimes_BA17$fecha)
Crimes_BA18$fecha <- lubridate::as_date(Crimes_BA18$fecha)
Crimes_BA19$fecha <- as.Date(as.character(Crimes_BA19$fecha), format = "%d-%m-%y")


Crimes_BA19 <- Crimes_BA19 %>%
  separate(fecha, sep="-", into = c("year", "month", "day"))
Crimes_BA18 <- Crimes_BA18 %>%
  separate(fecha, sep="-", into = c("year", "month", "day"))
Crimes_BA17 <- Crimes_BA17 %>%
  separate(fecha, sep="-", into = c("year", "month", "day"))

#Visualization 1 | Distributon

library(ggplot2)

Dist19 <- Crimes_BA19 %>%
  group_by(year) %>% 
  count() 
Dist18 <-Crimes_BA18 %>%
  group_by(year) %>% 
  count() 
DistributionYears <- full_join(Dist19, Dist18)
Dist17 <- Crimes_BA17 %>%
  group_by(year) %>% 
  count() 

DistributionYears <- full_join(Dist17, DistributionYears)

DistributionYears %>% 
  ggplot(aes(x = year, y = n))+
  geom_col(fill = "#00abff")+
  scale_x_discrete(guide = guide_axis(angle = 60))+
  theme_bw()+
  theme(axis.text.x=element_text(size=rel(1)))+
  ggtitle('2019')+
  xlab(" ")+
  ylab(" ")
labelsmonths <- c("January", "February", "March", "April", "May", "June", "July", "August", "September", "October", "November", "December")

Distribution19 <- Crimes_BA19 %>%
  group_by(month) %>% 
  count() %>% 
  ggplot(aes(x = month, y = n))+
  geom_col(fill = "#f8766d")+
  scale_x_discrete(labels = labelsmonths, guide = guide_axis(angle = 60))+
  theme_bw()+
  theme(axis.text.x=element_text(size=rel(1)))+
  ggtitle('2019')+
  xlab(" ")+
  ylab(" ")

Distribution18 <- Crimes_BA18%>%
  group_by(month) %>% 
  count() %>% 
  ggplot(aes(x = month, y = n))+
  geom_col(fill = "#7cae00")+
  scale_x_discrete(labels = labelsmonths, guide = guide_axis(angle = 60))+
  theme_bw()+
  theme(axis.text.x=element_text(size=rel(1)))+
  ggtitle('2018')+
  xlab("Month")+
  ylab(" ")


Distribution17 <- Crimes_BA17%>%
  group_by(month) %>% 
  count() %>% 
  ggplot(aes(x = month, y = n))+
  geom_col(fill = "#01bfc4")+
  scale_x_discrete(labels = labelsmonths, guide = guide_axis(angle = 60))+
  theme_bw()+
  theme(axis.text.x=element_text(size=rel(1)))+
  ggtitle('2017')+
  ylab("Number of Crimes")+
  xlab(" ")

Distribution <- (Distribution17 | Distribution18 | Distribution19)

Distribution + plot_annotation(
  title = 'Crimes Recorded among the years in the City of Buenos Aires', 
  theme=theme(plot.title=element_text(size=8))) & 
  theme(text=element_text("mono"))

#Crime per timeframe



labeltimes <- c("Afternoon", "Morning", "Evening", "Night")

Timeframes17 <- Crimes_BA17 %>% 
  group_by(timeframe, month) %>% 
  count() %>% 
  na.omit() %>% 
  ggplot(aes(x = timeframe, y = n, fill = timeframe))+
  geom_col( position = "dodge")+
  scale_x_discrete(labels = labeltimes, guide = guide_axis(angle = 60))+
  theme_bw()+
  theme(legend.position = "none", axis.text.x=element_text(size=rel(1)))+
  ggtitle('2017')+
  ylim(0,6000)+
  ylab("Number of Crimes")+
  xlab(" ")

Timeframes18 <- Crimes_BA18 %>% 
  group_by(timeframe, month) %>% 
  count() %>% 
  na.omit() %>% 
  ggplot(aes(x = timeframe, y = n, fill = timeframe))+
  geom_col(position = "dodge")+
  scale_x_discrete(labels = labeltimes, guide = guide_axis(angle = 60))+
  theme_bw()+
  theme(legend.position = "none",axis.text.x=element_text(size=rel(1)))+
  ggtitle('2018')+
  ylim(0,6000)+
  ylab("")+
  xlab("Timeframe")

Timeframes19 <- Crimes_BA19 %>% 
  group_by(timeframe, month) %>% 
  count() %>% 
  na.omit() %>% 
  ggplot(aes(x = timeframe, y = n, fill = timeframe))+
  geom_col(position = "dodge")+
  scale_x_discrete(labels = labeltimes, guide = guide_axis(angle = 60))+
  theme_bw()+
  theme(legend.position = "none", axis.text.x=element_text(size=rel(1)))+
  ylim(0,6000)+
  ggtitle('2019')+
  ylab(" ")+
  xlab(" ")

Graph_Timeframes <-  ( Timeframes17 | Timeframes18 | Timeframes19)

Graph_Timeframes +
  plot_annotation(
    title = 'Crimes Recorded per Timeframe in the City of Buenos Aires', theme=theme(plot.title=element_text(size=8))) & theme(text=element_text("mono"))

#Type of crime committed

CrimeYears <- Crimes_BA17 %>% 
  group_by(year, tipo_delito) %>% 
  na.omit() %>% 
  count() 

CrimeYears2 <- Crimes_BA18 %>% 
  group_by(year, tipo_delito) %>% 
  na.omit() %>% 
  count() 

CrimeYears4 <- Crimes_BA19 %>% 
  group_by(year, tipo_delito) %>% 
  na.omit() %>% 
  count() 

CrimeYears3 <- full_join(CrimeYears, CrimeYears2)

CrimeYears <- full_join(CrimeYears4, CrimeYears3)

CrimeYears %>% 
  ggplot(aes(x = year, y = n, color = tipo_delito, fill = tipo_delito))+
  geom_col(position="dodge")+
  theme_bw()+
  ylab("Number of Crimes")+
  xlab("Year")+
  theme(legend.position= "right")+
  plot_annotation(title = "Crimes' Categories in the City of Buenos Aires",theme=theme(plot.title = element_text(size=9))) & theme(text=element_text("mono"))



#map visualization

SafeArea17 <- Crimes_BA17  %>%
  group_by(comuna) %>% 
  count() %>% 
  ggplot(aes(x = comuna, y = n, fill = comuna == "1" | comuna == "4" | comuna == "14" ),position = "dodge")+
  geom_col()+  
  xlim(0,15)+
  ylim(0,20000)+
  scale_x_continuous(breaks = seq(1,15, by = 2))+
  ylab("Number of Crimes Committed")+
  xlab(" ")+
  ggtitle("2017")+
  theme(legend.position = "none", axis.text.x=element_text(size=rel(0.8)))

SafeArea18 <- Crimes_BA18  %>%
  group_by(comuna) %>% 
  count() %>% 
  ggplot(aes(x = comuna, y = n, fill = comuna == "1" | comuna == "3" | comuna == "4" ), position = "dodge")+
  geom_col()+
  ylim(0,20000)+
  scale_x_continuous(breaks = seq(1,15, by = 2))+
  xlab("Comuna")+
  ylab(" ")+
  ggtitle("2018")+
  theme(legend.position = "none", axis.text.x=element_text(size=rel(0.8)))

SafeArea19 <- Crimes_BA19  %>%
  group_by(comuna) %>% 
  count() %>% 
  ggplot(aes(x = comuna, y = n, fill = comuna == "1" | comuna == "3" | comuna == "4" ), position = "dodge")+
  geom_col()+
  xlab(" ")+
  ylab(" ")+
  ylim(0,20000)+
  ggtitle("2019")+
  scale_x_continuous(breaks = seq(1,15, by = 2))+
  theme(legend.position = "none", axis.text.x=element_text(size=rel(0.8)))

Graph_SafeArea <-  ( SafeArea17 | SafeArea18 |SafeArea19)

Graph_SafeArea + plot_annotation(
  title = 'Crimes Committed by Comuna in City of Buenos Aires', 
  theme=theme(plot.title=element_text(size=9))) & 
  theme(text=element_text("mono"))


Unsafest17 <- read.csv(textConnection("
Comuna,Lat,Long,N
1,-34.6152,-58.3738,16281		
4,-34.6493,-58.3964,10554	
14,-34.5889, -58.4306,10008	
"))

Unsafest18 <- read.csv(textConnection("
Comuna,Lat,Long,N
1,-34.6152,-58.3738,16736	
3,-34.6107,-58.4068,10430
4,-34.6493,-58.3964,9616	
"))

Unsafest19 <- read.csv(textConnection("
Comuna,Lat,Long,N
1,-34.6152,-58.3738,19452
3,-34.6107,-58.4068,11498	
4,-34.6493,-58.3964,10295
"))


Map17 <- leaflet(Unsafest17) %>% 
  addTiles() %>%
  addCircles(lng = ~Long, lat = ~Lat, weight = 1,
             radius = ~sqrt(N) * 10, popup = ~Comuna
  )

Map17 

Map18 <- leaflet(Unsafest18) %>% 
  addTiles() %>%
  addCircles(lng = ~Long, lat = ~Lat, weight = 1,
             radius = ~sqrt(N) * 10, popup = ~Comuna
  )

Map18

Map19 <- leaflet(Unsafest19) %>% 
  addTiles() %>%
  addCircles(lng = ~Long, lat = ~Lat, weight = 1,
             radius = ~sqrt(N) * 10, popup = ~Comuna
  )

Map19

wordcountaddin::word_count("Capstone-Project-CatalinaMasPohmajevic.Rmd")

