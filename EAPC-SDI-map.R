library(tidyverse)
library(dplyr)
rm(list=ls())
setwd("C:\\Users\\25470\\Desktop\\BCY\\三个指标EAPC-SDI")

library(sf)
# install.packages("patchwork")
library(patchwork)
library(ggplot2)
library(tidyverse)
library(ggmap)
library(rgdal)
library(maps)
library(dplyr)
library(sf)




data <- read.csv('EAPC-SDI-incidence.csv',header = T)
GBD <- read.csv("EAPC-SDI-incidence.csv")

location <- read.csv("location.csv")
GBD <- left_join(GBD,location,by="location")
colnames(GBD)
map <- st_read("世界国家.shp")
map <- st_set_crs(map,4326)
main_map_data <- left_join(map,GBD,by=c("NAME"="location3"))
colnames(main_map_data)
unique(main_map_data$age)


main_map_data <- main_map_data %>%
  filter(age=="15-39 years") %>%
  filter(year==2019)%>%
  mutate(label = cut(label, breaks = c(0,1,2,3,4,5,6,7,8,9,10),
                     labels = c("+High SDI","+High-middle SDI","+Middle SDI","+Low-middle SDI","+Low SDI",
                                "-High SDI","-High-middle SDI","-Middle SDI","-Low-middle SDI","-Low SDI"),
                     
                    include.lowest = T,right = T))
p1 <-main_map_data %>% 
  ggplot()+ geom_sf(data=main_map_data, 
                    aes(group=NAME,fill=label),
                    colour="black",size = 0.1) + 
  scale_fill_manual(values = c("#A60F16", "#DE2D27", "#FB6A4B", "#FD9272", "#FCBAA0","#07519D","#3282BD", "#6BAFD6","#9DCAE1","#C6DBF0"))+
  theme_void()+
  labs(x="", y="")+
  guides(fill = guide_legend(title='EAPC'))+
  theme(legend.position =  c(0.1,0.3),
          legend.key.height = unit(10, "pt"),
          legend.key.width = unit(15, "pt"),
        panel.grid=element_blank(),
        legend.title = element_text(size=8),
        legend.text = element_text(size = 8))
str(main_map_data)


worldData <-map_data("world")
small_map_data <- GBD %>%
  filter(age=="15-39 years") %>%
  filter(year==2019)
small_map_data$location[small_map_data$location == 'United States of America'] = 'USA'
small_map_data$location[small_map_data$location == 'Russian Federation'] = 'Russia'
small_map_data$location[small_map_data$location == 'United Kingdom'] = 'UK'
small_map_data$location[small_map_data$location == 'Congo'] = 'Republic of Congo'
small_map_data$location[small_map_data$location == "Iran (Islamic Republic of)"] = 'Iran'
small_map_data$location[small_map_data$location == "Democratic People's Republic of Korea"] = 'North Korea'
small_map_data$location[small_map_data$location == "Taiwan (Province of China)"] = 'Taiwan'
small_map_data$location[small_map_data$location == "Republic of Korea"] = 'South Korea'
small_map_data$location[small_map_data$location == "United Republic of Tanzania"] = 'Tanzania'
small_map_data$location[small_map_data$location == "Bolivia (Plurinational State of)"] = 'Bolivia'
small_map_data$location[small_map_data$location == "Venezuela (Bolivarian Republic of)"] = 'Venezuela'
small_map_data$location[small_map_data$location == "Czechia"] = 'Czech Republic'
small_map_data$location[small_map_data$location == "Republic of Moldova"] = 'Moldova'
small_map_data$location[small_map_data$location == "Viet Nam"] = 'Vietnam'
small_map_data$location[small_map_data$location == "Lao People's Democratic Republic"] = 'Laos'
small_map_data$location[small_map_data$location == "Syrian Arab Republic"] = 'Syria'
small_map_data$location[small_map_data$location == "North Macedonia"] = 'Macedonia'
small_map_data$location[small_map_data$location == "Micronesia (Federated States of)"] = 'Micronesia'
small_map_data$location[small_map_data$location == "Macedonia"] = 'North Macedonia'
small_map_data$location[small_map_data$location == "Trinidad and Tobago"] = 'Trinidad'
a <- small_map_data[small_map_data$location=="Trinidad",]
a$location <- "Tobago"
small_map_data <- rbind(small_map_data,a)
small_map_data$location[small_map_data$location == "Cabo Verde"] = 'Cape Verde'
small_map_data$location[small_map_data$location == "United States Virgin Islands"] = 'Virgin Islands'
small_map_data$location[small_map_data$location == "Antigua and Barbuda"] = 'Antigu'
a <- small_map_data[small_map_data$location == "Antigu",]
a$location <- 'Barbuda'
small_map_data <- rbind(small_map_data,a)
small_map_data$location[small_map_data$location == "Saint Kitts and Nevis"] = 'Saint Kitts'
a <- small_map_data[small_map_data$location == "Saint Kitts",]
a$location <- 'Nevis'
small_map_data <- rbind(small_map_data,a)
small_map_data$location[small_map_data$location == "Côte d'Ivoire"] = 'Ivory Coast'
small_map_data$location[small_map_data$location == "Saint Vincent and the Grenadines"] = 'Saint Vincent'
a <- small_map_data[small_map_data$location == "Saint Vincent",]
a$location <- 'Grenadines'
small_map_data <- rbind(small_map_data,a)
small_map_data$location[small_map_data$location == "Eswatini"] = 'Swaziland'
small_map_data$location[small_map_data$location == "Brunei Darussalam"] = 'Brunei'
small_map_data <- full_join(worldData,small_map_data,by=c("region"="location")) %>%
  filter(EAPC!="NA")
dim(small_map_data)                                    



small_map_data <- small_map_data %>%
  filter(age=="15-39 years") %>%
  filter(year==2019)%>%
  mutate(label = cut(label, breaks = c(0,1,2,3,4,5,6,7,8,9,10),
                     labels = c("+High SDI","+High-middle SDI","+Middle SDI","+Low-middle SDI","+Low SDI",
                                "-High SDI","-High-middle SDI","-Middle SDI","-Low-middle SDI","-Low SDI"),
                    
                     include.lowest = T,right = T))

fig <- small_map_data %>% 
  ggplot()+
  geom_polygon(aes(x=long,y=lat,group=group,fill=label),
               colour="black",size=0.1)+
  theme_bw()+
  scale_fill_manual(values =  c("#A60F16", "#DE2D27", "#FB6A4B", "#FD9272", "#FCBAA0","#07519D","#3282BD", "#6BAFD6","#9DCAE1","#C6DBF0"))+
  theme_void()+
  labs(x="", y="")+
  theme(legend.position = "none",
        legend.title =element_blank(),
        plot.title = element_text(color = "black",size = 12),
        legend.text = element_text(color = "black",size=12),
        panel.grid = element_blank(),
        panel.border = element_rect(color = "black",fill = NA,size = 0.1),
        axis.title.x = element_blank(),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),)

p2 <- fig+labs(x="",y="",title = "Caribbean and central America")+
  coord_cartesian(xlim = c(-92,-60),ylim = c(5,27))+
  theme(plot.title = element_text(size = 8))

p3 <- fig+ labs(x=" ",y="",title="Persian Gulf")+
  coord_cartesian(xlim = c(45,55),ylim = c(19,31))+
  theme(plot.title = element_text(size = 8))

p4 <- fig+ labs(x=" ",y="",title="Balkan Peninsula")+
  coord_cartesian(xlim = c(12,32),ylim = c(35,53))+
  theme(plot.title = element_text(size = 8))

p5 <- fig+ labs(x=" ",y="",title="Sotheast Asia")+
  coord_cartesian(xlim = c(98,123),ylim = c(-10,8))+
  theme(plot.title = element_text(size = 8))

p6 <- fig+ labs(x=" ",y="",title="West Africa") +
  coord_cartesian(xlim = c(-17,-7),ylim = c(7,20))+
  theme(plot.title = element_text(size = 8))

p7 <- fig+ labs(x=" ",y="",title="Eastern \nMediterranean")+
  coord_cartesian(xlim = c(32,37),ylim = c(29,35))+
  theme(plot.title = element_text(size = 8))

p8 <- fig+ labs(x=" ",y="",title="Northern Europe") +
  coord_cartesian(xlim = c(5,25),ylim = c(48,60))+
  theme(plot.title = element_text(size = 8))

library(patchwork)
A=(p6|p7)/p8
fig_incidence<- p1 +
  (p2+p3+p4+p5+A+plot_layout(ncol = 5,widths=c(1.5,1,1.1,1.2,1.2)))+
  plot_layout(ncol = 1,heights = c(9, 3))



data <- read.csv('EAPC-SDI-Deaths.csv',header = T)
GBD <- read.csv("EAPC-SDI-Deaths.csv")
setwd("C:\\Users\\25470\\Desktop\\乳腺癌论文\\三个指标EAPC-SDI")
location <- read.csv("location.csv")
GBD <- left_join(GBD,location,by="location")
colnames(GBD)
map <- st_read("世界国家.shp")
map <- st_set_crs(map,4326)
main_map_data <- left_join(map,GBD,by=c("NAME"="location3"))
colnames(main_map_data)
unique(main_map_data$age)


main_map_data <- main_map_data %>%
  filter(age=="15-39 years") %>%
  filter(year==2019)%>%
  mutate(label = cut(label, breaks = c(0,1,2,3,4,5,6,7,8,9,10),
                     labels = c("+High SDI","+High-middle SDI","+Middle SDI","+Low-middle SDI","+Low SDI",
                                "-High SDI","-High-middle SDI","-Middle SDI","-Low-middle SDI","-Low SDI"),
                     
                     include.lowest = T,right = T))
p1 <-main_map_data %>% 
  ggplot()+ geom_sf(data=main_map_data, 
                    aes(group=NAME,fill=label),
                    colour="black",size = 0.1) + 
  scale_fill_manual(values = c("#A60F16", "#DE2D27", "#FB6A4B", "#FD9272", "#FCBAA0","#07519D","#3282BD", "#6BAFD6","#9DCAE1","#C6DBF0"))+
  theme_void()+
  labs(x="", y="")+
  guides(fill = guide_legend(title='EAPC'))+
  theme(legend.position =  c(0.1,0.3),
        legend.key.height = unit(10, "pt"),
        legend.key.width = unit(15, "pt"),
        panel.grid=element_blank(),
        legend.title = element_text(size=8),
        legend.text = element_text(size = 8))


worldData <-map_data("world")
small_map_data <- GBD %>%
  filter(age=="15-39 years") %>%
  filter(year==2019)
small_map_data$location[small_map_data$location == 'United States of America'] = 'USA'
small_map_data$location[small_map_data$location == 'Russian Federation'] = 'Russia'
small_map_data$location[small_map_data$location == 'United Kingdom'] = 'UK'
small_map_data$location[small_map_data$location == 'Congo'] = 'Republic of Congo'
small_map_data$location[small_map_data$location == "Iran (Islamic Republic of)"] = 'Iran'
small_map_data$location[small_map_data$location == "Democratic People's Republic of Korea"] = 'North Korea'
small_map_data$location[small_map_data$location == "Taiwan (Province of China)"] = 'Taiwan'
small_map_data$location[small_map_data$location == "Republic of Korea"] = 'South Korea'
small_map_data$location[small_map_data$location == "United Republic of Tanzania"] = 'Tanzania'
small_map_data$location[small_map_data$location == "Bolivia (Plurinational State of)"] = 'Bolivia'
small_map_data$location[small_map_data$location == "Venezuela (Bolivarian Republic of)"] = 'Venezuela'
small_map_data$location[small_map_data$location == "Czechia"] = 'Czech Republic'
small_map_data$location[small_map_data$location == "Republic of Moldova"] = 'Moldova'
small_map_data$location[small_map_data$location == "Viet Nam"] = 'Vietnam'
small_map_data$location[small_map_data$location == "Lao People's Democratic Republic"] = 'Laos'
small_map_data$location[small_map_data$location == "Syrian Arab Republic"] = 'Syria'
small_map_data$location[small_map_data$location == "North Macedonia"] = 'Macedonia'
small_map_data$location[small_map_data$location == "Micronesia (Federated States of)"] = 'Micronesia'
small_map_data$location[small_map_data$location == "Macedonia"] = 'North Macedonia'
small_map_data$location[small_map_data$location == "Trinidad and Tobago"] = 'Trinidad'
a <- small_map_data[small_map_data$location=="Trinidad",]
a$location <- "Tobago"
small_map_data <- rbind(small_map_data,a)
small_map_data$location[small_map_data$location == "Cabo Verde"] = 'Cape Verde'
small_map_data$location[small_map_data$location == "United States Virgin Islands"] = 'Virgin Islands'
small_map_data$location[small_map_data$location == "Antigua and Barbuda"] = 'Antigu'
a <- small_map_data[small_map_data$location == "Antigu",]
a$location <- 'Barbuda'
small_map_data <- rbind(small_map_data,a)
small_map_data$location[small_map_data$location == "Saint Kitts and Nevis"] = 'Saint Kitts'
a <- small_map_data[small_map_data$location == "Saint Kitts",]
a$location <- 'Nevis'
small_map_data <- rbind(small_map_data,a)
small_map_data$location[small_map_data$location == "Côte d'Ivoire"] = 'Ivory Coast'
small_map_data$location[small_map_data$location == "Saint Vincent and the Grenadines"] = 'Saint Vincent'
a <- small_map_data[small_map_data$location == "Saint Vincent",]
a$location <- 'Grenadines'
small_map_data <- rbind(small_map_data,a)
small_map_data$location[small_map_data$location == "Eswatini"] = 'Swaziland'
small_map_data$location[small_map_data$location == "Brunei Darussalam"] = 'Brunei'
small_map_data <- full_join(worldData,small_map_data,by=c("region"="location")) %>%
  filter(EAPC!="NA")
dim(small_map_data)                                    
head(small_map_data)


small_map_data <- small_map_data %>%
  filter(age=="15-39 years") %>%
  filter(year==2019)%>%
  mutate(label = cut(label, breaks = c(0,1,2,3,4,5,6,7,8,9,10),
                     labels = c("+High SDI","+High-middle SDI","+Middle SDI","+Low-middle SDI","+Low SDI",
                                "-High SDI","-High-middle SDI","-Middle SDI","-Low-middle SDI","-Low SDI"),
                                       include.lowest = T,right = T))

fig <- small_map_data %>% 
  ggplot()+
  geom_polygon(aes(x=long,y=lat,group=group,fill=label),
               colour="black",size=0.1)+
  theme_bw()+
  scale_fill_manual(values =  c("#A60F16", "#DE2D27", "#FB6A4B", "#FD9272", "#FCBAA0","#07519D","#3282BD", "#6BAFD6","#9DCAE1","#C6DBF0"))+
  theme_void()+
  labs(x="", y="")+
  theme(legend.position = "none",
        legend.title =element_blank(),
        plot.title = element_text(color = "black",size = 12),
        legend.text = element_text(color = "black",size=12),
        panel.grid = element_blank(),
        panel.border = element_rect(color = "black",fill = NA,size = 0.1),
        axis.title.x = element_blank(),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),)

p2 <- fig+labs(x="",y="",title = "Caribbean and central America")+
  coord_cartesian(xlim = c(-92,-60),ylim = c(5,27))+
  theme(plot.title = element_text(size = 8))

p3 <- fig+ labs(x=" ",y="",title="Persian Gulf")+
  coord_cartesian(xlim = c(45,55),ylim = c(19,31))+
  theme(plot.title = element_text(size = 8))

p4 <- fig+ labs(x=" ",y="",title="Balkan Peninsula")+
  coord_cartesian(xlim = c(12,32),ylim = c(35,53))+
  theme(plot.title = element_text(size = 8))

p5 <- fig+ labs(x=" ",y="",title="Sotheast Asia")+
  coord_cartesian(xlim = c(98,123),ylim = c(-10,8))+
  theme(plot.title = element_text(size = 8))

p6 <- fig+ labs(x=" ",y="",title="West Africa") +
  coord_cartesian(xlim = c(-17,-7),ylim = c(7,20))+
  theme(plot.title = element_text(size = 8))

p7 <- fig+ labs(x=" ",y="",title="Eastern \nMediterranean")+
  coord_cartesian(xlim = c(32,37),ylim = c(29,35))+
  theme(plot.title = element_text(size = 8))

p8 <- fig+ labs(x=" ",y="",title="Northern Europe") +
  coord_cartesian(xlim = c(5,25),ylim = c(48,60))+
  theme(plot.title = element_text(size = 8))

library(patchwork)
A=(p6|p7)/p8
fig_deaths<- p1 +
  (p2+p3+p4+p5+A+plot_layout(ncol = 5,widths=c(1.5,1,1.1,1.2,1.2)))+
  plot_layout(ncol = 1,heights = c(9, 3))



data <- read.csv('EAPC-SDI-Dalys.csv',header = T)
GBD <- read.csv("EAPC-SDI-Dalys.csv")
setwd("C:\\Users\\25470\\Desktop\\乳腺癌论文\\三个指标EAPC-SDI")
location <- read.csv("location.csv")
GBD <- left_join(GBD,location,by="location")
colnames(GBD)
map <- st_read("世界国家.shp")
map <- st_set_crs(map,4326)
main_map_data <- left_join(map,GBD,by=c("NAME"="location3"))
colnames(main_map_data)
unique(main_map_data$age)


main_map_data <- main_map_data %>%
  filter(age=="15-39 years") %>%
  filter(year==2019)%>%
  mutate(label = cut(label, breaks = c(0,1,2,3,4,5,6,7,8,9,10),
                     labels = c("+High SDI","+High-middle SDI","+Middle SDI","+Low-middle SDI","+Low SDI",
                                "-High SDI","-High-middle SDI","-Middle SDI","-Low-middle SDI","-Low SDI"),
                     ### breaks需要根据自己的实际结果来调整
                     include.lowest = T,right = T))
p1 <-main_map_data %>% 
  ggplot()+ geom_sf(data=main_map_data, 
                    aes(group=NAME,fill=label),
                    colour="black",size = 0.1) + 
  scale_fill_manual(values = c("#A60F16", "#DE2D27", "#FB6A4B", "#FD9272", "#FCBAA0","#07519D","#3282BD", "#6BAFD6","#9DCAE1","#C6DBF0"))+
  theme_void()+
  labs(x="", y="")+
  guides(fill = guide_legend(title='EAPC'))+
  theme(legend.position =  c(0.1,0.3),
        legend.key.height = unit(10, "pt"),
        legend.key.width = unit(15, "pt"),
        panel.grid=element_blank(),
        legend.title = element_text(size=8),
        legend.text = element_text(size = 8))


worldData <-map_data("world")
small_map_data <- GBD %>%
  filter(age=="15-39 years") %>%
  filter(year==2019)
small_map_data$location[small_map_data$location == 'United States of America'] = 'USA'
small_map_data$location[small_map_data$location == 'Russian Federation'] = 'Russia'
small_map_data$location[small_map_data$location == 'United Kingdom'] = 'UK'
small_map_data$location[small_map_data$location == 'Congo'] = 'Republic of Congo'
small_map_data$location[small_map_data$location == "Iran (Islamic Republic of)"] = 'Iran'
small_map_data$location[small_map_data$location == "Democratic People's Republic of Korea"] = 'North Korea'
small_map_data$location[small_map_data$location == "Taiwan (Province of China)"] = 'Taiwan'
small_map_data$location[small_map_data$location == "Republic of Korea"] = 'South Korea'
small_map_data$location[small_map_data$location == "United Republic of Tanzania"] = 'Tanzania'
small_map_data$location[small_map_data$location == "Bolivia (Plurinational State of)"] = 'Bolivia'
small_map_data$location[small_map_data$location == "Venezuela (Bolivarian Republic of)"] = 'Venezuela'
small_map_data$location[small_map_data$location == "Czechia"] = 'Czech Republic'
small_map_data$location[small_map_data$location == "Republic of Moldova"] = 'Moldova'
small_map_data$location[small_map_data$location == "Viet Nam"] = 'Vietnam'
small_map_data$location[small_map_data$location == "Lao People's Democratic Republic"] = 'Laos'
small_map_data$location[small_map_data$location == "Syrian Arab Republic"] = 'Syria'
small_map_data$location[small_map_data$location == "North Macedonia"] = 'Macedonia'
small_map_data$location[small_map_data$location == "Micronesia (Federated States of)"] = 'Micronesia'
small_map_data$location[small_map_data$location == "Macedonia"] = 'North Macedonia'
small_map_data$location[small_map_data$location == "Trinidad and Tobago"] = 'Trinidad'
a <- small_map_data[small_map_data$location=="Trinidad",]
a$location <- "Tobago"
small_map_data <- rbind(small_map_data,a)
small_map_data$location[small_map_data$location == "Cabo Verde"] = 'Cape Verde'
small_map_data$location[small_map_data$location == "United States Virgin Islands"] = 'Virgin Islands'
small_map_data$location[small_map_data$location == "Antigua and Barbuda"] = 'Antigu'
a <- small_map_data[small_map_data$location == "Antigu",]
a$location <- 'Barbuda'
small_map_data <- rbind(small_map_data,a)
small_map_data$location[small_map_data$location == "Saint Kitts and Nevis"] = 'Saint Kitts'
a <- small_map_data[small_map_data$location == "Saint Kitts",]
a$location <- 'Nevis'
small_map_data <- rbind(small_map_data,a)
small_map_data$location[small_map_data$location == "Côte d'Ivoire"] = 'Ivory Coast'
small_map_data$location[small_map_data$location == "Saint Vincent and the Grenadines"] = 'Saint Vincent'
a <- small_map_data[small_map_data$location == "Saint Vincent",]
a$location <- 'Grenadines'
small_map_data <- rbind(small_map_data,a)
small_map_data$location[small_map_data$location == "Eswatini"] = 'Swaziland'
small_map_data$location[small_map_data$location == "Brunei Darussalam"] = 'Brunei'
small_map_data <- full_join(worldData,small_map_data,by=c("region"="location")) %>%
  filter(EAPC!="NA")
dim(small_map_data)                                    
head(small_map_data)


small_map_data <- small_map_data %>%
  filter(age=="15-39 years") %>%
  filter(year==2019)%>%
  mutate(label = cut(label, breaks = c(0,1,2,3,4,5,6,7,8,9,10),
                     labels = c("+High SDI","+High-middle SDI","+Middle SDI","+Low-middle SDI","+Low SDI",
                                "-High SDI","-High-middle SDI","-Middle SDI","-Low-middle SDI","-Low SDI"),
                  
                     include.lowest = T,right = T))

fig <- small_map_data %>% 
  ggplot()+
  geom_polygon(aes(x=long,y=lat,group=group,fill=label),
               colour="black",size=0.1)+
  theme_bw()+
  scale_fill_manual(values =  c("#A60F16", "#DE2D27", "#FB6A4B", "#FD9272", "#FCBAA0","#07519D","#3282BD", "#6BAFD6","#9DCAE1","#C6DBF0"))+
  theme_void()+
  labs(x="", y="")+
  theme(legend.position = "none",
        legend.title =element_blank(),
        plot.title = element_text(color = "black",size = 12),
        legend.text = element_text(color = "black",size=12),
        panel.grid = element_blank(),
        panel.border = element_rect(color = "black",fill = NA,size = 0.1),
        axis.title.x = element_blank(),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),)

p2 <- fig+labs(x="",y="",title = "Caribbean and central America")+
  coord_cartesian(xlim = c(-92,-60),ylim = c(5,27))+
  theme(plot.title = element_text(size = 8))

p3 <- fig+ labs(x=" ",y="",title="Persian Gulf")+
  coord_cartesian(xlim = c(45,55),ylim = c(19,31))+
  theme(plot.title = element_text(size = 8))

p4 <- fig+ labs(x=" ",y="",title="Balkan Peninsula")+
  coord_cartesian(xlim = c(12,32),ylim = c(35,53))+
  theme(plot.title = element_text(size = 8))

p5 <- fig+ labs(x=" ",y="",title="Sotheast Asia")+
  coord_cartesian(xlim = c(98,123),ylim = c(-10,8))+
  theme(plot.title = element_text(size = 8))

p6 <- fig+ labs(x=" ",y="",title="West Africa") +
  coord_cartesian(xlim = c(-17,-7),ylim = c(7,20))+
  theme(plot.title = element_text(size = 8))

p7 <- fig+ labs(x=" ",y="",title="Eastern \nMediterranean")+
  coord_cartesian(xlim = c(32,37),ylim = c(29,35))+
  theme(plot.title = element_text(size = 8))

p8 <- fig+ labs(x=" ",y="",title="Northern Europe") +
  coord_cartesian(xlim = c(5,25),ylim = c(48,60))+
  theme(plot.title = element_text(size = 8))

library(patchwork)
A=(p6|p7)/p8
fig_dalys<- p1 +
  (p2+p3+p4+p5+A+plot_layout(ncol = 5,widths=c(1.5,1,1.1,1.2,1.2)))+
  plot_layout(ncol = 1,heights = c(9, 3))





# install.packages("ggpubr") 
library(ggpubr) 
fig_all<-ggarrange(fig_incidence, fig_deaths, fig_dalys, ncol = 1, nrow = 3,
          labels = c("A","B","C"), 
          font.label = list(size = 14, face = "bold")) 
print(fig_all)
ggsave("SDI_EAPC地图.pdf",width = 9,height = 18)

