setwd('C:\\Users\\25470\\Desktop\\BCY\\map')
rm(list=ls())
library(ggmap)
library(rgdal)
library(maps)
library(dplyr)
library(sf)

GBD <- read.csv("15-39incidence_ASR.csv")%>%
  filter(year==2019)
location <- read.csv("location.csv")
GBD <- left_join(GBD,location,by="location")
quantile(GBD$ASR, seq(0, 1, 0.2))
colnames(GBD)
map <- st_read("世界国家.shp")
map <- st_set_crs(map,4326)
main_map_data <- left_join(map,GBD,by=c("NAME"="location3"))
colnames(main_map_data)


# 15-39ASR ------------------------------------------------------------------
unique(main_map_data$age)
main_map_data <- main_map_data %>%
  filter(age=="15-39 years") %>%
  filter(year==2019)%>%
  mutate(ASR = round(ASR, 7))%>% 
  mutate(ASR = cut(ASR, breaks = c(0,7.028915,11.214588,14.907468,19.083271,55.186),
                   labels = c("0 to <7.029","7.029 to <11.215","11.215 to <14.907",
                              "14.907 to <19.083","19.083 to 55.185"), 
                   include.lowest = T,right = T))

p1 <-main_map_data %>% 
  ggplot()+ geom_sf(data=main_map_data, 
                    aes(group=NAME,fill=ASR),
                    colour="black",size = 0.1) + 
  scale_fill_manual(values = c("#C8D6E7", "#9EBCDB", "#7091C7", "#4E70AF", "#375093"))+
  theme_void()+
  labs(x="", y="")+
  guides(fill = guide_legend(title='ASIR in 2019 (/10^5)'))+
  theme(legend.position =  c(0.1,0.15),
        panel.grid=element_blank(),
        legend.title = element_text(size=10))


worldData <-map_data("world")
small_map_data <- GBD %>%
  filter(age=="15-39 years") %>%
  filter(year==2019)%>%
  mutate(ASR = round(ASR, 7))%>% 
  mutate(ASR = cut(ASR, breaks = c(0,7.028915,11.214588,14.907468,19.083271,55.186),
                   labels = c("0 to <7.029","7.029 to <11.215","11.215 to <14.907",
                              "14.907 to <19.083","19.083 to 55.185"),  
                   include.lowest = T,right = T))

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
  filter(val!="NA")
dim(small_map_data)                                    
head(small_map_data)    

library(ggsci)
fig <- small_map_data %>% 
  ggplot()+
  geom_polygon(aes(x=long,y=lat,group=group,fill=ASR),
               colour="black",size=0.1)+
  theme_bw()+
  scale_fill_manual(values = c("#C8D6E7", "#9EBCDB", "#7091C7", "#4E70AF", "#375093"))+
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
plot<- p1 +
  (p2+p3+p4+p5+A+plot_layout(ncol = 5,widths=c(1.5,1,1.1,1.2,1.2)))+
  plot_layout(ncol = 1,heights = c(9, 3))

# install.packages("ggsave")
ggsave("BCY-2019-incidence_ASR地图.pdf",width = 9,height = 6)
