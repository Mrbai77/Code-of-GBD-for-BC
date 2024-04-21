rm(list=ls())
library(tidyverse)
library(dplyr)
library(ggplot2)
library(epitools)
library(reshape2)
setwd("C:\\Users\\25470\\Desktop\\BCY\\预测\\BAPC\\数据\\incidence_rate_BAPC_1990-2030")
incidence_ASR_1539 <- read.csv('incidence_rate_ASR_15-39.csv',header = T)
colnames(incidence_ASR_1539) <- c("year","age","val","up","low")
data_pro <- incidence_ASR_1539 %>% 
  filter(year>2018)
data_real <- incidence_ASR_1539 %>% 
  filter(year<2020)
plot <-ggplot(incidence_ASR_1539,aes(x=year,y=val))+
  geom_ribbon(data=incidence_ASR_1539,
            aes(x=year,ymin=low,ymax=up),
            alpha=0.6)+
  geom_line(data=incidence_ASR_1539,
            aes(x=year,y=val))

fig1 <- ggplot() + 
  geom_line(data = data_pro, aes(x = as.numeric(year), y = val),size=0.5,color = 'black')+
  geom_point(data = data_pro,aes(x = as.numeric(year), y = val),size=1,shape=21)+
  geom_ribbon(data = data_pro, aes(x = as.numeric(year), ymin = low, ymax = up), alpha = 0.4,fill = "#FFC1C1")+
  geom_point(data = data_real, aes(x = year, y = val),size=1,shape=19)+
  geom_line(data = data_real, aes(x = as.numeric(year), y = val),size=0.5,color = 'black')+
  theme_classic()+  #classic主题
  theme(panel.background = element_rect(color='black'), 
        plot.margin = margin(10,5,5,5,unit = 'mm'))+    
  labs(x='year',y='Agestd rate per 100000')+
  scale_x_continuous(expand = c(0,0))+                 
  labs(title = "15-39 age")+
  theme(plot.title = element_text(family = "serif",
                                  face = "bold",     
                                  size = 15,          
                                  hjust = 0.5))+
  theme(axis.title = element_text(size = 8), 
        axis.text = element_text(size = 8)) 



