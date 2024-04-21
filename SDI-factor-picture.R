rm(list=ls())
setwd('C:\\Users\\25470\\Desktop\\BCY\\4.SDI-影响因素') 
library(ggplot2)
library(ggsci)
LC <- read.csv('SDI-影响因素数据.csv',header = T)  
LC$val <- abs(LC$val)
unique(LC$location)
yearoder <- c("1990","1991", "1992","1993", "1994", "1995", "1996", "1997", "1998", "1999", 
              "2000", "2001", "2002", "2003", "2004","2005", "2006", "2007", "2008", "2009",
              "2010","2011","2012","2013","2014","2015","2016","2017","2018","2019")




case_Global <- subset(LC,LC$age=="15-39 years" & 
                          LC$metric== 'Number' &
                          LC$measure=='Deaths'&
                          LC$location=='Global')


LC_percent <- data.frame(year=rep(unique(case_Global$year),each=length(unique(case_Global$rei))),
                                                  rei=rep(unique(case_Global$rei),length(unique(case_Global$year))),
                         percent=rep(NA,times=length(unique(case_Global$rei))*length(unique(case_Global$year))))

a <- unique(case_Global$year)
b <- unique(case_Global$rei)

for (i in 1:length(unique(case_Global$year))){
  year_i <- a[i]
  data <- subset(case_Global,case_Global$year==year_i)[,c(4,6,9)]
  sum <- sum(data$val)
  data$percent <- data$val/sum
  data <- data[,-3]
  for (j in 1:length(unique(case_Global$rei))) {
    rei_j <- b[j]
    LC_percent[which(LC_percent$year==year_i & LC_percent$rei==rei_j),3] <- data$percent[which(data$rei==rei_j)]
  }
}

LC_Global <- LC_percent
LC_Global$location <- "High SDI"

LC_Global$text <- as.character(round(LC_Global$percent*100,1))  

LC_Global$year <- factor(LC_Global$year, 
                           levels=rev(yearoder), 
                           ordered=TRUE)
LC_Global$rei <- factor(LC_Global$rei,
                          levels=c("Smoking","Low physical activity", 
                                   "High fasting plasma glucose", "Diet high in red meat", "Secondhand smoke", "Alcohol use", "High body-mass index"),
                          ordered=TRUE)

p_LC_Global<-ggplot(data = LC_Global, aes(x = year, y= percent,fill =rei))+
  geom_bar(stat = 'identity',position = 'fill')+labs(x = '') +labs(y= '')+ 
  scale_fill_manual(values = c("Low physical activity"="#FFC312",
                               "Smoking"="#C4E538",
                               "High fasting plasma glucose"="#12CBC4", 
                               "Diet high in red meat"="#FDA7DF",
                               "Secondhand smoke"="#60B683", 
                               "High body-mass index"="#26A7FF",
                               "Alcohol use"="#FA7F6F"))+
  theme_light()+coord_flip()  + 
  geom_text(aes(label=text, y=percent), 
            position=position_stack(.5), vjust=0.3,
            size = 1.5) +

  theme(legend.position = "bottom")+theme(legend.key.size = unit(5, "pt"))+
  guides(fill = guide_legend(title='Cause'))+

  theme(axis.text.x = element_text(size = 7), 
        axis.text.y = element_text(size = 7))+
  ggtitle("Global")+ 
  theme(plot.title = element_text(hjust = 0.5, size = 10)) 
print(p_LC_Global)




case_High_SDI <- subset(LC,LC$age=="15-39 years" & 
                          LC$metric== 'Number' &
                          LC$measure=='Deaths'&
                          LC$location=='High SDI') 



LC_percent <- data.frame(year=rep(unique(case_High_SDI$year),each=length(unique(case_High_SDI$rei))),
                         
                         rei=rep(unique(case_High_SDI$rei),length(unique(case_High_SDI$year))),
                         percent=rep(NA,times=length(unique(case_High_SDI$rei))*length(unique(case_High_SDI$year))))

a <- unique(case_High_SDI$year)
b <- unique(case_High_SDI$rei)

for (i in 1:length(unique(case_High_SDI$year))){
  year_i <- a[i]
  data <- subset(case_High_SDI,case_High_SDI$year==year_i)[,c(4,6,9)]
  sum <- sum(data$val)
  data$percent <- data$val/sum
  data <- data[,-3]
  for (j in 1:length(unique(case_High_SDI$rei))) {
    rei_j <- b[j]
    LC_percent[which(LC_percent$year==year_i & LC_percent$rei==rei_j),3] <- data$percent[which(data$rei==rei_j)]
  }
}

LC_High_SDI <- LC_percent
LC_High_SDI$location <- "High SDI"

LC_High_SDI$text <- as.character(round(LC_High_SDI$percent*100,1))  

LC_High_SDI$year <- factor(LC_High_SDI$year, 
                      levels=rev(yearoder), 
                      ordered=TRUE)
LC_High_SDI$rei <- factor(LC_High_SDI$rei,
                     levels=c("Low physical activity", "High fasting plasma glucose", "Secondhand smoke", 
                              "Smoking", "Diet high in red meat", "High body-mass index", "Alcohol use"),
                     ordered=TRUE)

p_LC_High_SDI<-ggplot(data = LC_High_SDI, aes(x = year, y= percent,fill =rei))+
  geom_bar(stat = 'identity',position = 'fill')+labs(x = '') +labs(y= '')+ 
  
  scale_fill_manual(values = c("Low physical activity"="#FFC312",
                                 "Smoking"="#C4E538",
                                 "High fasting plasma glucose"="#12CBC4", 
                                 "Diet high in red meat"="#FDA7DF",
                                 "Secondhand smoke"="#60B683", 
                                 "High body-mass index"="#26A7FF",
                                 "Alcohol use"="#FA7F6F"))+
  theme_light()+coord_flip()  + 
  geom_text(aes(label=text, y=percent), 
            position=position_stack(.5), vjust=0.3,
            size = 1.5) +
  theme(legend.position = "none") +  
   guides(fill = guide_legend(title='Cause'))+
 
  theme(axis.text.x = element_text(size = 7),
        axis.text.y = element_blank(), 
        axis.line.y = element_blank(),  
        axis.ticks.y = element_blank()) +  
  ggtitle("High SDI")+ 
  theme(plot.title = element_text(hjust = 0.5, size = 10)) 
print(p_LC_High_SDI)




case_High_middle_SDI <- subset(LC,LC$age=="15-39 years" & 
                                 LC$metric== 'Number' &
                                 LC$measure=='Deaths'&
                                 LC$location=='High-middle SDI') 


LC_percent <- data.frame(year=rep(unique(case_High_middle_SDI$year),each=length(unique(case_High_middle_SDI$rei))),
                                                  rei=rep(unique(case_High_middle_SDI$rei),length(unique(case_High_middle_SDI$year))),
                         percent=rep(NA,times=length(unique(case_High_middle_SDI$rei))*length(unique(case_High_middle_SDI$year))))

a <- unique(case_High_middle_SDI$year)
b <- unique(case_High_middle_SDI$rei)

for (i in 1:length(unique(case_High_middle_SDI$year))){
  year_i <- a[i]
  data <- subset(case_High_middle_SDI,case_High_middle_SDI$year==year_i)[,c(4,6,9)]
  sum <- sum(data$val)
  data$percent <- data$val/sum
  data <- data[,-3]
  for (j in 1:length(unique(case_High_middle_SDI$rei))) {
    rei_j <- b[j]
    LC_percent[which(LC_percent$year==year_i & LC_percent$rei==rei_j),3] <- data$percent[which(data$rei==rei_j)]
  }
}

LC_High_middle_SDI <- LC_percent
LC_High_middle_SDI$location <- "High-middle SDI"


LC_High_middle_SDI$text <- as.character(round(LC_High_middle_SDI$percent*100,1))  
LC_High_middle_SDI$year <- factor(LC_High_middle_SDI$year, 
                           levels=rev(yearoder), 
                           ordered=TRUE)
LC_High_middle_SDI$rei <- factor(LC_High_middle_SDI$rei,
                          levels=c("Low physical activity", "Smoking", "High fasting plasma glucose", 
                                   "Secondhand smoke", "Diet high in red meat", "High body-mass index", "Alcohol use"),
                          ordered=TRUE)

p_LC_High_middle_SDI<-ggplot(data = LC_High_middle_SDI, aes(x = year, y= percent,fill =rei))+
  geom_bar(stat = 'identity',position = 'fill')+labs(x = '') +labs(y= '')+ 

  scale_fill_manual(values = c("Low physical activity"="#FFC312",
                               "Smoking"="#C4E538",
                               "High fasting plasma glucose"="#12CBC4", 
                               "Diet high in red meat"="#FDA7DF",
                               "Secondhand smoke"="#60B683", 
                               "High body-mass index"="#26A7FF",
                               "Alcohol use"="#FA7F6F"))+
  theme_light()+coord_flip()  + 
  geom_text(aes(label=text, y=percent), 
            position=position_stack(.5), vjust=0.3,
            size = 1.5) +
  theme(legend.position = "none") +  
  theme(axis.text.x = element_text(size = 7),
        axis.text.y = element_blank(),  
        axis.line.y = element_blank(),  
        axis.ticks.y = element_blank()) +  
  ggtitle("High-middle SDI")+  
  theme(plot.title = element_text(hjust = 0.5, size = 10)) 
print(p_LC_High_middle_SDI)




case_Middle_SDI <- subset(LC,LC$age=="15-39 years" & 
                            LC$metric== 'Number' &
                            LC$measure=='Deaths'&
                            LC$location=='Middle SDI') 


LC_percent <- data.frame(year=rep(unique(case_Middle_SDI$year),each=length(unique(case_Middle_SDI$rei))),
                         
                         rei=rep(unique(case_Middle_SDI$rei),length(unique(case_Middle_SDI$year))),
                         percent=rep(NA,times=length(unique(case_Middle_SDI$rei))*length(unique(case_Middle_SDI$year))))

a <- unique(case_Middle_SDI$year)
b <- unique(case_Middle_SDI$rei)

for (i in 1:length(unique(case_Middle_SDI$year))){
  year_i <- a[i]
  data <- subset(case_Middle_SDI,case_Middle_SDI$year==year_i)[,c(4,6,9)]
  sum <- sum(data$val)
  data$percent <- data$val/sum
  data <- data[,-3]
  for (j in 1:length(unique(case_Middle_SDI$rei))) {
    rei_j <- b[j]
    LC_percent[which(LC_percent$year==year_i & LC_percent$rei==rei_j),3] <- data$percent[which(data$rei==rei_j)]
  }
}

LC_Middle_SDI <- LC_percent
LC_Middle_SDI$location <- "Middle SDI"

LC_Middle_SDI$text <- as.character(round(LC_Middle_SDI$percent*100,1))  
LC_Middle_SDI$year <- factor(LC_Middle_SDI$year, 
                                  levels=rev(yearoder), 
                                  ordered=TRUE)
LC_Middle_SDI$rei <- factor(LC_Middle_SDI$rei,
                            c("Smoking", "Low physical activity", "High fasting plasma glucose", 
                              "Alcohol use", "Diet high in red meat", "Secondhand smoke", "High body-mass index"),
                                 ordered=TRUE)

p_LC_Middle_SDI<-ggplot(data = LC_Middle_SDI, aes(x = year, y= percent,fill =rei))+
  geom_bar(stat = 'identity',position = 'fill')+labs(x = '') +labs(y= '')+ 

  scale_fill_manual(values = c("Low physical activity"="#FFC312",
                               "Smoking"="#C4E538",
                               "High fasting plasma glucose"="#12CBC4", 
                               "Diet high in red meat"="#FDA7DF",
                               "Secondhand smoke"="#60B683", 
                               "High body-mass index"="#26A7FF",
                               "Alcohol use"="#FA7F6F"))+
  theme_light()+coord_flip()  + 
  geom_text(aes(label=text, y=percent), 
            position=position_stack(.5), vjust=0.3,
            size = 1.5) +
  theme(legend.position = "none") + 
  theme(axis.text.x = element_text(size = 7),
        axis.text.y = element_blank(), 
        axis.line.y = element_blank(),  
        axis.ticks.y = element_blank()) +  
  ggtitle("Middle SDI")+  
  theme(plot.title = element_text(hjust = 0.5, size = 10))  
print(p_LC_Middle_SDI)





case_Low_middle_SDI <- subset(LC,LC$age=="15-39 years" & 
                                LC$metric== 'Number' &
                                LC$measure=='Deaths'&
                                LC$location=='Low-middle SDI') 



LC_percent <- data.frame(year=rep(unique(case_Low_middle_SDI$year),each=length(unique(case_Low_middle_SDI$rei))),
                         
                         rei=rep(unique(case_Low_middle_SDI$rei),length(unique(case_Low_middle_SDI$year))),
                         percent=rep(NA,times=length(unique(case_Low_middle_SDI$rei))*length(unique(case_Low_middle_SDI$year))))

a <- unique(case_Low_middle_SDI$year)
b <- unique(case_Low_middle_SDI$rei)

for (i in 1:length(unique(case_Low_middle_SDI$year))){
  year_i <- a[i]
  data <- subset(case_Low_middle_SDI,case_Low_middle_SDI$year==year_i)[,c(4,6,9)]
  sum <- sum(data$val)
  data$percent <- data$val/sum
  data <- data[,-3]
  for (j in 1:length(unique(case_Low_middle_SDI$rei))) {
    rei_j <- b[j]
    LC_percent[which(LC_percent$year==year_i & LC_percent$rei==rei_j),3] <- data$percent[which(data$rei==rei_j)]
  }
}

LC_Low_middle_SDI <- LC_percent
LC_Low_middle_SDI$location <- "Low-middle SDI"



LC_Low_middle_SDI$text <- as.character(round(LC_Low_middle_SDI$percent*100,1))  
LC_Low_middle_SDI$year <- factor(LC_Low_middle_SDI$year, 
                             levels=rev(yearoder), 
                             ordered=TRUE)
LC_Low_middle_SDI$rei <- factor(LC_Low_middle_SDI$rei,
                                c("Smoking", "Low physical activity", "Diet high in red meat",
                                  "High fasting plasma glucose", "Alcohol use", "Secondhand smoke", "High body-mass index"),
                            ordered=TRUE)

p_LC_Low_middle_SDI<-ggplot(data = LC_Low_middle_SDI, aes(x = year, y= percent,fill =rei))+
  geom_bar(stat = 'identity',position = 'fill')+labs(x = '') +labs(y= '')+ 
  
  scale_fill_manual(values = c("Low physical activity"="#FFC312",
                               "Smoking"="#C4E538",
                               "High fasting plasma glucose"="#12CBC4", 
                               "Diet high in red meat"="#FDA7DF",
                               "Secondhand smoke"="#60B683", 
                               "High body-mass index"="#26A7FF",
                               "Alcohol use"="#FA7F6F"))+
  theme_light()+coord_flip()  + 
  geom_text(aes(label=text, y=percent), 
            position=position_stack(.5), vjust=0.3,
            size = 1.5) +
  theme(legend.position = "none") +  
  theme(axis.text.x = element_text(size = 7),
        axis.text.y = element_blank(),  
        axis.line.y = element_blank(),  
        axis.ticks.y = element_blank()) +  
  ggtitle("Low-middle SDI")+  
  theme(plot.title = element_text(hjust = 0.5, size = 10))  
print(p_LC_Low_middle_SDI)



case_Low_SDI <- subset(LC,LC$age=="15-39 years" & 
                         LC$metric== 'Number' &
                         LC$measure=='Deaths'&
                         LC$location=='Low SDI') 


LC_percent <- data.frame(year=rep(unique(case_Low_SDI$year),each=length(unique(case_Low_SDI$rei))),
                        
                         rei=rep(unique(case_Low_SDI$rei),length(unique(case_Low_SDI$year))),
                         percent=rep(NA,times=length(unique(case_Low_SDI$rei))*length(unique(case_Low_SDI$year))))

a <- unique(case_Low_SDI$year)
b <- unique(case_Low_SDI$rei)

for (i in 1:length(unique(case_Low_SDI$year))){
  year_i <- a[i]
  data <- subset(case_Low_SDI,case_Low_SDI$year==year_i)[,c(4,6,9)]
  sum <- sum(data$val)
  data$percent <- data$val/sum
  data <- data[,-3]
  for (j in 1:length(unique(case_Low_SDI$rei))) {
    rei_j <- b[j]
    LC_percent[which(LC_percent$year==year_i & LC_percent$rei==rei_j),3] <- data$percent[which(data$rei==rei_j)]
  }
}

LC_Low_SDI <- LC_percent
LC_Low_SDI$location <- "Low SDI"



LC_Low_SDI$text <- as.character(round(LC_Low_SDI$percent*100,1))  
LC_Low_SDI$year <- factor(LC_Low_SDI$year, 
                             levels=rev(yearoder), 
                             ordered=TRUE)
LC_Low_SDI$rei <- factor(LC_Low_SDI$rei,
                         c("Smoking", "Low physical activity", "High fasting plasma glucose",
                           "Diet high in red meat", "Secondhand smoke", "Alcohol use", "High body-mass index"),
                            ordered=TRUE)

p_LC_Low_SDI<-ggplot(data = LC_Low_SDI, aes(x = year, y= percent,fill =rei))+
  geom_bar(stat = 'identity',position = 'fill')+labs(x = '') +labs(y= '')+ 
 
  scale_fill_manual(values = c("Low physical activity"="#FFC312",
                               "Smoking"="#C4E538",
                               "High fasting plasma glucose"="#12CBC4", 
                               "Diet high in red meat"="#FDA7DF",
                               "Secondhand smoke"="#60B683", 
                               "High body-mass index"="#26A7FF",
                               "Alcohol use"="#FA7F6F"))+
  theme_light()+coord_flip()  + 
  geom_text(aes(label=text, y=percent), 
            position=position_stack(.5), vjust=0.3,
            size = 1.5) +
  theme(legend.position = "none") +  
  theme(axis.text.x = element_text(size = 7),
        axis.text.y = element_blank(),  
        axis.line.y = element_blank(),  
        axis.ticks.y = element_blank()) +  
  ggtitle("Low SDI")+
  theme(plot.title = element_text(hjust = 0.5, size = 10))  
print(p_LC_Low_SDI)






library(ggpubr) 
fig_all<-ggarrange(p_LC_Global,p_LC_High_SDI, p_LC_High_middle_SDI, p_LC_Middle_SDI, p_LC_Low_middle_SDI, p_LC_Low_SDI,ncol = 6, nrow = 1)+
  plot_layout(design = layout,guides = "collect") & 
print(fig_all)



p_list <- lapply(1:5, function(x) {
  if (x == 1) {
    return(p_LC_High_SDI)
  } else if (x == 2) {
    return(p_LC_High_middle_SDI)
  } else if (x == 3) {
    return(p_LC_Middle_SDI)
  } else if (x == 4) {
    return(p_LC_Low_middle_SDI)
  } else if (x == 5) {
    return(p_LC_Low_SDI)
  }
})





layout <- "
ABCDEF
"
p_LC_Global+p_LC_High_SDI+p_LC_High_middle_SDI+p_LC_Middle_SDI+p_LC_Low_middle_SDI+p_LC_Low_SDI+
  plot_layout(design = layout,
              guides = "collect") & 
  theme(legend.position = "bottom",legend.key.size = unit(5, "pt"))

ggsave("15-39岁death_factor排序.pdf",width = 15,height = 8)



library(patchwork)

combined_plot <- p_LC_Global + p_LC_High_SDI + p_LC_High_middle_SDI + p_LC_Middle_SDI + p_LC_Low_middle_SDI + p_LC_Low_SDI

combined_plot +
  plot_layout(design = layout) +
  theme(legend.position = "bottom", legend.key.size = unit(5, "pt"))
ggsave("15-39岁death_factor排序.pdf",width = 16,height = 8,dpi=1000)

