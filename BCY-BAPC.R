rm(list=ls())
# install.packages("caTools")
# install.packages("fanplot")
# install.packages("Epi")
# install.packages("BAPC", repos = "http://R-Forge.R-project.org")

library(BAPC)
library(INLA)
library(data.table)
library(tidyverse)
library(dplyr)
library(ggplot2)
library(epitools)
library(reshape2)


setwd("C:\\Users\\25470\\Desktop\\BCY\\预测")
BC <- read.csv('BC_data.csv')
age_stand <- read.csv('2019年世界女性标准人口百分比（来自联合国）.csv')
rownames(age_stand) <- 1:nrow(age_stand)
unique(BC$age)

BC <- BC %>% mutate(age=sub('-',replacement = ' to ', age)) %>%
  mutate(age=sub(' years',replacement = '', age)) %>%
  mutate(age=sub(' year',replacement = '', age)) %>%
  mutate(age=sub('95\\+',replacement = '95 plus', age)) %>%
  mutate(age=sub('Age to standardized',replacement = 'Age-standardized', age)) %>%
  mutate(age=sub('<5',replacement = 'Under 5', age)) %>%
  filter(val>0)
unique(BC$age)


ages <- c("15 to 19","20 to 24", "25 to 29",
          "30 to 34", "35 to 39", "40 to 44", "45 to 49", "50 to 54", "55 to 59",
          "60 to 64", "65 to 69", "70 to 74", "75 to 79", "80 to 84", "85 to 89",
          "90 to 94", "95 plus")

ages_2 <- c("<1 year","1 to 4", "5 to 9","10 to 14", "15 to 19","20 to 24", "25 to 29",
            "30 to 34", "35 to 39", "40 to 44", "45 to 49", "50 to 54", "55 to 59",
            "60 to 64", "65 to 69", "70 to 74", "75 to 79", "80 to 84", "85 to 89",
            "90 to 94", "95 plus")

ages_3 <- c("0 to 4", "5 to 9","10 to 14", "15 to 19","20 to 24", "25 to 29",
            "30 to 34", "35 to 39", "40 to 44", "45 to 49", "50 to 54", "55 to 59",
            "60 to 64", "65 to 69", "70 to 74", "75 to 79", "80 to 84", "85 to 89",
            "90 to 94", "95 plus")

str(age_stand)
wstand <- c(age_stand$std_population[1:20]/sum(age_stand$std_population[1:20]))%>% as.numeric()
sum(wstand)




BC_Female_incidence <- BC %>% filter(age %in% ages &
                                       sex == 'Female' &
                                       metric == 'Number' &
                                       measure == 'Incidence' &
                                       location== 'Global')
BC_Female_incidence_n <- reshape2::dcast(data = BC_Female_incidence, year~age, value.var = "val")

BC_Female_incidence_n[,'0 to 4'] <- 0
BC_Female_incidence_n[,'5 to 9'] <- 0
BC_Female_incidence_n[,'10 to 14'] <- 0
BC_Female_incidence_n <- BC_Female_incidence_n %>%
  arrange(year) 
rownames(BC_Female_incidence_n) <- BC_Female_incidence_n$year
BC_Female_incidence_n <- BC_Female_incidence_n[,-1] 
BC_Female_incidence_n <- BC_Female_incidence_n %>%
  apply(c(1,2), as.numeric) %>%
  apply(c(1,2), round) %>%
  as.data.frame() %>%
  select(ages_3) 



dirname <- dir("GBD_Population") 
file <- paste0(getwd(),"/GBD_Population/",dirname) 

var_name <- c('location_id',"location_name","sex_name","year_id","age_group_name","val")

GBD_population <- as.data.frame(matrix(nrow=0,ncol=length(var_name)))
names(GBD_population)=var_name 
for (a in file) { 
  data <- fread(a) %>% as.data.frame() %>% select(var_name) %>% 
    filter(age_group_name %in% ages_2 & location_id !=533)
  GBD_population <- rbind(GBD_population,data)
}


GBD_population <- GBD_population %>% mutate(sex_name = case_when(
  sex_name == "both" ~ "Both",
  sex_name == "male" ~ "Male",
  sex_name == "female" ~ "Female")) %>%
  select(-1)




prediction_var_name <- c("location_name","sex","year_id","age_group_name","val") ### 对列名重新命名
GBD_population_prediction <- fread('IHME_POP_2017_2100_POP_REFERENCE_Y2020M05D01.csv') %>%
  as.data.frame() %>%
  select(prediction_var_name) %>%
  filter(year_id %in% 2020:2030)

names(GBD_population_prediction) <- var_name[-1]
unique(GBD_population_prediction$age_group_name)

GBD_1year <- GBD_population_prediction %>%
  filter(age_group_name %in% c("Early Neonatal",
                               "Late Neonatal", "Post Neonatal")) %>%
  group_by(location_name,sex_name,year_id) %>%
  summarise(val=sum(val)) %>%
  mutate(age_group_name="<1 year") %>%
  select(var_name[-1])

GBD_population_prediction <- GBD_population_prediction %>% filter(!(age_group_name %in% c("Early Neonatal","Late Neonatal", "Post Neonatal"))) %>%
  rbind(GBD_1year)
unique(GBD_population_prediction$age_group_name)


GBD <- rbind(GBD_population,GBD_population_prediction)
unique(GBD$age_group_name)


GBD_age4 <- GBD %>% subset(age_group_name %in% c("<1 year","1 to 4")) %>%
  group_by(location_name,sex_name,year_id) %>%
  summarize(val=sum(val)) %>% mutate(age_group_name='0 to 4') %>%
  select(var_name[-1])
GBD <- rbind(GBD,GBD_age4) 


GBD <- subset(GBD, age_group_name %in% ages_3) %>%
  mutate(age_group_name=factor(age_group_name,levels=ages_3,ordered = T)) %>%
  arrange(age_group_name)


GBD_Global_Female <- GBD %>% filter(location_name=='Global' & sex_name == 'Female')

GBD_Global_Female_n <- reshape2::dcast(data = GBD_Global_Female,year_id ~ age_group_name,
                                       value.var = c("val")) %>%
  select(-1)



EC_pro <- matrix(data = NA, nrow = 2030-2019, ncol = ncol(GBD_Global_Female_n)) %>% as.data.frame()
rownames(EC_pro) <- seq(2020,2030,1)
colnames(EC_pro) <- names(BC_Female_incidence_n)
BC_Female_incidence_n  <- rbind(BC_Female_incidence_n , EC_pro)




Female_esoph <- APCList(BC_Female_incidence_n, GBD_Global_Female_n, gf = 5)
Female_bapc_result <- BAPC(Female_esoph, predict = list(npredict = 11, retro = TRUE),
                           secondDiff = FALSE, stdweight = wstand, verbose = F)

Female_bapc_result@agespec.rate[["0 to 4"]] <- matrix(0,nrow=2030-1989,ncol=2) %>% as.data.frame()
names(Female_bapc_result@agespec.rate[["0 to 4"]]) <- c('mean','sd')
Female_bapc_result@agespec.rate[["5 to 9"]] <- matrix(0,nrow=2030-1989,ncol=2) %>% as.data.frame()
names(Female_bapc_result@agespec.rate[["5 to 9"]]) <- c('mean','sd')
Female_bapc_result@agespec.rate[["10 to 14"]] <- matrix(0,nrow=2030-1989,ncol=2) %>% as.data.frame()
names(Female_bapc_result@agespec.rate[["10 to 14"]]) <- c('mean','sd')

Female_bapc_result@agespec.proj[["0 to 4"]] <- matrix(0,nrow=2030-1989,ncol=2) %>% as.data.frame()
names(Female_bapc_result@agespec.proj[["0 to 4"]]) <- c('mean','sd')
Female_bapc_result@agespec.proj[["5 to 9"]] <- matrix(0,nrow=2030-1989,ncol=2) %>% as.data.frame()
names(Female_bapc_result@agespec.proj[["5 to 9"]]) <- c('mean','sd')
Female_bapc_result@agespec.proj[["10 to 14"]] <- matrix(0,nrow=2030-1989,ncol=2) %>% as.data.frame()
names(Female_bapc_result@agespec.proj[["10 to 14"]]) <- c('mean','sd')





Female_proj <- agespec.proj(x = Female_bapc_result) %>% as.data.frame() ###
Female_proj_mean <- Female_proj[,colnames(Female_proj) %like% 'mean']
names(Female_proj_mean) <- ages_3



Female_rate <- agespec.rate(x = Female_bapc_result) %>% as.data.frame()
Female_rate_mean <- Female_rate[,colnames(Female_rate) %like% 'mean']*100000
colnames(Female_rate_mean) <- ages_3


Female_ASR <- agestd.rate(x =Female_bapc_result) %>% as.data.frame()
Female_ASR$mean <- Female_ASR$mean*100000
Female_ASR$year <- rownames(Female_ASR)


Female_sum_year <- apply(Female_proj_mean, 1, sum) %>% as.data.frame()
colnames(Female_sum_year) <- 'number'
Female_sum_year$year <- rownames(Female_sum_year)
Female_sum_year <- apply(Female_sum_year, 2, as.numeric) %>% as.data.frame()



plotBAPC(Female_bapc_result, scale=10^5, type = 'ageStdRate',showdata = T)
win.graph(width=40, height=40)
plotBAPC(Female_bapc_result, scale=10^5, type = 'ageSpecRate',showdata = T) 
win.graph(width=40, height=40)
plotBAPC(Female_bapc_result, scale=10^5, type = 'ageSpecProj',showdata = T) 



Female_bapc_result <- qapc(Female_bapc_result,percentiles=c(0.025,0.975))


Female_ASR <- agestd.rate(x = Female_bapc_result) %>% as.data.frame()*10^5
Female_ASR$year <- 1990:2030

Female_Aproj <- agestd.proj(x = Female_bapc_result) %>% as.data.frame()
Female_Aproj$year <- 1990:2030


Female_proj_mean <- matrix(NA,nrow= 2030-1989, ncol=length(ages_3)) %>% as.data.frame()
Female_proj_low <- matrix(NA,nrow= 2030-1989, ncol=length(ages_3)) %>% as.data.frame()
Female_proj_up <- matrix(NA,nrow= 2030-1989, ncol=length(ages_3)) %>% as.data.frame()


for (i in 1:length(ages_3)) {
  Female_proj_mean[,i] <- agespec.proj(Female_bapc_result)[[i]][,1]
  Female_proj_low[,i] <- agespec.proj(Female_bapc_result)[[i]][,3]
  Female_proj_up[,i] <- agespec.proj(Female_bapc_result)[[i]][,4]
}
names(Female_proj_mean) <- ages_3
rownames(Female_proj_mean) <- 1990:2030
names(Female_proj_up) <- ages_3
rownames(Female_proj_up) <- 1990:2030
names(Female_proj_low) <- ages_3
rownames(Female_proj_low) <- 1990:2030
head(Female_proj_mean)


Female_rate_mean <- matrix(NA,nrow= 2030-1989, ncol=length(ages_3)) %>% as.data.frame()
Female_rate_low <- matrix(NA,nrow= 2030-1989, ncol=length(ages_3)) %>% as.data.frame()
Female_rate_up <- matrix(NA,nrow= 2030-1989, ncol=length(ages_3)) %>% as.data.frame()

for (i in 1:length(ages_3)) {
  Female_rate_mean[,i] <- agespec.rate(Female_bapc_result)[[i]][,1]*10^5
  Female_rate_low[,i] <- agespec.rate(Female_bapc_result)[[i]][,3]*10^5
  Female_rate_up[,i] <- agespec.rate(Female_bapc_result)[[i]][,4]*10^5
}
names(Female_rate_mean) <- ages_3
rownames(Female_rate_mean) <- 1990:2030
names(Female_rate_up) <- ages_3
rownames(Female_rate_up) <- 1990:2030
names(Female_rate_low) <- ages_3
rownames(Female_rate_low) <- 1990:2030



# write.csv(Female_rate_mean,"Female_rate_incidence_mean_BAPC_1990-2030.csv",row.names =T)
# write.csv(Female_rate_low,"Female_rate_incidence_low_BAPC_1990-2030.csv",row.names =T)
# write.csv(Female_rate_up,"Female_rate_incidence_up_BAPC_1990-2030.csv",row.names =T)
# 
# write.csv(Female_proj_mean,"Female_numbere_incidence_mean_BAPC_1990-2030.csv",row.names =T)
# write.csv(Female_proj_low,"Female_number_incidence_low_BAPC_1990-2030.csv",row.names =T)
# write.csv(Female_proj_up,"Female_number_incidence_up_BAPC_1990-2030.csv",row.names =T)
# 
# write.csv(Female_ASR,"Female_rate_incidence_ASR_BAPC_1990-2030.csv",row.names =T)
# write.csv(Female_Aproj,"Female_number_incidence_ASR_BAPC_1990-2030.csv",row.names =T)




