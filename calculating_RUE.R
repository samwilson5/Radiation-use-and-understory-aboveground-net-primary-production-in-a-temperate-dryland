# read in incident photosynthetically active Radiation data
PARi = read.csv("G:/My Drive/Thesis/Lab_Data/RUE_point_PARi_try7.csv")

# read in fraction of fAPARh data
fAPARh = read.csv("G:/My Drive/Thesis/Lab_Data/RUE_point_fAPARh_try6.csv")
library(dplyr)
 
# join the two datasets, there are two fields that need to be joined on because
# each plot has a bunch of imagery dates
data = left_join(PARi,fAPARh,by=c('imageryDate'='imageryDate','plot'='plot'))

data = data %>% rename(PARi = rads)

# calculate RUE Monteith et al. 1979
data$rue = data$PARi * data$fAPARh

# calculate total RUE
RUE = data %>% aggregate(rue ~ plot,FUN=sum)

library(readxl)
library(stringr)

# read in biomass data
biomass = read_xlsx("G:/My Drive/Thesis/Lab_Data/Biomass_SW23_UGRB.xlsx")
biomass$biomass = 1

for(i in 1:80){
  biomass[i,22] = rowMeans(biomass[i,2:21])
  biomass[i,1] = str_sub(biomass[i,1],-2,-1)
}



biomass$Plot = as.integer(biomass$Plot)
biomass$biomass = biomass$biomass * 10
biomass = left_join(biomass,RUE,join_by(Plot==plot))

RUE$rue = (biomass$biomass / biomass$rue)
####################################3


