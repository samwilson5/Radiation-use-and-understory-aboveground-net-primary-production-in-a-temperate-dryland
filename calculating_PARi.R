# this script uses data from ERA5 and fAPARh data to calculate PARi 
# (incident photosynthetically active radiation)

# read in the radiation data (ERA5), downloaded from GEE
data = read.csv("G:/My Drive/Thesis/Lab_Data/RUE_radiationNEW.csv")
data = data %>% rename(date = system.time_start, rads = surface_solar_radiation_downwards)
# data cleaning
data$date = format(as.Date(data$date, format = "%b %d, %Y"),'%Y-%m-%d')
data$rads = as.numeric(gsub(",", "", data$rads))
# conversion
data$rads = data$rads * (10^-6)
# there are a bunch of readings per day, so sum them up to get daily
#data_agg = data %>% aggregate(rads ~ date,FUN=sum)

#radiation = data_agg %>% filter(date >= '2023-04-15' & date <= '2023-08-01')
#radiation = data_agg %>% filter(date >= '2023-04-15' & date <= '2023-09-01')
# read in fapar
fAPAR = read.csv("G:/My Drive/Thesis/Lab_Data/RUE_point_fAPARh_try2.csv")

# clean the data, remove na's
for(i in 1:80) {
  fAP = fAPAR %>% filter(plot == i)
  keeps = c('plot','imageryDate')
  fAP = fAP[keeps]
  fAP$date = fAP$imageryDate
  temp = left_join(radiation,fAP,join_by(date==date))
  temp$imageryDate = zoo::na.locf(temp$imageryDate,fromLast = TRUE,na.rm=FALSE)
  temp$plot = zoo::na.locf(temp$plot,fromLast = TRUE,na.rm=FALSE)
  plotRads = temp %>% aggregate(rads ~ imageryDate,FUN=sum)
  plotRads = plotRads
  plotRads$plot = i
  if (i==1){finalRads = plotRads}
  else{finalRads = rbind(finalRads,plotRads)}
}

# calculate PARi based on paruelo et al 
finalRads$rads = finalRads$rads * 0.48
write.csv(finalRads,"G:/My Drive/Thesis/Lab_Data/RUE_point_PARi_try7.csv")
