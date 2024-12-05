library(stringr)
library(dplyr)
# load radiation data
# this script calculates estimated ANPPh for simulated plots in the UGRB, WY
# data is taken from daymet, ERA5, and Sentinel-2: all downloaded from GEE
# the plot created at the bottom appears in Wilson et al 2025

# read in 2019 radiation data
ruerad2019 = read.csv("C:/Users/Sam/Downloads/UGRB_radiation_2019.csv")
ruerad2019$point = 1:1218
ruerad2019$longitude = 1
ruerad2019$latitude = 1
# clean the data
for(i in 1:1218){
  ruerad2019[i,369] =  substring(str_split((str_split(ruerad2019$.geo,':')[i])[[1]][4],',')[[1]][1],2)
  ruerad2019[i,370] = substring(toString((str_split(ruerad2019$.geo,':')[i])[[1]][4]),22,35)}
substrRight <- function(x, n){
  substr(x, nchar(x)-n+1, nchar(x))
}

for(i in 1:370){
  name = colnames(ruerad2019)[i]
  if (substring(toString(name),1,1) == 'X'){
    colnames(ruerad2019)[i]=substrRight(toString(name), 10)
  }
  
}

ruerad2019[ruerad2019 == 50] <- NA
#############################################
ruerad2020 = read.csv("C:/Users/Sam/Downloads/UGRB_radiation_2020.csv")
ruerad2020$point = 1:1218
ruerad2020$longitude = 1
ruerad2020$latitude = 1
for(i in 1:1218){
  ruerad2020[i,369] =  substring(str_split((str_split(ruerad2020$.geo,':')[i])[[1]][4],',')[[1]][1],2)
  ruerad2020[i,370] = substring(toString((str_split(ruerad2020$.geo,':')[i])[[1]][4]),22,35)}
substrRight <- function(x, n){
  substr(x, nchar(x)-n+1, nchar(x))
}

for(i in 1:370){
  name = colnames(ruerad2020)[i]
  if (substring(toString(name),1,1) == 'X'){
    colnames(ruerad2020)[i]=substrRight(toString(name), 10)
  }
  
}
ruerad2020[ruerad2020 == 50] <- NA
############################################
ruerad2021 = read.csv("C:/Users/Sam/Downloads/UGRB_radiation_2021.csv")
ruerad2021$point = 1:1218
ruerad2021$longitude = 1
ruerad2021$latitude = 1
for(i in 1:1218){
  ruerad2021[i,369] =  substring(str_split((str_split(ruerad2021$.geo,':')[i])[[1]][4],',')[[1]][1],2)
  ruerad2021[i,370] = substring(toString((str_split(ruerad2021$.geo,':')[i])[[1]][4]),22,35)}
substrRight <- function(x, n){
  substr(x, nchar(x)-n+1, nchar(x))
}
for(i in 1:370){
  name = colnames(ruerad2021)[i]
  if (substring(toString(name),1,1) == 'X'){
    colnames(ruerad2021)[i]=substrRight(toString(name), 10)
  }
  
}
ruerad2021[ruerad2021 == 50] <- NA
#######################################
ruerad2022 = read.csv("C:/Users/Sam/Downloads/UGRB_radiation_2022.csv")
ruerad2022$point = 1:1218
ruerad2022$longitude = 1
ruerad2022$latitude = 1
for(i in 1:1218){
  ruerad2022[i,369] =  substring(str_split((str_split(ruerad2022$.geo,':')[i])[[1]][4],',')[[1]][1],2)
  ruerad2022[i,370] = substring(toString((str_split(ruerad2022$.geo,':')[i])[[1]][4]),22,35)}
substrRight <- function(x, n){
  substr(x, nchar(x)-n+1, nchar(x))
}
for(i in 1:370){
  name = colnames(ruerad2022)[i]
  if (substring(toString(name),1,1) == 'X'){
    colnames(ruerad2022)[i]=substrRight(toString(name), 10)
  }
  
}
ruerad2022[ruerad2022 == 50] <- NA
#######################################
ruerad2023 = read.csv("C:/Users/Sam/Downloads/UGRB_radiation_2023.csv")
ruerad2023$point = 1:1218
ruerad2023$longitude = 1
ruerad2023$latitude = 1
for(i in 1:1218){
  ruerad2023[i,369] =  substring(str_split((str_split(ruerad2023$.geo,':')[i])[[1]][4],',')[[1]][1],2)
  ruerad2023[i,370] = substring(toString((str_split(ruerad2023$.geo,':')[i])[[1]][4]),22,35)}
substrRight <- function(x, n){
  substr(x, nchar(x)-n+1, nchar(x))
}
for(i in 1:370){
  name = colnames(ruerad2023)[i]
  if (substring(toString(name),1,1) == 'X'){
    colnames(ruerad2023)[i]=substrRight(toString(name), 10)
  }
  
}
ruerad2023[ruerad2023 == 50] <- NA
#######################################
# load in NDVI
rueNDVI2019 = read.csv("C:/Users/Sam/Downloads/UGRB_NDVI_2019.csv")
rueNDVI2019$point = 1:1218
rueNDVI2019$longitude = 1
rueNDVI2019$latitude = 1
# clean the data and add lat, long
for(i in 1:1218){
rueNDVI2019[i,222] =  substring(str_split((str_split(rueNDVI2019$.geo,':')[i])[[1]][4],',')[[1]][1],2)
rueNDVI2019[i,223] = substring(str_split((str_split(rueNDVI2019$.geo,':')[i])[[1]][4],',')[[1]][2],1,17)}

for(i in 1:223){
  name = colnames(rueNDVI2019)[i]
  if (substring(toString(name),1,1) == 'X'){
    colnames(rueNDVI2019)[i]=substrRight(toString(name), 10)
  }
  
}

rueNDVI2019[rueNDVI2019 == 50] <- NA
#####################################################
rueNDVI2020 = read.csv("C:/Users/Sam/Downloads/UGRB_NDVI_2020.csv")
rueNDVI2020$point = 1:1218
rueNDVI2020$longitude = 1
rueNDVI2020$latitude = 1
for(i in 1:1218){
  rueNDVI2020[i,222] =  substring(str_split((str_split(rueNDVI2020$.geo,':')[i])[[1]][4],',')[[1]][1],2)
  rueNDVI2020[i,223] = substring(str_split((str_split(rueNDVI2020$.geo,':')[i])[[1]][4],',')[[1]][2],1,17)}

for(i in 1:223){
  name = colnames(rueNDVI2020)[i]
  if (substring(toString(name),1,1) == 'X'){
    colnames(rueNDVI2020)[i]=substrRight(toString(name), 10)
  }
  
}

rueNDVI2020[rueNDVI2020 == 50] <- NA
#####################################################
rueNDVI2021 = read.csv("C:/Users/Sam/Downloads/UGRB_NDVI_2021.csv")
rueNDVI2021$point = 1:1218
rueNDVI2021$longitude = 1
rueNDVI2021$latitude = 1
for(i in 1:1218){
  rueNDVI2021[i,222] =  substring(str_split((str_split(rueNDVI2021$.geo,':')[i])[[1]][4],',')[[1]][1],2)
  rueNDVI2021[i,223] = substring(str_split((str_split(rueNDVI2021$.geo,':')[i])[[1]][4],',')[[1]][2],1,17)}

for(i in 1:223){
  name = colnames(rueNDVI2021)[i]
  if (substring(toString(name),1,1) == 'X'){
    colnames(rueNDVI2021)[i]=substrRight(toString(name), 10)
  }
  
}

rueNDVI2021[rueNDVI2021 == 50] <- NA
#####################################################

rueNDVI2022 = read.csv("C:/Users/Sam/Downloads/UGRB_NDVI_2022.csv")
rueNDVI2022$point = 1:1218
rueNDVI2022$longitude = 1
rueNDVI2022$latitude = 1
for(i in 1:1218){
  rueNDVI2022[i,222] =  substring(str_split((str_split(rueNDVI2022$.geo,':')[i])[[1]][4],',')[[1]][1],2)
  rueNDVI2022[i,223] = substring(str_split((str_split(rueNDVI2022$.geo,':')[i])[[1]][4],',')[[1]][2],1,17)}

for(i in 1:223){
  name = colnames(rueNDVI2022)[i]
  if (substring(toString(name),1,1) == 'X'){
    colnames(rueNDVI2022)[i]=substrRight(toString(name), 10)
  }
  
}

rueNDVI2022[rueNDVI2022 == 50] <- NA
#####################################################

rueNDVI2023 = read.csv("C:/Users/Sam/Downloads/UGRB_NDVI_2023.csv")
rueNDVI2023$point = 1:1218
rueNDVI2023$longitude = 1
rueNDVI2023$latitude = 1
for(i in 1:1218){
  rueNDVI2023[i,222] =  substring(str_split((str_split(rueNDVI2023$.geo,':')[i])[[1]][4],',')[[1]][1],2)
  rueNDVI2023[i,223] = substring(str_split((str_split(rueNDVI2023$.geo,':')[i])[[1]][4],',')[[1]][2],1,17)}

for(i in 1:223){
  name = colnames(rueNDVI2023)[i]
  if (substring(toString(name),1,1) == 'X'){
    colnames(rueNDVI2023)[i]=substrRight(toString(name), 10)
  }
  
}

rueNDVI2023[rueNDVI2023 == 50] <- NA
#####################################################

# put the years together for radiaiton and NDVI
rueRAD = ruerad2019 %>% left_join(ruerad2020,join_by('system.index'=='system.index')) %>%
  left_join(ruerad2021,join_by('system.index'=='system.index')) %>%
  left_join(ruerad2022,join_by('system.index'=='system.index')) %>%
  left_join(ruerad2023,join_by('system.index'=='system.index'))

rueNDVI = rueNDVI2019 %>% left_join(rueNDVI2020,join_by('.geo'=='.geo')) %>%
  left_join(rueNDVI2021,join_by('.geo'=='.geo')) %>%
  left_join(rueNDVI2022,join_by('.geo'=='.geo')) %>%
  left_join(rueNDVI2023,join_by('.geo'=='.geo'))

rueNDVI$max = 0
rueNDVI$min = 0

# need to get the upper and lower bounds for each site
for(i in 1:1218){
  keeps = colnames(rueNDVI)
  keeps = keeps[grep('20', keeps)]
  data = rueNDVI[keeps]
  data = data %>% slice(i)
  max = quantile(data,probs=0.98,na.rm=T)
  min = quantile(data,probs=0.05,na.rm=T)
  rueNDVI[i,1112] = max
  rueNDVI[i,1113] = min
}
#################################################


updatedNDVI = data.frame(date = as.Date(keeps,'%Y.%m.%d'))
data = rueNDVI[keeps]
for(i in 1:1218){
  for (j in 1:1082){
    num = data[i,j]
    updatedNDVI[j,i+1] = num
  }}
reference_ndvi = updatedNDVI
reference_ndvi[1083,2:1219] = rueNDVI$V223.y
reference_ndvi[1084,2:1219] = rueNDVI$V222
###############################################



workingNDVI = updatedNDVI[,colSums(is.na(updatedNDVI))<nrow(updatedNDVI)]
reference_ndvi = reference_ndvi[,colSums(is.na(reference_ndvi[1:1082,]))<nrow(reference_ndvi[1:1082,])]

# certain values need to be removed becaouse they are extreme outliers
toDrop = c('2019-01-06','2019-03-22','2020-01-05','2020-05-04',
           '2021-01-04','2022-03-30','2022-01-04')
workingNDVI = workingNDVI %>%
  filter(! date %in% toDrop)
workingNDVI = workingNDVI[order(workingNDVI$date),]
seasonalNDVI = data.frame(date = as.Date(workingNDVI[,1],'%Y.%m.%d'))
trendNDVI = data.frame(date = as.Date(workingNDVI[,1],'%Y.%m.%d'))
remainderNDVI = data.frame(date = as.Date(workingNDVI[,1],'%Y.%m.%d'))
library(zoo)
library(stlplus)
# break the ndvi into parts using seasonal trend decomposition loess
for(i in 1:892){
  time = na.approx(workingNDVI[,i+1],rule=2)
  ts = ts(time,frequency = 215,start=c(2019,1),end=c(2023,215))
  loess = stl(ts,s.window='per')
  seasonalNDVI[,i+1] = as.data.frame(seasonal(loess))
  trendNDVI[,i+1] = as.data.frame(trend(loess))
  remainderNDVI[,i+1] = as.data.frame(remainder(loess))
}


xA_NDVI = data.frame(date = as.Date(workingNDVI[,1],'%Y.%m.%d'))
seasonal_amp = data.frame(date = as.Date(workingNDVI[,1],'%Y.%m.%d'))
last_year=10
for(i in 1:892){
  for (j in 1:1075){
    year = format(as.Date(seasonalNDVI[j,1]), format = "%Y")
    if (year!=last_year){
    start = as.Date(paste0(year,'-01-01'))
    end = as.Date(paste0(year,'-12-31'))
    filtered = seasonalNDVI %>% filter(date>=start & date<=end)
    xc_min = min(filtered[,i+1])
    xc_max = max(filtered[,i+1])}
    else{
      xc_min=xc_min
      xc_max=xc_max
    }
    xA_NDVI[j,i+1] = xc_max - xc_min
    xc_adj = max(seasonalNDVI[j,i+1],(workingNDVI[j,i+1]-trendNDVI[j,i+1]),na.rm=TRUE)
    seasonal_amp[j,i+1] = (xc_adj - xc_min)/(xc_max-xc_min)
    last_year = year
  }
}



herbaceous_NDVI = data.frame(date = as.Date(workingNDVI[,1],'%Y.%m.%d'))
d=0.04 #blanco et al 2016
lambda = 0.1 #blanco et al 2016
# calculate the herbaceous component of NDVI
for(i in 1:892){
  s = mean(seasonal_amp[,i+1])
  for(j in 1:1075){
    st = seasonal_amp[j,i+1]
    xA = xA_NDVI[j,i+1]
    xt = trendNDVI[j,i+1]
    herbaceous_NDVI[j,i+1]=(st*((1-lambda*s)*xA - (lambda*xt))) + (lambda*st*d)
  }
}


# months with snow typically in the UGRB, assign 0 to NDVI
snowymonths = c('12','01','02','03')
for(i in 1:892){
  for(j in 1:1075){
    month = format(as.Date(seasonalNDVI[j,1]), format = "%m")
    if(month %in% snowymonths){
      val =  herbaceous_NDVI[j,i+1]
      if (val>=0.1){
      herbaceous_NDVI[j,i+1] = herbaceous_NDVI[j,i+1]*0#.1
    }}
    
  }
}
reference_hndvi = herbaceous_NDVI
reference_hndvi[1083,2:893] = reference_ndvi[1083,2:893]
reference_hndvi[1084,2:893] = reference_ndvi[1084,2:893]
####################################
columns = colnames(seasonalNDVI)
columns = columns[grep('x', columns)]
library(tidyr)
ndvi_long <- seasonalNDVI %>% pivot_longer(columns, names_to = "plot", values_to = "val")
library(ggplot2)
ggplot(ndvi_long, aes(x = date, y = val, color = plot)) + 
  geom_line(color='grey80')+
  theme(legend.position='none')
#########################################################
fPARh = data.frame(date = as.Date(workingNDVI[,1],'%Y.%m.%d'))

#############################################
# calculating fAPARh for each site at each date, see calculating_fAPARh.R for details
for(i in 1:892){
  highQuant = quantile(herbaceous_NDVI[,i+1],probs = 0.98)
  lowQuant = quantile(herbaceous_NDVI[,i+1],probs=0.05)
  sub = -(1.5445* lowQuant)
  for (j in 1:1075){
    num = herbaceous_NDVI[j,i+1]
    if(num<lowQuant){fap = 0}
    else if(num>highQuant){fap=0.95}
    else{fap=min((sub+1.5445*herbaceous_NDVI[j,i+1]),0.95)}
    fPARh[j,i+1] = fap
  }
}
reference_fapar = fPARh
reference_fapar[1083,2:893] = reference_ndvi[1083,2:893]
reference_fapar[1084,2:893] = reference_ndvi[1084,2:893]
##############################################
fPARh_long <- fPARh %>% pivot_longer(columns, names_to = "plot", values_to = "val")
ggplot(fPARh_long, aes(x = date, y = val, color = plot)) + 
  geom_line()+
  theme(legend.position='none')



RADkeeps = colnames(rueRAD)
RADkeeps = RADkeeps[grep('20', RADkeeps)]
updatedRads = data.frame(date = as.Date(RADkeeps,'%Y.%m.%d'))

raddata = rueRAD[RADkeeps]
for(i in 1:1218){
  for (j in 1:1821){
    num = raddata[i,j]
    updatedRads[j,i+1] = num
  }
}

rad_long <- updatedRads %>% pivot_longer(columns, names_to = "plot", values_to = "val")
ggplot(rad_long, aes(x = date, y = val,color=plot)) + 
  geom_line(color='grey80')+
  theme(legend.position='none')

# updatedNDVI, updatedRads, fPARh
# estimate ANPPh at each date at each site Monteith et al 1979
anpp_estimate = data.frame(date = as.Date(workingNDVI[,1],'%Y.%m.%d'))
updatedRads_short = updatedRads %>% filter(date %in%  as.Date(workingNDVI[,1],'%Y.%m.%d'))
reference_updatedRads_short = updatedRads_short
reference_updatedRads_short[1083,2:893] = reference_ndvi[1083,2:893]
reference_updatedRads_short[1084,2:893] = reference_ndvi[1084,2:893]
anpp_estimate = data.frame(anpp_estimate[order(anpp_estimate$date),])##
updatedRads = updatedRads[order(updatedRads$date),]
fPARh = fPARh[order(fPARh$date),]
for(i in 1:892){
  for (j in 1:1075){
    anpp_estimate[j,i+1] = ((updatedRads[j,i+1]/1000000)*0.48) * fPARh[j,i+1] * 0.56
  }
}

reference_anpp = anpp_estimate
reference_anpp[1083,2:893] = reference_ndvi[1083,2:893]
reference_anpp[1084,2:893] = reference_ndvi[1084,2:893]
#write.csv(reference_anpp,"G:/My Drive/reference_anpp.csv")
#write.csv(reference_hndvi,"G:/My Drive/reference_hndvi.csv")
#write.csv(reference_updatedRads_short,"G:/My Drive/reference_rads.csv")
#write.csv(reference_fapar,"G:/My Drive/reference_fapar.csv")

herbaceous_NDVI$mean = rowMeans(herbaceous_NDVI[,2:893],na.rm = T)
updatedRads_short$mean = rowMeans(updatedRads_short[,2:1219],na.rm=T)
fPARh$mean = rowMeans(fPARh[,2:893],na.rm=T)
anpp_estimate$mean = rowMeans(anpp_estimate[,2:893],na.rm=T)

ndvi_long <- updatedNDVI %>% pivot_longer(columns, names_to = "plot", values_to = "val")
rad_long <- updatedRads_short %>% pivot_longer(columns, names_to = "plot", values_to = "val")
fPARh_long <- fPARh %>% pivot_longer(columns, names_to = "plot", values_to = "val")
anpp_long <- anpp_estimate %>% pivot_longer(columns, names_to = "plot", values_to = "val")
anpp_estimate = anpp_estimate %>% rename(date = colnames(anpp_estimate)[1])
ggplot(anpp_estimate%>%filter(!is.na(mean)), aes(x = date, y = mean)) + 
  geom_line()

x=fPARh %>% filter(mean==min((as.data.frame(fPARh %>% filter(date>='2023-01-01'&date<='2023-12-31'))$mean))) #minimum occurred on Jan 5, 2021
# 2019min = Feb 17, 2020min= Feb 19, 2021min= Jan 5, 2022min = Feb 19, 2023min = Jan 5
x=fPARh %>% filter(mean==max((as.data.frame(fPARh %>% filter(date>='2023-01-01'&date<='2023-12-31'))$mean))) #maximum occurred on June 6, 2023
# 2019max = June 12, 2020max = June 6, 2021max = June 8, 2022max = June 16, 2023max = June 6
x=anpp_estimate %>% filter(mean==min((as.data.frame(anpp_estimate %>% filter(date>='2022-01-01'&date<='2022-12-31'))$mean))) #minimum occurred on Jan 1, 2023
# 2019min = Feb 10, 2020min = Feb 19,2021min = Jan 5, 2022min= December 30, 2023min=Jan 5, 
x=anpp_estimate %>% filter(mean==max((as.data.frame(anpp_estimate %>% filter(date>='2023-01-01'&date<='2023-12-31'))$mean))) #maximum occurred on June 26, 2023
# 2019max = July 2, 2020max = June 6, 2021max = June 26, 2022max = June 1, 2023max = June 26
updatedRads_short %>% filter(mean==min(updatedRads_short$mean)) #minimum occurred on Nov 11, 2019
z=updatedRads_short %>% filter(mean==max(updatedRads_short$mean)) #maximum occurred on June 22, 2022

herbaceous_NDVI$cv = apply(herbaceous_NDVI[,2:894], 1, sd, na.rm=TRUE)
updatedRads_short$cv = apply(updatedRads_short[,2:1219], 1, sd, na.rm=TRUE)
fPARh$cv = apply(fPARh[,2:894], 1, sd, na.rm=TRUE)
anpp_estimate$cv = apply(anpp_estimate[,2:894], 1, sd, na.rm=TRUE)

write.csv(reference_updatedRads_short,"G:/My Drive/reference_rads.csv")
#############################################################################3
# load in daymet data taken from Gee
rueclimate2019 = read.csv("D:/misc/UGRB_climate_2019.csv")

for(i in 1:1095){
  name = colnames(rueclimate2019)[i]
  if (substring(toString(name),1,1) == 'X'){
    if(grepl('ppt', name, fixed = TRUE)){
      colnames(rueclimate2019)[i]=paste0('ppt_',substrRight(toString(name), 10))
    }
    else if(grepl('tmin', name, fixed = TRUE)){
      colnames(rueclimate2019)[i]=paste0('tmin_',substrRight(toString(name),10))
    }
    else if(grepl('tmax', name, fixed = TRUE)){
      colnames(rueclimate2019)[i]=paste0('tmax_',substrRight(toString(name),10))
    }
  }
  
}
rueclimate2019[rueclimate2019 == 50] <- NA
######################################################3
rueclimate2020 = read.csv("D:/misc/UGRB_climate_2020.csv")
for(i in 1:1095){
  name = colnames(rueclimate2020)[i]
  if (substring(toString(name),1,1) == 'X'){
    if(grepl('ppt', name, fixed = TRUE)){
      colnames(rueclimate2020)[i]=paste0('ppt_',substrRight(toString(name), 10))
    }
    else if(grepl('tmin', name, fixed = TRUE)){
      colnames(rueclimate2020)[i]=paste0('tmin_',substrRight(toString(name),10))
    }
    else if(grepl('tmax', name, fixed = TRUE)){
      colnames(rueclimate2020)[i]=paste0('tmax_',substrRight(toString(name),10))
    }
  }
  
}
rueclimate2020[rueclimate2020 == 50] <- NA
################################################333
rueclimate2021 = read.csv("D:/misc/UGRB_climate_2021.csv")
for(i in 1:1095){
  name = colnames(rueclimate2021)[i]
  if (substring(toString(name),1,1) == 'X'){
    if(grepl('ppt', name, fixed = TRUE)){
      colnames(rueclimate2021)[i]=paste0('ppt_',substrRight(toString(name), 10))
    }
    else if(grepl('tmin', name, fixed = TRUE)){
      colnames(rueclimate2021)[i]=paste0('tmin_',substrRight(toString(name),10))
    }
    else if(grepl('tmax', name, fixed = TRUE)){
      colnames(rueclimate2021)[i]=paste0('tmax_',substrRight(toString(name),10))
    }
  }
  
}
rueclimate2021[rueclimate2021 == 50] <- NA
#####################################################
rueclimate2022 = read.csv("D:/misc/UGRB_climate_2022.csv")
for(i in 1:1095){
  name = colnames(rueclimate2022)[i]
  if (substring(toString(name),1,1) == 'X'){
    if(grepl('ppt', name, fixed = TRUE)){
      colnames(rueclimate2022)[i]=paste0('ppt_',substrRight(toString(name), 10))
    }
    else if(grepl('tmin', name, fixed = TRUE)){
      colnames(rueclimate2022)[i]=paste0('tmin_',substrRight(toString(name),10))
    }
    else if(grepl('tmax', name, fixed = TRUE)){
      colnames(rueclimate2022)[i]=paste0('tmax_',substrRight(toString(name),10))
    }
  }
  
}
rueclimate2022[rueclimate2022 == 50] <- NA
######################################################
rueclimate2023 = read.csv("D:/misc/UGRB_climate_2023.csv")
for(i in 1:1095){
  name = colnames(rueclimate2023)[i]
  if (substring(toString(name),1,1) == 'X'){
    if(grepl('ppt', name, fixed = TRUE)){
      colnames(rueclimate2023)[i]=paste0('ppt_',substrRight(toString(name), 10))
    }
    else if(grepl('tmin', name, fixed = TRUE)){
      colnames(rueclimate2023)[i]=paste0('tmin_',substrRight(toString(name),10))
    }
    else if(grepl('tmax', name, fixed = TRUE)){
      colnames(rueclimate2023)[i]=paste0('tmax_',substrRight(toString(name),10))
    }
  }
  
}
rueclimate2023[rueclimate2023 == 50] <- NA
#############################
rueClimate = rueclimate2019 %>% left_join(rueclimate2020,join_by('.geo'=='.geo')) %>%
  left_join(rueclimate2021,join_by('.geo'=='.geo')) %>%
  left_join(rueclimate2022,join_by('.geo'=='.geo')) %>%
  left_join(rueclimate2023,join_by('.geo'=='.geo'))

ClimateColumns = colnames(rueClimate)
pptColumns = ClimateColumns[grep('ppt', ClimateColumns)]
ruePPT = rueClimate[pptColumns]
tminColumns = ClimateColumns[grep('tmin', ClimateColumns)]
rueTMIN = rueClimate[tminColumns]
tmaxColumns = ClimateColumns[grep('tmax', ClimateColumns)]
rueTMAX = rueClimate[tmaxColumns]
climateDates = c()
for(i in 1:1821){climateDates[i]=substrRight(toString(pptColumns[i]),10)}
updatedPPT = data.frame(date = as.Date(climateDates,'%Y.%m.%d'))
for(i in 1:1218){
  for (j in 1:1821){
    num = ruePPT[i,j]
    updatedPPT[j,i+1] = num
  }
}
updatedTMIN = data.frame(date = as.Date(climateDates,'%Y.%m.%d'))
for(i in 1:1218){
  for (j in 1:1821){
    num = rueTMIN[i,j]
    updatedTMIN[j,i+1] = num
  }
}
updatedTMAX = data.frame(date = as.Date(climateDates,'%Y.%m.%d'))
for(i in 1:1218){
  for (j in 1:1821){
    num = rueTMAX[i,j]
    updatedTMAX[j,i+1] = num
  }
}

updatedPPT$mean = rowMeans(updatedPPT[,2:1219],na.rm = T)
updatedTMIN$mean = rowMeans(updatedTMIN[,2:1219],na.rm=T)
updatedTMAX$mean = rowMeans(updatedTMAX[,2:1219],na.rm=T)
updatedTMAX$lower = 1
updatedTMAX$upper = 1
for(i in 1:1821){
  updatedTMAX[i,1221] = quantile(updatedTMAX[i,2:1219],probs = 0.05,na.rm=T)
  updatedTMAX[i,1222] = quantile(updatedTMAX[i,2:1219],probs = 0.95,na.rm=T)
}




simple_hndvi = data.frame(date=herbaceous_NDVI$date,mean=herbaceous_NDVI$mean)
simple_rads = data.frame(date=updatedRads_short$date,mean=updatedRads_short$mean)
simple_fparh = data.frame(date=fPARh$date,mean=fPARh$mean)
simple_anpp = data.frame(date=anpp_estimate$date,mean=anpp_estimate$mean)
write.csv(simple_anpp,"G:/My Drive/simple_anpp.csv")

# create a four panel plot
p1 = ggplot(simple_hndvi, aes(x=date)) + 
  geom_line(aes(y=mean))+
  labs(y='Max Temperature (C)',
       x='Date')+
  theme_bw()+
  scale_x_date(breaks = seq(as.Date("2019-01-01"), as.Date('2024-01-01'), by = "12 months"),
               date_labels = "%B, %Y")
p2 = ggplot(updatedRads_short%>%filter(!is.na(mean)), aes(date)) + 
  geom_line(aes(y=mean),color='black')+
  labs(y='PARi',
       x='Date')+
  theme_bw()+
  scale_x_date(breaks = seq(as.Date("2019-01-01"), as.Date('2024-01-01'), by = "6 months"),
               date_labels = "%B, %Y")
p3 = ggplot(fPARh%>%filter(!is.na(mean)), aes(date)) + 
  geom_line(aes(y=mean),color='black')+
  labs(y='fAPARh',
       x='Date')+
  theme_bw()+
  scale_x_date(breaks = seq(as.Date("2019-01-01"), as.Date('2024-01-01'), by = "6 months"),
               date_labels = "%B, %Y")
p4 = ggplot(anpp_estimate%>%filter(!is.na(mean)), aes(date)) + 
  geom_line(aes(y=mean),color='black')+
  labs(y=expression('Herbaceous ANPP (g/m'^2~')' ),
       x='Date')+
  theme_bw()+
  scale_x_date(breaks = seq(as.Date("2019-01-01"), as.Date('2024-01-01'), by = "6 months"),
               date_labels = "%B, %Y")
library(ggpubr)
figure <- ggarrange(p1,p2,p3,p4,
                    ncol = 1, nrow = 4)

anpp = read.csv("G:/My Drive/reference_anpp.csv")
maxes = c()
for (i in 1:892){
  top = max(anpp[1:893,i+2],na.rm=T)
  maxes = c(maxes,top)
}

x=updatedTMAX %>% filter(mean==max((as.data.frame(updatedTMAX %>% filter(date>='2023-01-01'&date<='2023-12-31'))$mean))) 
# 2019max = Sept 1, 2020max = Aug 18, 2021max = July 19, 2022max = July 17, 2023max = July 23
