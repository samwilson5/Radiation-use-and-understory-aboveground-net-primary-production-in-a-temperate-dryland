# this script calculate fAPARh (fraction of available photosynthetically active radiation for the herbaceous componenet)
# using ndvi data from sentinel 2


library(stringr)
# read in NDVI data
rueNDVI = read.csv("D:/Thesis/Lab_Data/RUE_NDVI_Jan5.csv")

# a bunch of the fields weren't numeric after the export from GEE
rueNDVI$imageryYear = as.numeric(rueNDVI$system.index)
rueNDVI$imageryMonth = as.numeric(rueNDVI$system.index)
rueNDVI$imageryDay = as.numeric(rueNDVI$system.index)
rueNDVI$imageryDate = as.Date(rueNDVI$date)

# this for loop breaks apart the GEE export name of columns then returns a column 
# with the data of imagery for each value
for (i in 1:53651) {
  index = str_split(rueNDVI[i,1],'')
  year = index[[1]][1:4]
  year = as.numeric(paste0(year[1],year[2],year[3],year[4],''))
  rueNDVI[i,12] = year
  month = index[[1]][5:6]
  month = as.numeric(paste0(month[1],month[2],''))
  rueNDVI[i,13] = month
  day = index[[1]][7:8]
  day = as.numeric(paste0(day[1],day[2],''))
  rueNDVI[i,14] = day
  date = paste0(year,'-',month,'-',day,'')
  rueNDVI[i,15] = as.Date(date,format = "%Y-%m-%d")
  }

keeps <- c("NDVI", "plot",'date','imageryDate')
rueUpdate = rueNDVI[keeps]

library('dplyr')

# create dataframes for each plot
for (i in 1:80){
  df = paste0('RUE',i)
  selected = rueUpdate %>% filter(plot == i)
  assign(df,selected)
}
# read in the data
point_dates = read.csv("G:/My Drive/Summer/RUE_points/All_RUE_Plots.csv")
keeps = c('plot','date')
point_dates = point_dates
library('stats')
library(stlplus)
library(zoo)
keeps = c('NDVI','imageryDate')



for (i in 1:80) {
  # get the dataframe for each plot 
  name = paste0('RUE',i)
  data = get(name)
  
  # make sure there is only one value per imagery date
  data = data %>% aggregate(NDVI ~ imageryDate,FUN=mean)
#ndvi_ts = xts(data$NDVI, order.by=data$imageryDate)
  # create a daily dataframe that is empty
  daily = as.data.frame(seq(as.Date("2018-04-23"), as.Date("2023-12-30"), by="days"))
  daily = daily %>% rename(date = `seq(as.Date("2018-04-23"), as.Date("2023-12-30"), by = "days")`)
  # join the daily data to the known NDVI data
  daily_ndvi = left_join(daily,data,join_by(date==imageryDate))
  # interpolate to fill the na's
  daily_ndvi$NDVI = na.approx(daily_ndvi$NDVI,rule=2)

  # convert the dataframe to a timeseries object
  ndvi_ts = ts(daily_ndvi$NDVI,frequency = 365,start=c(2018,112),end=c(2023,364))
  
  # apply an stl procedure on the time series
  ndvi_stl = stl(ndvi_ts,s.window='per')
  # store the seasonal data seperately
  seasonal = as.data.frame(ndvi_stl$time.series[,1])
  s = mean(daily_ndvi$NDVI)
  d=0.04 #blanco et al 2016
  lambda = 0.1 #blanco et al 2016
  # extract the trend data
  trend = as.data.frame(ndvi_stl$time.series[,2])
  
  
  seasonal$date = daily_ndvi$date
  
  annual_maxes = seasonal %>%
    mutate(year = format(as.Date(date, format = "%Y-%m-%d"), "%Y")) %>%
    aggregate(x ~ year,FUN=max) %>%
    rename(max = x)
  annual_mins = seasonal %>%
    mutate(year = format(as.Date(date, format = "%Y-%m-%d"), "%Y")) %>%
    aggregate(x ~ year,FUN=min) %>%
    rename(min = x)
  # add the new data to the seasonal object
  seasonal = seasonal %>%
    mutate(year = format(as.Date(date, format = "%Y-%m-%d"), "%Y")) %>%
    left_join(annual_maxes,join_by(year==year)) %>%
    left_join(annual_mins,join_by(year==year))
  # replace seasonal with S(t)
  seasonal[,1] = ((seasonal[,1] - seasonal[,5])/(seasonal[,4] - seasonal[,5]))
  seasonal = seasonal %>% rename(seasonalNDVI = x)
  seasonal$ht = 1
  # calculate the herbaceous component, blanco et al 2016
  for(j in 1:2078) {
    seasonal[j,6] = seasonal[j,1]*((1-lambda*s)*(seasonal[j,4]-seasonal[j,5]) - (lambda*trend[j,1])) + (lambda*seasonal[j,1]*d)
  }
# merge the two!
  ndvi = left_join(daily,seasonal,join_by(date==date))
  remainder = mean(ndvi_stl$time.series[,3])
  # calculate fAPARh
  ndvi$fAPARh = 0.89*ndvi$ht - remainder #blanco et al 2016
  
  ndvi$plot = (point_dates %>% filter(plot == i))$plot
  dates = (data %>% filter(imageryDate >= '2023-04-15' & imageryDate <= (point_dates %>% filter(plot == i))$date))

  assign(paste0('ndvi',i),ndvi %>% filter(date >= '2023-04-15' & date <= (point_dates %>% filter(plot == i))$date))
  
  assign(paste0('ndvi',i),left_join(dates,get(paste0('ndvi',i)),join_by(imageryDate==date)))
  if (i == 1){final_rue = ndvi1}
  else{final_rue = rbind(final_rue,get(paste0('ndvi',i)))}}
  
  
# write the data to a csv  
write.csv(final_rue,"G:/My Drive/Thesis/Lab_Data/RUE_point_fAPARh_try6.csv")
plot(ndvi_stl)
