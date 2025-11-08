library(tidyr)
library(daymetr)
library(lubridate)
library(dplyr)
library(climatol)
library(ggplot2)
Vancouver_data <- download_daymet(site = "UGRB",
                                  lat = 42.8679,
                                  lon = -109.8634,
                                  start = 1990,
                                  end = 2020,
                                  internal = TRUE,
                                  simplify = TRUE) #simplify returns tidy data, ready for analysis

Vancouver_site <- Vancouver_data[1,1:5]

Vancouver_sub <- Vancouver_data[(Vancouver_data$measurement=="prcp..mm.day." | Vancouver_data$measurement=="tmax..deg.c." | Vancouver_data$measurement=="tmin..deg.c."),]

Vancouver_sub$year.yday <- paste(Vancouver_sub$year, Vancouver_sub$yday, sep="-")

Vancouver_sub$month <- month(as.POSIXlt(Vancouver_sub$year.yday, format="%Y-%j"), label=TRUE)

Vancouver_sub <- pivot_wider(Vancouver_sub, id_cols=c(year, yday, month), names_from=measurement, values_from=value)

#to means for each month within year
tmp.mod1_Van <- Vancouver_sub %>% group_by(year, month) %>% summarize(PPT_mm=sum(prcp..mm.day.), Tmax_C=mean(tmax..deg.c.), Tmin_C=mean(tmin..deg.c.), Tabs_C=min(tmin..deg.c.))

#to means for each month across years
Vancouver_climate.normal <- tmp.mod1_Van %>% group_by(month) %>% summarize(PPT_mm=mean(PPT_mm), Tmax_C=mean(Tmax_C), Tmin_C=mean(Tmin_C), Tabs_C=min(Tabs_C))

#remove month in left column
Vancouver_climate.normal <- Vancouver_climate.normal[,-1]

Vancouver_climate.normal <- as.data.frame(t(Vancouver_climate.normal), stringsAsFactors = FALSE)

Vancouver_site

diagwl(Vancouver_climate.normal,cols=NULL,stname='UGRB', est="Nevada 02", alt=2127, per="1990 - 2020", mlab="en")

recent_data <- download_daymet(site = "UGRB",
                               lat = 42.8679,
                               lon = -109.8634,
                               start = 2023,
                               end = 2023,
                               internal = TRUE,
                               simplify = TRUE) #simplify returns tidy data, ready for analysis
recent_site <- recent_data[1,1:5]
recent_sub <- recent_data[(recent_data$measurement=="prcp..mm.day." | recent_data$measurement=="tmax..deg.c." | recent_data$measurement=="tmin..deg.c."),]
recent_sub$year.yday <- paste(recent_sub$year, recent_sub$yday, sep="-")
recent_sub$month <- month(as.POSIXlt(recent_sub$year.yday, format="%Y-%j"), label=TRUE)
recent_sub <- pivot_wider(recent_sub, id_cols=c(year, yday, month), names_from=measurement, values_from=value)
recent_agg = recent_sub %>% aggregate(prcp..mm.day. ~ month,FUN = sum)
sum(recent_agg$prcp..mm.day.)
sum(unlist(Vancouver_climate.normal[1,]))

df = data.frame(#x=c(1:12),
  x = c(1:12),
  y=unlist(Vancouver_climate.normal[1,]),
  z = recent_agg$prcp..mm.day.)

ggplot(data=df,aes(x=x))+
  geom_line(aes(y=cumsum(y),colour='30 Year Avg')) +
  geom_line(aes(y=cumsum(z),colour='2023'))+
  scale_x_discrete(name = 'Month',limits = c('Jan','Feb','Mar','Apr','May','Jun','Jul','Aug','Sep','Oct','Nov','Dec'))+
  scale_y_continuous(name = 'Cumulative Precipitation (mm)')+
  annotate('text',label='30 yr MAP - 309 \n 2023 Total Precip - 403',x=3,
           y=350) +
  scale_colour_manual("", 
                      values = c("30 Year Avg"="blue", "2023"="green")) +
  theme_bw()


locations = read.csv("D:/All_RUE_Plots.csv")
historic.full = data.frame(month = 1:12)
recent.full = data.frame(month=1:12)

for(i in 1:80){
  latitude = locations[i,5]
  longitude = locations[i,4]
  
  historic_i <- download_daymet(site = "UGRB",
                                    lat = latitude,
                                    lon = longitude,
                                    start = 1990,
                                    end = 2020,
                                    internal = TRUE,
                                    simplify = TRUE) #simplify returns tidy data, ready for analysis
  
  historic_sub <- historic_i[(historic_i$measurement=="prcp..mm.day." | historic_i$measurement=="tmax..deg.c." | historic_i$measurement=="tmin..deg.c."),]
  
  historic_sub$year.yday <- paste(historic_sub$year, historic_sub$yday, sep="-")
  
  historic_sub$month <- month(as.POSIXlt(historic_sub$year.yday, format="%Y-%j"), label=TRUE)
  
  historic_sub <- pivot_wider(historic_sub, id_cols=c(year, yday, month), names_from=measurement, values_from=value)
  
  #to means for each month within year
  tmp.mod1_Van <- historic_sub %>% group_by(year, month) %>% summarize(PPT_mm=sum(prcp..mm.day.), Tmax_C=mean(tmax..deg.c.), Tmin_C=mean(tmin..deg.c.), Tabs_C=min(tmin..deg.c.))
  
  #to means for each month across years
  historic.normal <- tmp.mod1_Van %>% group_by(month) %>% summarize(PPT_mm=mean(PPT_mm), Tmax_C=mean(Tmax_C), Tmin_C=mean(Tmin_C), Tabs_C=min(Tabs_C))
  
  #remove month in left column
  historic.normal <- historic.normal[,-1]
  
  historic.normal <- as.data.frame(t(historic.normal), stringsAsFactors = FALSE)
  historic.full[,i+1] = unlist(historic.normal[1,])
  
  recent_data <- download_daymet(site = "UGRB",
                                 lat = latitude,
                                 lon = longitude,
                                 start = 2023,
                                 end = 2023,
                                 internal = TRUE,
                                 simplify = TRUE) #simplify returns tidy data, ready for analysis
  recent_site <- recent_data[1,1:5]
  recent_sub <- recent_data[(recent_data$measurement=="prcp..mm.day." | recent_data$measurement=="tmax..deg.c." | recent_data$measurement=="tmin..deg.c."),]
  recent_sub$year.yday <- paste(recent_sub$year, recent_sub$yday, sep="-")
  recent_sub$month <- month(as.POSIXlt(recent_sub$year.yday, format="%Y-%j"), label=TRUE)
  recent_sub <- pivot_wider(recent_sub, id_cols=c(year, yday, month), names_from=measurement, values_from=value)
  recent_agg = recent_sub %>% aggregate(prcp..mm.day. ~ month,FUN = sum)
  recent.full[,i+1] = recent_agg$prcp..mm.day.
  
}

f.25 = function(x) quantile(x,probs=.25)
f.75 = function(x) quantile(x,probs=.75)

df = data.frame(
  month = c(1:12),
  historic = apply(historic.full[,2:81],1,median),
  recent = rowMeans(recent.full[,2:81]),
  historic.25 = apply(historic.full[,2:81],1,f.25),
  historic.75 = apply(historic.full[,2:81],1,f.75))

#df2 = data.frame(month =c(1:12,1:12,1:12,1:12),
#                 type = c(replicate(12,'30 Year Avg'),replicate(12,'30 Year 75th Percentile'),
#                          replicate(12, '30 Year 25th Percentile'))
historic.total = round(sum(df$historic))
recent.total = round(sum(df$recent))

ggplot(data=df,aes(x=month))+
  geom_line(aes(y=cumsum(historic),colour='30 Year Avg')) +
  geom_line(aes(y=cumsum(recent),colour='2023'))+
  scale_x_discrete(name = 'Month',limits = c('Jan','Feb','Mar','Apr','May','Jun','Jul','Aug','Sep','Oct','Nov','Dec'))+
  scale_y_continuous(name = 'Cumulative Precipitation (mm)')+
  annotate('text',label=paste0('30 year Avg Total - ',as.character(historic.total),' mm \n 2023 Total Precip - ',as.character(recent.total),' mm'),x=3,
           y=315) +
  scale_colour_manual("", 
                      values = c("30 Year Avg"="blue", "2023"="red")) +
  theme_bw()



ggplot(data=df,aes(x=month))+
  geom_line(aes(y=cumsum(historic),colour='30 Year Avg')) +
  geom_line(aes(y=cumsum(recent),colour='2023'))+
  geom_line(aes(y=cumsum(historic.25),colour = '30 Year IQR'),linetype='dashed')+
  geom_line(aes(y = cumsum(historic.75),colour='30 Year IQR'),linetype='dashed')+
  scale_x_discrete(name = 'Month',limits = c('Jan','Feb','Mar','Apr','May','Jun','Jul','Aug','Sep','Oct','Nov','Dec'))+
  scale_y_continuous(name = 'Cumulative Precipitation (mm)')+
  annotate('text',label=paste0('30 Year Avg Total - ',as.character(historic.total),' mm \n 2023 Total Precip - ',as.character(recent.total),' mm'),x=4,
           y=315,size = 5) +
  scale_colour_manual(name = '',
  values = c("30 Year Avg"="blue","30 Year IQR" = 'blue', "2023"="red"),
                      breaks = c('30 Year Avg','30 Year IQR','2023')) +
  guides(colour = guide_legend(override.aes = list(linetype = c('solid','dashed','solid'))))+
  theme_bw(base_size = 15)
ggsave("figure3_highRes.pdf", path = "C:/Users/Sam/Desktop/Ecosphere_finalFigures", 
       units="cm", width=18, height=13, dpi=600, device = "pdf", bg = "white")
