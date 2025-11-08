library(ggplot2)
library(dplyr)
anpp = read.csv("D:/misc/spatial_anpp.csv")
columns = colnames(anpp) 
cols.2019 = columns[grepl("2019",columns)]
cols.2020 = columns[grepl("2020",columns)]
cols.2021 = columns[grepl("2021",columns)]
cols.2022 = columns[grepl("2022",columns)]
cols.2023 = columns[grepl("2023",columns)]

dat.2019 = anpp[c('plot',cols.2019)]
dat.2020 = anpp[c('plot',cols.2020)]
dat.2021 = anpp[c('plot',cols.2021)]
dat.2022 = anpp[c('plot',cols.2022)]
dat.2023 = anpp[c('plot',cols.2023)]

mean.2019 = data.frame(plot = dat.2019$plot,anpp.2019 = rowSums(dat.2019[,c(2:216)]))
mean.2020 = data.frame(plot = dat.2020$plot,anpp.2020 = rowSums(dat.2020[,c(2:216)]))
mean.2021 = data.frame(plot = dat.2021$plot,anpp.2021 = rowSums(dat.2021[,c(2:216)]))
mean.2022 = data.frame(plot = dat.2022$plot,anpp.2022 = rowSums(dat.2022[,c(2:216)]))
mean.2023 = data.frame(plot = dat.2023$plot,anpp.2023 = rowSums(dat.2023[,c(2:216)]))

all.anpp = mean.2019 %>% left_join(mean.2020,join_by(plot==plot)) %>%
  left_join(mean.2021,join_by(plot==plot)) %>%
  left_join(mean.2022,join_by(plot==plot)) %>%
  left_join(mean.2023,join_by(plot==plot))
all.anpp$mean = rowMeans(all.anpp[,c(2:6)],na.rm = T)
ppt = read.csv('D:/Thesis/Chapter1_simulations/UGRB_PPT_mean.csv')
keeps = c('plot','ppt')
ppt = ppt[keeps]

df = all.anpp %>% left_join(ppt,join_by(plot==plot))

ggplot(df %>% filter(ppt<=800),aes(x = ppt, y = mean/10)) + 
  geom_point()+
  stat_smooth(method = 'lm',
              formula = y ~ x + I(x^2))+
  labs(y=expression('Herbaceous ANPP (g/m'^2*')' ),
       x='Annual Precipitation (mm)')+
  theme_bw(base_size = 15)+
  theme(axis.text.x = element_text(size=13),axis.text.y = element_text(size=13))
ggsave("figure5_highRes.pdf", path = "C:/Users/Sam/Desktop/Ecosphere_finalFigures", 
       units="cm", width=18, height=13, dpi=600, device = "pdf", bg = "white")


df$square = df$ppt^2
z = lm(df$mean ~ df$ppt + df$square)
summary(z)
