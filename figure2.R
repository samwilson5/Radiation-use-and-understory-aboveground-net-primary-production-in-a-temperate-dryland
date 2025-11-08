library(readxl)
library(ggplot2)
library(ggpubr)
biomass = read_xlsx('D:/Thesis/Lab_Data/Biomass_SW23_UGRB.xlsx')
biomass$mean = rowMeans(biomass[,2:21])
biomass$mean = biomass$mean * 10
rue = read.csv('D:/Thesis/Lab_Data/RUE_jan13.csv')

p1 = ggplot(data = biomass, aes(x=mean)) +
  geom_histogram(bins = 20,fill="darkgrey", color="black")+
  labs(x=expression("Herbaceous ANPP (g/m"^2*')'),
       y = 'Count')+
  geom_vline(xintercept = mean(biomass$mean),
             linewidth = 1)+
  geom_vline(xintercept = quantile(biomass$mean,probs = 0.25),
             linewidth = 1, linetype = 'dashed')+
  geom_vline(xintercept = quantile(biomass$mean,probs = 0.75),
             linewidth = 1, linetype = 'dashed')+
  theme_bw(base_size = 15)+
  scale_y_continuous(limits = c(0,20),expand = c(0, 0))

p2 = ggplot(data = rue, aes(x=rue)) +
  geom_histogram(bins = 20,fill="darkgrey", color="black")+
  labs(x="Herbaceous RUE (g/Mj)",
       y = 'Count')+
  geom_vline(xintercept = mean(rue$rue),
             linewidth = 1)+
  geom_vline(xintercept = quantile(rue$rue,probs = 0.25),
             linewidth = 1, linetype = 'dashed')+
  geom_vline(xintercept = quantile(rue$rue,probs = 0.75),
             linewidth = 1, linetype = 'dashed')+
  theme_bw(base_size = 15)+
  scale_y_continuous(limits = c(0,20),expand = c(0, 0))

ggarrange(p1,p2,ncol = 2,nrow = 1)       
ggsave("figure2_highRes.pdf", path = "C:/Users/Sam/Desktop/Ecosphere_finalFigures", 
       units="cm", width=18, height=13, dpi=600, device = "pdf", bg = "white")
