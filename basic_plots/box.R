rm(list=ls())
library(ggplot2)
library(dplyr)
data <- read_excel('d:/file/R/r_codes/example.xlsx',sheet = 'melt')
data <- data %>% filter(facet=='setosa')
palette <- 
  
# single----
p1 <- ggplot(data = data,aes(x=group,y = value))+
  geom_boxplot(aes(color=group),fill="white",width=0.5,size = 0.7,alpha= 1,position = position_dodge(1),notch = F,
               outlier.colour = "black", outlier.size=1.5, outlier.alpha = 1)+ # box
  stat_summary(fun.y='mean',geom='point',shape=5,size=1.5,color='black')+ # mean
  labs(x="x_axis", y="y_axis")+
  theme_bw()+
  theme(panel.grid=element_blank())+
  theme(legend.position="none")+
  theme(text = element_text(color = "black",size=18))
p1
ggsave('123.pdf', p1, width = 4, height = 4,units = 'in',dpi = 600)

p2 <- ggplot(data = data,aes(x=group,y = value))+
  geom_boxplot(aes(fill=group),width=0.5,size = 0.7,alpha= 1,position = position_dodge(1),notch = F,
               outlier.colour = "black", outlier.size=1.5, outlier.alpha = 1)+ # box
  stat_summary(fun.y='mean',geom='point',shape=5,size=1.5,color='black')+ # mean
  labs(x="x_axis", y="y_axis")+
  theme_bw()+
  theme(panel.grid=element_blank())+
  theme(legend.position="none")+
  theme(text = element_text(color = "black",size=18))
p2
ggsave('123.pdf', p2, width = 4, height = 4,units = 'in',dpi = 600)

# group----
p3 <- ggplot(data = data,aes(x=facet,y = value,color=group))+
  geom_boxplot(aes(color=group),fill="white",width=0.5,size = 0.7,alpha= 1,position = position_dodge(0.8),notch = F,
               outlier.colour = "black", outlier.size=1.5, outlier.alpha = 1)+ # box
  stat_summary(aes(group=group),fun.y='mean',geom='point',shape=5,size=1.5,color='black',
               position=position_dodge(0.8))+ # mean
  labs(x="x_axis", y="y_axis")+
  theme_bw()+
  theme(panel.grid=element_blank())+
  theme(legend.position="none")+
  theme(text = element_text(color = "black",size=18))
p3 
ggsave('./bar.pdf', p3, width = 4.5, height = 4.5,units = 'in',dpi = 600)

p4 <- ggplot(data = data,aes(x=facet,y = value,fill=group))+
  geom_boxplot(aes(fill=group),width=0.5,size = 0.7,alpha= 1,position = position_dodge(0.8),notch = F,
               outlier.colour = "black", outlier.size=1.5, outlier.alpha = 1)+ # box
  stat_summary(aes(group=group),fun.y='mean',geom='point',shape=5,size=1.5,color='black',
               position=position_dodge(0.8))+ # mean
  labs(x="x_axis", y="y_axis")+
  theme_bw()+
  theme(panel.grid=element_blank())+
  theme(legend.position="none")+
  theme(text = element_text(color = "black",size=18))
p4 
ggsave('./box.pdf', p4, width = 4.5, height = 4.5,units = 'in',dpi = 600)
