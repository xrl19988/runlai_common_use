rm(list=ls())
library(ggplot2)
library(dplyr)
library(readxl)
library(viridis)
data <- read_excel('d:/OneDrive - cau.edu.cn/rcode/example.xlsx',sheet = 'melt')
data <- data %>% filter(facet=='setosa')

ggplot(data = data,aes(x=group,y = value))+
  geom_violin(mapping = aes(color = group),alpha=0.5,width=1,size=0.6,position=position_dodge(width=0.8), trim = T)+
  geom_jitter(aes(color=group),position=position_jitter(0.1),size=0.8,alpha=0.8)+
  # geom_boxplot(mapping = aes(fill = group),alpha= 1, width=0.03,size = 0.6,position = position_dodge(width=0.8),
  #              outlier.size=1.5, outlier.alpha = 1, outlier.colour = "black")+
  stat_summary(fun.y='mean',geom='crossbar',size=0.2,width=0.1,color='black') +

  labs(x="x_axis", y="y_axis")+
  theme_bw()+
  theme(panel.grid=element_blank())+
  theme(legend.position="none")+
  theme(text = element_text(color = "black",size=16),
        axis.text = element_text(color='black'))
