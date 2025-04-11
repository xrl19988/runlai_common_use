rm(list=ls())
library(ggplot2)
library(dplyr)
library(gghalves)
library(ggdist)
library(readxl)

plotdata <- read_excel('e:/research/r_codes/example.xlsx',sheet = 'raw')

p1 <- ggplot(data = plotdata,aes(x=Species,y = Sepal.Length))+
  # geom_half_point(mapping = aes(color = Species), side = "l",
  #                 range_scale =0.5, alpha =0.5)+
  geom_boxplot(alpha= 0.5, outlier.size=2,outlier.alpha = 1,outlier.color = "black",outlier.shape = 21, 
               width=0.0,size = 0.8)+ # box max & min
  stat_summary(color = 'black', geom='errorbar',width=0.0, size = 2,
                   fun.min=function(x){mean(x)-sd(x)},fun.max = function(x){mean(x)+sd(x)})+ # ERROR BAR
  stat_halfeye(aes(fill=Species),width = 0.25, .width = 0,point_colour = NA)+ 
  stat_summary(fun.y='mean',geom='point',shape=21,size=3,fill='black')+ # mean
  coord_flip()+
  labs(x="x_axis", y="y_axis")+
  theme_bw()+
  # theme(panel.grid=element_blank())+
  theme(legend.position="none")+
  theme(text = element_text(color = "black",size=18))
p1
ggsave('e:/research/r_codes/basic_plots/halfeye2.pdf', p1, width = 5, height = 5,units = 'in',dpi = 600)
