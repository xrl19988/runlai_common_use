rm(list=ls())
library(ggplot2)
library(dplyr)
library(gghalves)
library(ggdist)
data <- read_excel('d:/file/R/r_codes/example.xlsx',sheet = 'melt')
data <- data %>% filter(facet=='setosa')
palette <- 
  
  # single----
p1 <- ggplot(data = data,aes(x=group,y = value))+
  geom_half_point(mapping = aes(color = group), side = "l",
                  range_scale =0.5, alpha =0.5)+
  geom_boxplot(aes(color=group),alpha= 0.5, outlier.size=1,outlier.alpha = 0,outlier.color = "black",
               width=0.1,size = 0.8,position = position_dodge(1))+
  stat_halfeye(mapping = aes(fill=group),width = 0.25, justification = -0.65, .width = 0,point_colour = NA)+ # 中位数
  stat_summary(fun.y='mean',geom='point',shape=5,size=1.5,color='black')+ # mean
  labs(x="x_axis", y="y_axis")+
  theme_bw()+
  theme(panel.grid=element_blank())+
  theme(legend.position="none")+
  theme(text = element_text(color = "black",size=18))
p1
ggsave('123.pdf', p1, width = 4, height = 4,units = 'in',dpi = 600)

# group----
p3 <- ggplot(data = data,aes(x=facet,y = value,color=group))+
  geom_half_point(mapping = aes(color = group), side = "l",
                  range_scale =0.5, alpha =0.5)+
  geom_boxplot(aes(color=group),alpha= 0.5, outlier.size=1,outlier.alpha = 0,outlier.color = "black",
               width=0.1,size = 0.8,position = position_dodge(1))+
  stat_halfeye(mapping = aes(fill=group),width = 0.25, justification = -0.65, .width = 0,point_colour = NA)+ # 中位数
  stat_summary(aes(group=group),fun.y='mean',geom='point',shape=5,size=1.5,color='black',
               position=position_dodge(0.8))+ # mean
  labs(x="x_axis", y="y_axis")+
  theme_bw()+
  theme(panel.grid=element_blank())+
  theme(legend.position="none")+
  theme(text = element_text(color = "black",size=18))
p3 
ggsave('./bar.pdf', p3, width = 4.5, height = 4.5,units = 'in',dpi = 600)
