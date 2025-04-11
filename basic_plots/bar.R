rm(list=ls())
library(ggplot2)
library(dplyr)
plotdata <- read_excel('d:/file/R/r_codes/example.xlsx',sheet = 'Sheet3')
plotdata <- plotdata %>% filter(facet=='setosa')
palette <- 

# single----
p1 <- ggplot(plotdata, aes(x = group, y = mean, fill = group))+
  geom_errorbar(aes(x=group,ymax = mean+se, ymin = mean-se),# errorbar:sd or se
                position = position_dodge(width = 0.8),width = 0.4,size = 0.5)+
  geom_bar(stat = "identity",position = "dodge",color="transparent", width = 0.8, alpha= 1,size = 0.5)+
  # scale_fill_manual(values = palette)+
  # scale_color_manual(values = palette)+
  scale_y_continuous(expand=c(0,0),limits = c(0,max(na.omit(plotdata$max))*1.1))+
  labs(x="x_axis", y="y_axis")+
  theme_classic()+
  theme(legend.position="none")+
  theme(text = element_text(color = "black",size=18))
p1
ggsave('123.pdf', p1, width = 4, height = 4,units = 'in',dpi = 600)

# group----
p2 <- ggplot(plotdata, aes(x = facet, y = mean, fill = group))+
  geom_errorbar(aes(x=facet,ymax = mean+se, ymin = mean-se),# errorbar:sd or se
                position = position_dodge(width = 0.8),width = 0.4,size = 0.5)+
  geom_bar(stat = "identity",position = "dodge",color="transparent", width = 0.8, alpha= 1,size = 0.5)+
  # scale_fill_manual(values = palette)+
  # scale_color_manual(values = palette)+
  scale_y_continuous(expand=c(0,0),limits = c(0,max(na.omit(plotdata$max))*1.1))+
  labs(x="x_axis", y="y_axis")+
  theme_classic()+
  theme(legend.position="none")+
  theme(text = element_text(color = "black",size=18))
p2 
ggsave('./bar.pdf', p2, width = 4.5, height = 4.5,units = 'in',dpi = 600)

