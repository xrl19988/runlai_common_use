library(ggplot2)
x <- c(13,13,13,32,32,32,47,47,47,65,65,65,87,87,87)
y <- c(2.41633,3.6898,2.7102,4.76735,3.5102,2.38367,1.30612,3.15102,2.33469,2.36735,0.946939,1.82857,0.391837,0.669388,1.19184)
z <- c('min','max','cri','max','cri','min','min','max','cri','max','min','cri','min','cri','max')
  
data <- data.frame(x,y)
data$x <- factor(data$x,levels = c('13','32','47','65','87'))

p1 <- ggplot(data = data,aes(x=x,y=y,group=z,shape=z,linetype=z))+
  geom_point(size=2)+
  geom_line()+
  # scale_fill_manual(values = palette)+
  # scale_color_manual(values = palette)+
  labs(x="DC stage", y="N concentration (%)")+
  theme_classic()+
  theme(legend.position="top")+
  theme(text = element_text(color = "black",size=16))
p1
ggsave('p1.pdf', p1, width = 5, height = 5, units = 'in', dpi = 600)
