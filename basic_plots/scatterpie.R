
treat <- c('T1','T2','T3','CK1','CK2','T4')
ML1 <- c(1/2,2/3,3/4,1,0,2/3)
SN086 <- c(1/2,1/3,1/4,0,1,0)
SN083 <- c(0,0,0,0,0,1/3)

data <- data.frame(treat,ML1,SN086,SN083)
row.names(data) <- data[,1]
data$x <- c(1,2,3,1,2,3)
data$y <- c(2,2,2,1,1,1)

# data2 <- gather(data,'cultivar','rate',-treat)
# ggplot(data=data2,aes(x=treat,y=rate,fill=cultivar)) + 
#   geom_bar(stat = "identity",position = "stack")

library(ggplot2)
library(scatterpie)
# mycolor <- c('#fa2d2c','#0079c4','#a2cff0')
mycolor <- c('#dd7b32','#064077','#a2cff0')

ggplot() + geom_scatterpie(aes(x=x, y=y), data=data,cols=colnames(data)[2:4],color='transparent',pie_scale = 4) +
  geom_text(data=data,aes(x=x+0.4,y=y,label=treat))+
  scale_fill_manual(values = mycolor)+
  theme_void()+
  theme(legend.position = 'top')+
  coord_fixed()

