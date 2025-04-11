rm(list=ls())

library(vegan)
library(ggrepel)
library(ggplot2)
library(ggpubr)
library(ggsci)
#添加新罗马字体
design<-read.csv("RDA_design.csv",row.names = 1)
sampledata<-read.csv("RDA_otu_ITS.csv",row.names = 1)
#读取数据，依次为otu数据、环境因子数据、分组信息

env <- design[,4:13]#环境因子
group <-  design[,1:3]#分组依据
sampledata <- t(sampledata)
#对OTU数据进行hellinger转化
# sampledata <- decostand(sampledata,method = "hellinger")
group <- as.list(group)
# #定义分组的填充颜色
# col <- c("#F8766D", "#7CAE00", "#00BFC4", "#C77CFF")


#先进行DCA分析
dca <- decorana(veg = sampledata)
dca
#根据DCA1的Axis Lengths值进行选择，如果>4.0选CCA；如果在3.0-4.0之间，选RDA和CCA都可以；如果<3.0, 选择RDA分析即可。

#RDA

#RDA分析    
rda <- rda(sampledata, env, scale = TRUE)
rdascore <- scores(rda)
rdascore$sites
rda$CCA$biplot
rdascore$species
# write.csv(rdascore$sites,file="rda.sample.csv")
# write.csv(rda$CCA$biplot,file="rda.env.csv")
# write.csv(rdascore$species,file="rda.species.csv")

RDAE <- as.data.frame(rda$CCA$biplot[,1:2])

RDAS1 <- rdascore$sites[,1]*0.2
RDAS2 <- rdascore$sites[,2]*0.2

plotdata <- data.frame(rownames(rdascore$sites), RDAS1, RDAS2, group$group,group$year,group$stage)
colnames(plotdata) <- c("sample","RDAS1","RDAS2","group","year","stage")

rda1 <- round(rda$CCA$eig[1]/sum(rda$CCA$eig)*100,2)
rda2 <- round(rda$CCA$eig[2]/sum(rda$CCA$eig)*100,2)


#RDA plot        
  plotdata$year <- factor(plotdata$year,levels = c('Third','Second','First'))
  plotdata$stage <- factor(plotdata$stage,levels = c('B','S','T','F','H'))
                           
PP <-ggplot(plotdata, aes(RDAS1, RDAS2)) +
  geom_point(aes(fill = year, color = year,shape = stage),size = 5) + 
  scale_color_npg()+
  # stat_chull(geom = "polygon", aes(group = group, color = group, fill = group), alpha = 0.1)+ 
  xlab(paste("RDA1 ( ",rda1,"%"," )", sep = "")) + 
  ylab(paste("RDA2 ( ",rda2,"%"," )", sep = "")) +
  geom_segment(data = RDAE, aes(x = 0, y = 0, xend = RDAE[,1], yend = RDAE[,2]),
               colour = "black", size = 0.8,
               arrow = arrow(angle = 30, length = unit(0.4, "cm"))) +
  geom_text_repel(data = RDAE, segment.colour = "black",
                  aes(x = RDAE[,1], y = RDAE[,2], label = rownames(RDAE)),size=8) +
  geom_vline(aes(xintercept = 0), linetype = "dotted") +
  geom_hline(aes(yintercept = 0), linetype = "dotted") +
  theme(panel.background = element_rect(fill = "white", colour = "black"), 
        panel.grid = element_blank(),
        axis.title = element_text(color = "black", size = 18),
        axis.ticks.length = unit(0.4,"lines"),
        axis.ticks = element_line(color = "black"),
        axis.line = element_line(colour = "black"),
        axis.title.x = element_text(colour = "black", size = 18),
        axis.title.y = element_text(colour="black", size = 18),
        axis.text = element_text(colour = "black", size = 18),
        legend.title = element_blank(),
        legend.text = element_text(size = 18), legend.key = element_blank(),
        plot.title = element_text(size = 22, colour = "black", 
                                  face = "bold", hjust = 0.5)) +
  theme(text=element_text(size=20))

PP
ggsave('ITS_RDA.pdf', PP, width = 10, height = 8,units = 'in',dpi = 600)


#置换检验 
envfit <- envfit(rda, env, permutations  = 999)
r <- as.matrix(envfit$vectors$r)
p <- as.matrix(envfit$vectors$pvals)
env.p <- cbind(r,p)
colnames(env.p) <- c("r2","p-value")
KK <- as.data.frame(env.p)
KK
write.csv(as.data.frame(env.p),file="ITS_rdaenvfit.csv")



#CCA
cca <- cca(sampledata, env, scale = TRUE)
ccascore <- scores(cca)
ccascore$sites
cca$CCA$biplot
ccascore$species


#提取主要信息
write.csv(ccascore$sites, file = "cca.sample.csv")
write.csv(cca$CCA$biplot, file = "cca.env.csv")
write.csv(ccascore$species, file = "cca.species.csv")


#为绘图准备数据
CCAE <- as.data.frame(cca$CCA$biplot[,1:2])

CCAS1 <- ccascore$sites[,1]*0.3
CCAS2 <- ccascore$sites[,2]*0.3

plotdata <- data.frame(rownames(ccascore$sites), CCAS1, CCAS2, group$V2)
colnames(plotdata) <- c("sample","CCAS1","CCAS2","group")

cca1 <- round(cca$CCA$eig[1]/sum(cca$CCA$eig)*100,2)
cca2 <- round(cca$CCA$eig[2]/sum(cca$CCA$eig)*100,2)

#绘制CCA图
P <- ggplot(plotdata, aes(CCAS1, CCAS2)) +
  geom_point(aes(fill = group, color = group),size = 5) + 
  scale_fill_manual(values = col)+
  stat_chull(geom = "polygon", aes(group = group, color = group, fill = group), alpha = 0.1) +
  xlab(paste("CCA1 ( ",cca1,"%"," )", sep = "")) + 
  ylab(paste("CCA2 ( ",cca2,"%"," )", sep = "")) +
  geom_segment(data = CCAE, aes(x = 0, y = 0, xend = CCAE[,1], yend = CCAE[,2]),
               colour = "black", size = 0.8,
               arrow = arrow(angle = 30, length = unit(0.4, "cm"))) +
  geom_text_repel(data = CCAE, segment.colour = "black",
                  aes(x = CCAE[,1], y = CCAE[,2], 
                      label = rownames(CCAE)),size=8) +
  geom_vline(aes(xintercept = 0), linetype = "dotted") +
  geom_hline(aes(yintercept = 0), linetype = "dotted") +
  theme(panel.background = element_rect(fill = "white", colour = "black"), 
        panel.grid = element_blank(),
        axis.title = element_text(color = "black", size = 18),
        axis.ticks.length = unit(0.4,"lines"),
        axis.ticks = element_line(color = "black"),
        axis.line = element_line(colour = "black"),
        axis.title.x = element_text(colour = "black", size = 18),
        axis.title.y = element_text(colour="black", size = 18),
        axis.text = element_text(colour = "black", size = 18),
        legend.title = element_blank(),
        legend.text = element_text(size = 18), legend.key = element_blank(),
        plot.title = element_text(size = 22, colour = "black", 
                                  face = "bold", hjust = 0.5)) +
  theme(text=element_text(family="A",size=20))

P

#保存绘图
library(eoffice)
topptx(filename="细菌CCA.pptx",height = 6,width = 8)


#置换检验       
envfit <- envfit(cca,env,permutations  = 999)  
r <- as.matrix(envfit$vectors$r) 
p <- as.matrix(envfit$vectors$pvals)
env.p <- cbind(r,p)
colnames(env.p) <- c("r2","p-value")
K <- as.data.frame(env.p)
K

write.csv(as.data.frame(env.p), file = "ccaenvfit.csv")
