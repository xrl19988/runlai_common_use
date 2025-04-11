setwd('e:/research/data/FCR_microbiome/3验证/')

# extact sequence of healthy asvs using tbtools
# E:\research\data\FCR_microbiome\3验证\isolation\otus_RZ.fa
# E:\research\data\FCR_microbiome\3验证\isolation\healthy_asvs.fa

# blast at NCBI

# 
data.final<-read_excel("./isolation/blast.xlsx",sheet = 'data')
head(data.final)

data.final$strain <- factor(data.final$strain,levels = c('RL83','RN92','RN186','RN91'))
data.final$group <- factor(data.final$group,levels = c('Shared','JMH','XMH'))

palette <-  c(Bacillus='#377EB8', Lysobacter='#FFFF33',Parafilimonas='#984EA3', Pedobacter='#FF7F00',Pseudomonas='#A65628',Rhizobium='#F0027F',
              Unassigned='#7ac7c4',Others='#928a97',Devosia='#a8e29a')

data.final$similarity2 <- ifelse(data.final$similarity == 100,'100%',
                                 ifelse(data.final$similarity < 100 & data.final$similarity >= 97,'< 100% & >= 97%',
                                        ifelse(data.final$similarity < 97 & data.final$similarity >= 95, '< 97% & >= 95%',
                                               ifelse(data.final$similarity < 95 & data.final$similarity >= 90,'< 95% & >= 90%','< 90%'))))
data.final$similarity2 <- factor(data.final$similarity2 ,levels = c('< 90%','< 95% & >= 90%','< 97% & >= 95%','< 100% & >= 97%','100%'))
# 100%, < 100% & >= 97%, < 97% & >= 95%, < 95%

library(ggplot2)
g <- ggplot(data.final,aes(x=strain,y=asv))+
  geom_point(aes(size= similarity2,color=genus2))+
  # geom_text(aes(label = similarity))+
  theme_bw()+
  facet_wrap(~group,scales = "free_y")+
  # theme(panel.grid = element_blank(),
  #       axis.text.x=element_text(angle=90,hjust = 1,vjust=0.5))+
  scale_color_manual(values = palette)+
  # scale_color_gradient(low="lightgrey",high="blue")+
  labs(x='Strains',y='Healthy-related ASVs')+
  theme(text = element_text(color = "black",size=18))
  # guides(size=guide_legend(order=3))'
g
ggsave('./isolation/similarity.pdf', g, width = 15, height = 16,units = 'in',dpi = 600)
