rm(list=ls())#clear Global Environment
setwd("D:/OneDrive - cau.edu.cn/rcode/ordination_analysis")

#加载包
library(ggplot2)#绘图包
library(ggpubr)
library(ggsignif)
library(readxl)
library(vegan)

##读取表
NDVI <- read_excel("./ToZBJ_PCA_Template.xlsx",sheet='ndvi')
ndvi_group <- read_excel("./ToZBJ_PCA_Template.xlsx",sheet='ndvi_group')
NDVI <- decostand(NDVI, method = "standardize") # 标准化

biomass <- read_excel("./ToZBJ_PCA_Template.xlsx",sheet='biomass')
biomass_group <- read_excel("./ToZBJ_PCA_Template.xlsx",sheet='biomass_group')
biomass <- decostand(biomass, method = "standardize") # 标准化

# 合并
group <- rbind(ndvi_group,biomass_group)
data <- rbind(NDVI,biomass)
rownames(data) <- paste0(group$Varieties,"_",group$factor)

#pca分析(此处使用R内置函数prcomp()函数进行分析)
df_PCA <- prcomp(data,scal=TRUE)
#预览pca分析结果
df_PCA_sum <- summary(df_PCA)
#提取出PC1及PC2的坐标
PC12 <- df_PCA$x[,1:2]
#计算各主成分解释度
pc <- df_PCA_sum$importance[2,]*100

PC12 <- as.data.frame(PC12)
#给PC12添加samp1es变量
# PC12$samples <- row.names(PC12)

#将绘图数据和分组合并
df <- cbind(PC12,group)
head(df)

#绘图
p1<-ggplot(data=df,aes(x=PC1,y=PC2))+#指定数据、X轴、Y轴，颜色
  geom_point(aes(color=Varieties,shape=factor),size=3)+#绘制点图并设定大小
  geom_vline(xintercept = 0,lty="dashed")+
  geom_hline(yintercept = 0,lty="dashed")+#图中虚线
  # geom_text(mapping = aes(label=Varieties))+
  labs(x=paste0("Dim1 (",pc[1],"%)"),
       y=paste0("Dim2 (",pc[2],"%)"))+#将x、y轴标题改为贡献度
  # scale_color_manual(values = color) +#点的颜色设置
  theme_bw()+
  theme(legend.key.size = unit(12,'pt'))
p1
ggsave('ToZBJ_PCA.pdf', p1, width = 5, height = 4,units = 'in',dpi = 600)
