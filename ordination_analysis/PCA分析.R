rm(list=ls())#clear Global Environment

#加载包
library(ggplot2)#绘图包
library(ggpubr)
library(ggsignif)

##读取 OTU 丰度表
otu <- read.csv('DCH/PCA_S.csv', row.names = 1, header =T,stringsAsFactors
                = FALSE)
#otu <- otu[,1:12]
#otu <- decostand(otu, method = "hellinger")#根据需求是否进行hellinger转化

otu <- data.frame(t(otu))#转置OTU表
# 转录组可运行以下代码
# vsd <- otu[, which(colSums(otu) > 0)] #去除全是0的列
# # 检查是否还有常数列
# which(apply(vsd, 2, var)==0)
# # 去除常数列
# vsd <- vsd[ , which(apply(vsd, 2, var) != 0)]
otu <- otu[ , which(apply(otu, 2, var) != 0)]
#pca分析(此处使用R内置函数prcomp()函数进行分析)
df_PCA <- prcomp(otu,scal=TRUE)
#预览pca分析结果
df_PCA_sum <- summary(df_PCA)
#提取出PC1及PC2的坐标
PC12 <- df_PCA$x[,1:2]
#计算各主成分解释度
pc <- df_PCA_sum$importance[2,]*100

##permanova(adonis) euclidean, bray
adonis_1 <- adonis2(otu~group,data = group, distance = 'bray', permutations = 999)
adonis_1

###绘图######
group <- read.csv('DCH/group_s.csv', stringsAsFactors = FALSE)
group <- group[1:12,]
#PCl2原来是matrix,转化为data.frame
PC12 <- as.data.frame(PC12)
#给PC12添加samp1es变量
PC12$samples <- row.names(PC12)
#修改列名
colnames(group) <- c("samples","group")
#将绘图数据和分组合并
df <- merge(PC12,group,by="samples")
head(df)

#绘图
color=c("#73669a","#b783af","#f6a674",'#fddb73')
p1<-ggplot(data=df,aes(x=PC1,y=PC2,
                       color=group))+#指定数据、X轴、Y轴，颜色
  geom_point(size=3)+#绘制点图并设定大小
  # stat_ellipse(aes(color = group), geom = 'polygon', level = 0.95,size = 0.8,
  #              alpha = 1, show.legend = FALSE,linetype="solid",fill="NA") + # 95%的置信椭圆，重复>4时才可以使用
  # geom_text_repel(label=df$samples,max.overlaps = Inf)+ # 添加标签，且不遮挡、全部显示
  geom_vline(xintercept = 0,lty="dashed")+
  geom_hline(yintercept = 0,lty="dashed")+#图中虚线
  labs(x=paste0("PC1 (",pc[1],"%)"),
       y=paste0("PC2 (",pc[2],"%)"))+#将x、y轴标题改为贡献度
  # scale_color_manual(values = color) +#点的颜色设置
  theme_bw()+
  theme(strip.background = element_rect(fill= "#d9d9d9"))+
  theme(strip.text = element_text(size=13,color = "black"))+
  theme(panel.grid.major=element_line(colour=NA),
        panel.background = element_rect(fill = "transparent",colour = NA),
        plot.background = element_rect(fill = "transparent",colour = NA),
        panel.grid.minor = element_blank())+
  theme(title=element_text(size=14,colour = "black"))+
  theme(axis.text.x = element_text(size = 12, angle =0, hjust = 0.5,
                                   colour = "black"))+#y轴标签
  theme(axis.text.y = element_text(size = 12, angle =0, hjust = 0.5,
                                   colour = "black"))+#x轴标签
  theme(axis.title.y = element_text(size = 15, angle =90, hjust = 0.5,
                                    colour = "black"))+#y轴标题
  theme(axis.title.x = element_text(size= 15, angle =0, hjust = 0.5,
                                    colour = "black"))+#x轴标题
  theme(legend.position="right")+#图例位置
  theme(legend.title = element_text(colour="black",size=14))+#图例标题
  theme(legend.text = element_text(colour="black",size = 12),#图例标签
        plot.title = element_text(hjust=0.5),
        legend.key = element_blank())#标题位置
p1
ggsave('HMK_T/PCA.pdf', p1, width = 4, height = 4,units = 'in',dpi = 600)

##########PCA图添加箱线图######
#在PCA图的x和y轴添加箱线图，可以实现进一步展示组间差异
# 绘制y轴为PC1值的分组箱线图
p2 <- ggplot(df,aes(x=V1,y=PC1))+#指定数据
  stat_boxplot(geom = "errorbar", width=0.4,size=0.5)+#添加误差线,注意位置，放到最后则这条线不会被箱体覆盖
  coord_flip()+
  geom_boxplot(aes(fill=V1), #绘制箱线图函数
               outlier.colour="white",size=0.5)+#异常点去除
  xlab("") + ylab("")+
  scale_fill_manual(values= color)+#指定颜色
  geom_signif(comparisons = list(c("JM22","XM26")),# 设置需要比较的组
              map_signif_level = T, #是否使用星号显示
              test = t.test, ##计算方法
              # y_position = c(5.8,6.8,6.2),#图中横线位置设置
              tip_length = c(c(0,0),
                             c(0,0),
                             c(0,0)),#横线下方的竖线设置
              size=0.5,color="black",textsize = 5)+
  theme_bw()+
  theme(strip.background = element_rect(fill= "#d9d9d9"))+
  theme(strip.text = element_text(size=13,color = "black"))+
  theme(panel.grid.major=element_line(colour=NA),
        panel.background = element_rect(fill = "transparent",colour = NA),
        plot.background = element_rect(fill = "transparent",colour = NA),
        panel.grid.minor = element_blank())+
  theme(title=element_text(size=14,colour = "black"))+
  theme(axis.text.x = element_text(size = 12, angle =0, hjust = 0.5,
                                   colour = "black"))+#y轴标签
  theme(axis.text.y = element_text(size = 12, angle =0, hjust = 0.5,
                                   colour = "black"))+#x轴标签
  theme(axis.title.y = element_text(size = 15, angle =90, hjust = 0.5,
                                    colour = "black"))+#y轴标题
  theme(axis.title.x = element_text(size= 15, angle =0, hjust = 0.5,
                                    colour = "black"))+#x轴标题
  theme(legend.position="none")+#图例位置
  theme(legend.title = element_text(colour="black",size=14))+#图例标题
  theme(legend.text = element_text(colour="black",size = 12),#图例标签
        plot.title = element_text(hjust=0.5),
        legend.key = element_blank())#标题位置
p2
# 绘制y轴为PC2值的分组箱线图
p3 <- ggplot(df,aes(x=V2,y=PC2))+#指定数据
  stat_boxplot(geom = "errorbar", width=0.4,size=0.5)+#添加误差线,注意位置，放到最后则这条线不会被箱体覆盖
  geom_boxplot(aes(fill=V2), #绘制箱线图函数
               outlier.colour="white",size=0.5)+#异常点去除
  xlab("") + ylab("")+
  scale_fill_manual(values= color)+#指定颜色
  geom_signif(comparisons = list(c("Diseased","Healthy")),# 设置需要比较的组
              map_signif_level = T, #是否使用星号显示
              test = t.test, ##计算方法
              # y_position = c(5.8,6.8,6.2),#图中横线位置设置
              tip_length = c(c(0,0),
                             c(0,0),
                             c(0,0)),#横线下方的竖线设置
              size=0.5,color="black",textsize = 5)+
  theme_bw()+
  theme(strip.background = element_rect(fill= "#d9d9d9"))+
  theme(strip.text = element_text(size=13,color = "black"))+
  theme(panel.grid.major=element_line(colour=NA),
        panel.background = element_rect(fill = "transparent",colour = NA),
        plot.background = element_rect(fill = "transparent",colour = NA),
        panel.grid.minor = element_blank())+
  theme(title=element_text(size=14,colour = "black"))+
  theme(axis.text.x = element_text(size = 12, angle =0, hjust = 0.5,
                                   colour = "black"))+#y轴标签
  theme(axis.text.y = element_text(size = 12, angle =0, hjust = 0.5,
                                   colour = "black"))+#x轴标签
  theme(axis.title.y = element_text(size = 15, angle =90, hjust = 0.5,
                                    colour = "black"))+#y轴标题
  theme(axis.title.x = element_text(size= 15, angle =0, hjust = 0.5,
                                    colour = "black"))+#x轴标题
  theme(legend.position="none")+#图例位置
  theme(legend.title = element_text(colour="black",size=14))+#图例标题
  theme(legend.text = element_text(colour="black",size = 12),#图例标签
        plot.title = element_text(hjust=0.5),
        legend.key = element_blank())#标题位置
p3
# ggpubr::ggarrange()函数对图进行拼接
ggarrange(p2, NULL, p1, p3, widths = c(5,3), heights = c(2,5), align = "hv")
#7x6 导出