rm(list=ls())
setwd("d://file/R/")
setwd("DT21Y/2.beta/")
library(ggplot2)
library(ggrepel)
library(vegan)
library(plyr)
library(doBy)
library(ggsci)

##读取 OTU 丰度表
otu <- read.csv('in/ASV_table_Re.CSV', row.names = 1, header =T,stringsAsFactors = FALSE)
otu <- decostand(otu, method = "hellinger")#根据需求是否进行hellinger转化
otu <- data.frame(t(otu))#转置OTU表
# otu <- otu[4:45,]
##读取样本分组：第一列名改为Sample，第二列改为Group，不然后面对应不上
group <- read.csv('in/group_Re.CSV', stringsAsFactors = FALSE)
colnames(group)[3] <- "Group"
# group <- group[4:45,]
#计算
##permanova(adonis) euclidean, bray
adonis_1 <- adonis2(otu~Group,data = group, distance = 'bray', permutations = 999)
adonis_1
# 或anosim分析
anosim = anosim(otu, group$Group, permutations = 999, distance = "bray")
anosim
{
  #不用改
  #计算了样本间成对的 Bray-curtis 距离，详情 ?vegdist
  bray_dis <- vegdist(otu, method = 'bray')
  #PCoA 排序，详情 ?cmdscale
  pcoa <- cmdscale(bray_dis, k = (nrow(otu) - 1), eig = TRUE)
  #提取前两轴的贡献度
  pcoa_exp <- pcoa$eig/sum(pcoa$eig)
  pcoa1 <- paste('PCo1(', round(100*pcoa_exp[1], 2), '%)')
  pcoa2 <- paste('PCo2(', round(100*pcoa_exp[2], 2), '%)')
  #提取前两轴的的坐标，并添加样本的分组信息
  dat <- data.frame(pcoa$point)[1:2]
  dat$Sample <- rownames(dat)
  dat <- merge(dat, group, by = 'Sample')#将样本的轴贡献率与分组对应起来
  names(dat)[2:3] <- c('pcoa1', 'pcoa2')
}
# 输出pcoa1、pcoa2
write.csv(dat,"out/pcoa-HEL_Re.csv",row.names = F)

#画边缘参数,重复小于4时运行这条
group_border <- plyr::ddply(dat, 'group',function(df) df[chull(df[[2]], df[[3]]), ])#规定边缘区域
#主分组
# dat$Year <- factor(dat$Year,levels = c('Third','Second','First'))
# dat$Stage <- factor(dat$Stage,levels = c('B','S','T','F','H'))

##添加样本与质心点(以均值为例)连线
#计算均值：
pcoa_mean <- summaryBy(pcoa1 + pcoa2 ~ Group, dat, FUN = mean)
pcoa_mean
data <- merge(dat, pcoa_mean, by = 'Group')
head(data)

mycolor <- c('#484363','#248277','#e6af30','#b80201')
mycolor <- c('#5e6abc','#f14e38')

p <- ggplot(data = dat, aes(pcoa1, pcoa2)) +
  geom_vline(xintercept = 0,lty="dashed", size = 0.5, color = 'grey')+
  geom_hline(yintercept = 0,lty="dashed", size = 0.5, color = 'grey')+#图中虚线
  geom_point(aes(color = group), size = 3.5) +
  # geom_polygon(data = group_border, aes(fill = group),alpha=0.1) + #画边缘
  stat_ellipse(aes(color = group), geom = 'polygon', level = 0.95,size = 0.8,
               alpha = 1, show.legend = FALSE,linetype="solid",fill="NA") + # 95%的置信椭圆，重复>4时才可以使用
  # geom_segment(data = data, aes(x = pcoa1.mean, y = pcoa2.mean,
  #                               xend = pcoa1, yend = pcoa2, color = Group),
  #              alpha = 0.6,size = 0.5, show.legend = FALSE) +# 添加质心
  labs(x = pcoa1, y = pcoa2)+
  scale_color_manual(values = mycolor)+##定义点的颜色
  scale_fill_manual(values = mycolor)+##定义置信区间的颜色
  theme_bw()+
  theme(panel.grid=element_blank())+ # 统一去除灰色背景和网格线
  theme(title=element_text(size=14,colour = "black"))+
  theme(axis.text.x = element_text(size = 12, angle =0, hjust = 0.5,
                                   colour = "black"))+#y轴标签
  theme(axis.text.y = element_text(size = 12, angle =0, hjust = 0.5,
                                   colour = "black"))+#x轴标签
  theme(axis.title.y = element_text(size = 15, angle =90, hjust = 0.5,
                                    colour = "black"))+#y轴标题
  theme(axis.title.x = element_text(size= 15, angle =0, hjust = 0.5,
                                    colour = "black"))+#x轴标题
  theme(legend.position="top")+#图例位置
  theme(legend.title = element_text(colour="white",size=14))+#图例标题
  theme(legend.text = element_text(colour="black",size = 12),#图例标签
        plot.title = element_text(hjust=0.5),
        legend.key = element_blank())#标题位置
p
ggsave('fig/pcoa-HEL_re.pdf', p, width = 4, height = 4,units = 'in',dpi = 600)
