##将一个 ceRNA 网络图以冲击图展示
library(reshape2)
library(ggalluvial)
rm(list = ls())
#读取数据，lncRNA-miRNA-mRNA 的关系表
ceRNA <- read.csv('', stringsAsFactors = FALSE)

#预指定颜色，这个示例中 lncRNA、miRNA 和 mRNA 总计 36 个，需指定 36 种颜色
color <- c(JMD = '#484363', JMH = '#248277', XMD = '#e6af30', XMH = '#b80201', 
           Actinobacteria='#a393eb',Bacteroidetes='#62d2a2',Firmicutes='#f9a1bc',
           Proteobacteria='#ea5455',Saccharibacteria='#ffd460',
           Bacillus='#377EB8', Dyadobacter='#4DAF4A', Parafilimonas='#984EA3', Pedobacter='#FF7F00',
           Lysobacter='#FFFF33', Pseudomonas='#A65628',Rhizobium='#F0027F',
           Unassigned='#7ac7c4',Others='#928a97')

# color <- c('#8DD3C7', '#FFFFB3', '#BEBADA', '#FB8072', '#80B1D3', '#FDB462',
#            '#B3DE69', '#FCCDE5', '#BC80BD', '#CCEBC5', '#FFED6F', '#E41A1C',
#            '#377EB8', '#4DAF4A', '#984EA3', '#FF7F00', '#FFFF33', '#A65628',
#            '#F781BF', '#66C2A5', '#6181BD', '#F34800', '#64A10E', '#FF00FF',
#            '#c7475b', '#049a0b', '#BEAED4', '#FDC086', '#FFFF99', '#386CB0',
#            '#4253ff', '#ff4308', '#D8D155', '#F0027F', '#9FAED4', '#F7CDBD')


#整理为 ggplot2 的作图格式
# ceRNA$freq <- 1
ceRNA <- melt(ceRNA, id = 'freq')
variable <- summary(ceRNA$variable)
ceRNA$flow <- rep(1:variable[1], length(variable))
head(ceRNA)  #查看整理后的数据结构

#绘制冲击图展示 lncRNA-miRNA-mRNA 的关系网络
sankey <- ggplot(ceRNA, aes(x = variable, y = freq, stratum = value, alluvium = flow, fill = value)) +
  geom_stratum(color= "transparent") +  #冲击图中的堆叠柱形图
  geom_flow(aes.flow = 'forward') +  #冲击图连线绘制
  scale_fill_manual(values = color) +  #颜色赋值
  # geom_text(stat = 'stratum', infer.label = TRUE, size = 2.5) +  #添加 lncRNA、miRNA 和 mRNA 标签
  # # scale_x_discrete(limits = c('lncRNA', 'miRNA', 'mRNA')) +  #定义 lncRNA、miRNA 和 mRNA 列的展示顺序
  theme(legend.position = 'none', panel.background = element_blank(), line = element_blank(), axis.text.y = element_blank()) +
  labs(x = '', y = '')
sankey
ggsave("sankey.pdf",sankey, width = 6, height = 4,units = 'in',dpi = 600)
