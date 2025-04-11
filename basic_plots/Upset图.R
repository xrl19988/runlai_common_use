rm(list=ls())
# 载入R包：
require(ggplot2)
require(plyr)
require(gridExtra)
require(grid)
library(UpSetR)

# 案例：
# movies <- read.csv(system.file("extdata", "movies.csv", package = "UpSetR"),
#                    header=TRUE, sep=";" )
# 导入数据
movies <- read.csv('out/upset/diffup_Rz.csv',header = T,row.names = 1)
########## 基础绘图 ----------------
library(RColorBrewer)
upset(movies,
      # 数据集数量：
      nsets = 6,
      # 柱形的最大数目：NA为所有
      nintersects = NA,
      point.size = 3, line.size = 0.7, text.scale = 1.5,
      # 柱形图与矩阵图大小比例：
      mb.ratio = c(0.6, 0.4),
      set_size.show = T, #显示每个集合的总元素数(在左侧条形图中)
      main.bar.color = 'black', #上方柱形图颜色
      matrix.color = '#225EA8', #交点颜色
      # 修改左侧柱形颜色：
      sets.bar.color = c("#225EA8"),
      # 柱形排序：
      order.by = c("freq", "degree"), decreasing = c(T, F),
      # 修改交集的颜色
      # queries = list(list(query = intersects, params = list("JMDVSXMD"),
      #                     color= "#006bba", active = T)),
      # 矩形散点的阴影颜色：
      shade.color = NA
      )
# 6x12