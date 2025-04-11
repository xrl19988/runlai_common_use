setwd("d://file/R/")
setwd("DT21Y/2.asvs_VENN/")
library(venn)         #韦恩图（venn 包，适用样本数 2-7）
library(VennDiagram) 
library(ggplot2)
rm(list=ls())
#示例文件
venn_dat <- read.delim('E:\research\r_codes\basic_plots/venn_exp.txt') #输入文件格式
# 读取数据文件
venn_dat <- read.csv("out/upset/diffvenn_Rz.csv",header = T)#读入表格
venn_list <- list(venn_dat[,1], venn_dat[,2]
                  , venn_dat[,3], venn_dat[,4]
                  # , venn_dat[,5], venn_dat[,6]
                  )   # 制作韦恩图搜所需要的列表文件
names(venn_list) <- colnames(venn_dat[1:4])    # 把列名赋值给列表的key值
venn_list = purrr::map(venn_list,na.omit)      # 删除列表中每个向量中的NA

#作图
mycolor <- c('#484363','#248277','#e6af30','#b80201')
mycolor <- c('#f88580','#48c5c3')
venn(venn_list,
     zcolor= mycolor, # 调整颜色，style是默认颜色，bw是无颜色，当然也可以自定义颜色
     opacity = 0.6,  # 调整颜色透明度
     box = F,        # 是否添加边框
     ilcs = 1,     # 数字大小
     sncs = 1,        # 组名字体大小
     )

# 更多参数 ?venn查看

# 查看交集详情,并导出结果
inter <- get.venn.partitions(venn_list)
for (i in 1:nrow(inter)) inter[i,'values'] <- paste(inter[[i,'..values..']], collapse = '|')
inter <- subset(inter, select = -..values.. )
inter <- subset(inter, select = -..set.. )
write.table(inter, "out/upset/diffvenn-result_Rz.csv", row.names = FALSE, sep = ',', quote = FALSE)
