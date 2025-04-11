# install.packages("treemap")
# install.packages("HistData")
# install.packages("RColorBrewer")

library(HistData)
library(treemap)
library(dplyr)
library(RColorBrewer)

#示例----
# Load the data to your R environment 加载示例数据，霍乱流行情况
data("Cholera")

# Check the dataframe structure 检查数据结构
str(Cholera)

# 绘图
treemap(Cholera,
        index=c("region","district"), #Index在树图中定义两个层次：区域region和地区district；后者是前者的细分
        vSize="cholera_deaths", #vSize指定霍乱死亡率cholera_deaths来定义矩形的大小
        vColor = "region", #vColor指定更高层次region矩形颜色的区域；
        type = "categorical", #type通知函数vColor是一个分类变量；
        # formatting options格式选项:
        palette = brewer.pal(n = 5, name = "Accent"), #palette更改调色板颜色，n为更高层次矩形region数目
        align.labels=list(
          c("left", "top"), 
          c("right", "bottom")
        ),                      #align.labels指定矩形上的标签对齐方式，c("left", "top")为左上角
        border.col = "white",   #border.col指定矩形边界颜色
        bg.labels = 255,        #bg.labels指定矩形上的标签底色，用于确定标签的透明度，介于0-255之间
        position.legend = "bottom") #图例的位置，没有为none，置于底部为bottom


#差异富集属----
data <- read.csv("treemapLGN0W0.csv",header=T)
str(data)

treemap(data,
        index=c("treatment","genus"), #Index在树图中定义两个层次：
        vSize="abundance", #vSize指定abundance来定义矩形的大小
        vColor = "treatment", #vColor指定更高层次region矩形颜色的区域；
        type = "categorical", #type通知函数vColor是一个分类变量；
        # formatting options格式选项:
        palette = brewer.pal(n = 5, name = "Accent"), #palette更改调色板颜色，n为更高层次矩形region数目
        align.labels=list(
          c("left", "top"), 
          c("right", "bottom")
        ),                      #align.labels指定矩形上的标签对齐方式，c("left", "top")为左上角
        border.col = "white",   #border.col指定矩形边界颜色
        bg.labels = 255,        #bg.labels指定矩形上的标签底色，用于确定标签的透明度，介于0-255之间
        position.legend = "none") #图例的位置，没有为none，置于底部为bottom

