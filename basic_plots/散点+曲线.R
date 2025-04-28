# 加载 ggplot2 包
library(ggplot2)

# 创建示例数据
set.seed(123)
data <- data.frame(
  x = rep(1:10, 2),
  y = c(rnorm(10, 1:10, 1), rnorm(10, 1:10 + 2, 1)),
  group = rep(c("Group A", "Group B"), each = 10)
)

# 绘制图形
ggplot(data, aes(x = x, y = y, color = group)) +
  # 绘制散点
  geom_point(aes(shape = "Scatter"), size = 3) +
  # 绘制曲线
  geom_smooth(aes(linetype = "Curve"), method = "loess", se = FALSE) +
  # 手动设置颜色比例尺
  scale_color_manual(name = "Group", values = c("Group A" = "red", "Group B" = "blue")) +
  # 手动设置形状比例尺
  scale_shape_manual(name = "Element", values = c("Scatter" = 16)) +
  # 手动设置线型比例尺
  scale_linetype_manual(name = "Element", values = c("Curve" = 1)) +
  # 设置图例位置
  theme(legend.position = "right") +
  labs(title = "Separate Legends for Scatter and Curve",
       x = "X",
       y = "Y")
