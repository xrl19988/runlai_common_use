# 演示数据
exam_scores <- data.frame(
  row.names = c("Student.1", "Student.2", "Student.3"),
  Biology = c(7.9, 3.9, 9.4),
  Physics = c(10, 20, 0),
  Maths = c(3.7, 11.5, 2.5),
  Sport = c(8.7, 20, 4),
  English = c(7.9, 7.2, 12.4),
  Geography = c(6.4, 10.5, 6.5),
  Art = c(2.4, 0.2, 9.8),
  Programming = c(0, 0, 20),
  Music = c(20, 20, 20)
)
exam_scores

library(fmsb)
df <- rbind(rep(20,10) , rep(0,10) , exam_scores)# max & min

# 绘制第一个学生的数据
student1_data <- df[c("Max", "Min", "Student.1"), ]
mycol <- c("#EF4A2B","#D52026","#1C265B","#008646","#00AB9B")
radarchart(df,
           axistype = 1,
           pty = 16, # 32 & 16
           cglcol="grey", cglty=1, axislabcol="black",  cglwd=0.8, # net
           caxislabels=seq(0,20,5),
           seg = 4,
           centerzero =T,# 开关中心
           plty = 1,
           pcol=mycol , pfcol=scales::alpha(mycol,0) , plwd=2)
  