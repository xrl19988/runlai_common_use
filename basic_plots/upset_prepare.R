library(dplyr)
rm(list=ls())
data1 <- read.csv('D:/downloadS/WeChat Files/wxid_99gdtwko13mi22/FileStorage/File/2024-07/AllASV(1).csv')
colnames(data1) <- 'A'
data2 <- read.csv('D:/downloadS/WeChat Files/wxid_99gdtwko13mi22/FileStorage/File/2024-07/EnrichinW0(1).csv')
colnames(data2) <- 'B'
unique_df <- data.frame(unique(data1$A,data2$B))
colnames(unique_df ) <- 'X'

DS <- unique_df %>% mutate(A = ifelse(unique_df$X %in% data1$A , 1, 0), 
                          B = ifelse(unique_df$X %in% data2$B , 1, 0)) 

