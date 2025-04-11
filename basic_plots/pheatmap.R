data.1 <- read.csv('top_de_exp1 的副本.csv',row.names = 1)
library(pheatmap)
data.2 <- decostand(data.1,"standardize",MARGIN = 1)
ph <- pheatmap(data.2, color = colorRampPalette(c("#3f70b1", "#fffff1", "#d93931"))(50),
               scale = 'row',
               border=FALSE,number_color = "black",
               # annotation_col = annotation_col,
               # annotation_colors = ann_colors,
               # labels_col = labels_col,
               cellwidth = 20, cellheight = 15,cluster_cols = T)
ph
ggsave('ph.pdf',ph,width = 8,height = 8,dpi = 300)
