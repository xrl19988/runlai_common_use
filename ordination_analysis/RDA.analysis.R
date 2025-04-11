
rm(list = ls())# 清除环境变量
# 数据导入----
# 合并数据
data <- read.csv("tes1.csv") 
# 提取环境变量
env <- data[,c(4:9)]
# 提取响应变量
ghg <- as.data.frame(data[,2:3])
# colnames(ghg) <- "CH4.flux"
# ghg <- as.data.frame(scale(ghg,center=T,scale=F))

# DCA分析----
dca <- decorana(veg = ghg)
#根据DCA1的Axis Lengths值进行选择，如果>4.0选CCA；如果在3.0-4.0之间，选RDA和CCA都可以；如果<3.0, 选择RDA分析即可。

# RDA分析----
# RDA <- rda(data[,c(2,3,8:11)],data[,4:7])
RDA <- rda(ghg ~ ., env, scale = F)
summary(RDA)
# rdascore <- scores(RDA)
# 简单作图观察结果
plot(RDA)#绘制图形
add_sp_ar<- scores(RDA, choices = 1:2, display = 'sp')#提取物种位置数据，本例中为三个多样性指数
arrows(0, 0, add_sp_ar[ ,1], add_sp_ar[ ,2], length =  0, lty = 1, col = 'red')#加上红色箭头

# 置换检验----
envfit <- envfit(RDA, env, permutations  = 499)
r <- as.matrix(envfit$vectors$r)
p <- as.matrix(envfit$vectors$pvals)
env.p <- cbind(r,p)
colnames(env.p) <- c("r2","p-value")
KK <- as.data.frame(env.p)
KK

# 环境因子解释率----
## 条件效应Conditional effect（将所有环境变量作为解释变量进行RDA分析，依据置换检验的显著性（p值或F值）对环境因子进行重要性的排位。然后按重要性依次加入解释变量做RDA分析，每运行一次所获得校正R2减去前一次运行的校正R2，就是当次新加入环境因子的条件效应）
anova(RDA, by = "term", permutations=499)
rs1 <- RsquareAdj(rda(ghg ~ Soil.moisture, env))$adj.r.squared
rs2 <- RsquareAdj(rda(ghg ~ Soil.moisture+Air.temperature, env))$adj.r.squared
rs2-rs1

# 前向选择----
## vegan包ordiR2step()前向选择----
# 基于 999 次置换检验，详情 ?ordiR2step()
# rda_tb_forward_r <- ordiR2step(rda(ghg~1,env, scale = F), scope = formula(RDA),R2scope = TRUE,
#                                direction = 'forward', permutations = 999)
rda_tb_forward_r <- ordistep(rda(ghg~1,env, scale = F), formula(RDA), direction = 'forward', permutations = 499)# trace = 0

#细节部分查看
summary(rda_tb_forward_r)
#比较选择前后校正后 R2 的差异
RsquareAdj(RDA)$adj.r.squared
RsquareAdj(rda_tb_forward_r)$adj.r.squared

## packfor包前向选择----
library(packfor)
#forward.sel.par() 基于参数的方法，通过 F 检验评估 R2 的显著性，详情 ?forward.sel.par
#forward.sel() 基于非参数的方法，通过置换检验的原理评估 R2 的显著性，详情 ?forward.sel
simpleRDA2 <-vegan:::simpleRDA2
fora <- forward.sel.par(Y = ghg[c('CH4.flux')], X = env[c('Soil.moisture', 'Soil.NO3..N', 'Soil.NH4..N', 'Air.temperature')],
                R2more = 0, alpha = 1) # 如果想查看所有变量的 R2 贡献和 p 值，可设置 alpha=1 和 R2more=0
fora

## rdacca.hp做层次分割----
library(rdacca.hp)
res <- rdacca.hp(ghg,env,method="RDA", type = 'adjR2', scale = FALSE)   # 每个解释变量占总变化量的比例,var.part = TRUE分解两组或多组变量贡献度的情况
re
plot(res)

# RDA作图信息提取----
#RDA得分
samples <- as.data.frame(RDA$CCA$u[,1:2]) # extract samples
  samples$Irrigation <- as.factor(env$Irrigation)
  samples$Nitrogen <- as.factor(env$Nitrogen)
envi <- as.data.frame(RDA$CCA$biplot[,1:2])# extract envs
species <- as.data.frame(RDA$CCA$v[,1:2])# extract species
# species <- as.data.frame(rdascore$species) 

# RDA轴解释率计算
RDAS <- summary(RDA)
RDASA <- round(RDAS$cont$importance,4)

# visualization----
p <- ggplot()+
  geom_hline(yintercept = 0,linetype ="dashed")+geom_vline(xintercept = 0,linetype ="dashed")+
  geom_point(data=samples,aes(x=RDA1,y=RDA2))+
  geom_point(data=species,aes(x=RDA1,y=RDA2), size = 3, color = "green")+
  geom_segment(data=species,aes(x=0,y=0,xend=RDA1*0.5,yend=RDA2*0.5),colour="#009A3D",size=0.5,
               arrow=arrow(length=unit(0.3,"cm")))+
  geom_text(data=species,aes(x=RDA1*0.8,y=RDA2*0.8,label=rownames(species)),size=3.5,
            colour="#009A3D")+ #修改hjust的参数可以调节标签距离箭头的远近
  
  geom_segment(data=envi,aes(x=0,y=0,xend=RDA1,yend=RDA2),colour="red",size=0.5,
               arrow=arrow(length=unit(0.3,"cm")))+
  geom_text(data=envi,aes(x=RDA1*0.8,y=RDA2*0.8,label=rownames(envi)),size=3.5,
            colour="red")+ #修改hjust的参数可以调节标签距离箭头的远近
  xlab(paste('RDA1 (',RDASA[2,1]*100,"%)", sep = " "))+ylab(paste('RDA2 (',RDASA[2,2]*100,"%)", sep = " "))+
  theme_bw()+
  scale_y_continuous(limits = c(-1,1))+
  scale_x_continuous(limits = c(-1,1))
p
ggsave("p.pdf",p,height = 6,width = 6,dpi = 300)

