
library(ggplot2)
library(moonBook)
library(webr)


browser=c("MSIE","Firefox","Chrome","Safari","Opera")
share=c(50,21.9,10.8,6.5,1.8)
df=data.frame(browser,share)

pi <- PieDonut(df,aes(browser,count=share),
         r0=0,start=3*pi/2,labelpositionThreshold=0.1,
         showPieName=FALSE)+
  theme_void()
pi
ggsave('./diff_sig/diffsig/pi.pdf', pi, width = 4, height = 4, units = 'in', dpi = 600)
