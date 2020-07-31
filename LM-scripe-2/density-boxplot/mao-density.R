#加载需要的依赖包
library(ggplot2,quietly = T)
library(reshape2,quietly = T)
#library(optparse,quietly = T)
library(openxlsx,quietly = T)

ra <- read.xlsx("raw_nor.xlsx",sheet="raw")

ra2 <-ra[,-1]

#原始数据密度图

#读取原始数据后的处理部分
ff<-melt(ra2)

variable<-factor(ff$variable,levels=unique(ff[,1]))

ff$variable<-factor(ff$variable,levels=unique(ff[,1]))

#使用ggplot2绘图部分
p<-ggplot(ff,aes(x=value,color=variable))+
  stat_density(position = "identity",geom = 'line',alpha=0.5,size=1)+
  labs(x="",y="Density (original)") +
  #theme(legend.position="top")+
  scale_fill_discrete(guide = FALSE)+
  guides(color=guide_legend(title=NULL))+
  theme(legend.text=element_text(size=15))+
  theme(axis.text.x=element_text(angle=60,hjust = 1,size=15),axis.text.y=element_text(size=15),title = element_text(size=15))+
  theme(panel.grid.major =element_blank(), panel.grid.minor = element_blank(),panel.background = element_blank(),axis.line = element_line(colour = "black"),plot.margin=unit(c(2,2,2,2), "cm"))

# 保存图像部分

ggsave("./density.pdf", height=10, width=12, plot=p)
ggsave("./density.png", type="cairo-png", height=10, width=12, plot=p)


















