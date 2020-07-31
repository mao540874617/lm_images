#加载需要的依赖包
library(ggplot2,quietly = T)
library(reshape2,quietly = T)
#library(optparse,quietly = T)
library(openxlsx,quietly = T)

setwd("/outport")

#################################
#density
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


##################################
#boxplot
#读取数据,箱线图的数据为raw_nor.xlsx

nor <- read.xlsx("raw_nor.xlsx")

nor2 <- nor[,-1]

ff<-melt(nor2)

#设置一个因子,因子的水平就是列名,主要用于指定箱线图的颜色和图例
variable <- factor(ff[,1],levels=unique(ff[,1]))
## 这一步的作用是对ff数据框按照bariable进行一次排序,否则做出来的图名字会不整齐
ff$variable<-factor(ff$variable,levels=unique(ff[,1]))

#利用包ggplot2绘图
#原始数据箱线图
p<-ggplot(ff,aes(x=variable,y=value,color=variable))+
    stat_boxplot(geom = "errorbar",width=0.35)+
    geom_boxplot(aes(fill=variable))+
    geom_boxplot(alpha=0.5)+
    labs(x="",y="Original Protein Expression") +
    #theme(legend.position="top")+
    scale_fill_discrete(guide = FALSE)+
    guides(color=guide_legend(title=NULL))+
    theme(legend.text=element_text(size=15))+
    theme(axis.text.x=element_text(angle=60,hjust = 1,size=15),axis.text.y=element_text(size=15),title = element_text(size=15))+
    theme(panel.grid.major =element_blank(), panel.grid.minor = element_blank(),panel.background = element_blank(),axis.line = element_line(colour = "black"),plot.margin=unit(c(2,2,2,2), "cm"))

ggsave("./boxplot.pdf", height=10, width=12, plot=p)
ggsave("./boxplot.png", type="cairo-png", height=10, width=12, plot=p)

#####################################