#载入程序包
library(PerformanceAnalytics,quietly = T)
library(openxlsx,quietly = T)

setwd("/outport")

da <- read.xlsx("raw_nor.xlsx")
da1 <- da[,-1]


# 如果数据框中有空值,那么就用整张表中的最小值去替换空值
if(is.na(sum(da1))){
  da1[is.na(da1)]<-min(na.omit(da1))
}

#切换行和列
#corr<-cor(da1)

#保存及绘图部分
png("./samplecorr.png",width=1100,height=1100)
chart.Correlation(da1, histogram=TRUE, pch=19)
dev.off()











