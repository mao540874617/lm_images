#导入需要的包
library(ggplot2)
library(openxlsx)

setwd("/mao")

#读取数据部分
res_dir <- dir(path="./", ignore.case = F)
res_dir
pro_dataname <- dir(path="./",pattern="^Protein quantitation.*.xlsx$")
data_pro <- read.xlsx(pro_dataname, sheet = 1, startRow = 2, colNames = T, rowNames = F, detectDates = F)

#数据处理部分
dim(data_pro)
head(data_pro)
rownames(data_pro) <- data_pro[,1]
head(rownames(data_pro))
data_pro_new <- data.frame(row.names(data_pro), data_pro$Accession, data_pro$`MW.[kDa]`)
colnames(data_pro_new) <- c("row.names", "Accession", "MW.[kDa]")
dim(data_pro_new)
head(data_pro_new, n = 10)

#ggplot绘图部分
#abc <- gsub(" ", "_", i) ## 命名方法
p <- ggplot(data_pro_new, aes(data_pro_new$`MW.[kDa]`)) +
  geom_histogram(binwidth = 50, fill = "brown1", col = "black") +
  theme_bw() +
  theme(panel.border = element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), axis.line = element_line(colour = "black")) +
  theme(plot.margin = unit(rep(2,4),'lines'), aspect.ratio = .6) +
  theme(legend.text = element_text(size = 15, colour = "black")) +
  theme(legend.title = element_text(face = "bold", size = 20)) +
  theme(axis.title.x = element_text(size=16), axis.title.y=element_text(size=16)) +
  xlab("Molecular weight(kDa)") +
  ylab("Number of identified proteins")

#保存数据部分先不写
ggsave(("./pepnum.pdf"), width = 12, height = 7.5, units = "in",plot = p)
ggsave(("./pepnum.png"), width = 12, height = 7.5, units = "in",plot = p)

