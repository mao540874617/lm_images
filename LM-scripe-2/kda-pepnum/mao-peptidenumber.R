## 导入R包
###############################################################################################################################################################################################
#
#analysis_need_packages <- c("ggplot2", "plotrix", "openxlsx", "readxl", "ggrepel", "corrplot", "progress", "ggrepel", "praise", "ComplexHeatmap", "plyr", "circlize", "grid")
#sapply(analysis_need_packages, library, character.only = T)
###############################################################################################################################################################################################

library(ggplot2)
library(openxlsx)


setwd("/mao")

#读取数据部分
pep_dataname <- dir(path = ".",pattern = "^Peptide groups*")
data_pep <- read.xlsx(pep_dataname, sheet = 1, startRow = 2, colNames = T, rowNames = F, detectDates = F)

#显示行列数
dim(data_pep)
data_pep_new <- data.frame(data_pep$Annotated.Sequence,data_pep$Master.Protein.Accessions)

#数据处理部分
#head(data_pep_new, n = 10)
#更改列名
colnames(data_pep_new) <- c("Annotated.Sequence","Master.Protein.Accessions")
pep <- unlist(table(data_pep_new$Master.Protein.Accessions))
pep <- as.data.frame(pep)
class(pep)
colnames(pep) <- c("Master.Protein.Accessions", "Freq")
pep <- pep[order(pep$Freq), ] # 按照Freq列由小到大对数据框进行排序

#绘图部分
p <- ggplot(pep, aes(pep$Freq)) +
  geom_histogram(binwidth = 5, fill = "dodgerblue", col = "black") +
  theme_bw() +
  theme(panel.border = element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), axis.line = element_line(colour = "black")) +
  theme(plot.margin = unit(rep(2,4),'lines'), aspect.ratio = .6) +
  theme(legend.text = element_text(size = 15, colour = "black")) +
  theme(legend.title = element_text(face = "bold", size = 20)) +
  theme(axis.title.x = element_text(size=16), axis.title.y=element_text(size=16)) +
  xlab("Peptide number") +
  ylab("Number of identified proteins")

#保存数据部分(暂时不写)
ggsave(("./Molecular_weight_distribution.pdf"), width = 12, height = 7.5, units = "in",plot = p)
ggsave(("./Molecular_weight_distribution.png"), width = 12, height = 7.5, units = "in",plot = p)





