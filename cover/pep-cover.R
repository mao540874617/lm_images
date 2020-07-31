library(ggplot2)
library(openxlsx)
library(plotrix)#用于绘制3D饼图


#### 5.3.1.读取原始蛋白质谱搜库数据：Protein quantitation*.xlsx
pro_dataname <- dir(path="./",pattern="^Protein quantitation.*.xlsx$")
data_pro <- read.xlsx(pro_dataname, sheet = 1, startRow = 2, colNames = T, rowNames = F, detectDates = F)
dim(data_pro)

#### 5.3.2.数据处理
rownames(data_pro) <- data_pro$Accession
data_pie_new <- data.frame(rownames(data_pro), data_pro$`Coverage.[%]`)
head(data_pie_new)
colnames(data_pie_new) <- c("Accession", "Coverage")
head(data_pie_new)
dim(data_pie_new)
max(data_pie_new$Coverage)
min(data_pie_new$Coverage)
A <- NROW(data_pie_new[data_pie_new$Coverage <= 10,])
B <- NROW(data_pie_new[data_pie_new$Coverage > 10 & data_pie_new$Coverage <= 20,])
C <- NROW(data_pie_new[data_pie_new$Coverage > 20 & data_pie_new$Coverage <= 30,])
D <- NROW(data_pie_new[data_pie_new$Coverage > 30 & data_pie_new$Coverage <= 40,])
E <- NROW(data_pie_new[data_pie_new$Coverage > 40,])

#绘图部分
x <- c(A, B, C, D, E)
labels <- c("0-10%", "10-20%", "20-30%", "30-40%",">40%")
#绘制2D饼图部分

pdf("./Peptide_coverage_2D.pdf", width = 12, height = 7.5)
pie(x, labels = labels, col = rainbow(length(x)), main = "The rate of peptide coverage(%)")
dev.off()

png("./Peptide_coverage_2D.png", type = "cairo", width = 1200, height = 750)
pie(x, labels = labels, col = rainbow(length(x)), main = "The rate of peptide coverage(%)")
dev.off()



#绘制3D饼图部分
#pie3D(x, labels = labels, explode = 0.1, height = 0.25, main = "The rate of peptide coverage(%)")

pdf("./Peptide_coverage_3D.pdf", width = 12, height = 7.5)
pie3D(x, labels = labels, explode = 0.1, height = 0.25, main = "The rate of peptide coverage(%)")
dev.off()
png("./Peptide_coverage_3D.png", type = "cairo", width = 1200, height = 750)
pie3D(x, labels = labels, explode = 0.1, height = 0.25, main = "The rate of peptide coverage(%)")
dev.off()


