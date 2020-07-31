library(openxlsx)
library(corrplot)

setwd("/outport")

sample_info_1 <- read.xlsx("sample_information.xlsx", sheet = "样品信息", startRow = 1, colNames = TRUE, rowNames = FALSE, detectDates = FALSE)
sample_info_2 <- read.xlsx("sample_information.xlsx", sheet = "比较组信息", startRow = 1, colNames = TRUE, rowNames = FALSE, detectDates = FALSE)


data <- read.xlsx("差异蛋白筛选结果.xlsx", sheet = "CNV_retina_control_retina", startRow = 1, colNames = TRUE, rowNames = FALSE, detectDates = FALSE)
dim(data)
class(data)
rownames(data) <- data[,1]

samp_data_order_pv <- data[order(data$`P-value`),] # 按照p value值进行升序排序
samp_numb <- sample_info_1$样品编号
sm <- match(samp_numb, colnames(samp_data_order_pv))
sm <- as.numeric(sm[!is.na(sm)])
samp_data_order_pv_new <- samp_data_order_pv[,sm]

#判断蛋白数量够不够50个
if (NROW(samp_data_order_pv_new) >= 50) {
  top50_prot <- data.frame(samp_data_order_pv_new[1:50,]) # 提取样品的top50蛋白质谱表达数据
  dim(top50_prot)
  head(top50_prot)
}else if (NROW(samp_data_order_pv_new) >= 10 & NROW(samp_data_order_pv_new) < 50) {
  pn <- NROW(samp_data_order_pv_new)
  topless50 <- paste0("top",pn,"_prot") # topless50 代替paste0("top",pn,"_prot")
  topless50 <- as.data.frame(samp_data_order_pv_new) # 获取所有蛋白数据
}else {
  print(paste0("注意：比较组",new_compa_groups,"的差异蛋白数太少，所以无法绘制蛋白相关性热图"))
}

# NA替换为比较组差异蛋白表达最小值
top50_prot_temp <- na.omit(top50_prot)
data_min <- apply(top50_prot_temp, 2, min)
data_min_numeric <- as.numeric(data_min)
data_min_numeric1 <- data_min_numeric[!is.na(data_min_numeric)]
data_min_value <- min(data_min_numeric1)
top50_prot[is.na(top50_prot)] <- data_min_value
#
top50_prot_t <- t(top50_prot) # 数据框转置
##############################################################################
#最后用于展现在图中的数据
top50_prot_t_corr <- cor(top50_prot_t) # 计算top50蛋白的相关性系数
##############################################################################

## 绘图及保存
col <- colorRampPalette(c("blue","white", "red"))(1000) # 准备绘图颜色

pdf("./correlation.pdf", width = 10, height = 10)
M <- corrplot(top50_prot_t_corr, method = "circle", type = "upper", tl.col = "black", tl.cex = 0.8, col = col, order = "hclust", diag = F)
dev.off()





#dim(samp_data_order_pv)
#head(colnames(samp_data_order_pv))
#head(rownames(samp_data_order_pv))




