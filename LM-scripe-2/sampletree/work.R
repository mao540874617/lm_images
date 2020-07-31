#导入需要的包
library(openxlsx)

setwd("/outport")

##########################################################
# 样品层次聚类树：依据数据为可信或总可信样品数据
################################################
sample_info_1 <- read.xlsx("sample_information.xlsx", sheet = "样品信息", startRow = 1, colNames = TRUE, rowNames = FALSE, detectDates = FALSE)

sample_info_2 <- read.xlsx("sample_information.xlsx", sheet = "比较组信息", startRow = 1, colNames = TRUE, rowNames = FALSE, detectDates = FALSE)


dep <- dir(path = "./", pattern = "^差异蛋白筛选结果")

des <- dir(path = "./", pattern = "^差异位点筛选结果")

depp <- dir(path = "./", pattern = "^差异肽段筛选结果")
###########################################################################

trusted_prot <- read.xlsx("./差异蛋白筛选结果.xlsx", sheet = "可信蛋白", startRow = 1, colNames = TRUE, rowNames = FALSE, detectDates = FALSE)

trusted_prot_clean <- trusted_prot[, match(sample_info_1$作图编号, colnames(trusted_prot))]

trusted_prot_clean_data <- data.frame(trusted_prot$Accession, trusted_prot_clean)

colnames(trusted_prot_clean_data)[1]
colnames(trusted_prot_clean_data)[1] <- "Accession"

rownames(trusted_prot_clean_data) <- trusted_prot_clean_data$Accession
trusted_prot_clean_data$Accession <- NULL

#########################################
#NA替换为比较组差异蛋白表达最小值

trusted_prot_clean_data_temp <- na.omit(trusted_prot_clean_data)
data_min <- apply(trusted_prot_clean_data_temp, 2, min)
data_min_numeric <- as.numeric(data_min)
data_min_numeric1 <- data_min_numeric[!is.na(data_min_numeric)]
data_min_value <- min(data_min_numeric1)
trusted_prot_clean_data[is.na(trusted_prot_clean_data)] <- data_min_value

############################################
#修改样品名称为原始的名称：主要是连字符"-"在数据处理过程被设置为"."
colnames(trusted_prot_clean_data) <- colnames(trusted_prot_clean)

trusted_prot_clean_t <- t(trusted_prot_clean_data)
trusted_prot_clean_t_dist <- dist(trusted_prot_clean_t)
trusted_prot_clean_t_dist_mat <- as.matrix(trusted_prot_clean_t_dist)

#########################################
#
sampletree <- hclust(trusted_prot_clean_t_dist, method = "average")

plot(sampletree, main = NULL, sub="", xlab="", col = "Blue")

######################################
# Save as PDF
pdf(file = "SamplesTree.pdf", width = 7.65, height = 7.05)
plot(sampletree, main = NULL, sub="", xlab="", col = "Blue")
dev.off()

# Save as PNG
png(file = "SamplesTree.png", width = 734, height = 603)
plot(sampletree, main = NULL, sub="", xlab="", col = "Blue")
dev.off()





