####################
library(openxlsx)
library(ComplexHeatmap)
library(circlize)
###################
sample_info_1 <- read.xlsx("sample_information.xlsx", sheet = "样品信息", startRow = 1, colNames = TRUE, rowNames = FALSE, detectDates = FALSE)
sample_info_2 <- read.xlsx("sample_information.xlsx", sheet = "比较组信息", startRow = 1, colNames = TRUE, rowNames = FALSE, detectDates = FALSE)


#####################
rownames(sample_info_1) <- sample_info_1$作图编号
samp_numb <- sample_info_1$样品编号
compa_groups <- 1:length(sample_info_2$比较组)
#######################

for (iii in compa_groups) {
    print(gsub("/","_",sample_info_2[iii,1]))
    new_compa_groups <- gsub("/","_",sample_info_2[iii,1])
    print(new_compa_groups)
    compa_groups_data <- read.xlsx("差异蛋白筛选结果.xlsx", sheet = new_compa_groups, startRow = 1, colNames = TRUE, rowNames = FALSE, detectDates = FALSE)
    dim(compa_groups_data)
    class(compa_groups_data)
    rownames(compa_groups_data) <- compa_groups_data$Accession
}

compa_groups_data <- read.xlsx("差异蛋白筛选结果.xlsx", sheet = new_compa_groups, startRow = 1, colNames = TRUE, rowNames = FALSE, detectDates = FALSE)
samp_names <- sample_info_1$作图编号
sm <- match(samp_names, colnames(compa_groups_data))
sm <- as.numeric(sm[!is.na(sm)])
samp_names <- sample_info_1$作图编号
sm <- match(samp_names, colnames(compa_groups_data))
sm <- as.numeric(sm[!is.na(sm)]) #@ 去掉NA值
compa_groups_clean_data <- compa_groups_data[,sm]


#########################

compa_groups_clean_data_temp <- na.omit(compa_groups_clean_data)
data_min <- apply(compa_groups_clean_data_temp, 2, min)
data_min_numeric <- as.numeric(data_min)
data_min_numeric1 <- data_min_numeric[!is.na(data_min_numeric)]
data_min_value <- min(data_min_numeric1)
compa_groups_clean_data[is.na(compa_groups_clean_data)] <- data_min_value

##################################


data_new <- t(apply(compa_groups_clean_data, 1, scale))

col_space_30 <- c("orange", "green", "magenta","red","pink","blue","darkgreen","yellow", "brown", "turquoise","cyan","gold","aquamarine","tomato","tan","lawngreen","cornflowerblue", "hotpink","firebrick","darkviolet","orangered", "orchid","darkmagenta","snow4","slateblue", "rosybrown","deepskyblue","lightseagreen","darkviolet","azure4")

sn <- NCOL(data_new)
sacol <- col_space_30[1:sn]


##################################
compa_groups_clean_data_samplenames <- as.data.frame(colnames(compa_groups_clean_data))
colnames(compa_groups_clean_data_samplenames) <- "Sample_names"
print(compa_groups_clean_data_samplenames)
group_names <- merge.data.frame(compa_groups_clean_data_samplenames, sample_info_1, by.x = "Sample_names", by.y = "作图编号", all.x = T)
group_name1 <- unique(group_names$样品分组)[1]
group_name2 <- unique(group_names$样品分组)[2]
group_infor <- as.data.frame(plyr::count(group_names$样品分组))
group1_rep_num <- group_infor[which(group_infor == group_name1),]$freq
group2_rep_num <- group_infor[which(group_infor == group_name2),]$freq
new_col <- list(groups = c(group_name1 = "sienna1", group_name2 = "lightseagreen"))
names(new_col$groups) <- c(group_name1, group_name2)


###################################
violin_height <- log(NROW(compa_groups_clean_data) + 1)
data_new <- as.matrix(data_new)
colnames(data_new) <- colnames(compa_groups_clean_data)

ha = HeatmapAnnotation("Violin" = anno_density(data_new,
                                               height = unit(violin_height, "cm"),
                                               type = "violin",
                                               gp = gpar(fill = sacol)),
                       border = F,
                       groups = c(rep(group_name1, group1_rep_num ), rep(group_name2, group2_rep_num)),
                       col = new_col)

#####################################

pdf("./complexheatmap.pdf", width = 7.5, height = 12)
draw(Heatmap(data_new,
             col = colorRamp2(c(-2, 0, 2), c("navy", "white", "firebrick")),
             name = " ", # 表达图例的标题为空
             top_annotation = ha,
             na_col = "grey",
             row_names_gp = gpar(fontsize = 4),
             column_names_gp = gpar(fontsize = 14),
             cluster_columns = T))
dev.off()

png("./complexheatmap.png", width = 750, height = 1200)
draw(Heatmap(data_new,
             col = colorRamp2(c(-2, 0, 2), c("navy", "white", "firebrick")),
             # col <- colorRampPalette(c("blue", "white", "red"))(1000),
             # name = "expression", # 表达图例的标题为：expression
             name = " ", # 表达图例的标题为空
             top_annotation = ha,
             na_col = "grey",
             row_names_gp = gpar(fontsize = 6),
             column_names_gp = gpar(fontsize = 18),
             # column_title_rot = 90,
             # column_title_side = "top",
             cluster_columns = T))
dev.off()

###########################






