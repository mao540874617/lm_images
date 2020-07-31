#加载必须的包
library(dplyr,quietly = T)
library(stringr,quietly = T)
library(ggplot2,quietly = T)
library(ggpubr,quietly = T)
library(gtable,quietly = T)
library(grid,quietly = T)
library(Hmisc,quietly = T)#top15

setwd("/outport")

###########################################################################
#top30
################################
limit70 <- function(s) {
    k <- as.character(s)
    if(str_length(s)>70){k <- paste0(substr(k,1,67),"..." )}
    return(k)
}

################################
limit <- function(s,n) {
    k <- as.character(s)
    if(str_length(s)>n){k <- paste0(substr(k,1,n-3),"..." )}
    return(k)
}

##############################
ggplot2.two_y_axis <- function(g1, g2) {
    g1 <- ggplotGrob(g1)
    g2 <- ggplotGrob(g2)
    pp <- c(subset(g1$layout, name == 'panel', se = t:r))
    
    g1 <- gtable_add_grob(g1, g2$grobs[[which(g2$layout$name == 'panel')]], pp$t, pp$l, pp$b, pp$l)
    
    hinvert_title_grob <- function(grob){
        widths <- grob$widths
        grob$widths[1] <- widths[3]
        grob$widths[3] <- widths[1]
        grob$vp[[1]]$layout$widths[1] <- widths[3]
        grob$vp[[1]]$layout$widths[3] <- widths[1]
        
        # Fix the justification
        grob$children[[1]]$hjust <- 1 - grob$children[[1]]$hjust
        grob$children[[1]]$vjust <- 1 - grob$children[[1]]$vjust
        grob$children[[1]]$x <- unit(1, 'npc') - grob$children[[1]]$x
        grob
    }
    
    # Get the y axis title from g2
    index <- which(g2$layout$name == 'ylab-l') # Which grob contains the y axis title?
    ylab <- g2$grobs[[index]]        # Extract that grob
    ylab <- hinvert_title_grob(ylab)     # Swap margins and fix justifications
    
    # Put the transformed label on the right side of g1
    g1 <- gtable_add_cols(g1, g2$widths[g2$layout[index, ]$l], pp$r)
    g1 <- gtable_add_grob(g1, ylab, pp$t, pp$r + 1, pp$b, pp$r + 1, clip = 'off', name = 'ylab-r')
    
    # Get the y axis from g2 (axis line, tick marks, and tick mark labels)
    index <- which(g2$layout$name == 'axis-l')  # Which grob
    yaxis <- g2$grobs[[index]]          # Extract the grob
    
    yaxis$children[[1]]$x <- unit.c(unit(0, 'npc'), unit(0, 'npc'))
    
    # Second, swap tick marks and tick mark labels
    ticks <- yaxis$children[[2]]
    ticks$widths <- rev(ticks$widths)
    ticks$grobs <- rev(ticks$grobs)
    
    # Third, move the tick marks
    ticks$grobs[[1]]$x <- ticks$grobs[[1]]$x - unit(1, 'npc') + unit(3, 'pt')
    
    # Fourth, swap margins and fix justifications for the tick mark labels
    ticks$grobs[[2]] <- hinvert_title_grob(ticks$grobs[[2]])
    
    # Fifth, put ticks back into yaxis
    yaxis$children[[2]] <- ticks
    
    # Put the transformed yaxis on the right side of g1
    g1 <- gtable_add_cols(g1, g2$widths[g2$layout[index, ]$l], pp$r)
    g1 <- gtable_add_grob(g1, yaxis, pp$t, pp$r + 1, pp$b, pp$r + 1, clip = 'off', name = 'axis-r')
    grid.newpage()
    grid.draw(g1)
}
#############################
g1 <- function(d_1,groupname,type){
    g1<-ggplot(data=go30, aes(x=term, y=ListHits/ListTotal, width=0.6, fill=category, space=0.6,cex.main=3,cex.lab=2)) +
        geom_bar(stat="identity",position=position_dodge(0.7),width=0.5) +
        labs(x="", y="Percent of proteins", title = paste0(groupname,"(",type,"): ","Top 30 GO Terms")) +
        theme_bw() + scale_fill_manual(values=unique(go30$color)) +
        theme(axis.text.x=element_text(angle = 45, hjust=1, size=14,color=go30$color))+
        theme(legend.position=c(-0.12,0.8), legend.key.width=unit(1, "lines"))+
        theme(legend.text=element_text(size=14))+
        theme(text=element_text(size=14)) +
        theme(plot.title = element_text(hjust = 0.5, vjust=4, size=18))+
        theme(plot.margin=unit(c(2,2,2,18), "lines"))+
        theme(panel.grid =element_blank()) +
        # geom_hline(yintercept=-log(0.05,10) ,colour="#990000", linetype="dashed") +
        theme(panel.border = element_blank(),panel.grid.major =element_blank(), panel.grid.minor = element_blank(),panel.background = element_blank(),axis.line = element_line(colour = "black")) +
        border()
    return(g1)
}

##############################
g2 <- function(d_1,groupname,type){
    g2<-ggplot(data=go30, aes(x=term, y=ListHits, width=0.6, fill=category, space=0.6,cex.main=3,cex.lab=2)) +
        geom_bar(stat="identity",position=position_dodge(0.7),width=0.5) +
        labs(x="", y="Number of proteins", title = paste0(groupname,"(",type,"): ","Top 30 GO Terms")) +
        theme_bw() + scale_fill_manual(values=unique(go30$color)) +
        theme(axis.text.x=element_text(angle = 45, hjust=1, size=14,color=go30$color))+
        theme(legend.position=c(-0.12,0.8), legend.key.width=unit(1, "lines"))+
        theme(legend.text=element_text(size=14))+
        theme(text=element_text(size=14)) +
        theme(plot.title = element_text(hjust = 0.5, vjust=4, size=18))+
        theme(plot.margin=unit(c(2,2,2,18), "lines"))+
        theme(panel.grid =element_blank()) +
        # geom_hline(yintercept=-log(0.05,10) ,colour="#990000", linetype="dashed") +
        theme(panel.border = element_blank(),panel.grid.major =element_blank(), panel.grid.minor = element_blank(),panel.background = element_blank(),axis.line = element_line(colour = "black")) +
        border()
    return(g2)
}
##############################
#获得文件名
con <- "CNV_choroid_control_choroid-diff-protein.xls" 
    
con

##############################
#
tre <- strsplit(con[1],"-diff-protein.xls")[[1]]

dir(path =paste0("enrich/GO_enrichment/",tre), pattern = "enrichment-go-*")

tud <- dir(path = "./", pattern = "enrichment-go-*")
tud
tudd <- strsplit(strsplit(tud,"-")[[1]][length(strsplit(tud,"-")[[1]])],".xls")[[1]]
tudd

got<-read.delim("D:/脚本/LM2020-11815-mmu/1.dep/enrich/GO_enrichment/CNV_choroid_control_choroid/enrichment-go-CNV_choroid_control_choroid-Down.xls", sep="\t", header=T, quote="")

############################
#
go30<-rbind(filter(got,ListHits!=1,category=="biological_process")[1:10,],
            filter(got,ListHits!=1,category=="cellular_component")[1:10,],
            filter(got,ListHits!=1,category=="molecular_function")[1:10,])

go30<-na.omit(go30)

goterm <- apply(go30["term"], 1, limit70)

go30["term"]<-goterm
go15<-rbind(filter(go30,category=="biological_process")[1:5,],
            filter(go30,category=="cellular_component")[1:5,],
            filter(go30,category=="molecular_function")[1:5,])
na.omit(go15)->go15
go30<-go30 %>% arrange(category, desc(ListHits))

go30[which(go30$category=="biological_process"), "color"] <- "#4DAF4A"
go30[which(go30$category=="cellular_component"), "color"] <- "#377EB8"
go30[which(go30$category=="molecular_function"), "color"] <- "#E41A1C"



go30$term <- factor(go30$term, levels=go30$term)

p1<-g1(go30,tre,tudd)
p2<-g2(go30,tre,tudd)


#############################
#
ggsave("./top30.pdf", ggplot2.two_y_axis(p1, p2),onefile = FALSE, height=10, width=18)

ggsave("./top30.png", ggplot2.two_y_axis(p1, p2),type="cairo-png", height=10, width=18)

#################################################################################################
#top15
limit70 <- function(s) {
    k <- as.character(s)
    if(str_length(s)>70){k <- paste0(substr(k,1,67),"..." )}
    return(k)
}
################################
limit <- function(s,n) {
    k <- as.character(s)
    if(str_length(s)>n){k <- paste0(substr(k,1,n-3),"..." )}
    return(k)
}
##############################
ggplot2.two_y_axis <- function(g1, g2) {
    g1 <- ggplotGrob(g1)
    g2 <- ggplotGrob(g2)
    pp <- c(subset(g1$layout, name == 'panel', se = t:r))
    
    g1 <- gtable_add_grob(g1, g2$grobs[[which(g2$layout$name == 'panel')]], pp$t, pp$l, pp$b, pp$l)
    
    hinvert_title_grob <- function(grob){
        widths <- grob$widths
        grob$widths[1] <- widths[3]
        grob$widths[3] <- widths[1]
        grob$vp[[1]]$layout$widths[1] <- widths[3]
        grob$vp[[1]]$layout$widths[3] <- widths[1]
        
        # Fix the justification
        grob$children[[1]]$hjust <- 1 - grob$children[[1]]$hjust
        grob$children[[1]]$vjust <- 1 - grob$children[[1]]$vjust
        grob$children[[1]]$x <- unit(1, 'npc') - grob$children[[1]]$x
        grob
    }
    
    # Get the y axis title from g2
    index <- which(g2$layout$name == 'ylab-l') # Which grob contains the y axis title?
    ylab <- g2$grobs[[index]]        # Extract that grob
    ylab <- hinvert_title_grob(ylab)     # Swap margins and fix justifications
    
    # Put the transformed label on the right side of g1
    g1 <- gtable_add_cols(g1, g2$widths[g2$layout[index, ]$l], pp$r)
    g1 <- gtable_add_grob(g1, ylab, pp$t, pp$r + 1, pp$b, pp$r + 1, clip = 'off', name = 'ylab-r')
    
    # Get the y axis from g2 (axis line, tick marks, and tick mark labels)
    index <- which(g2$layout$name == 'axis-l')  # Which grob
    yaxis <- g2$grobs[[index]]          # Extract the grob
    
    yaxis$children[[1]]$x <- unit.c(unit(0, 'npc'), unit(0, 'npc'))
    
    # Second, swap tick marks and tick mark labels
    ticks <- yaxis$children[[2]]
    ticks$widths <- rev(ticks$widths)
    ticks$grobs <- rev(ticks$grobs)
    
    # Third, move the tick marks
    ticks$grobs[[1]]$x <- ticks$grobs[[1]]$x - unit(1, 'npc') + unit(3, 'pt')
    
    # Fourth, swap margins and fix justifications for the tick mark labels
    ticks$grobs[[2]] <- hinvert_title_grob(ticks$grobs[[2]])
    
    # Fifth, put ticks back into yaxis
    yaxis$children[[2]] <- ticks
    
    # Put the transformed yaxis on the right side of g1
    g1 <- gtable_add_cols(g1, g2$widths[g2$layout[index, ]$l], pp$r)
    g1 <- gtable_add_grob(g1, yaxis, pp$t, pp$r + 1, pp$b, pp$r + 1, clip = 'off', name = 'axis-r')
    grid.newpage()
    grid.draw(g1)
}
#############################
g1 <- function(d_1,groupname,type){
    g1<-ggplot(data=go30, aes(x=term, y=ListHits/ListTotal, width=0.6, fill=category, space=0.6,cex.main=3,cex.lab=2)) +
        geom_bar(stat="identity",position=position_dodge(0.7),width=0.5) +
        labs(x="", y="Percent of proteins", title = paste0(groupname,"(",type,"): ","Top 30 GO Terms")) +
        theme_bw() + scale_fill_manual(values=unique(go30$color)) +
        theme(axis.text.x=element_text(angle = 45, hjust=1, size=14,color=go30$color))+
        theme(legend.position=c(-0.12,0.8), legend.key.width=unit(1, "lines"))+
        theme(legend.text=element_text(size=14))+
        theme(text=element_text(size=14)) +
        theme(plot.title = element_text(hjust = 0.5, vjust=4, size=18))+
        theme(plot.margin=unit(c(2,2,2,18), "lines"))+
        theme(panel.grid =element_blank()) +
        # geom_hline(yintercept=-log(0.05,10) ,colour="#990000", linetype="dashed") +
        theme(panel.border = element_blank(),panel.grid.major =element_blank(), panel.grid.minor = element_blank(),panel.background = element_blank(),axis.line = element_line(colour = "black")) +
        border()
    return(g1)
}
##############################
g2 <- function(d_1,groupname,type){
    g2<-ggplot(data=go30, aes(x=term, y=ListHits, width=0.6, fill=category, space=0.6,cex.main=3,cex.lab=2)) +
        geom_bar(stat="identity",position=position_dodge(0.7),width=0.5) +
        labs(x="", y="Number of proteins", title = paste0(groupname,"(",type,"): ","Top 30 GO Terms")) +
        theme_bw() + scale_fill_manual(values=unique(go30$color)) +
        theme(axis.text.x=element_text(angle = 45, hjust=1, size=14,color=go30$color))+
        theme(legend.position=c(-0.12,0.8), legend.key.width=unit(1, "lines"))+
        theme(legend.text=element_text(size=14))+
        theme(text=element_text(size=14)) +
        theme(plot.title = element_text(hjust = 0.5, vjust=4, size=18))+
        theme(plot.margin=unit(c(2,2,2,18), "lines"))+
        theme(panel.grid =element_blank()) +
        # geom_hline(yintercept=-log(0.05,10) ,colour="#990000", linetype="dashed") +
        theme(panel.border = element_blank(),panel.grid.major =element_blank(), panel.grid.minor = element_blank(),panel.background = element_blank(),axis.line = element_line(colour = "black")) +
        border()
    return(g2)
}
##############################

con <- "CNV_choroid_control_choroid-diff-protein.xls"

tre <- strsplit(con[1],"-diff-protein.xls")[[1]]

dir("./", pattern = "enrichment-go-*")

tud <- dir(path ="./", pattern = "enrichment-go-*")

tudd <- strsplit(strsplit(tud,"-")[[1]][length(strsplit(tud,"-")[[1]])],".xls")[[1]]


got<-read.delim("./enrichment-go-CNV_choroid_control_choroid-Down.xls", sep="\t", header=T, quote="")

#######################################################################
go30<-rbind(filter(got,ListHits!=1,category=="biological_process")[1:10,],
            filter(got,ListHits!=1,category=="cellular_component")[1:10,],
            filter(got,ListHits!=1,category=="molecular_function")[1:10,])

go30<-na.omit(go30)

goterm <- apply(go30["term"], 1, limit70)

go30["term"]<-goterm
go15<-rbind(filter(go30,category=="biological_process")[1:5,],
            filter(go30,category=="cellular_component")[1:5,],
            filter(go30,category=="molecular_function")[1:5,])
na.omit(go15)->go15

go15 <- go15[order(go15$term,decreasing=TRUE),]
go15$category<-str_replace_all(go15$category,c("biological_process"="BP","cellular_component"="CC","molecular_function"="MF"))
go15$term<-capitalize(go15$term)
go15$term <- factor(go15$term, levels=go15$term)
##############################################################
#绘图部分
p<-ggplot(go15,aes(Enrichment_score,term))+
    geom_point()+
    facet_grid(category ~ .,scales="free")+
    geom_point(aes(size=ListHits,color=pval)) +
    guides(size = guide_legend(order=1))+
    labs(x = "Enrichment_score", y = "",title = paste0(tre," (",tudd,")")) +
    theme(plot.title = element_text(size=14, family = "ArialMT")) +
    theme_bw()+
    theme(strip.text=element_text(size=rel(1.2)),strip.background=element_rect(fill="white"))+
    scale_colour_gradient(low="red",high="orange")+
    theme(axis.text.x = element_text(size=14),axis.text.y = element_text(size = 14),title = element_text(size=14))+
    theme(panel.background = element_rect(fill='honeydew3', colour='white'))

ggsave("./top15.pdf", height=10, width=10, plot=p)
ggsave("./top15.png", type="cairo-png", height=10, width=10, plot=p)
########################################################################################################












