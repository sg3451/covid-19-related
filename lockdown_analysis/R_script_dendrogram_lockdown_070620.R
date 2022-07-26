getwd()
setwd('C:/Users/gmssujo/Dropbox/covid_analysis/lockdown_analysis_June30data_070520/lockdown')
dir()

install.packages("ape")
library(ape)
library(RColorBrewer)

data = read.delim("input_dendrogram_lockdown_updatedNZLetc_122120.txt", sep='\t', header=T)
dim(data)
str(data)
rnames<- data[,2]  # assign labels in column 1 to "rnames"
# rnames = data[15:30,1] if only rows 15-30 of the data is useful 
mat_data<- data.matrix(data[,3:ncol(data)])  
# mat_data = data.matrix(data [15:30,2:7]) if only requiring a #matrix of rows 15-30 andcolumns 2-7 of the data 
rownames(mat_data) = rnames                  # assign row names
dim(mat_data)

dist <- dist(mat_data, diag=TRUE)

# Hierarchical Clustering with hclust
hc <- hclust(dist)

# Plot the result and save as file
png("dendrogram_lockdown_clustered_nocolor_122120.png", width=17, height=8, units="in", res=600)
pdf("dendrogram_lockdown_clustered_nocolor_122120.pdf", width=17, height=8)
plot(hc)
dev.off()

#create more sophisticated dendrograms from the ape package
png("dendrogram_regular_lockdown_122120.png", height=15, width=8, units="in", res=600)
pdf("dendrogram_regular_lockdown_122120.pdf", height=15, width=8)
colors = c("red", "blue", "limegreen", "magenta", "purple","black","skyblue")
plot(as.phylo(hc), cex = 0.6, label.offset = 0.05, tip.color = colors[clus7])
dev.off()

#cladogram
plot(as.phylo(hc), type = "cladogram", cex = 0.6, 
     label.offset = 0.05)

#unrooted tree
plot(as.phylo(hc), type = "unrooted", cex = 0.6,
     no.margin = TRUE)

#fan
plot(as.phylo(hc), type = "fan", cex=0.6, label.offset = 0.05)

#radial
plot(as.phylo(hc), type = "radial",cex=0.6, label.offset = 0.05)

# Cut the dendrogram into 6 clusters and color
png("dendrogram_circular_lockdown_070620.png", width=10, height=10, units="in", res=600)
pdf("dendrogram_circular_lockdown_070620.pdf", width=10, height=10)
colors = c("red", "blue", "limegreen", "magenta", "purple","black","skyblue")
#colors = brewer.pal(6, "Set1")
clus7 = cutree(hc, 7)
plot(as.phylo(hc), type = "fan", tip.color = colors[clus6],
     label.offset = 0.05, cex = 0.9, font = 2)
dev.off()

#store dendrogram as an object
dhc <- as.dendrogram(hc)

# set the margin
par(mar=c(4,4,2,2))

# Plot the Second group
plot(dhc[[1]] , main= "zoom on a part of the dendrogram")


#=======================================================================
#plot heatmap
install.packages("pheatmap")
library(pheatmap)

colors = colorRampPalette(c("skyblue","ivory", "coral"))(3)

colorbreaks = c(seq(-1,-0.01, length=2),
                seq(-0.009,0.009, length=2),
                seq(0.01,1, length=2))

pdf("phtmp3_lockdown_effects_122220.pdf",width = 6,height = 20)
png("phtmp3_lockdown_effects_122220.png", width = 6, height = 20, units='in',res = 300,pointsize = 6)

#if using color break, bk, then add breaks=bk to pheatmap command; scale options are 'row','column','none'

pheatmap(mat_data,show_rownames=T,scale="none", col=colors, na_col="aliceblue",cellwidth=10, cellheight = 11,cutree_rows=6,
         border_color = "grey60", cluster_cols=T,cluster_rows=T,treeheight_col=0, fontsize_col=10, fontsize_row=8, angle_col=90)

dev.off()

