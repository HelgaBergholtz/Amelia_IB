#################################################################
########## GSEA correlation B-cells vs gene expression ##########
#################################################################

### AIM: understand which biological processes are correlated with B-cell abundance in DCIS

# first had to install BiocManager
if (!requireNamespace("BiocManager", force = TRUE))
  install.packages("BiocManager")
BiocManager::install("fgsea")

# load packages

library(ggplot2)
library(ggpubr)
library(fgsea) # you will need to install this package




# load the results of the correlation analyses (B-cell vs gene expression)

load("correlation_bcell_gene_expression.Rdata", verbose = T)

### TASK (if not done previously): MAKE A PLOT OF THE CORRELATION COEFFICIENT ON THE X-AXIS AND AND P-VALUE (ON A LOG10 SCALE) ON THE Y-AXIS! Find out how you can includ the gene name for the top and bottom 5 genes 


# trying smth new
cor_results <- cor_results[order(abs(cor_results$cor.coeff)), ]
top_genes <- cor_results$GeneName[(nrow(cor_results) - 2):nrow(cor_results)]

cor_results <- cor_results[order(abs(cor_results$cor.coeff)), ]
bottom_genes <- cor_results$GeneName[1:3]

ggplot(cor_results, aes(cor.coeff, -log10(FDR))) +
  geom_point() + # Add points
  geom_text(data = subset(cor_results, GeneName %in% c(top_genes, bottom_genes)), aes(label = GeneName), vjust = -0.5, hjust = 0.5, size = 3) + # Add text annotations for both top and bottom genes
  scale_x_continuous(name = "Correlation Coefficient") + # X-axis label
  scale_y_continuous(name = "-log10(FDR)") # Y-axis label
  

# make vector to be used for GSEA
cor_results$gsea_vector = ifelse(cor_results$cor.coeff < 0, -1, 1) * (1- cor_results$P.value) # we will use the p-value to sort the gene list for gene set enrichment analyses. However, it is relevant if the correlation is positive or negative. Make sure you understand how this value is calculated.


gsea_vector = cor_results$gsea_vector ; names(gsea_vector) =  cor_results$GeneName 

gsea_vector = sort(gsea_vector, decreasing = T)


# load gene set - it is possible to download more .gmt-files online  - I will :)
GO_file = "h.all.v2022.1.Hs.symbols.gmt" # hallmark gene set from https://www.gsea-msigdb.org/gsea/msigdb/

myGO = fgsea::gmtPathways(GO_file)

set.seed(1234) # the results will be slightly different every time you run the algorithm. Google set.seed to understand what is does

?fgsea # read the help file to understand the parameters
?fgseaMultilevel

fgRes <- fgsea(pathways = myGO,
               stats = gsea_vector,
               minSize=15, ## minimum gene set size
               maxSize=400 ## maximum gene set size
) 

fgRes$Enrichment = ifelse(fgRes$NES > 0, "Up-regulated", "Down-regulated")
install.packages("data.table")
install.packages("openxlsx")
library(openxlsx)
write.xlsx(fgRes, "fgRes.xlsx")

# TASK: understand what the columns in fgRes means!
# TASK: export the fgRes table. This can be included as a table in the report.  

# However, it is also nice to plot the results

filtRes2 = fgRes[fgRes$padj<0.1,]

filtRes2$pathway = gsub("HALLMARK_", "", filtRes2$pathway)
filtRes2$pathway = gsub("_", " ", filtRes2$pathway)

# two alternatives: choose the one you like best (or make your own - but)

# 
ggplot(filtRes2, aes(reorder(pathway, NES), NES)) +
  geom_point( aes(size = padj), shape=21, fill = "grey") +
  geom_point( aes(fill = Enrichment, size = padj), shape=21, data = filtRes2[filtRes2$padj<0.1,]) +
  scale_fill_manual(values = c("Down-regulated"  =  "orange", "Up-regulated" = "red")) +
  scale_size_continuous(range = c(10,5), breaks = c(0.05,0.1,0.2)) +
  geom_hline(yintercept = 0) +
  coord_flip() +
  labs(x="Pathway", y="Normalized Enrichment Score") +
  theme_bw()

color = ifelse(filtRes2$NES > 0, "#9DC183", "#82CAFF")

#
ggplot(filtRes2, aes(x = reorder(pathway, NES), y = NES)) +
  geom_bar(stat = "identity", fill = color, color = "black",size = 0.25)+ 
  coord_flip()+
  labs(x="Pathway", y="Normalized Enrichment Score") +
  theme_bw()+
  theme(axis.text = element_text(size = 10), axis.title = element_text(size = 14))

# 
ggplot(filtRes2, aes(x = pathway, y = -log10(padj), fill = Enrichment)) +
  geom_bar(stat = "identity") +
  labs(x = "Pathway", y = "-log10(Adjusted p-value)", title = "Enriched Pathways") +
  theme(axis.text.x = element_text(angle = 75, hjust = 1)) +
  scale_fill_manual(values = c("Up-regulated" = "#ED2939", "Down-regulated" = "orange"))

# 
ggplot(filtRes2, aes(x = NES, y = -log10(padj), color = Enrichment)) +
  geom_point(size = 2) +
  labs(x = "Normalized Enrichment Score (NES)", y = "-log10(Adjusted p-value)",
       title = "Volcano Plot of GSEA Results") +
  scale_color_manual(values = c("Up-regulated" = "red", "Down-regulated" = "orange")) +
  theme_minimal()

# maybe use a heatmap?

color_palette <- c("Up-regulated" = "red", "Down-regulated" = "orange")
ggplot(filtRes2, aes(x = 1, y = pathway, fill = NES)) +
  geom_tile() +
  scale_fill_gradientn(colors = color_palette, name = "NES") +
  theme_minimal() +
  labs(x = NULL, y = NULL, title = "Heatmap of GSEA Results")

# TASK: interpret the plot :-)

