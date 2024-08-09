
valuegenes=c("TCL1A", "TIMD4", "CD19")
valuegenesdata=expr_umo_dcis[valuegenes,]
plotdata=cbind(annot_umo_dcis, t(valuegenesdata))

plotdata = cbind(annot_umo_dcis, t(expr_umo_dcis[c("TCL1A", "TIMD4", "CD19"), ]))
colors = c("TCL1A" = "blue", "TIMD4" = "red", "CD19" = "orange")

negvaluegenes=c("RGS12","APBB2","CTSF")
negvaluegenesdata=expr_umo_dcis[negvaluegenes,]
plotdata=cbind(annot_umo_dcis, t(negvaluegenesdata))


plotdata = cbind(annot_umo_dcis, t(expr_umo_dcis[c("RGS12", "APBB2", "CTSF"), ]))
colors = c("RGS12" = "blue", "APBB2" = "red", "CTSF" = "orange")

# SCATTERPLOTS FOR ALL OF THEM SEPARATE: LOWEST R-VALUE:

ggplot(plotdata, aes(x = B.cells, y = CTSF))+
  geom_point()+
  ylab("Gene expression CTSF")+
  scale_color_manual(values = colors)+
  stat_cor(method = "pearson", size = 5)+
  geom_smooth(method = "lm", se = FALSE)+
  theme_classic(base_size = 15)

ggplot(plotdata, aes(x = B.cells, y = RGS12))+
  geom_point()+
  ylab("Gene expression RGS12")+
  scale_color_manual(values = colors)+
  stat_cor(method = "pearson", size = 5)+
  geom_smooth(method = "lm", se = FALSE)+
  theme_classic(base_size = 15)

ggplot(plotdata, aes(x = B.cells, y = APBB2))+
  geom_point()+
  ylab("Gene expression APBB2")+
  scale_color_manual(values = colors)+
  stat_cor(method = "pearson", size = 5)+
  geom_smooth(method = "lm", se = FALSE)+
  theme_classic(base_size = 15)

# HIGHEST R-VALUE:
plotdata = cbind(annot_umo_dcis, t(expr_umo_dcis[c("TCL1A", "TIMD4", "CD19"), ]))
colors = c("TCL1A" = "blue", "TIMD4" = "red", "CD19" = "orange")

ggplot(plotdata, aes(x = B.cells, y = CD19))+
  geom_point()+
  ylab("Gene expression CD19")+
  scale_color_manual(values = colors)+
  stat_cor(method = "pearson", size = 5)+
  geom_smooth(method = "lm", se = FALSE)+
  theme_classic(base_size = 15)

ggplot(plotdata, aes(x = B.cells, y = TCL1A))+
  geom_point()+
  ylab("Gene expression TCL1A")+
  scale_color_manual(values = colors)+
  stat_cor(method = "pearson", size = 5)+
  geom_smooth(method = "lm", se = FALSE)+
  theme_classic(base_size = 15)

ggplot(plotdata, aes(x = B.cells, y = TIMD4))+
  geom_point()+
  ylab("Gene expression TIMD4")+
  scale_color_manual(values = colors)+
  stat_cor(method = "pearson", size = 5)+
  geom_smooth(method = "lm", se = FALSE)+
  theme_classic(base_size = 15)


# HIGHEST AND LOWEST CORRELATED GENES WITH FDR (2.5 = HIGHEST AS ITS NEGATIVE)
ggplot(cor_results, aes(cor.coeff,-log10(FDR)))+
  geom_point()

ordered = cor_results[order(cor_results$cor.coeff, decreasing = T),]  
top3 = head(ordered, 3)
bottom3 = tail(ordered,3)
genes_to_be_labeled = c(top3$GeneName, bottom3$GeneName)
ggplot(cor_results, aes(x = cor.coeff, y = -log10(FDR)))+
  geom_hline(yintercept = -log10(0.05), color = "grey")+
  geom_point(color = ifelse(cor_results$GeneName %in% genes_to_be_labeled, "red", "black"), size = ifelse(cor_results$GeneName %in% genes_to_be_labeled, 2, 2))+
  geom_text_repel(aes(label = ifelse(GeneName %in% genes_to_be_labeled, GeneName, "")), max.overlaps = 100, min.segment.length = 0.1, size = 4)+
  theme_pubr()

  
library(ggrepel)

