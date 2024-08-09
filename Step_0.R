
genes=c("ESR1", "PGR","ERBB2", "MKI67")
genedata=expr_umo_dcis[genes,]
plotdata=cbind(annot_umo_dcis,t(genedata))

library(ggplot2)

#creating the actual boxplot :O scary

#Correlation between Subtypes and the ESR1 gene / the estrogen gene
ggplot(plotdata, aes(x= factor(Subtype), y= ESR1))+
  geom_boxplot()+
  labs(title= "ESR1 against DCS Subtypes",
       x = "Subtype",
       y = "ESR1")+
  theme_bw(base_size = 15)+ # theme_pubr()
  theme(plot.title = element_text(hjust = 0.5, size = 20))


#Correlation between Subtypes and the PGR gene / progesterone gene
ggplot(plotdata, aes(x = factor(Subtype), y= PGR))+
  geom_boxplot()+
  labs(title= "PGR against DCIS Subtypes",
       x = "Subtype",
       y = "PGR")+
  theme_bw(base_size = 15)+
  theme(plot.title = element_text(hjust = 0.5, size = 20))


#Correlation between Subtypes and the ERBB2 gene / codes for HER2 gene
ggplot(plotdata, aes(x = factor(Subtype), y= ERBB2))+
  geom_boxplot()+
  labs(title = "ERBB2 against DCIS Subtypes",
       x = "Subtype",
       y = "ERBB2")+
  theme_bw(base_size = 15)+
  theme(plot.title = element_text(hjust = 0.5, size = 20))

#Correlation between Subtypes and the MKI67 gene / cancer marker
ggplot(plotdata, aes(x = factor(Subtype), y = MKI67))+
  geom_boxplot()+
  labs(title = "MKI67 against DCIS Subtypes",
       x = "Subtype",
       y = "MKI67")+
  theme_bw(base_size = 15)+
  theme(plot.title = element_text(hjust = 0.5, size = 20))

#correlation between subtypes and b.cell abundance tihi

ggplot(plotdata, aes(x = factor(B.cells), y = Subtype))+
  geom_boxplot()+
  labs(title = "B.cells against DCIS Subtypes",
       x = "B.cells",
       y = "Subtype")+
  theme_bw(base_size = 15)+
  theme(plot.title = element_text(hjust = 0.5, size = 20))

genes=c("AURKA", "TPX2","BIRC5")
genedata=expr_umo_dcis[genes,]
plotdata=cbind(annot_umo_dcis,t(genedata))

ggplot(plotdata, aes(x= factor(Subtype), y= AURKA))+
  geom_boxplot()+
  labs(title= "AURKA against DCS Subtypes",
       x = "Subtype",
       y = "AURKA")+
  theme_bw(base_size = 15)+ # theme_pubr()
  theme(plot.title = element_text(hjust = 0.5, size = 20))

ggplot(plotdata, aes(x= factor(Subtype), y= TPX2))+
  geom_boxplot()+
  labs(title= "TPX2 against DCS Subtypes",
       x = "Subtype",
       y = "TPX2")+
  theme_bw(base_size = 15)+ # theme_pubr()
  theme(plot.title = element_text(hjust = 0.5, size = 20))

ggplot(plotdata, aes(x= factor(Subtype), y= BIRC5))+
  geom_boxplot()+
  labs(title= "BIRC5 against DCS Subtypes",
       x = "Subtype",
       y = "BIRC5")+
  theme_bw(base_size = 15)+ # theme_pubr()
  theme(plot.title = element_text(hjust = 0.5, size = 20))


