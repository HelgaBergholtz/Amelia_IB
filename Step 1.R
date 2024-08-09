# first try, using step 0 method 

markers=c("CD19", "CD38", "MS4A1")
markersdata=expr_umo_dcis[markers,]
plotdata=cbind(annot_umo_dcis, t(markersdata))



library(ggplot2)


# install.packages("ggplot2")
library(ggplot2)
library(ggpubr)


 


# second try, using the exisiting script, CD19
cor(annot_umo_dcis$B.cells, t(expr_umo_dcis["CD19",]))
cor.test(annot_umo_dcis$B.cells, t(expr_umo_dcis["CD19",]))
plotdata = cbind(annot_umo_dcis, t(expr_umo_dcis["CD19", ]))

ggplot(plotdata, aes(B.cells, CD19))+
  geom_point()+
  stat_cor(size = 5)+
  theme_classic(base_size = 15)

# CD38
cor(annot_umo_dcis$B.cells, t(expr_umo_dcis["CD38",]))
cor.test(annot_umo_dcis$B.cells, t(expr_umo_dcis["CD38",]))
plotdata = cbind(annot_umo_dcis, t(expr_umo_dcis["CD38", ]))

ggplot(plotdata, aes(B.cells, CD38))+
  geom_point()+ 
  stat_cor(size = 5)+
  theme_classic(base_size = 15)

# MS4A1
cor(annot_umo_dcis$B.cells, t(expr_umo_dcis["MS4A1",]))
cor.test(annot_umo_dcis$B.cells, t(expr_umo_dcis["MS4A1",]))
plotdata = cbind(annot_umo_dcis, t(expr_umo_dcis["MS4A1", ]))

ggplot(plotdata, aes(B.cells, MS4A1))+
  geom_point()+
  stat_cor(size = 5)+
  theme_classic(base_size = 15) 

# everything together 
plotdata = cbind(annot_umo_dcis, t(expr_umo_dcis[c("CD19", "MS4A1", "CD38"), ]))
colors = c("CD19" = "blue", "MS4A1" = "red", "CD38" = "orange")

ggplot(plotdata, aes(x = B.cells))+
  geom_point(aes(y = CD19, color = "CD19"))+
  geom_point(aes(y = MS4A1, color = "MS4A1"))+
  geom_point(aes(y = CD38, color = "CD38"))+
  ylab("B.cell markers")+
  scale_color_manual(values = colors)+
  theme_classic()


