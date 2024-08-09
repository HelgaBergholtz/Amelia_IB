
##################################################################
########## Introduction to the UMO gene expression data ##########
##################################################################



###############
## load data ##
###############

install.packages("ggplot2")
install.packages("ggpubr")

library(ggplot2)
library(ggpubr)
library(ggrepel)

remove.packages("dplyr")
install.packages("dplyr",dependencies=TRUE)

setwd("/Users/liaros/Documents/EE") # set working directory (make a directory for data analyses and insert the path here). The Rdata-files must be in that folder. OBS. MAC-syntax for file path may be different than for PC. Need
list.files()

load("expression_data_umo_dcis.RData") # a data frame with the expression data
load("annotations_umo_dcis.Rdata") # a data frame with information about the samples (metadata)



##########################
## Get to know the data ##
##########################


# What are the names of the two objects you have loaded - the expression data
# of the DCIS and annotations of them with more information

# inspect the objects 

View(annot_umo_dcis)
View(expr_umo_dcis)


# What are the dimensions of the two data frames
# for the annotation part, it is 57 13. for the expression, it is 19518 , 57. 

dim(annot_umo_dcis)
dim(expr_umo_dcis)


# what do the row names of expr_umo_dcis show? it shows the DCIS sample
# from each person. 

# what do the column names of expr_umo_dcis show? it shows the genes in the DCIS

# what sample metadata do you have? I have 57 samples of DCIS, along with
# different genes present in those samples. 

# how are the to two data frames connected? they give a more wider understanding of what genes are present in the DCIS and that each person has a different amount of genes. 

# we are going to use the information from the two dataframes together, it is therefore important to ensure that they are in the same order. We can compare two vectors using "=="

colnames(expr_umo_dcis) == annot_umo_dcis$SampleID 


# how many samples do you have of the different subtypes? We have 25 samples of Luminal A, 5 samples of Luminal B, 14 samples of HER2 and 13 samples of Basal-like. 

table(annot_umo_dcis$Subtype)


# what is the age range in your dataset? The age range is from 26-82. 

range(annot_umo_dcis$Age)


# what is the mean and median age? the mean age is 55.6 years old, while the median age is 54.8 years old. 

mean(annot_umo_dcis$Age) 
median(annot_umo_dcis$Age)


# what is the range, mean and median of b-cell abundance? the mean: 0.09, the median: 0.056, the range: 0.0000000 0.2777611 (this looks wrong though)

mean(annot_umo_dcis$B.cells)
median(annot_umo_dcis$B.cells)
range(annot_umo_dcis$B.cells)






#####################################
## Some more challenging exercizes ##
#####################################


# make a boxplot showing subtype vs b-cells. Find out what the box and the whiskers mean (google). Try to understand the ggplot syntax

annot_umo_dcis$B.cells <- as.factor(annot_umo_dcis$B.cells)
head(annot_umo_dcis)

ggplot(annot_umo_dcis, aes(x=B.cells, y=Subtype))+
  geom_boxplot(size = 0.5, aes(fill= Subtype),outlier.colour = "hotpink",outlier.shape = 8,outlier.size = 4)+
  scale_fill_manual(values = c("Basal" = "lightgreen", "LumA" = "red", "LumB" = "lightblue",  "Her2" = "lightpink"))+
  geom_point()+
  theme_classic()

coord_flip()

    
geom_boxplot()+ # making the boxplot
geom_point()+ # making the points
theme_classic2() # making the plot look nicer


install.packages("ggplot2")
library(ggplot2)


# calculate and visualize correlation between B cells and macrophages

?cor() # access to help file on the function cor( )

annot_umo_dcis$B.cells <- as.numeric(annot_umo_dcis$B.cells)
annot_umo_dcis$Macrophages <- as.numeric(annot_umo_dcis$Macrophages)


cor(annot_umo_dcis$B.cells, annot_umo_dcis$Macrophages)
cor.test(annot_umo_dcis$B.cells, annot_umo_dcis$Macrophages) # compare results with line above
# the correlation in the first line is -0.6326199, the correlation in the second line is -0.6326199

# code for plotting scatterplots
ggplot(annot_umo_dcis, aes(B.cells, Macrophages))+
  geom_point(size = 3, shape=21, aes(fill = Subtype))+ # make points with a larger size and colored by subtype
  scale_fill_manual(values = c("Basal" = "lightyellow", "LumA" = "lightpink", "LumB" = "red",  "Her2" = "lightgreen"))+
  theme_classic()
  

# what statistics are shown in the plot? What does R mean. What does p mean? you have the correlation between Macrophages and B.cells, including all subtypes present. R value means the Pearson-moment correlatio, which shows the relationship between two variables. P value represents the probability of getting a non-zero correlation coefficient
# find some other variables to plot 
# are you able to change the colors?
# are you able to change what you color for? try to color for e.g. age or size.
# what happens to the plot if you change the last line to theme_gray() ?

genes=c("ESR1", "PGR","ERBB2", "MKI67")
genedata=expr_umo_dcis[genes,]
plotdata=cbind(annot_umo_dcis,t(genedata))


ggplot(plotdata, aes(x=genedata, y=Subtype))
