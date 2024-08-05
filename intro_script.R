
##################################################################
########## Introduction to the UMO gene expression data ##########
##################################################################



###############
## load data ##
###############

#install.packages("ggplot2")
#install.packages("ggpubr")

library(ggplot2)
library(ggpubr)

setwd("D:/Students/Amelia/") # set working directory (make a directory for data analyses and insert the path here). The Rdata-files must be in that folder. OBS. MAC-syntax for file path may be different than for PC. Need

load("expression_data_umo_dcis.RData") # a data frame with the expression data
load("annotations_umo_dcis.Rdata") # a data frame with information about the samples (metadata)




##########################
## Get to know the data ##
##########################


# What are the names of the two objects you have loaded

# inspect the objects 

View(annot_umo_dcis)
View(expr_umo_dcis)


# What are the dimensions of the two data frames

dim(annot_umo_dcis)
dim(expr_umo_dcis)


# what do the row names of expr_umo_dcis show?

# what do the column names of expr_umo_dcis show?

# what sample metadata do you have?

# how are the to two data frames connected?

# we are going to use the information from the two dataframes together, it is therefore important to ensure that they are in the same order. We can compare two vectors using "=="

colnames(expr_umo_dcis) == annot_umo_dcis$SampleID 


# how many samples do you have of the different subtypes?

table(annot_umo_dcis$Subtype)


# what is the age range in your dataset?

range(annot_umo_dcis$Age)


# what is the mean and median age?

mean(annot_umo_dcis$Age)
median(annot_umo_dcis$Age)


# what is the range, mean and median of b-cell abundance 




#####################################
## Some more challenging exercizes ##
#####################################


# make a boxplot showing subtype vs b-cells. Find out what the box and the whiskers mean (google). Try to understand the ggplot syntax

ggplot(annot_umo_dcis, aes(Subtype, B.cells))+
  geom_boxplot()+ # making the boxplot
  geom_point()+ # making the points
  theme_classic2() # making the plot look nicer




# calculate and visualize correlation between B cells and macrophages

?cor() # access to help file on the function cor( )


cor(annot_umo_dcis$B.cells, annot_umo_dcis$Macrophages)
cor.test(annot_umo_dcis$B.cells, annot_umo_dcis$Macrophages) # compare results with line above


# code for plotting scatterplots
ggplot(annot_umo_dcis, aes(B.cells, Macrophages))+ 
  geom_point(size = 3, aes(color = Subtype))+ # make points with a larger size and colored by subtype
  scale_colour_manual(values = c("Basal" = "red", "LumA" = "darkblue", "LumB" = "skyblue",  "Her2" = "hotpink"))+
  stat_cor()+
  theme_classic()

# what statistics are shown in the plot? What does R mean. What does p mean?
# find some other variables to plot 
# are you able to change the colors?
# are you able to change what you color for? try to color for e.g. age or size.
# what happens to the plot if you change the last line to theme_gray() ?





