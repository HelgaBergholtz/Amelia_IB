######################################################################
########## Correlating B-cell abundance and gene expression ##########
######################################################################



#### load data 

library(ggplot2)
library(ggpubr)

setwd("/Users/liaros/Documents/EE") # change to your working directory

load("expression_data_umo_dcis.RData") # gene expression data
load("annotations_umo_dcis.Rdata") # metadata




#### correlate b-cell abundance with one gene (her MKI67) to see how it works and inspect the result. 

annot_umo_dcis$B.cells <- as.numeric(annot_umo_dcis$B.cells)

cor(annot_umo_dcis$B.cells, t(expr_umo_dcis["MKI67",])) # the cor() function calculates the correlation coefficient. Change the genename if you want to test another (must be changed in all lines where it is mentioned)

cor.test(annot_umo_dcis$B.cells, t(expr_umo_dcis["MKI67",])) #  cor.test is the function that lets you calculate the correlation between your two vectors, and also calculate if the correlation is significantly different from 0. There is a help file for all functions which you can access by putting a "?" before the function: 
?cor.test

# inspect the results from the cor.test. Can you find the correlation coefficient? and the p-value? 


# plot the data for which the test was calculated: include the correlation coefficient and the p-value in the plot

plotdata = cbind(annot_umo_dcis, t(expr_umo_dcis["MKI67", ]))

ggplot(plotdata, aes(B.cells, MKI67))+
  geom_point()+ # here you can add colors as you wish
  stat_cor()+ # this lines includes statistics. Compare with the statistics you calculated above. 
  theme_classic()

# test run on for loop
for (i in 1:5) {
  print(i)
}

         
         


#### correlating ALL genes with B-cell abundance

# make a result dataframe in which your for-loop puts data into

cor_results = data.frame("GeneName" = rownames(expr_umo_dcis), "cor.coeff" = NA, "P-value" = NA) # this is an empty result matrix that you are going to use to save the result from the for-loop. This data frame has the same order as your gene expression data matrix.



# for-loop. Do a google search and find out what a for-loop is and how it works. 

for (i in 1:nrow(cor_results)){ # the for-loop goes through all i's from 1 to 19518 (all rows in the data)
  print(i) # this just prints the i
  test_result = cor.test(annot_umo_dcis$B.cells, t(expr_umo_dcis[i,])) # this creates an object saving the result of the i'th test
  cor_results$cor.coeff[i] =  test_result$estimate # this saves the correlation coefficient to the cor_result
  cor_results$P.value[i] = test_result$p.value # this saves the p-value to cor_results
  
}
  
  
cor_results$FDR = p.adjust(cor_results$P.value, method = "fdr") # calculates the false discovery rate (FDR). Find out what FDR is.  



# inspect the result matrix. which 3 genes are highest and lowest correlated with b-cell abundance? Find out something about these genes. 
# Make scatterplots of these genes
# Plot the expression of these genes to compare the subtypes (which plot would you use?)


save(cor_results, file = "correlation_bcell_gene_expression.Rdata") # saves the correlation results as an RData-file so that you can use it for the gene set enrichment analyses
 
  