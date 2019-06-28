l0_outputs <- read.csv("C:/Users/CDouglas/VarroaPop/l0/outputs_archive/summ_outputs.csv")
is.data.frame(test2) #Check that the dataset has been read in correctly and is formatted as a data frame
rownames(test) <- test[ ,1] #Set the first column as the rownames
test$X <- NULL #Delete the first column (redundant now that rownames have been set)
test2 <- t(test) #Transposes the rows into columns
colnames(test2) #Check that the transposition has retained the column names
test2 <- data.frame(test2) #Reset the list as a dataframe (transposing the data screws up the list format)

library(data.table)
setDT(test2, keep.rownames = TRUE) []

library(splitstackshape)
test3 <- cSplit(test2, "rn", ".")
test3 <- test3[ , c(6,7,1,2,3,4,5)]
colnames(test3) <- c("param","i","CCA3","CCA4","CCA5","CCA6","CCA7")


##### Trying to compare means using summary statistics
l0_msd$X <- NULL
cca.df <- data.frame(cca = c(3,4,5,6,7))
n.df <- data.frame(n = c(10000,10000,10000,10000,10000))
l0_msd <- cbind(cca.df, n.df, l0_msd)