## Calculate relative abundance

getwd()

setwd("/Users/paulasuredahorrach/documents/universitat/holanda/projecte")

#Open data
tax_table <- read.table("~/Documents/Universitat/Holanda/Projecte/IBD_taxonomy_metaphlan2.txt", sep = "\t", header = T, row.names = 1)
t_tax_table <- as.data.frame(t(tax_table))


##Function to calculate mean excluding 0 values
 nzmean <- function(x){
   mean(x[x!=0])
 }

##Function to calculate nº of 0
 nzsum <- function(x){
   sum (x==0)
 }

##Function to calculate nº of non-0
 nsum <- function(x){
   sum (x!=0)
 }

my_results=matrix(ncol = 4, nrow=ncol(t_tax_table)) 
  
## Loop

for (i in 1:ncol(t_tax_table)) {
  
  x = nzmean(t_tax_table[,i])
  y = mean(t_tax_table[,i])
  z = nzsum(t_tax_table[,i])
  a = nsum(t_tax_table[,i])
  
  my_results[i,1] = x
  my_results[i,2] = y
  my_results[i,3] = z
  my_results[i,4] = a

}

# The column names from the original table = row names from the new table
rownames(my_results) = colnames(t_tax_table)

# Give names to the columns of the new table
colnames(my_results) = c("Non-zero mean", "Mean", "Nº of 0", "Nº of non-0") 

results <- as.data.frame(my_results)
  results[is.na(results)] <- 0

abundance_results <- write.table(results, file = "~/abundance_results.txt", quote = F, sep = "\t")
