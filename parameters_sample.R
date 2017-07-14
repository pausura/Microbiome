### Calculate parameters per sample

getwd()

setwd("/Users/paulasuredahorrach/documents/universitat/holanda/projecte")

#Num reads
reads_table <- read.table("~/Documents/Universitat/Holanda/Projecte/read_depth_per_sample_ibd.txt", sep = "\t", header = T)

## Count num species
  tax_table <- read.table("~/Documents/Universitat/Holanda/Projecte/IBD_taxonomy_metaphlan2.txt", sep = "\t", header = T, row.names = 1)
  
  loop_table <- as.data.frame(matrix(nrow = nrow(tax_table) , ncol = ncol(tax_table)))
  
## Count only "|s_" --> 7 fields
  for (i in 1:nrow(tax_table)) {
       
        if (count.fields(textConnection(row.names(tax_table[i,])), sep="|") == 7){
             #print (paste0("Species found: ", row.names(tax_table[i,]))) ##Loop check
          
          loop_table[i,] = tax_table[i,]
          
          }
     
      }

# Give row names to the new table
  row.names(loop_table) = row.names(tax_table)
  
# Give column names to  new table
  colnames(loop_table) = colnames(tax_table)

##Remove all rows with NA values  
  species_table <- na.omit(loop_table)
  
  sp_table <- write.table(species_table, file = "~/species_table.txt", quote = F, sep = "\t")

        ## Calculate number of species per sample ##

  species_results=as.data.frame(matrix(ncol = 1, nrow=ncol(species_table)))
 
   ##Function to calculate nº of non-0
  nsum <- function(x){
    sum (x!=0)  }
  
  ## Loop
  
  for (i in 1:ncol(species_table)) {
  
    a = nsum(species_table[,i])
    
    species_results[i,1] = a
    
  }

colnames(species_results) = c("Nº of species")
rownames(species_results) = colnames(species_table)
  
#Introduce rownames to merge
    species_results$IBDFEC <- rownames(species_results)



sp_results <- write.table(species_results, file = "~/species_results.txt", quote = F, sep = "\t")

### Shannon Index #### - diversity per sample

library(vegan)

#Traspose species_result table -- diversity function <- species:columns / samples:rows
t_species_table <- as.data.frame(t(species_table))

 #Calculate Shannon Index

diversity_table <- as.data.frame(diversity(t_species_table, index = "shannon"))

dv_results <- write.table(diversity_table, file = "~/diversity_table.txt", quote = F, sep = "\t")
  
  #Introduce rownames to merge
    diversity_table$IBDFEC <- rownames(diversity_table)
 

  #### MERGE: nº of reads / nº of species / shannon diversity
  MyMerge <- function(x, y){
    df <- merge(x, y, by= "IBDFEC", all.x= TRUE, all.y= TRUE)
    return(df)
  }
 
 sample_table <- Reduce(MyMerge, list(diversity_table, species_results, reads_table))
 
 merge_results <- write.table(sample_table, file = "~/merge_table.txt", quote = F, sep = "\t")
