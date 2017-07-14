
### Principal coordinate analysis (PCoA) ###

getwd()

setwd("/Users/paulasuredahorrach/documents/universitat/holanda/projecte")


## Create a filum table 
tax_table <- read.table("~/Documents/Universitat/Holanda/Projecte/IBD_taxonomy_DUDes.txt", sep = "\t", header = T, row.names = 1)

loop_table <- as.data.frame(matrix(nrow = nrow(tax_table) , ncol = ncol(tax_table)))

## Count only "|p_" --> 2 fields

for (i in 1:nrow(tax_table)) {
  
  if (count.fields(textConnection(row.names(tax_table[i,])), sep="|") == 2){
    #print (paste0("Species found: ", row.names(tax_table[i,]))) ##Loop check
    
    loop_table[i,] = tax_table[i,]
    
  }
  
}

# Give row names to the new table
row.names(loop_table) = row.names(tax_table)

# Give column names to  new table
colnames(loop_table) = colnames(tax_table)

##Remove all rows with NA values  
filum_table <- na.omit(loop_table)

fl_table <- write.table(filum_table, file = "~/filum_table_DUDes.txt", quote = F, sep = "\t")

  ##Transpose the filum table for the diversity function
t_filum_table <- as.data.frame(t(filum_table))

library(vegan)
library(ggplot2)

      #Generate a distance matrix - Bray method
  beta <- vegdist(t_filum_table, method="bray")
      #cmdscale -> multidimensional scaling of a data matrix (distance matrix)
  my_pcoa <- as.data.frame(cmdscale(beta, k = 4))
    
    # Calculate the mean for each phyla to see the most abundants
  summary(t_filum_table$`k__Archaea|p__Euryarchaeota`) # As many times as columns (taxa)
          ## Based on the mean: Actinobacteria, Bacteroidetes and Firmicutes are the dominant phyla

  ##### PCoA plot 
  
      #Actinobacteria
   ## To show the abundance of actinobacteria in the plot (actinobacteria status)
  a_status <- t_filum_table$`k__Bacteria|p__Actinobacteria`
  
  a_my_pcoa2 <- data.frame(my_pcoa, a_status, stringsAsFactors = FALSE)
    #Change column names
        colnames(a_my_pcoa2) <- c("PCoA1", "PCoA2", "PCoA3", "PCoA4", "a_status")

     #Define colors   
        a_my_pcoa2$color<-"black"
  
  a_pcoa_plot <- ggplot(a_my_pcoa2, aes(x=a_my_pcoa2$PCoA1 , y=a_my_pcoa2$PCoA2, geom="blank",colour=a_my_pcoa2$a_status)) + geom_point () + theme_classic() + labs(x="PCoA1",y="PCoA2")
  a_pcoa_plot
  
      #Bacteroidetes
    ## To show the abundance of bacteroidetes in the plot (bacteroidetes status)
   b_status <- t_filum_table$`k__Bacteria|p__Bacteroidetes`
  
  b_my_pcoa2 <- data.frame(my_pcoa, b_status, stringsAsFactors = FALSE)
      #Change column names
          colnames(b_my_pcoa2) <- c("PCoA1", "PCoA2", "PCoA3", "PCoA4", "b_status")
  
   #Define colors   
      b_my_pcoa2$color<-"black"
  
  b_pcoa_plot <- ggplot(b_my_pcoa2, aes(x=b_my_pcoa2$PCoA1 , y=b_my_pcoa2$PCoA2, geom="blank",colour=b_my_pcoa2$b_status)) + geom_point () + theme_classic() + labs(x="PCoA1",y="PCoA2")
  b_pcoa_plot
  
      #Firmicutes
    ## To show the abundance of firmicutes in the plot (firmicutes status)
  f_status <- t_filum_table$`k__Bacteria|p__Firmicutes`
  
  f_my_pcoa2 <- data.frame(my_pcoa, f_status, stringsAsFactors = FALSE)
      #Change column names
          colnames(f_my_pcoa2) <- c("PCoA1", "PCoA2", "PCoA3", "PCoA4", "f_status")
  
  #Define colors   
    f_my_pcoa2$color<-"black"
  
  f_pcoa_plot <- ggplot(f_my_pcoa2, aes(x=f_my_pcoa2$PCoA1 , y=f_my_pcoa2$PCoA2, geom="blank",colour=f_my_pcoa2$f_status)) + geom_point () + theme_classic() + labs(x="PCoA1",y="PCoA2")
  f_pcoa_plot



  
  
  
  