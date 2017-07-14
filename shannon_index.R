
### Shannon Index Analysis ### - from filum data

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

alpha <- as.data.frame(diversity(t_filum_table,index="shannon"))

intestinal_groups <- read.table("~/Documents/Universitat/Holanda/Projecte/intestinal_groups.txt", sep = "\t", header = T, row.names = 1)

group_taxa <- merge(intestinal_groups, alpha, by="row.names")
rownames(group_taxa) <- group_taxa[,1]
group_taxa2 <- group_taxa[,-1]
colnames(group_taxa2)[2] <- "diversity"


##### Violin plot
violin_plot <- ggplot(group_taxa2, aes(x=group_taxa2$Group, y=group_taxa2$diversity, fill=group_taxa2$Group)) + labs (y="Shannon Diversity Index", x="Group") + geom_violin(trim=FALSE) + geom_boxplot(width = 0.1) + scale_fill_manual(values=c("deepskyblue", "yellow","red","deepskyblue")) + theme_classic() + theme(legend.position="none") + theme(axis.text.x = element_text(hjust = 1, size=16,color="black"))   
violin_plot

##Alternative function   
## Violin plot (version 2.0)
library(vioplot)
x1 <- group_taxa2$diversity[group_taxa2$Group=="normal"]
x2 <- group_taxa2$diversity[group_taxa2$Group=="intermediate"]
x3 <- group_taxa2$diversity[group_taxa2$Group=="small intestine"]

vioplot(x1, x2, x3, names=c("normal", "intermediate", "small intestine"), 
        col="white")



## Taules inutils, pero informatives - see below


## INTERMEDIATE GROUP

intermediate_taxa <- subset(group_taxa_table2, group_taxa_table2$Group=="intermediate")
intermediate_taxa2 <- intermediate_taxa[,-1]

##Create a loop to calculate the mean of each intestinal group / taxa
#Create an empty table: nrows = ncolumns original table

intermediate_results=as.data.frame(matrix(ncol = 1, nrow=ncol(intermediate_taxa2)))

for (i in 1:ncol(intermediate_taxa2)) { # For each loop goes to the next column (numerical way)
  #print (my_table[,i]) # Right loop verification
  
  # Keep in "x" the result from describe function (done in the columns) - for each factor
  x=describe(intermediate_taxa2[,i])
  
  # In the new table ("x"): keep different values in the different columns (in crhonological order)
  intermediate_results[i,1]=x$mean
  
}

rownames(intermediate_results) = colnames(intermediate_taxa2)
colnames(intermediate_results) <- "Intermediate"

## SMALL INTESTINE GROUP

intestine_taxa <- subset(group_taxa_table2, group_taxa_table2$Group=="small intestine")
intestine_taxa2 <- intestine_taxa[,-1]

##Create a loop to calculate the mean of each intestinal group / taxa
#Create an empty table: nrows = ncolumns original table

intestine_results=as.data.frame(matrix(ncol = 1, nrow=ncol(intestine_taxa2)))

for (i in 1:ncol(intestine_taxa2)) { # For each loop goes to the next column (numerical way)
  #print (my_table[,i]) # Right loop verification
  
  # Keep in "x" the result from describe function (done in the columns) - for each factor
  x=describe(intestine_taxa2[,i])
  
  # In the new table ("x"): keep different values in the different columns (in crhonological order)
  intestine_results[i,1]=x$mean
  
}

rownames(intestine_results) = colnames(intestine_taxa2)
colnames(intestine_results) <- "Small Intestine"

## NORMAL GROUP

normal_taxa <- subset(group_taxa_table2, group_taxa_table2$Group=="normal")
normal_taxa2 <- normal_taxa[,-1]

##Create a loop to calculate the mean of each intestinal group / taxa
#Create an empty table: nrows = ncolumns original table

normal_results=as.data.frame(matrix(ncol = 1, nrow=ncol(normal_taxa2)))

for (i in 1:ncol(normal_taxa2)) { # For each loop goes to the next column (numerical way)
  #print (my_table[,i]) # Right loop verification
  
  # Keep in "x" the result from describe function (done in the columns) - for each factor
  x=describe(normal_taxa2[,i])
  
  # In the new table ("x"): keep different values in the different columns (in crhonological order)
  normal_results[i,1]=x$mean
  
}

rownames(normal_results) = colnames(normal_taxa2)
colnames(normal_results) <- "Normal"  

## Merge the results for each categories in the same table

MyMerge <- function(x, y){
  df <- merge(x, y, by=0, all.x= TRUE, all.y= TRUE)
  return(df)
}

shannon_table <- Reduce(MyMerge, list(intermediate_results, intestine_results, normal_results))

#Arrenge the duplicated column in excel
table_shannon <- write.table(shannon_table, file = "~/shannon_table_DUDes.txt", sep = "\t")
shannon_table2 <- read.table("~/Documents/Universitat/Holanda/Projecte/Results_DUDesdata/shannon_table_DUDes.txt", sep = "\t", header = T, row.names = 1)
