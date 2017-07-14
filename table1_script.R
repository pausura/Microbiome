##### Table 1 script #####

##Indicates working directory
getwd()

##Changes workind directory
setwd("/Users/paulasuredahorrach/documents/universitat/holanda/projecte")

##Open data
new_metadata <- read.table("~/Documents/Universitat/Holanda/Projecte/new_metadata_txt.txt", sep = "\t", header = T, row.names = 1)

#Categorical values - create function
      tblFun <- function(x) {
          #Create a table
        tbl <- table(x)
          #Combine columnes/rows to get the counts and percentage (creates new table -> res)
        res <- cbind(tbl,round(prop.table(tbl)*100,2))
          #Give names to the columns
        colnames(res) <- c('Count','Percentage')
        res
      }

#Continuous variables - two packages
      library (psych)
        describe()
      library (pastecs)
        stat.desc()

##Divide data in groups
   #CD
      table_cd <- subset(new_metadata,new_metadata$DiagnosisCurrent=="CD")
   #UC
      table_uc <- subset(new_metadata,new_metadata$DiagnosisCurrent=="UC")        
   #Ileal Resection
      table_ir <- subset(new_metadata,new_metadata$ResectionIlealAny=="yes")
   #Colonic Resection
      table_cr <- subset(new_metadata, new_metadata$ResectionColonicAny=="yes")
   #Stoma
      table_st <- subset(new_metadata, new_metadata$CurrentStoma=="yes")
   #Pouch
      table_pc <- subset(new_metadata, new_metadata$CurrentPouch=="yes")
        

### Loop for ###

#Create an empty table: nrows = ncolumns original table
my_results=matrix(ncol = 5, nrow=ncol(table_cd))

for (i in 1:ncol(table_cd)) { # For each loop goes to the next column (numerical way)
  #print (my_table[,i]) # Right loop verification
  
# Condition: if the column values are numerical/continuous
  if ( is.numeric(table_cd[,i])){
    #print (paste0("Column numeric: ", i)) # Right loop verification
    
  # Keep in "x" the result from describe function (done in the columns) - for each factor
    x=describe(table_cd[,i])
  
  # In the new table ("x"): keep different values in the different columns (in crhonological order)
    my_results[i,1]="numerical"
    my_results[i,2]=x$median
    my_results[i,3]=x$mean
    my_results[i,4]=x$sd
    my_results[i,5]=x$n
    
  } 
  
  # Condition: if the column values are categorical
  else {
    #print (paste0("Column categorical ", i)) # Right loop verification
    
  # Keep in "x" the result from tblFun function (done in the columns) - for each factor
    x=tblFun(table_cd[,i])
  
  # In the new table ("x"): keep different values in the different columns (in crhonologial order)
    my_results[i,1]="Categorical"
      # toString to keep the possible different values/categories in the same vector/column
    my_results[i,2]=toString(rownames(x))
      # First column table x = 'Count'
    my_results[i,3]=toString(x[,1]) 
      # Second column table x = 'Percentage'
    my_results[i,4]=toString(x[,2])
      # Sum of the values on column1 ("x")
    my_results[i,5]=sum(x[,1])
    
  }
}

# The column names from the original table = row names from the new table
rownames(my_results) = colnames(table_cd)

# Give names to the columns of the new table
colnames(my_results) = c("Type", "Categories/Median", "Counts / Mean", "SD / %", "Number of non-zeros (n)") 


## Export the new table
write.table (my_results,file = "~/cd_summary_table.txt", quote = F, sep = "\t")




