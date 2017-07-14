  
  ### Plot reads distribution per sample ###

getwd()

setwd("/Users/paulasuredahorrach/documents/universitat/holanda/projecte")

library(ggplot2)

#Num reads
reads_table <- read.table("~/Documents/Universitat/Holanda/Projecte/read_depth_per_sample_ibd.txt", sep = "\t", header = T)
  reads_table$Sample <- row.names(reads_table)
  reads_table2 <- reads_table[,-1]

reads_vector <- scan(reads_table2)

hist_plot <- hist(reads_table2$Reads)

  ggplot(reads_table, aes(x=reads_table$IBDFEC, y=reads_table$Reads, fill=reads_table$IBDFEC)) + labs (y="N reads", x="Sample") + geom_histogram()   


  p7 <- ggplot(numeric_reads_table, aes(x = reads_table2$Sample)) + geom_histogram(aes(y =reads_table2$Reads ), binwidth = 5)
  p7
  