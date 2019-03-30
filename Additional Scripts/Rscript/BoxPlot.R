#!/usr/bin/env Rscript
args = commandArgs(trailingOnly=TRUE)

## import library
#install.packages('ggplot2', repos='http://cran.us.r-project.org')
library(ggplot2)
library(reshape2)

## program...
#Open CSV-file
df = read.csv(args[1], header=TRUE)


#Box-Plot
myplot = ggplot(melt(df,id.vars = 1), aes(variable, value, fill=variable)) + #Variables/Values + color 
  geom_boxplot()+ #Create Box-Plot
  geom_jitter()+ #Show values
  theme(axis.text.x=element_text(angle=90, vjust=0.4,hjust=1)) #Fix Lables

#Save plot
ggsave(myplot, file=args[2])