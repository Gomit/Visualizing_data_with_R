#!/usr/bin/env Rscript
args = commandArgs(trailingOnly=TRUE)

## import library
#install.packages('ggplot2', repos='http://cran.us.r-project.org')
library(ggplot2)

## program...
#Open CSV-file
df = read.csv(args[1], header=TRUE)

#Column nr x-axis
x = colnames(df)[2]
#Column nr y-axis
y = colnames(df)[3]

#Regressionline
myplot = ggplot(df) + #Insert data
  geom_jitter(aes_string(x, y), shape=1, colour="blue") + 
  geom_smooth(aes_string(x, y), method=lm, se=TRUE,color="blue") #Reg.Line

#Save plot
ggsave(myplot, file=args[2])