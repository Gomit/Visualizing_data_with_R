#!/usr/bin/env Rscript
args = commandArgs(trailingOnly=TRUE)

## import library
#install.packages('ggplot2', repos='http://cran.us.r-project.org')
library(ggplot2)
library(reshape2)
library(gridExtra)

## program...
#Open CSV-file
df = read.csv(args[1], header=TRUE)

#Column nr x-axis
x = colnames(df)[2]
#Column nr y-axis
y1 = colnames(df)[3]
#Column nr y-axis
y2 = colnames(df)[4]
#Column nr y-axis
y3 = colnames(df)[5]

#Regressionline
complot = ggplot(df) + #Insert data
  geom_jitter(aes_string(x, y1), shape=1, colour="blue") + 
  geom_smooth(aes_string(x, y1), method=lm, se=FALSE,color="blue") + #col2-3 Reg.Line
  
  geom_jitter(aes_string(x, y2), shape=1, colour="green") + 
  geom_smooth(aes_string(x, y2), method=lm, se=FALSE,color="green") + #col2-4 Reg.Lin
  
  geom_jitter(aes_string(x, y3), shape=1, colour="red") + 
  geom_smooth(aes_string(x, y3), method=lm, se=FALSE,color="red") + #col2-5 Reg.Lin
  
  labs(x = "col.1", y = "Col.2-Col.n") #Axis Labels

p1 <- ggplot(df) + #Insert data
  geom_jitter(aes_string(x, y1), shape=1, colour="blue") + 
  geom_smooth(aes_string(x, y1), method=lm, se=TRUE,color="blue")           #set one color for all points
p2 <- ggplot(df) + #Insert data
  geom_jitter(aes_string(x, y2), shape=1, colour="green") + 
  geom_smooth(aes_string(x, y2), method=lm, se=TRUE,color="green")       #set color scale by a continuous variable
p3 <- ggplot(df) + #Insert data
  geom_jitter(aes_string(x, y3), shape=1, colour="red") + 
  geom_smooth(aes_string(x, y3), method=lm, se=TRUE,color="red")  #set color scale by a factor variable

myplot = grid.arrange(p1, p2, p3,complot, ncol=2) #Create plots with two columns
#Save plot
ggsave(myplot, file=args[2])