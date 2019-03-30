#!/usr/bin/env Rscript
args = commandArgs(trailingOnly=TRUE)

## program...

#Open CSV-file
df = read.csv(args[1], header=TRUE)

#Load packeges
install.packages('pastecs', repos='http://cran.us.r-project.org')
install.packages('boot', repos='http://cran.us.r-project.org')

#Load Library
library(pastecs)

#Get summary
newData = stat.desc(df)

#Write CSV-file
write.csv(newData, file=args[2])
