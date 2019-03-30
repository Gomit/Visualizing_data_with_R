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
myplot1 = ggplot(df) + #Insert data
  geom_jitter(aes_string(x, y), shape=1, colour="blue") + 
  geom_smooth(aes_string(x, y), method=lm, se=TRUE,color="blue") #col2-3 Reg.Line

#Equation + r^2
lm_eqn = function(m) {
  
  l <- list(a = format(coef(m)[1], digits = 2), # y-intercept
            b = format(abs(coef(m)[2]), digits = 2), # slope
            r2 = format(summary(m)$r.squared, digits = 3)); # r^2
  
  #Format equation text
  if (coef(m)[2] >= 0)  {
    eq <- substitute(italic(y) == a + b %.% italic(x)*","~~italic(r)^2~"="~r2,l)
  } else {
    eq <- substitute(italic(y) == a - b %.% italic(x)*","~~italic(r)^2~"="~r2,l)    
  }
  
  as.character(as.expression(eq));                 
}

## lm(th_sd ~ th_mean, df) => lm(y ~ x, df) => lm(col2 ~ col1, df) => for r^2
myplot2 = myplot1 + geom_text(aes(x = 13, y = 7, label = lm_eqn(lm(th_sd ~ th_mean, df))), parse = TRUE)

#Save plot
ggsave(myplot2, file=args[2])