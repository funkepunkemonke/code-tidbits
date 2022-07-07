setwd("C:\\Users\\funke\\Downloads")
library(tidyverse)
dat <- read_excel("tatidata.xlsx")
str(dat)
#Examine the boxplots for both variables to check for outliers
boxplot(dat$Total_active_proportion) #dep/response variable
boxplot(dat$Daily_avg_temp) #ind variable 1
boxplot(dat$imp_sur) #ind variable 2 - might have outliers on the lower end. Are there really sites with zero imp surface?
#test for normality
shapiro.test(dat$Total_active_proportion) 
shapiro.test(dat$Daily_avg_temp)
shapiro.test(dat$imp_sur) #imp_sur is not normally distributed, let's look at the histogram
hist(dat$imp_sur) #that's bimodal which usually indicates you’ve got two different groups, ie wooded vs urban; might want to make categorical
#test for zero trouble
plot(table(dat$Total_active_proportion), type = "h", xlab = "Observed values", ylab = "Frequency")

#Look at scatterplot to see what the relationship might be
ggplot(dat, aes(x=Daily_avg_temp, y=Total_active_proportion)) + geom_point()+geom_smooth(method = "glm" )
ggplot(dat, aes(x=imp_sur, y=Total_active_proportion)) + geom_point()+geom_smooth(method = "glm" )

#what is the best model? All of them look good
models <- list(
  "Null" =  glm(Total_active_proportion~1, dat),
  "Temp" =  glm(Total_active_proportion~Daily_avg_temp, dat),
  "Imp"  =  glm(Total_active_proportion~imp_sur, dat),
  "Full" =  glm(Total_active_proportion~imp_sur+Daily_avg_temp, dat),
  "Int"  =  glm(Total_active_proportion~imp_sur*Daily_avg_temp, dat))
AICcmodavg::aictab(models)  

MuMIn::model.sel(models) #imp surf barely makes a diff..temp really doesn't either. 

#I would fail to reject the null. 
