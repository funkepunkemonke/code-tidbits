# Aly Putnam Text as Data 

## The 'bibliometrix' package has been a great aid in converting a .bib file to a corpus. I searched Web of Science and downloaded the complete .bib file from the serach results. This results in a corpus. 

## tutorial here: https://www.bibliometrix.org/vignettes/Introduction_to_bibliometrix.html

install.packages("bibliometrix", dependencies=TRUE) ### installs bibliometrix package
library(bibliometrix) ### load bibliometrix package
library(tidyverse)    ### load tidyverse package

file<-"file.bib"

d <- convert2df(file, format="bibtex", dbsource="wos") ### import and convert data
r <- biblioAnalysis(d) ###  calculates main bibliometric measures
summary(r) #Descriptive analysis
plot(r)

r_sub <- r[which(r$AB == "searchterm"), ] ## Column AB is abstract
r_sub
 
