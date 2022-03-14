
http://stackoverflow.com/questions/15505607/diagonal-labels-orientation-on-x-axis-in-heatmaps
getwd()
setwd("/Users/beeway/Desktop/Interest Rates/Mypapers/DV/heatmap")

install.packages("tidyverse") # read csv

## US recessions up to date
#### Divide the whole sample into two parts
library(tidyverse)
md=read.csv("BC1855.csv")

colSums(md)

# heatmapping requires matrix format 
md=as.data.frame(md)
rownames(md)=md$Date
mdhm=t(md[,13:2])
colnames(mdhm)=md$Year

# recession month distribution
colSums(mdhm)
table(mdhm)
# 576/(1404+576)=0.29

library(RColorBrewer)
library(pheatmap) # pheatmap() function requires that the data be a matrix format
# heatmap levels: horizontal - columns; vertical - rows

# 1855-1936 W=850, H=450
pheatmap(mdhm[,1:82],cluster_row=F,cluster_col=F, legend = F) 

# 1937-2021
pheatmap(mdhm[,83:167],cluster_row=F,cluster_col=F, legend = F) 

pheatmap(mdhm,cluster_row=F,cluster_col=F, legend = F, filename = "bc.png") 
