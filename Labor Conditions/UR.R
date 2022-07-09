
#library("readxl");mir=read_excel("1954.xlsx")
library(tidyverse); ur=read.csv("UR.csv")

# graph the rolling unconditional recession probability time series
date=seq(as.Date('1948-01-01'), to=as.Date('2022-05-01'), format="%Y%m" , by="month")
ur=ur$UNRATE; md=data.frame(date, ur)

library(ggplot2); library(scales)

# Year on X-axis
ggplot(md, aes(x=date)) + geom_line(aes(y=ur, col=1)) + geom_line(aes(y=ur, col=1)) + theme_bw() +
  theme(legend.title = element_blank(), legend.position = "none", axis.title.y.right = element_text()) +
    labs(x="", y="U.S. Unemployment Rate (%)", caption="Source: Biwei Chen (2022). Data: BLS; FRED.") + 
    scale_y_continuous(limits = c(0,15), breaks=seq(0,15,1), sec.axis=dup_axis(name="")) +
    scale_x_date(date_labels="%Y",date_breaks = "1 years", date_minor_breaks="1 years", expand=c(0.02,0.02)) + 
    theme(axis.text.x=element_text(angle=90, vjust=0.5, size = 9))  

############################################################
# Graph with NBER recessions - shaded areas

xl=as.Date(c( "1949-01-01", "1953-08-01", "1957-09-01", "1960-05-01", "1970-01-01", 
              "1973-12-01", "1980-02-01", "1981-08-01", "1990-08-01", "2001-04-01", "2008-01-01", "2020-03-01"))
xr=as.Date(c( "1949-10-01", "1954-05-01", "1958-04-01", "1961-02-01", "1970-11-01", 
              "1975-03-01", "1980-07-01", "1982-11-01", "1991-03-01", "2001-11-01", "2009-06-01", "2020-04-01"))

g = ggplot(md, aes(x=date)) + geom_line(aes(y=ur, col=1)) + geom_line(aes(y=ur, col=1)) + theme_bw() +
  theme(legend.title = element_blank(), legend.position = "none", axis.title.y.right = element_text()) +
  labs(x="", y="U.S. Unemployment Rate (%)", caption="Source: Biwei Chen (2022). Data: BLS; FRED.") + 
  scale_y_continuous(limits = c(0,15), breaks=seq(0,15,1), expand=c(0,0), sec.axis=dup_axis(name="")) +
  scale_x_date(date_labels="%Y",date_breaks = "1 years", date_minor_breaks="1 years", expand=c(0.02,0.02)) + 
  theme(axis.text.x=element_text(angle=90, vjust=0.5, size = 9))  

g + annotate("rect", xmin=xl, xmax=xr, ymin =rep(0,12), ymax =rep(15,12), alpha = .45) 

