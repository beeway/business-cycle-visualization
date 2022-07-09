
#library("readxl");mir=read_excel("1954.xlsx")
library(tidyverse); d1=read.csv("LPRS.csv")

# graph the rolling unconditional recession probability time series
date=seq(as.Date('1948-01-01'), to=as.Date('2022-06-01'), format="%Y%m" , by="month")
lr=d1$LR; mlr=d1$MLR; wlr=d1$WLR
d2=data.frame(date, lr, mlr, wlr)

library(ggplot2)

# Year on X-axis
ggplot(d2, aes(x=date)) + geom_line(aes(y=lr, col="lr")) + 
  geom_line(aes(y=mlr, col="mlr")) + geom_line(aes(y=wlr, col="wlr")) + theme_bw() +
  theme(legend.title = element_blank(), legend.position = "none", axis.title.y.right = element_text()) +
  labs(x="", y="", caption="Source: Biwei Chen (2022). Data: BLS; FRED.") + 
  scale_y_continuous(limits = c(30, 90), breaks=seq(30,90,2), sec.axis=dup_axis(name="")) +
  scale_x_date(date_labels="%Y",date_breaks = "1 years", date_minor_breaks="1 years", expand=c(0.01,0.01)) + 
  theme(axis.text.x=element_text(angle=90, vjust=0.5, size = 9))  +
  theme(legend.title = element_blank(), legend.position = c(0.85,0.15)) +
  scale_color_discrete(labels=c("Labor Force Participation (%)", "Labor Force Participation - Men",
                                "Labor Force Participation - Women")) 

xl=as.Date(c( "1949-01-01", "1953-08-01", "1957-09-01", "1960-05-01", "1970-01-01", 
              "1973-12-01", "1980-02-01", "1981-08-01", "1990-08-01", "2001-04-01", "2008-01-01", "2020-03-01"))
xr=as.Date(c( "1949-10-01", "1954-05-01", "1958-04-01", "1961-02-01", "1970-11-01", 
              "1975-03-01", "1980-07-01", "1982-11-01", "1991-03-01", "2001-11-01", "2009-06-01", "2020-04-01"))

g=ggplot(d2, aes(x=date)) + geom_line(aes(y=lr, col="lr")) + 
  geom_line(aes(y=mlr, col="mlr")) + geom_line(aes(y=wlr, col="wlr")) + theme_bw() +
  theme(legend.title = element_blank(), legend.position = "none", axis.title.y.right = element_text()) +
  labs(x="", y="", caption="Source: Biwei Chen (2022). Data: BLS; FRED; NBER.") + 
  scale_y_continuous(limits = c(30, 90), breaks=seq(30,90,2), sec.axis=dup_axis(name=""), expand=c(0,0)) +
  scale_x_date(date_labels="%Y",date_breaks = "1 years", date_minor_breaks="1 years", expand=c(0.01,0.01)) + 
  theme(axis.text.x=element_text(angle=90, vjust=0.5, size = 9))  +
  theme(legend.title = element_blank(), legend.position = c(0.87,0.15)) +
  scale_color_discrete(labels=c("Labor Force Participation (%)", "Labor Force Participation - Men",
                                "Labor Force Participation - Women")) 

  g + annotate("rect", xmin=xl, xmax=xr, ymin =rep(30,12), ymax =rep(90,12), alpha = .45) 


#############################################################333
# Percent scale
# https://scales.r-lib.org/reference/label_percent.html

  ggplot(d2, aes(x=date)) + geom_line(aes(y=lr/100, col="lr")) + geom_line(aes(y=er/100, col="er")) + theme_bw() +
    theme(legend.title = element_blank(), legend.position = "none", axis.title.y.right = element_text()) +
    labs(x="", y="", caption="Source: Biwei Chen (2022). Data: BLS; FRED.") + 
    scale_y_continuous(labels = scales::percent_format(accuracy = 3)) +
    #scale_y_continuous(limits = c(50, 70), breaks=seq(50,70, 1), sec.axis=dup_axis(name=""), expand=c(0,0)) +
    scale_x_date(date_labels="%Y",date_breaks = "1 years", date_minor_breaks="1 years", expand=c(0.01,0.01)) + 
    theme(axis.text.x=element_text(angle=90, vjust=0.5, size = 9))  +
    theme(legend.title = element_blank(), legend.position = c(0.15,0.9)) +
    scale_color_discrete(labels=c("Labor Force Participation Rate", "Employment-to-Population Ratio")) 
  
