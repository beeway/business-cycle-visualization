
library("readxl")
mir=read_excel("1954.xlsx")

# graph the rolling unconditional recession probability time series
date=seq(as.Date('1954-07-01'), to=as.Date('2022-05-01'), format="%Y%m" , by="month")
md=data.frame(date, mir[,-1])

library(ggplot2); library(scales)

# Year on X-axis
ggplot(md, aes(x=date)) + geom_line(aes(y=ffr, col="ffr")) + geom_line(aes(y=t1y, col="t1y")) + 
         geom_line(aes(y=t5y, col="t5y")) + geom_line(aes(y=t10y, col="t10y")) + 
         theme_bw()  + labs(x="", y="Interest Rates (%)", caption="Source: FRB; FRED.") + scale_y_continuous(labels = ) + 
  theme(legend.title = element_blank(), legend.position = c(0.75,0.87)) +
  scale_color_discrete(labels=c("Federal Fund's Rate", "Treasury 1-Year Yield", 
                                "Treasury 5-Year Yield", "Treasury 10-Year Yield")) +
  scale_x_date(date_labels="%Y",date_breaks = "1 years", date_minor_breaks="1 years", expand=c(0.02,0.02)) + 
  theme(axis.text.x=element_text(angle=90, vjust=0.5, size = 9))  

# Year and Month on X-axis
ggplot(md, aes(x=date)) + geom_line(aes(y=ffr, col="ffr")) + 
  geom_line(aes(y=t1y, col="t1y")) + 
  geom_line(aes(y=t5y, col="t5y")) + 
  geom_line(aes(y=t10y, col="t10y")) +
  theme_bw()  + labs(x="", y="Interest Rates (%)", caption="Source: FRB; FRED.") + scale_y_continuous(labels = ) + 
  theme(legend.title = element_blank(), legend.position = c(0.75,0.87)) +
  scale_color_discrete(labels=c("Federal Fund's Rate", "Treasury 1-Year Yield", 
                                "Treasury 5-Year Yield", "Treasury 10-Year Yield")) +
  scale_x_date(date_labels="%Y-%m",date_breaks = "1 years", date_minor_breaks="1 years", expand=c(0.02,0.02)) + 
  theme(axis.text.x=element_text(angle=90, vjust=0.5, size = 9))  

############################################################
# Change the Y-axis to percentage scale
# If scale for 'y' is already present, adding another scale for 'y', which will replace the existing scale.

date=seq(as.Date('1954-07-01'), to=as.Date('2022-05-01'), format="%Y%m" , by="month")
mir1=mir[,-1]/100
md1=data.frame(date, mir1)

# Year on X-axis
ggplot(md1, aes(x=date)) + geom_line(aes(y=ffr, col="ffr")) + geom_line(aes(y=t1y, col="t1y")) + 
  geom_line(aes(y=t5y, col="t5y")) + geom_line(aes(y=t10y, col="t10y")) + 
  theme_bw()  + labs(x="", y="", caption="Source: FRB; FRED.") + scale_y_continuous(labels = ) + 
  theme(legend.title = element_blank(), legend.position = c(0.75,0.87)) +
  scale_color_discrete(labels=c("Federal Fund's Rate", "Treasury 1-Year Yield", 
                                "Treasury 5-Year Yield", "Treasury 10-Year Yield")) +
  scale_x_date(date_labels="%Y",date_breaks = "1 years", date_minor_breaks="1 years", expand=c(0.02,0.02)) + 
  theme(axis.text.x=element_text(angle=90, vjust=0.5, size = 9)) + scale_y_continuous(labels = percent)

# Year and Month on X-axis
ggplot(md1, aes(x=date)) + geom_line(aes(y=ffr, col="ffr")) + geom_line(aes(y=t1y, col="t1y")) + 
  geom_line(aes(y=t5y, col="t5y")) +   geom_line(aes(y=t10y, col="t10y")) +
  theme_bw()  + labs(x="", y="", caption="Source: FRB; FRED.") +
  theme(legend.title = element_blank(), legend.position = c(0.75,0.87)) + scale_y_continuous(labels = percent)+
  scale_color_discrete(labels=c("Federal Fund's Rate", "Treasury 1-Year Yield", 
                                "Treasury 5-Year Yield", "Treasury 10-Year Yield")) +
  scale_x_date(date_labels="%Y-%m",date_breaks = "1 years", date_minor_breaks="1 years", expand=c(0.02,0.02))+ 
  theme(axis.text.x=element_text(angle=90, vjust=0.5, size = 9)) + scale_y_continuous(labels = percent)
  
############################################################

# NBER recessions - shaded areas
xl=as.Date(c( "1957-09-01", "1960-05-01", "1970-01-01", "1973-12-01", "1980-02-01", "1981-08-01",  
              "1990-08-01", "2001-04-01", "2008-01-01", "2020-03-01"))
xr=as.Date(c( "1958-04-01", "1961-02-01", "1970-11-01", "1975-03-01", "1980-07-01", "1982-11-01",  
              "1991-03-01", "2001-11-01", "2009-06-01", "2020-04-01"))

g=ggplot(md1, aes(x=date)) + geom_line(aes(y=ffr, col="ffr")) + geom_line(aes(y=t1y, col="t1y")) + 
  geom_line(aes(y=t5y, col="t5y")) +   geom_line(aes(y=t10y, col="t10y")) +
  theme_bw()  + labs(x="", y="", caption="Source: FRB; FRED; NBER.") +
  theme(legend.title = element_blank(), legend.position = c(0.87,0.87)) + scale_y_continuous(labels = percent)+
  scale_color_discrete(labels=c("Federal Fund's Rate", "Treasury 1-Year Yield", 
                                "Treasury 5-Year Yield", "Treasury 10-Year Yield")) +
  scale_x_date(date_labels="%Y-%m",date_breaks = "1 years", date_minor_breaks="1 years", expand=c(0.02,0.02))+ 
  theme(axis.text.x=element_text(angle=90, vjust=0.5, size = 9)) + 
  scale_y_continuous(labels = percent, expand=c(0,0))

g + annotate("rect", xmin=xl, xmax=xr, ymin =rep(0,10), ymax =rep(0.2,10), alpha = .45) 

############################################################

# Full NBER Recession Dates

xl=as.Date(c( "1854-12-01","1857-07-01","1860-11-01","1865-05-01","1869-07-01",
              "1873-11-01","1882-04-01","1887-04-01","1890-08-01","1893-02-01",
              "1896-01-01","1899-07-01","1902-10-01","1907-07-01","1910-02-01",
              "1913-02-01","1918-09-01","1920-02-01","1923-06-01","1926-11-01",
              "1929-09-01","1937-06-01","1945-03-01","1949-01-01","1953-08-01",
              "1957-09-01","1960-05-01","1970-01-01","1973-12-01","1980-02-01", 
              "1981-08-01","1990-08-01","2001-04-01","2008-01-01","2020-03-01"))

xr=as.Date(c( "1854-12-01","1858-12-01","1861-06-01","1867-12-01","1870-12-01",
              "1879-03-01","1885-05-01","1888-04-01","1891-05-01","1894-06-01",
              "1897-06-01","1900-12-01","1904-08-01","1908-06-01","1912-01-01",
              "1914-12-01","1919-03-01","1921-07-01","1924-07-01","1927-11-01",
              "1933-03-01","1938-06-01","1945-10-01","1949-10-01","1954-05-01",
              "1958-04-01","1961-02-01","1970-11-01","1975-03-01","1980-07-01", 
              "1982-11-01","1991-03-01","2001-11-01","2009-06-01","2020-12-01"))

g + annotate("rect", xmin=xl, xmax=xr, ymin =rep(0,35), ymax =rep(1,35), alpha = .45) 

