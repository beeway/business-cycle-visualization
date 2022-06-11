#  Full sample recession probabilities

library(readxl); ds=read_excel("1953pred.xlsx")

attach(ds)

# experiments 
obs=nrow(ds)
y=rec[13:obs];
x1=sls[1:(obs-12)];  
m1=glm(y ~ x1, family=binomial(link="probit"), data=ds); 
nd1=data.frame(x1=sls)
op1=predict(m1, newdata=nd1, type="response")

y=rec[13:obs];xb=B[1:(obs-12)]
ms=glm(y~xb-1, family=binomial(link="probit"), data=ds)
nd=data.frame(xb=B)
ps=predict(ms, newdata=nd, type="response")
plot(ps)
lines(ps)

y=rec[13:obs]; xb=B[1:(obs-12)]; xd=D[1:(obs-12)]; xf=F[1:(obs-12)]; xh=H[1:(obs-12)]; xu=U[1:(obs-12)]
ms=glm(y~xb+xb+xf+xh+xu-1, family=binomial(link="probit"), data=ds)
nd=data.frame(xb=B, xd=D, xf=F, xh=H, xu=U)
ps=predict(ms, newdata=nd, type="response")
t=c(1:813)
plot(t, ps); lines(ps)


###### combined comparison
obs=nrow(ds); y=rec[13:obs]; 
x1=sls[1:(obs-12)]; x2=sms[1:(obs-12)]; x3=slm[1:(obs-12)]; x4=curv[1:(obs-12)]
xb=B[1:(obs-12)]; xd=D[1:(obs-12)]; xf=F[1:(obs-12)]; xh=H[1:(obs-12)]; xu=U[1:(obs-12)]

m1=glm(y ~ x1, family=binomial(link="probit"), data=ds); 
m2=glm(y ~ x2, family=binomial(link="probit"), data=ds); 
m3=glm(y ~ x3, family=binomial(link="probit"), data=ds); 
m4=glm(y ~ x4, family=binomial(link="probit"), data=ds); 


mc=glm(y ~ x1+x2, family=binomial(link="probit"), data=ds); 
ms=glm(y ~ xb+xd+xf+xh+xu-1, family=binomial(link="probit"), data=ds); 
ma=glm(y ~ xb+xd+xf+xh+xu+x1+x2-1, family=binomial(link="probit"), data=ds); 

nd1=data.frame(x1=sls)
nd2=data.frame(x2=sms)
nd3=data.frame(x3=slm)
nd4=data.frame(x4=curv)
ndc=data.frame(x1=sls, x2=sms)
nds=data.frame(xb=B, xd=D, xf=F, xh=H, xu=U)
nda=data.frame(xb=B, xd=D, xf=F, xh=H, xu=U, x1=sls, x2=sms)

op1=predict(m1, newdata=nd1, type="response")
op2=predict(m2, newdata=nd2, type="response")
op3=predict(m3, newdata=nd3, type="response")
op4=predict(m4, newdata=nd4, type="response")
opc=predict(mc, newdata=ndc, type="response")
ops=predict(ms, newdata=nds, type="response")
opa=predict(ma, newdata=nda, type="response")

library(ggplot2)

date=seq(as.Date('1953-04-01'), to=as.Date('2020-12-01'), format="%Y%m" , by="month")

# Recession probabilities in 12 months
# spreads and curvature compared
orp=data.frame(date,op1,op2,op3,op4)
g=ggplot(orp, aes(date,op1)) + geom_line(aes(col="op1")) + geom_line(aes(y=op2, col="op2")) + 
  geom_line(aes(y=op3, col="op3")) + geom_line(aes(y=op4, col="op4")) + 
  theme_bw() + theme(legend.title = element_blank(), legend.position = c(0.75,0.85)) + labs(x="", y="") + 
  scale_x_date(date_breaks = "2 years", expand=c(0.01,0.09), date_minor_breaks="1 years", date_labels="%Y") +
  scale_y_continuous(breaks=seq(0,1,0.2), expand=c(0,0.001), labels = scales::percent_format(accuracy = 1)) +
  scale_color_discrete(labels=c("Treasury long-short yield spread", "Treasury median-short yield spread", 
                                "Treasury long-median yield spread", "Treasury yield curve curvature"))

xl=as.Date(c( "1957-09-01", "1960-05-01", "1970-01-01", "1973-12-01", "1980-02-01", 
              "1981-08-01",  "1990-08-01", "2001-04-01", "2008-01-01", "2020-03-01"))
xr=as.Date(c( "1958-04-01", "1961-02-01", "1970-11-01", "1975-03-01", "1980-07-01", 
              "1982-11-01", "1991-03-01", "2001-11-01", "2009-06-01", "2020-12-01"))
g + annotate("rect", xmin=xl, xmax=xr, ymin =rep(0,10), ymax =rep(1,10), alpha = .45) 


# Recession probabilities in 12 months
# spreads (two most useful) vs shapes
orp=data.frame(date,op1,op2,op3,ops)
g=ggplot(orp, aes(date,op1)) + geom_line(aes(col="op1")) + geom_line(aes(y=op2, col="op2")) + 
  geom_line(aes(y=op3, col="op3"))  +  geom_line(aes(y=ops, col="ops")) +  
  theme_bw() + theme(legend.title = element_blank(), legend.position = c(0.75,0.85)) + labs(x="", y="") + 
  scale_x_date(date_breaks = "2 years", expand=c(0.01,0.09), date_minor_breaks="1 years", date_labels="%Y") +
  scale_y_continuous(breaks=seq(0,1,0.1), expand=c(0,0.001), labels = scales::percent_format(accuracy = 1)) +
  scale_color_discrete(labels=c("Treasury long-short yield spread", "Treasury median-short yield spread", 
                                "Treasury long-median yield spread", "Treasury yield curve shapes"))

xl=as.Date(c( "1953-08-01", "1957-09-01", "1960-05-01", "1970-01-01", "1973-12-01", "1980-02-01", 
              "1981-08-01",  "1990-08-01", "2001-04-01", "2008-01-01", "2020-03-01"))
xr=as.Date(c( "1954-05-01", "1958-04-01", "1961-02-01", "1970-11-01", "1975-03-01", "1980-07-01", 
              "1982-11-01", "1991-03-01", "2001-11-01", "2009-06-01", "2020-12-01"))
g + annotate("rect", xmin=xl, xmax=xr, ymin =rep(0,11), ymax =rep(1,11), alpha = .45) 


