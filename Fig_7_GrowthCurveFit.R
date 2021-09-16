library(minpack.lm)
library(ggplot2)
library(ggthemes)
dat <- read.csv('mie_fig7_GrowthCurveFit.csv')
y <- dat$RFU
t <- dat$time_h
rich <-nlsLM(y~a*(1+v*exp(k*(l-t)))^(-1/v),start=list(l=2,a=12000,k=5,v=5))
ypred <- predict(rich,t)
v <- ggplot(data = dat,aes(x=t,y=y))+geom_point(shape=".")+geom_line(aes(x=t,y=ypred))+theme_tufte()
w <- v+theme(axis.line = element_line(size = 0.25, colour = "black"))+theme(text=element_text(size=12),legend.position="None")
ggsave(plot=w,width = 4, height = 3, dpi = 300, filename = "miefig7.pdf")
