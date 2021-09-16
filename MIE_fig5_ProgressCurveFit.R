library(ggplot2)
library(ggthemes)
library(lamW)
library(minpack.lm)	
dat <- read.csv("MIE_fig5_ProgressCurveFit.csv")
t <- dat$t
P <- dat$y
g <- ggplot(data = dat, aes(x=t, y=P)) + geom_point(shape = ".")+theme_tufte()+theme(axis.line = element_line(size = 0.25, colour = "black"))+theme(text=element_text(size=16))+scale_y_continuous(breaks=seq(0,120,by=20))+scale_x_continuous(breaks=seq(0,120,by=20))
s <- 135 #this is S0 value determined from the plateau of the progress curve
IntMM <- nlsLM(P~s-K*lambertW0(s/K*exp((s-V*t*60)/K)),start=list(K=10,V=2)) 
summary(IntMM)
Ppred <- predict(IntMM,t)
g2 <- ggplot(data = dat) + geom_point(aes(x=t,y=P),shape = ".")+geom_line(aes(x=t,y=Ppred))+theme_tufte()+theme(axis.line = element_line(size = 0.25, colour = "black"))+theme(text=element_text(size=16))+scale_y_continuous(breaks=seq(0,120,by=20))+scale_x_continuous(breaks=seq(0,120,by=20))
ggsave(plot = g2, width = 4, height = 3, dpi = 300, filename = "miefig3.pdf")
