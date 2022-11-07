source('hsluv.R')
source('functions.R')
library(ggplot2)
h=runif(1,0,360)
  
p=pal(hsluv_rgb(c(h,runif(1,0,100),10)),
      hsluv_rgb(c(h-runif(1,0,200),runif(1,0,100),90)))

co=colorblind(p(256))
cowplot::plot_grid(
  volcan(co[[1]]),
  volcan(co[[2]]),
  volcan(co[[3]]),
  volcan(co[[4]]),ncol=2)

data=read.csv('../assets/scores.csv')|>
  within({all=mapply(\(a,b,c,d)mean(c(a,b,c,d)),n,p,d,t)})

a=aggregate(cbind(h,s,l)~group,\(x)x[2]-x[1],data=data)
a$mean=aggregate(all~group,FUN=\(x)max(x,na.rm=T),data=data)$all

a$h[a$h>=0]=a$h[a$h>=0]-360
a$s[a$s<=0]=a$l[a$l<=0]+100

summary(lm(mean~h+s+l,data=a))

cat('\f')
round(sapply(p(2),\(x)rgb_hsluv(col2rgb(x)[,1]/255)))



#ggplot(a,aes(floor(l/10)*10,mean))+geom_point(stat='summary')+geom_errorbar(stat='summary')+geom_smooth(method=lm)



