##return semi-random color based on input color##
rand_col=function(l){
  color=l[[1]]
  
  if(all(color!='error')){
    col=color
    col[1]=col[1]+sample(c(rnorm(1,-60,50),rnorm(1,60,50)),1)
    col[2]=runif(1,60,90)
    col[3]=ifelse(col[3]<50, runif(1,70 ,90),runif(1,0,30))
    
    if(col[3]<color[3]){return(list(col,color))}
    return(list(color,col))}
  
  return(color)}

##check that input is a color, convert to hsluv if match##
col_check=function(x){
  return(tryCatch({
    if(length(x)==3){rgb_hsluv(x)} #separate functions due to weird way r treats ifelse
    else{rgb_hsluv(col2rgb(x)[,1]/255)}},
    error=\(e)'error'))}

##generate palette based on zero, one, or two input colors##
pal=function(col1=NULL,col2=NULL,...){
  options=list(...)
  if('seed'%in%names(options)){set.seed(options$seed)}
  
  colors=list('col1'=col1,'col2'=col2)
  colors=colors[lengths(colors)>0]
  
  if(length(colors)==0){return(rand_pal(...))}
  
  for(i in 1:length(colors)){colors[[i]]=col_check(colors[[i]])}
  
  if(length(colors)==1){colors=rand_col(colors)}
  
  if(any(colors=='error')){
    cat('Error: color not recognized\n')
    return(NULL)}
  
  if(length(colors)==2){
    if(colors[[2]][3]<colors[[1]][3]){gen_pal(rev(colors),...)}
    else{gen_pal(colors,...)}}}

##generate random palette with no input colors##
rand_pal=function(...){
  cols=c(runif(1,0,360),runif(1,10,60),runif(1,10,25))
  pal(hsluv_rgb(cols),...)}

##generate palette with direct input to intial and final color, chroma, saturation, and luminance##
gen_pal=function(cols,...){
  start=cols[[1]]
  if(start[3]>30){start[3]=runif(1,10,25)}
  end=cols[[2]]
  if(end[3]<65){end[3]=runif(1,70,85)}
  
  parms=list(...)
  
  default=c('cv'=0,'sv'=0,'lv'=0,'cm'=1,'sm'=1,'lm'=1,'cp'=1,'sp'=1,'lp'=1,'csm'=0)
  for(i in names(default)){if(is.null(parms[[i]])){parms[[i]]=default[i]}}
  
  parms=within(parms,{
    chroma=    cv+csm*sin((1:128)/128*3.14159*2)+cm*((seq(start[1]^cp,end[1]^cp,length.out=128))^(1/cp))
    saturation=sv+sm*((seq(start[2]^sp,end[2]^sp,length.out=128))^(1/sp))*.98+1
    luminosity=lv+lm*((seq(start[3]^lp,end[3]^lp,length.out=128))^(1/lp))*.98+1
    
    ramp=mapply(\(h,s,l)hsluv(h,s,l),chroma,saturation,luminosity)})
  
  return(colorRampPalette(parms$ramp))}

##generate palette with major color clusters from chosen image##
from_img=function(path,...){
  img=imager::load.image(path)|>
    as.data.frame()|>
    reshape2::dcast(x+y~cc,fun.aggregate=\(x)x[1])|>
    setNames(c('x','y','r','g','b'))
  
  km=kmeans(img[-2:0],10)
  major=rgb(km$centers[,1],km$centers[,2],km$centers[,3])[1:4]
  
  palettes=sapply(major,\(x)sapply(major[1:match(x,major)],\(y)if(y!=x){pal(x,y,...)}))|>
    unlist()
  return(palettes)}

##convert colors to color vision modes##
colorblind=function(colors){
  return(list(
    'Trichromatic'=colors,
    'Protanopia'  =colorspace::simulate_cvd(colors,colorspace::interpolate_cvd_transform(colorspace::protanomaly_cvd)),
    'Deutanopia'  =colorspace::simulate_cvd(colors,colorspace::interpolate_cvd_transform(colorspace::deutanomaly_cvd)),
    'Tritanopia'  =colorspace::simulate_cvd(colors,colorspace::interpolate_cvd_transform(colorspace::tritanomaly_cvd)),
    'Achromomatic'  =colorspace::desaturate(colors)))}

##plot comparisons between color vision modes##
safe=function(pal){
  if(!is.character(pal)){pal=pal(256)}
  
  types=colorblind(pal)
  plots=lapply(types,\(p){
    ggplot2::ggplot()+
      ggplot2::geom_tile(ggplot2::aes(1:length(p),1),fill=p,color=NA)+
      ggplot2::theme_void()+
      ggplot2::ggtitle(names(types)[match(list(p),types)])+
      ggplot2::theme(plot.title=ggplot2::element_text(hjust=.5))})
  
  grid=do.call(cowplot::plot_grid,c(plots,'ncol'=1))
  return(grid)}

##plot volcano dataset with given pal##
volcan=function(pal){
  dv=dim(volcano)
  data=data.frame('x'=rep(1:dv[1],dv[2]),'y'=rep(1:dv[2],each=dv[1]))|>
    within({
      d=as.vector(volcano)
      d=norm(d)
      f=round(d*(length(pal)-1))})
  
  return(
    ggplot2::ggplot(data,ggplot2::aes(x,y,fill=f))+
      ggplot2::geom_tile()+
      ggplot2::theme_void()+
      ggplot2::scale_fill_gradientn(colors=pal)+
      ggplot2::guides(fill='none'))}

##plot color channels and luminance##
channels=function(pal){
  data=t(col2rgb(pal))|>
    as.data.frame()|>
    within({lum=col2rgb(colorspace::desaturate(pal))[1,]})
  
  return(
    ggplot2::ggplot(data,ggplot2::aes(x=1:nrow(data)))+
      ggplot2::geom_line(ggplot2::aes(y=red),  color='red',  size=2)+
      ggplot2::geom_line(ggplot2::aes(y=green),color='green',size=2)+
      ggplot2::geom_line(ggplot2::aes(y=blue), color='blue', size=2)+
      ggplot2::geom_line(ggplot2::aes(y=lum),  color='grey', size=3)+
      ggplot2::theme_classic()+
      ggplot2::ylab('')+
      ggplot2::xlab(''))}

##plot z-order curve##
zorder=function(pal){
  z=matrix(c(0, 1, 2, 3),byrow=T,ncol=2)
  for(i in c(4,16,64)){z=rbind(cbind(z,i+z),i*2+cbind(z,i+z))}
  
  data=data.frame('x'=rep(1:16,16),'y'=rep(1:16,each=16))|>
    within({
      d=as.vector(z)
      d=norm(d)
      f=round(d*(length(pal)))})
  
  return(
    ggplot2::ggplot(data,ggplot2::aes(x,y,fill=f))+
      ggplot2::geom_tile()+
      ggplot2::theme_void()+
      ggplot2::scale_fill_gradientn(colors=pal)+
      ggplot2::guides(fill='none'))}

##plot z-order curve, volcano, channels, and color modes##
test=function(pal){
  return(
    cowplot::plot_grid(
      zorder(pal),
      volcan(pal),
      channels(pal),
      safe(pal)))}