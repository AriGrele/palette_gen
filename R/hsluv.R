##modified from HSluv GLSL port by William Malo (https://github.com/williammalo)

radians=function(a){return(a/180*3.1415926)}                                    #convert degrees to radians

degrees=function(a){return(a/3.1415926*180)}                                    #convert radians to degrees

dot=function(v1,v2){sum(v1*v2)}                                                 #vector dot product

euclid=function(v){return(sqrt(sum(v^2)))}                                      #euclidean length of vector

norm=function(x){                                                               #scale vector between 0 and 1
  x=x-min(x)
  return(x/max(x))}

raylength=function(a,x,y){                                                      #length of ray at intersection with colorspace border
  len=y/(sin(a)-x*cos(a))
  return(ifelse(len<c(0,0,0),1000,len))}                                        #return arbitrarily large number if length negative

maxchroma=function(l,h){                                                        #maximum chroma value for given luminosity and saturation
  hrad=radians(h)
  m2=matrix(
    c(3.2409699419045214,-0.96924363628087983,0.055630079696993609,
      -1.5373831775700935,1.8759675015077207,-0.20397695888897657,
      -0.49861076029300328,0.041555057407175613,1.0569715142428786),
    ncol=3,byrow=T)
  
  sub1=(l+16.0)^3/1560896.0
  sub2=ifelse(sub1>0.0088564516790356308,sub1,l/903.2962962962963)
  
  top1=(284517.0*m2[1,]-94839.0*m2[3,])*sub2
  bottom=(632260.0*m2[3,]-126452.0*m2[2,])*sub2
  top2=(838422.0*m2[3,]+769860.0*m2[2,]+731718.0*m2[1,])*l*sub2
  
  x1=top1/bottom
  y1=top2/bottom
  x2=top1/(bottom+126452.0)
  y2=(top2-769860.0*l)/(bottom+126452.0)
  
  lengths0=raylength(hrad,x1,y1)                                                #two ray lengths at edge intersection
  lengths1=raylength(hrad,x2,y2)
  
  return(min(c(lengths0,lengths1)))}                                            #return smallest ray length

##convert linear rbg to hsluv
linear_hsluv=function(C){return(ifelse(C<=rep(0.0031308,3),12.92*C,1.055*C^(1.0/2.4)-0.055))}
##convert hsluv to linear rgb
hsluv_linear=function(C){return(ifelse(C>rep(0.04045,3),((C+0.055)/(1.0+0.055))^2.4,C/12.92))}

##convert luv luminance to xyz luminance##
l_y=function(l){return(ifelse(l<=8.0,l/903.2962962962963,((l+16.0)/116.0)^3.0))}
##convert xyz luminance to luv luminance##
y_l=function(Y){return(ifelse(Y<=0.0088564516790356308,Y*903.2962962962963,116.0*Y^(1/3)-16.0))}

#convert xyz to rgb##
xyz_rgb=function(channels){
  m=matrix(
    c(3.2409699419045214,-1.5373831775700935,-0.49861076029300328,
      -0.96924363628087983,1.8759675015077207,0.041555057407175613,
      0.055630079696993609,-0.20397695888897657,1.0569715142428786),ncol=3)
  return(linear_hsluv(channels%*%m))}
##convert rgb to xyz##
rgb_xyz=function(channels){
  m=matrix(
    c(0.41239079926595948,0.35758433938387796,0.18048078840183429,
      0.21263900587151036,0.71516867876775593,0.072192315360733715,
      0.019330818715591851,0.11919477979462599,0.95053215224966058),ncol=3)
  return(hsluv_linear(channels)%*%m)}

##convert luv to xyz##
luv_xyz=function(channels){
  U=channels[2]/(13.0*channels[1])+0.19783000664283681
  V=channels[3]/(13.0*channels[1])+0.468319994938791
  
  Y=l_y(channels[1])
  X=2.25*U*Y/V
  
  return(c(X,Y,(3./V-5.)*Y-(X/3.)))}
##convert xyz to luv##
xyz_luv=function(channels){
  L=y_l(channels[2])
  div=1/dot(channels,c(1,15,3)) 
  
  return(c(1,(52*(channels[1]*div)-2.57179),(117*(channels[2]*div)-6.08816))*L)}

##convert lch to luv
lch_luv=function(channels){
  hrad=radians(channels[3])
  return(c(channels[1],cos(hrad)*channels[2],sin(hrad)*channels[2]))}
##convert luv to lch
luv_lch=function(channels){
  C=euclid(channels[2:3])
  H=degrees(atan2(channels[3],channels[2]))
  if(H<0.0){H=360.0+H}
  
  return(c(channels[1],C,H))}

##convert hsluv to lch##
hsluv_lch=function(channels){
  channels[2]=channels[2]*maxchroma(channels[3],channels[1])*.01                #unscale chroma by maximum in hsluv
  return(rev(channels))}
##convert lch to hsluv##
lch_hsluv=function(channels){
  channels[2]=channels[2]/(maxchroma(channels[1],channels[3])*.01)              #scale chroma by maximum in hsluv
  return(rev(channels))}

##convert lch to rgb##
lch_rgb=function(channels){return(xyz_rgb(luv_xyz(lch_luv(channels))))}
##convert rgb to lch##
rgb_lch=function(channels){return(luv_lch(xyz_luv(rgb_xyz(channels))))}

##convert hsluv to rgb##
hsluv_rgb=function(channels){return(lch_rgb(hsluv_lch(channels)))}
##convert rgb to hsluv##
rgb_hsluv=function(channels){return(lch_hsluv(rgb_lch(channels)))}

##cleaner call to rgb_hsluv, and return black if out of gamut##
hsluv=function(h,s,l){
  col=hsluv_rgb(c(h,s,l))
  if(sum((is.na(col))>0)){col=c(0,0,0)}
  if(max(abs(col))>1){col=c(0,0,0)}
  if(min(col)<0){col=c(0,0,0)}
  
  return(rgb(col[1],col[2],col[3]))}