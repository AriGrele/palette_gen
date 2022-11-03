source('hsluv.R')
source('functions.R')

palette=pal()                                                                   #random colors
test(palette(256))

palette=pal('gold')                                                             #one fixed color
test(palette(256))

palette=pal('lightblue','darkgreen')                                            #two fixed colors
test(palette(256))

palettes=pal('../assets/monarch.jpg')                                           #grab colors from image
bars(palettes)
test(palettes[[1]](256))

palette=pal('#1A1525','pink',mode='diverging')                                  #diverging palette
test(palette(256))

palette=pal(mode='qualitative')                                                 #qualitative palette
test(palette(6))  

palette=mix(pal(),pal())                                                        #mixed color palettes
test(palette(10))

pal(seed=1,cv=0,sv=0,lv=0,cm=1,sm=1,lm=1,cp=1,sp=1,lp=1,sinv=0,sinm=1)|>safe()  #full suite of modifiers 

pal(seed=1,sm=0,sv=0)(100)|>safe()                                              #very desaturated

pal(seed=1,sm=0,sv=80)(100)|>safe()                                             #very saturated

vals=30*log10(seq(10^2,10^.1,length.out=128))                                   #increasing saturation
pal(seed=1,sm=0,sv=vals)(100)|>safe()

pal(seed=1,sm=0,sv=80,lp=2)(100)|>safe()                                        #emphasize light shades

pal(seed=1,sm=0,sv=80,lp=.5)(100)|>safe()                                       #emphasize dark shades

pal(seed=1,sm=0,sv=80,lp=.5,cm=5)(100)|>safe()                                  #large chroma range

pal(seed=1,sm=0,sv=80,lp=.5,sinv=5)(100)|>safe()                                #sinusoidal chroma

pal(seed=1,sm=0,sv=80,lp=.5,sinv=1000)(10)|>safe()                              #very sinusoidal chroma

##plotting options##
palette=pal('#281A2C','#FDFECC')

bars(list(palette))                                                             #example gradients from list of pals

safe(palette(100))                                                              #comparison of color vision modes

volcan(palette(100))                                                            #volcano plot

channels(palette(100))                                                          #color channels and luminosity 

zorder(palette(100))                                                            #z-order curve of gradient

test(palette(100))                                                              #combination of diagnostic plots

library(ggplot2)
ggplot(iris,aes(Sepal.Length,Sepal.Width,color=Species))+                       #discrete colors
  geom_point(size=4)+
  scale_color_manual(values=palette(3))

ggplot(iris,aes(Sepal.Length,Sepal.Width,color=Sepal.Width))+                   #continuous colors
  geom_point(size=4)+
  scale_color_gradientn(colors=palette(256))