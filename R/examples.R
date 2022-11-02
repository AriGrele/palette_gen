source('hsluv.R')
source('functions.R')

palette=pal()#random colors
test(palette(256))

palette=pal('gold')#one fixed color
test(palette(256))

palette=pal('lightblue','darkgreen')#two fixed colors
test(palette(256))

#grab colors from image

#diverging palette

#qualitative palette

#mixed color palettes




pal(seed=1,cv=0,sv=0,lv=0,cm=1,sm=1,lm=1,cp=1,sp=1,lp=1,csm=0)|>safe()#full suite of modifiers 

pal(seed=1,sm=0,sv=0)(100)|>safe()#very desaturated

pal(seed=1,sm=0,sv=80)(100)|>safe()#very saturated

pal(seed=1,sm=0,sv=80,lp=2)(100)|>safe()#emphasize light shades

pal(seed=1,sm=0,sv=80,lp=.5)(100)|>safe()#emphasize dark shades

pal(seed=1,sm=0,sv=80,lp=.5,cm=5)(100)|>safe()#large chroma range

pal(seed=1,sm=0,sv=80,lp=.5,csm=5)(100)|>safe()#sinusoidal chroma

pal(seed=1,sm=0,sv=80,lp=.5,csm=1000)(10)|>safe()#very sinusoidal chroma

