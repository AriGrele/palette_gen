source('hsluv.R')
source('functions.R')

library(pals)
n=100
pal.bands(ocean.algae,
          ocean.amp,
          ocean.balance,
          ocean.curl,
          ocean.deep,
          ocean.delta,
          ocean.dense,
          ocean.gray,
          ocean.haline,
          ocean.ice,
          ocean.matter,
          ocean.oxy,
          ocean.phase,
          ocean.solar,
          ocean.speed,
          ocean.tempo,
          ocean.thermal,
          ocean.turbid)
13
1,2,5,7,8,9,10,11,12,14,15,16,17,18

n=2
l=list(
  ocean.algae,#
  ocean.amp,#
  ocean.balance,
  ocean.curl,
  ocean.deep,#
  ocean.delta,
  ocean.dense,#
  ocean.gray,
  ocean.haline,
  ocean.ice,
  ocean.matter,
  ocean.oxy,
  ocean.phase,
  ocean.solar,
  ocean.speed,
  ocean.tempo,
  ocean.thermal,
  ocean.turbid)

l[[7]](2)

bars(list(,l[[7]]))

a=pal("#D9F9DC","#062109",sm=0,sv=seq(80,50,length.out=128))
test(a(100))

b=pal("#3A0913","#F4F0EF",sm=0,sv=seq(80,50,length.out=128))
test(b(100))

d=pal("#231726","#F6F5AA",sp=30)
test(d(100))

e=pal("#330E22","#E8F3F2",sp=2)
test(e(100))

rgb_hsluv(col2rgb("#E6F1F1")[,1]/255)

hsluv(340,65,10);hsluv(190,15,95)
