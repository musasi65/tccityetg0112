
library(rlist)
??rlist

x <- list(p1 = list(type='A',score=list(c1=10,c2=8)),
          p2 = list(type='B',score=list(c1=9,c2=9)),
          p3 = list(type='B',score=list(c1=9,c2=7)))
m <- List(x)
m$filter(type=='B')$
  map(score$c1) []

m$group(type)$
  map(g ~ List(g)$
        map(score)$
        call(unlist)$
        call(mean) []) []


map()	# low resolution map of the world
map('usa')
install.packages('map')


# Subsetting, extracting, and assigning

p <- List(list(a=1,b=2))
p

str(p['a'])
str(p[['a']])
p$a <- 2

p
p['b'] <- NULL
p[['a']] <- 3
p
p['a']
