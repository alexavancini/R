#Vector arithmetic
1+1
2+2
2-1
2-2
2+4+6
2*10
10/2

(2+2)*(10-5)
4*5
((1+1)+(1+1))*((5+5)-(10-5))

2^2
2^4
2^6
2*2
2*2*2*2
2*2*2*2*2*2

x=10
x
y=20
y
z=30
z

x+y+z
x-y-z
(x-y)-z
(x+y)-z

(x+y+z)^2
(10+20+30)*(10+20+30)
(x+y+z)^3
(10+20+30)*(10+20+30)*(10+20+30)

sqrt(16)
sqrt(9)
sqrt(25)

w=c(10,20,30,40,50)
w
a=c(1:10)
a

a1=1.5:10
a1

b=w+a
b

#Generating regular sequences
seq(1,10,by=0.5)
seq(1,50, by=5)
f=seq(1,10,by=0.5)

s5 <- rep(i, times=5)
s5

sum(f)
a1
sum(a1)
1.5+2.5+3.5+4.5+5.5+6.5+7.5+8.5+9.5


f=seq(1,10,by=0.5)
f
f1 = seq(length=51,from=-5, by=.2)
f1
f2 = seq(length=51,from=-5, by=.1)
f2

rep(5,3)
rep(10,5)
rep(8,10)

length(w)
length(a)
length(f)

mean(w)
median(w)
mean(a)
median(a)

mean(b)
median(b)
sd(a)

c=c(11:20,1:10,21:30)
c
c1=c("alex","amauri","angela","allan")
c1
sort(c1)
sort.list(c1)

sort(c)
sort.list(c)

order(c)
rev(sort(c))
d[!Worm.density=="NA",]

mean(c)
median(c)

max(c)
min(c)
range(c)

log(10)
log(100)

log10(10)
log10(100)

log(10, base = 3.4076)
log(10, base = 2)


sin(9)
cos(9)
tan(9)

asin(1)
acos(1)
atan(1)

pi

sin(pi)
cos(pi)
tan(pi)

exp(2)

factorial(2)
factorial(4)
factorial(6)



d=(x+y+z)-mean(w)*median(c)/sqrt(y)+max(a)^4
d


e=((x+y+z)-mean(w))*(median(c)/sqrt(y))+(max(a)^4)+(log(10)-sin(9)-tan(8))+pi
e

e1=((x+y+z)-mean(w))*
  (median(c)/sqrt(y))+
  (max(a)^4)+
  (log(10)-sin(9)-tan(8))+
  pi
e1

e==e1

ceiling(1.9999)
ceiling(1.6666)
ceiling(1.2222)
ceiling(1.0001)

floor(1.9999)
floor(1.6666)
floor(1.2222)
floor(1.0001)

round(1.4)
round(1.5)

round(1.4999999999999)

round(4.6789, digits = 3)
round(4.6789, digits = 2)
round(4.6789, digits = 1)

#Logical vectors

10 > 5
10 < 5

g=20
g>15
15<g

h=50
g>50
g<50

i=20
g==i
g>=i
g<=i

g!=h
g!=i

i & g < h
i & g > h

i & h < g
i & h > g
