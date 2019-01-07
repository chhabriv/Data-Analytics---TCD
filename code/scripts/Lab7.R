#Q1
lambd=1
k=5
set.seed(19)
#generate Uniform samples between 0,1
uniform=runif(2000, min=0, max=1)
#transform function from uniform to exponential
x=-log(1-uniform)/lambd

#Q2. sum of 5 samples 
gamma=replicate(2000,sum(-log(1-(runif(k, min=0, max=1)))/lambd))

#Q3.
Qk=3
QLambd=2
Pk=3.5
PLambd=3

point=function(k,lambd,x)
{
  m <- (1/gamma(k)) * (x^(k-1)) * ((lambd^k)) * (exp((-lambd*x)))
}

#generate points on the x axis between 0,10
t=seq(0.0001,10,by=0.0001)
q=point(Qk,QLambd,t)
p=point(Pk,PLambd,t)
m=max(p/q)
m
mq=m*q
plot(t,p,type="l",col="red",ylim = c(0,1), xlim=c(0,10),ylab="U",xlab = "X")
lines(t,q,col="green")
lines(t,mq,col="blue")
samples=c()
count=0
req=4000
for (i in 1:length(mq)) {
  x=rgamma(1,3,2)
  p=point(Pk,PLambd,x)
  q=point(Qk,QLambd,x)
  ui=runif(1,0,1)
  if(ui>p) {
    points(x,ui,type="p",pch=".",col="red")
    next
  }
  count=count+1
  samples=append(samples,x)
  points(x,ui,type="p",pch=".")
  if(count==req) break
}
length(samples)
