x=c(19,12,8,4,3,3,1)
n=sum(x)
#assume
x0=10
x1=x[1]-x0

meanF=function(x0,x1){
f=c(x0,x1,x[2],x[3],x[4],x[5],x[6],x[7])
fmean=mean(f)
fmean
}

getPoissonValues=function(x,lambd){
  value= ((exp(-lambd))*(lambd^x))/factorial(x)
  value
}

limit=500
cf0=c()
cf1=c()
cLambdEstimate=c()
for (i in 1:limit) {
  xMean=meanF(x0,x1)
  a=(xMean*n)+1
  b=0.4+n
  samples=rgamma(1000,a,b)
  lambdEst=mean(samples)
  print(lambdEst)
  cLambdEstimate=append(cLambdEstimate,lambdEst)
  x0=(getPoissonValues(0,lambdEst))*50
  x1=x[1]-x0
  cf0=append(cf0,x0)
  cf1=append(cf1,x1)
}