x=c(12.8,10.5,13.2,13.0,7.0,11.0,13.4,13.2,9.5,11.0,10.9,4.6,5.8,3.2,9.8,0.2,11.2,7.2,14.7,5.9,9.7,17.6,
    8.5,6.8,7.2,12.2,16.7,10.4,14.2,5.7)
mu=8
sigSq=4
alpha=5
beta=1
tao=1/var(x)
tao0=1/sigSq
xmean=mean(x)
n=length(x)

muestimate=function(n,xmean,tao,mu,tao0){
  meanofrnorm = ((n*xmean*tao)+(mu*tao0))/((n*tao)+tao0)
  sdofnorm= ((n*tao)+tao0)^-1
  samplesformu = rnorm(1000, meanofrnorm,sdofnorm)
  estimate=mean(samplesformu)
  }

#samplesformumean = muestimates(n,xmean,tao,mu,tao0)


# gammaDistr=function(alpha,beta,S){
#   value=(1/gamma(alpha))*(((beta*tao)^(alpha-1))*(exp(-beta*tao)))
# }

taoestimate=function(n,x,samplesformumean,alpha,beta){
  S=sum((x-samplesformumean)^2)
  gammashape=(alpha+(n/2))
  gammarate=(beta+(S/2))
  samplesfortao=rgamma(1000,gammashape,gammarate)
  estimate=mean(samplesfortao)
  }

#samplesfortaomean=taoestimate(n,x,samplesformumean,alpha,beta)

limit=500
cmuestimate=c()
csigestimate=c()
for (i in 1:limit) {
  samplesformumean = muestimates(n,xmean,tao,mu,tao0)
  samplesfortaomean= taoestimate(n,x,samplesformumean,alpha,beta)
  cmuestimate=append(cmuestimate,samplesformumean)
  csigestimate=append(csigestimate,1/samplesfortaomean)
  tao=samplesfortaomean
}
plot(cmuestimate,type="l",xlim=c(1,500),ylim=c(min(cmuestimate),max(ctaoestimate)),col="green")
lines(csigestimate,type="l",col="blue")

length(cmuestimate)
mean(cmuestimate)
length(csigestimate)
mean(csigestimate)
