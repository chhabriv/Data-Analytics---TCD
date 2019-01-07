max=100
randomSamples=10000
x=c()
x=replicate(max,runif(randomSamples),simplify = FALSE)
y=c()

#for(i in 1:max)
#{
#  x[[i]]=runif(10000)
#}

for(i in 1:max)
{
  sumVector=c()
  totalVector=x[[1]]
  if(i>1)
  {
    for(j in 2:i)
    {
      sumVector=x[[j]]
      totalVector=totalVector+sumVector
    }
  }
  y[[i]]=totalVector
}

hist(y[[1]])
hist(y[[2]])
hist(y[[5]])
hist(y[[10]])
hist(y[[30]])
hist(y[[100]])

x1=runif(10000)
x2=runif(10000)
x3=runif(10000)
x4=runif(10000)
y1=x1
hist(y1)
y2=x1+x2
hist(y2)
y3=x1+x2+x3
hist(y3)
y4=x1+x2+x3+x4
hist(y4)