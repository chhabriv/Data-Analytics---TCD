start_time <- Sys.time()

totalCount=1000000
totalCountDecrementer=1000000
primeCount=0

while (totalCountDecrementer!=0) {
  
  x=sample(1:1000000,1)
  y=sample(1:1000000,1)
  
  smaller=min(x,y)
  
  prime=TRUE
  
  for(i in 2:sqrt(smaller)){
    if(x%%i==0 & y%%i==0) {
      prime=FALSE
      break
    }
  }
  #print(x)
  #print(y)
  if(prime) 
  {
    #print("Numbers are prime to each other")
    primeCount=primeCount+1
  }
  #else {
    #print("Numbers are not prime to each other")
  #}
  totalCountDecrementer=totalCountDecrementer-1
  #print(totalCountDecrementer)
}
print(paste("Probability that 2 numbers are prime to each other is: ",primeCount/totalCount))
end_time <- Sys.time()
print(start_time)
print(end_time)
print(end_time - start_time)
