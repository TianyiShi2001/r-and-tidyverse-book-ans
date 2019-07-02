##############################

primeNumbers <- 2L

for (num in 3:400000) {
  isPrime <- TRUE
  for (pnum in primeNumbers) {
    if (pnum > sqrt(num)) {
      break
    } else if (num %% pnum == 0) {
      isPrime <- FALSE
      break
    }
  }
  if (isPrime) {
    primeNumbers <- append(primeNumbers, num)  ## common mistake
  }
}

###############################
# 用for loop

prime.list <- function(i){
  primeNumbers <- 2L
  for (num in 3:i) {
    isPrime <- TRUE
    for (pnum in primeNumbers) {
      if (pnum > sqrt(num)) {
        break
      } else if (num %% pnum == 0) {
        isPrime <- FALSE
        break
      }
    }
    if (isPrime) {
      primeNumbers <- append(primeNumbers, num)  ## common mistake
    }
  }
  return(primeNumbers)
}

primeNumbers <- 2L
primeNumbers <- prime.list(400000)
pNwIndex <- data.frame(index = 1:length(primeNumbers),num = primeNumbers)
plot(pNwIndex$index[1:100],pNwIndex$num[1:100])

####################################
# 虽然看起来简洁，实际上对于计算机来说更复杂。耗时更久。

prime.list.any <- function(i){
  primeNumbers <- 2L
  for (num in 3:i) {
    isPrime <- !any(num %% primeNumbers[1:sum(primeNumbers <= sqrt(num))] == 0)
    if (isPrime) {
      primeNumbers <- append(primeNumbers, num)
    }
  }
  return(primeNumbers)
}

####################################
# 用all

prime.list.all <- function(i){
  primeNumbers <- 2L
  for (num in 3:i) {
    isPrime <- all(num %% primeNumbers[1:sum(primeNumbers <= sqrt(num))] != 0)
    if (isPrime) {
      primeNumbers <- append(primeNumbers, num)
    }
  }
  return(primeNumbers)
}

#####################################
#用while


prime.list.while <- function(i){
  primeNumbers <- 2L
  for (num in 3:i) {
    isPrime <- TRUE
    j = 1
    while (primeNumbers[j] <= sqrt(num)){
      if (num %% primeNumbers[j] == 0) {
        isPrime <- FALSE
        break
      }
      j = j+1
    }
    if (isPrime) {
      primeNumbers <- append(primeNumbers, num)  ## common mistake
    }
  } 
  return(primeNumbers)
}

#############################

n1 <- 100000
system.time(prime.list(n1))
system.time(prime.list.any(n1))
system.time(prime.list.while(n1))
system.time(prime.list.all(n1))

##########################Speed
library(tidyverse)

number <- seq(10000,100000,5000)

time <- vector("numeric")
for(i in number) {
  time <- append(time, system.time(prime.list(i))[3])
}
timeData <- tibble(func = "prime.list", number = number, time = time)

for(i in number) {
  timeData <- add_row(timeData, func = "prime.list.while", number = i, time = system.time(prime.list.while(i))[3])
}


for(i in number) {
  timeData <- add_row(timeData, func = "prime.list.any", number = i, time = system.time(prime.list.any(i))[3])
}

for(i in number) {
  timeData <- add_row(timeData, func = "prime.list.all", number = i, time = system.time(prime.list.all(i))[3])
}


ggplot(data = timeData, mapping = aes(x=number, y = time, color = func))+
  geom_point()+
  geom_smooth(se=FALSE, method = 'loess', formula = 'y ~ x')+
  theme_light()

## is.prime

is.prime <- function(n) n == 2L || all(n %% 2L:ceiling(sqrt(n)) != 0)

is.prime(999983)

primeNumbers <- 2L
for (num in 3:400000) {
  if(is.prime(num)){
    primeNumbers <- append(primeNumbers,num)
  }
}


## Number of pnums
primeNumbers <- 2L
numberPrimes <- 1
numberPrimesList <- 1L
for (num in 3:10000) {
  isPrime <- TRUE
  for (pnum in primeNumbers) {
    if (pnum > sqrt(num)) {
      break
    } else if (num %% pnum == 0) {
      isPrime <- FALSE
      break
    }
  }
  if (isPrime) {
    numberPrimes <- numberPrimes + 1
    numberPrimesList <- append(numberPrimesList, numberPrimes)
    primeNumbers <- append(primeNumbers, num)
  } else {
    numberPrimesList <- append(numberPrimesList, numberPrimes)
  }
}

n <- 2:10000
plot(n[9000:9300],numberPrimesList[9000:9300])


length(prime.list.all(n1))
