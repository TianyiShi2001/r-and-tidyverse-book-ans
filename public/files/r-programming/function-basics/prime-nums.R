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
    primeNumbers <- c(primeNumbers, num)  ## common mistake
  }
}

###############################
# 用for loop

prime.list.for <- function(i){
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
      primeNumbers <- c(primeNumbers, num)
    }
  }
  return(primeNumbers)
}

primeNumbers <- 2L
primeNumbers <- prime.list.for(400000)
pNwIndex <- data.frame(index = 1:length(primeNumbers),num = primeNumbers)
plot(pNwIndex$index[1:100],pNwIndex$num[1:100])



# any ---------------------------------------------------------------------
# 虽然看起来简洁，实际上对于计算机来说更复杂。耗时更久。

prime.list.any <- function(i){
  primeNumbers <- 2L
  for (num in 3:i) {
    isPrime <- !any(num %% primeNumbers[1:sum(primeNumbers <= sqrt(num))] == 0)
    if (isPrime) {
      primeNumbers <- c(primeNumbers, num)
    }
  }
  return(primeNumbers)
}


# 用all --------------------------------------------------------------------



prime.list.all <- function(i){
  primeNumbers <- 2L
  for (num in 3:i) {
    isPrime <- all(num %% primeNumbers[1:sum(primeNumbers <= sqrt(num))] != 0)
    if (isPrime) {
      primeNumbers <- c(primeNumbers, num)
    }
  }
  return(primeNumbers)
}

# 用while ------------------------------------------------------------------

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
      primeNumbers <- c(primeNumbers, num)  ## common mistake
    }
  } 
  return(primeNumbers)
}


# 用`%in%` --------------------------------------------------------------

prime.list.in <- function(i){
  primeNumbers <- 2L
  for (num in 3:i) {
    isPrime <- TRUE
    test <- primeNumbers[primeNumbers<=floor(sqrt(num))]
    isPrime <- !(0 %in% (num %% test))
    if (isPrime) {
      primeNumbers <- c(primeNumbers, num)  
    }
  }
  return(primeNumbers)
}


# 用apply ------------------------------------------------------------------

prime.list.map <- function(i){
  primeNumbers <- 2L
  map(3:i, ~{
    test <- primeNumbers[primeNumbers<=floor(sqrt(.))]
    if (all(.%%test != 0)) primeNumbers <<- c(primeNumbers, .)
    })
  return(primeNumbers)
}

prime.list.map1 <- function(i){
  primeNumbers <- 2L
  map(3:i, ~{
    test <- primeNumbers[primeNumbers<=floor(sqrt(.))]
    if (!(0 %in% (.%%test))) primeNumbers <<- c(primeNumbers, .)
  })
  return(primeNumbers)
}

library(purrr)

prime.list.map2 <- function(i){
  primeNumbers <- 2
  map(3:i, ~{
    test <- primeNumbers[primeNumbers<=floor(sqrt(.))]
    if (is.na(match(0, .%%test))) primeNumbers <<- c(primeNumbers, .)
  })
  return(primeNumbers)
}

prime.list.lapply <- function(i){
  primeNumbers <- 2
  lapply(3:i, function(x){
    test <- primeNumbers[primeNumbers<=floor(sqrt(x))]
    if (is.na(match(0, x%%test))) primeNumbers <<- c(primeNumbers, x)
  })
  return(primeNumbers)
}

# 小测速度 --------------------------------------------------------------

n1 <- 100000
system.time(prime.list.for(n1))
system.time(prime.list.any(n1))
system.time(prime.list.while(n1))
system.time(prime.list.all(n1))
system.time(prime.list.in(n1))
system.time(prime.list.map(n1))
system.time(prime.list.map2(n1))
system.time(prime.list.lapply(n1))

prime.list.in(n1)
prime.list.for(n1)
prime.list.map1(n1)
prime.list.map2(n1)
prime.list.lapply(n1)


# 速度绘图 --------------------------------------------------------------
library(tidyverse)

number <- seq(10000,100000,5000)

time <- vector("numeric")
for(i in number) {
  time <- c(time, system.time(prime.list.for(i))[3])
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

# `is.prime` --------------------------------------------------------------

is.prime <- function(n) n == 2L || all(n %% 2L:ceiling(sqrt(n)) != 0)

is.prime(999983)

primeNumbers <- 2L
for (num in 3:400000) {
  if(is.prime(num)){
    primeNumbers <- c(primeNumbers,num)
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
    numberPrimesList <- c(numberPrimesList, numberPrimes)
    primeNumbers <- c(primeNumbers, num)
  } else {
    numberPrimesList <- c(numberPrimesList, numberPrimes)
  }
}

n <- 2:10000
plot(n[9000:9300],numberPrimesList[9000:9300])


length(prime.list.all(n1))
