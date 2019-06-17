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


plot(primeNumbers)



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
