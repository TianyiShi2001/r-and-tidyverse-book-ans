# 斐波那契数列 (0, 1, 1, 2, 3, 5, 8, ...)或者(1, 1, 2, 3, 5, 8, ...)

fibon <- function (len = 10) {
  result <- c(1, 1)
  i = 3
  while (i <= len) {
    result[i] = result[i-1] + result[i-2]
    i = i+1
  }
  return(result)
}

########进化版

fibon <- function (len = 10) {
  result <- c(1, 1)
  if (len < 3) {
    return("斐波那契数列第0，1，2项为：(0, 1, 1)。请输入大于或等于3的整数查看更多。")
  } else {
    i = 3
    while (i <= len) {
      result[i] = result[i-1] + result[i-2]
      i = i+1
    }
  }
  result = c(0, result)
  if (len != as.integer(len)) {
    print("注意，你输入了一个非整数，将无视小数点后面的数值。")
  }
  print(paste("从第0项到第",floor(len),"项的斐波那契数列为："))
  return(result)
}


fibon(40)
fibon(-9)
fibon(9.5)

system.time(fibon(40))

a = 3.1
as.integer(a) == a

hist(rpois(10000, lambda = 5))


