fibon <- function (len = 10) {
  if (len == as.integer(len) & len > 0) {
    result = c(1, 1)
    i = 3
    while (i <= len) {
      result[i] = result[i-1] + result[i-2]
      i = i+1
    }
    return(result[1:len])
  } else {
    return("请输入一个正整数。")
  }
}

