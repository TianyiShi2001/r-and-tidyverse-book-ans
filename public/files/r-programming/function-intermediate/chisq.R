#########精简版
Z <- matrix(rep(rnorm(1000000), 6), nrow = 6)^2

X <- Z^2

Q <- matrix(nrow = 6, ncol = 1000000)

for (i in (1+1):6) {
  Q[1,] = Z[1,]
  Q[i,] = Q[(i-1),] + Z[i,]
}

plot(NULL, xlim=c(0.23,6), ylim = c(0,1),
     main = expression(paste('Q ~ ', chi^'2', '(k)')), 
     xlab = "x", 
     ylab= expression(f[k]*'(x)')
    )
colors <- c('blue', 'black', 'red', 'green', 'gray', 'orange')
for (i in 1:6) {
  lines(density(Q[i,]),
        col=colors[i],
        lwd=2)
}
legend('topright',c('k=1','k=2','k=3','k=4','k=5','k=6'),
       fill = colors)

#########易懂版


Z1 = rnorm(1000000)
Z2 = rnorm(1000000)
Z3 = rnorm(1000000)
Z4 = rnorm(1000000)
Z5 = rnorm(1000000)
Z6 = rnorm(1000000)

Q1 = Z1^2
Q2 = Z1^2 + Z2^2
Q3 = Z1^2 + Z2^2 + Z3^2
Q4 = Z1^2 + Z2^2 + Z3^2 + Z4^2
Q5 = Z1^2 + Z2^2 + Z3^2 + Z4^2 + Z5^2
Q6 = Z1^2 + Z2^2 + Z3^2 + Z4^2 + Z5^2 + Z6^2

plot(NULL, xlim=c(0.23,6), ylim = c(0,1),
     main = expression(paste('Q ~ ', chi^'2', '(k)')), 
     xlab = "x", 
     ylab= expression(f[k]*'(x)')
)
lines(density(Q1),col='blue',lwd=2)
lines(density(Q2),col='black',lwd=2)
lines(density(Q3),col='red',lwd=2)
lines(density(Q4),col='green',lwd=2)
lines(density(Q5),col='gray',lwd=2)
lines(density(Q6),col='orange',lwd=2)
legend('topright',c('k=1','k=2','k=3','k=4','k=5','k=6'),
       fill = c('blue','black','red','green','gray','orange'))

########### 直接套公式

chisq_pdf <- function(x, k = 6){
  return(
    x^(k/2-1)*exp(-x/2)/
    (2^(k/2)*gamma(k/2))
  )
}


plot(NULL, xlim=c(0,10), ylim = c(0,1),
     main = expression(paste('Q ~ ', chi^'2', '(k)')), 
     xlab = "x", 
     ylab= expression(f[k]*'(x)')
)

colors <- c('blue', 'black', 'red', 'green', 'gray', 'orange')

for (i in 1:6) {
  curve(chisq_pdf(x, i), 0 ,10, col = colors[i], add = TRUE)
}

legend('topright',c('k=1','k=2','k=3','k=4','k=5','k=6'),
       fill = colors)






