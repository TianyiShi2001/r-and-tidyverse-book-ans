#########精简版
X_squared <- matrix(rep(rnorm(1000000), 6), nrow = 6)^2

Q <- matrix(nrow = 6, ncol = 1000000)

for (i in (1+1):6) {
  Q[1,] = X_squared[1,]
  Q[i,] = Q[(i-1),] + X_squared[i,]
}

plot(NULL, xlim=c(0.23,6), ylim = c(0,1),
     main = expression(paste('X ~ ', chi^'2', '(k)')), 
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


X1 = rnorm(1000000)
X2 = rnorm(1000000)
X3 = rnorm(1000000)
X4 = rnorm(1000000)
X5 = rnorm(1000000)
X6 = rnorm(1000000)

Q1 = X1^2
Q2 = X1^2 + X2^2
Q3 = X1^2 + X2^2 + X3^2
Q4 = X1^2 + X2^2 + X3^2 + X4^2
Q5 = X1^2 + X2^2 + X3^2 + X4^2 + X5^2
Q6 = X1^2 + X2^2 + X3^2 + X4^2 + X5^2 + X6^2

plot(NULL, xlim=c(0.23,6), ylim = c(0,1),
     main = expression(paste('X ~ ', chi^'2', '(k)')), 
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