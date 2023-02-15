#Movimiento Browniano simple unidimensional
N <- 1000
dis <- rnorm(N,0,1)
dis_1 <- 2*0.1 - dis
dis <- cumsum(dis)
dis_1 <- cumsum(dis_1)
plot(dis, 
     type = "l", main = "Brownian Motion in one dimention", xlab = "Time", ylab = "Displacement", col = "blue")
#lines(dis_1, col = "orange")