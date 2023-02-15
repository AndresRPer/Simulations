#Movimiento Browniano Bidimensional con proceso de llegada de Poisson
n=1000
xdis <- rnorm(n,0,1)
ydis <- rnorm(n,0,1)
xdis <- cumsum(xdis)
ydis <- cumsum(ydis)
at <- rpois(n,1)
for(i in n)
  if (at[i] !=0)
    xdis[i] <- xdis[i] * at[i]
    ydis[i] <- ydis[i] * at[i]
plot(xdis,ydis,type="l",main="Moviento Browniano Bidimensional con proceso de llegada de Poisson",
     xlab="Desplazamiento en X",ylab="Desplazamiento en Y", col="gold")
