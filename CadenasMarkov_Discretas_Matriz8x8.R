#Simular cadenas de markov a tiempo discreto de acuerdo a una matriz de trancisión P
run.mc.sim <- function(P,num.iters=55) {
  #número posible de estados
  num.states <- nrow(P)
  
  #guadrar los X_t estados en el tiempo
  states<- numeric(num.iters)
  
  #Empezar en el primer estado
  states[1]<-1
  for(t in 2:num.iters){
    #vector de probabilidades para simular el siguiente estado X_(t+1)
    p <- P[states[t-1], ]
    
    #obtener del mulinomial y determinar el estado
    states[t]<-which(rmultinom(1,1,p)==1)
  }
  return(states)
}

#Crear matriz de transición
P <- t(matrix(c( 1/3, 2/3,   0,   0,  0,   0,   0,   0,
                 1/3, 1/3, 1/3,   0,  0,   0,   0,   0,
                 0, 1/3, 1/3, 1/3,  0,   0,   0,   0,
                 0,   0, 1/3, 1/3, 1/3,  0,   0,   0,
                 0,   0,   0, 1/3, 1/3, 1/3,  0,   0,
                 0,   0,   0,   0, 1/3, 1/3, 1/3,  0,
                 0,   0,   0,   0,   0, 1/3, 1/3, 1/3,
                 0,   0,   0,   0,   0,   0, 2/3, 1/3), nrow=8, ncol=8))

#Correr varias cadenas y graficar el proceso
#
num.chains<-2
num.iterations<-55

#cada columna almacena la secuenca de estados para cada cadena
chain.states<-matrix(NA,ncol=num.chains,nrow=num.iterations)

#simluar la cadena y graficar
for(c in seq_len(num.chains)){
  chain.states[,c]<-run.mc.sim(P)
}

matplot(chain.states, type='l', lty=1, col=1:5, ylim=c(0,9), ylab="Estado",
        xlab="Tiempo", main="Cadena de Markov a tiempo discreto", sub="Matriz de 8x8")
abline(h=1, lty=5)
abline(h=8,lty=5)


