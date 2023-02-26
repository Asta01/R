d <- 20
W <- 2
M <- 100
a_0 <- 4
a_1 <- -2
b <- 0.5
X <- matrix(0,d+2,d+2)

N_x <- function(){
  N <- 0
  for(i in 2:(d+1)){
    for(j in 2:(d+1)){
      N <- N + X[i,j]*X[i+1,j] + X[i,j]*X[i,j+1]
    }
  }
  return(N)
}

E_x <- function(){
  N <- N_x()
  S <- sum(X)
  E <- exp(-b*a_0*S-b*a_1*N)
  return(E)
}

N <- numeric(M)
S <- numeric(M)
for(v in 1:M){
  for (i in 2:(d+1)){
    for (j in 2:(d+1)){
      X[i,j] <- rbinom(1,1,0.5)
    }
  }
  
  for (k in 1:W){
    for (i in 2:d+1){
      for (j in 2:d+1){
        X[i,j] <- 0
        pi_0 <- E_x()
        X[i,j] <- 1
        pi_1 <- E_x()
        PI_1 <- pi_1/(pi_1+pi_0)
        X[i,j] <- rbinom(1,1,PI_1)
      }
    }
  }
  N[v] <- N_x()
  S[v] <- sum(X)
}

E <- a_0*S+a_1*N
hist(E)
