MCoption2 <- function(M,S,K,T,r,sigma){
  T <- T #time to expiry (we have already defined this variable)
  N <- 250 #number of sub intervals
  dt <- T/N #length of each time sub interval
  time <- seq(from=0, to=T, by=dt) #time moments in which we simulate the process
  length(time) #it should be N+1
  M <- 10000 #number of simulations (paths)
  S0 <- S #initial condition (price of the underlying today)
  
  Z <- rnorm(M, mean=0, sd=1)
  WT <- sqrt(T) * Z
  ST = S0*exp((r - 0.5*sigma^2)*T + sigma*WT)
  simulated_call_payoffs <- exp(-r*T)*pmax(ST-K,0)
  Call_price_MC_anal <- mean(simulated_call_payoffs)
  return(Call_price_MC_anal)}


BScall <- function(S, K, r, T, sigma){
  
  
  d1 <- (log(S/K) + (r + sigma^2/2)*T) / (sigma*sqrt(T))
  d2 <- d1 - sigma*sqrt(T)
  
  value <- S*pnorm(d1) - K*exp(-r*T)*pnorm(d2)
  return(value)}



#example to plot
BScall(32,28,0.05,1,0.2)
MCoption2(2^8,32,28,1,0.05,0.2)
          