m <- 1000            #number of replicas
mu_verd <- 300       #true value of the population parameter
sigma <- 50          #variance parameter of the population
mu_test <- c(seq(250, 450, 10))   #test values
M <- length(mu_test)              #number of Monte Carlo replicas
power <- numeric(M)               #vector to storage the empirrical power of the test
nobs <- c(20,30,40,50,100)        #sampling size
power_nobs <- matrix(0,length(nobs),M)
c <- 1
for(j in nobs) {
  for (i in 1:M) {
    mu <- mu_test[i]
    p_value <- replicate(m, expr = {
      x <- rnorm(j, mean = mu, sd = sigma)
      test_t <- t.test(x, alternative = "greater", mu = mu_verd)
      test_t$p.value})
    power[i] <- mean(p_value <= 0.05)
  }
  power_nobs[c,] <- power
  c = c+1
}
