#ex1


# Example usage
death <- c(0, 1, 2, 3, 4, 0)
obs_1 <- c(109, 65, 22, 3, 1, 0)
obs_2 <- c(144, 91, 32, 11, 2, 0)

calculate_sigma <- function(death, obs) {
  sigma_y <- 0
  for (i in 1:6) {
    sigma_y <- sigma_y + (obs[i] * death[i])
  }
  return(sigma_y)
}

sigma_y_1 <- calculate_sigma(death, obs_1)
sigma_y_2 <- calculate_sigma(death, obs_2)

n_1 <- sum(obs_1)
n_2 <- sum(obs_2)


n.sample <- 1000
delta.lambda <- 1/n.sample


#foe n1 observertion
alpha <- 1 + sigma_y_1
mu <- n_1
lambda <- seq(from = 0 , by = delta.lambda , length.out = n.sample )

lambda_posterior <- dgamma(lambda , alpha , mu)


mean <- delta.lambda * sum(lambda * lambda_posterior)
variance <- delta.lambda * sum(lambda ^ 2 * lambda_posterior) - mean ^ 2
median <- qgamma(0.5 , alpha , mu )
ci <- qgamma(c(0.025 , 0.975) , alpha , mu)



plot(lambda , lambda_posterior  , type = 'l' ,lwd = 1.7 ,  col = 'navyblue')



#print the results
cat("mean: ", round(mean, 5), "\n")
cat("variance: ", round(variance, 5), "\n")
cat("median: ", round(median, 5), "\n")
plot(lambda , lambda_posterior  , type = 'l' ,lwd = 1.7 ,  col = 'navyblue')
abline(v = ci[1] , lty = 'dashed' , lwd = 1.2, col = 'orange')
abline(v = ci[2] , lty = 'dashed' , lwd = 1.2, col = 'orange')





plot(lambda, lambda_posterior, type = 'l', lwd = 1.7, col = 'navyblue')
abline(v = mean, lty = 'dashed', lwd = 1.2, col = 'green')
abline(v = median, lty = 'dashed', lwd = 1.2, col = 'red')
abline(v = variance , lty = 'dashed' , lwd = 1.2 , col = 'pink')
abline(v = ci[1], lty = 'dashed', lwd = 1.2, col = 'orange')
abline(v = ci[2], lty = 'dashed', lwd = 1.2, col = 'orange')

# Legend
legend("topleft", legend = c( "Mean", "Median", "Variance"), 
       lty = c("dashed", "dashed", "dashed"),
       lwd = c(1.2, 1.2, 1.2),
       col = c( "green", "red", "pink" ))


#for n2 obsevation

alpha <- 1 + sigma_y_2
mu <- n_2
lambda <- seq(from = 0 , by = delta.lambda , length.out = n.sample )

lambda_posterior <- dgamma(lambda , alpha , mu)


mean <- delta.lambda * sum(lambda * lambda_posterior)
variance <- delta.lambda * sum(lambda ^ 2 * lambda_posterior) - mean ^ 2
median <- qgamma(0.5 , alpha , mu )
ci <- qgamma(c(0.025 , 0.975) , alpha , mu)



plot(lambda , lambda_posterior  , type = 'l' ,lwd = 1.7 ,  col = 'navyblue')



#print the results
cat("mean: ", round(mean, 5), "\n")
cat("variance: ", round(variance, 5), "\n")
cat("median: ", round(median, 5), "\n")
plot(lambda , lambda_posterior  , type = 'l' ,lwd = 1.7 ,  col = 'navyblue')
abline(v = ci[1] , lty = 'dashed' , lwd = 1.2, col = 'orange')
abline(v = ci[2] , lty = 'dashed' , lwd = 1.2, col = 'orange')





plot(lambda, lambda_posterior, type = 'l', lwd = 1.7, col = 'navyblue')
abline(v = mean, lty = 'dashed', lwd = 1.2, col = 'green')
abline(v = median, lty = 'dashed', lwd = 1.2, col = 'red')
abline(v = variance , lty = 'dashed' , lwd = 1.2 , col = 'pink')
abline(v = ci[1], lty = 'dashed', lwd = 1.2, col = 'orange')
abline(v = ci[2], lty = 'dashed', lwd = 1.2, col = 'orange')

# Legend
legend("topleft", legend = c( "Mean", "Median", "Variance"), 
       lty = c("dashed", "dashed", "dashed"),
       lwd = c(1.2, 1.2, 1.2),
       col = c( "green", "red", "pink" ))





alpha <- 0.5 + sigma_y_1
mu <- n_1


lambda_posterior <- dgamma(lambda , alpha , mu)


mean <- delta.lambda * sum(lambda * lambda_posterior)
variance <- delta.lambda * sum(lambda ^ 2 * lambda_posterior) - mean ^ 2
median <- qgamma(0.5 , alpha , mu )
ci <- qgamma(c(0.025 , 0.975) , alpha , mu)





plot(lambda , lambda_posterior  , type = 'l' ,lwd = 1.7 ,  col = 'navyblue')



#print the results
cat("mean: ", round(mean, 5), "\n")
cat("variance: ", round(variance, 5), "\n")
cat("median: ", round(median, 5), "\n")
plot(lambda , lambda_posterior  , type = 'l' ,lwd = 1.7 ,  col = 'navyblue')
abline(v = ci[1] , lty = 'dashed' , lwd = 1.2, col = 'orange')
abline(v = ci[2] , lty = 'dashed' , lwd = 1.2, col = 'orange')





plot(lambda, lambda_posterior, type = 'l', lwd = 1.7, col = 'navyblue')
abline(v = mean, lty = 'dashed', lwd = 1.2, col = 'green')
abline(v = median, lty = 'dashed', lwd = 1.2, col = 'red')
abline(v = variance , lty = 'dashed' , lwd = 1.2 , col = 'pink')
abline(v = ci[1], lty = 'dashed', lwd = 1.2, col = 'orange')
abline(v = ci[2], lty = 'dashed', lwd = 1.2, col = 'orange')

# Legend
legend("topleft", legend = c( "Mean", "Median", "Variance"), 
       lty = c("dashed", "dashed", "dashed"),
       lwd = c(1.2, 1.2, 1.2),
       col = c( "green", "red", "pink" ))








alpha <- 0.5 + sigma_y_1
mu <- n_1


lambda_posterior <- dgamma(lambda , alpha , mu)


mean <- delta.lambda * sum(lambda * lambda_posterior)
variance <- delta.lambda * sum(lambda ^ 2 * lambda_posterior) - mean ^ 2
median <- qgamma(0.5 , alpha , mu )
ci <- qgamma(c(0.025 , 0.975) , alpha , mu)



plot(lambda , lambda_posterior  , type = 'l' ,lwd = 1.7 ,  col = 'navyblue')



#print the results
cat("mean: ", round(mean, 5), "\n")
cat("variance: ", round(variance, 5), "\n")
cat("median: ", round(median, 5), "\n")
plot(lambda , lambda_posterior  , type = 'l' ,lwd = 1.7 ,  col = 'navyblue')
abline(v = ci[1] , lty = 'dashed' , lwd = 1.2, col = 'orange')
abline(v = ci[2] , lty = 'dashed' , lwd = 1.2, col = 'orange')





plot(lambda, lambda_posterior, type = 'l', lwd = 1.7, col = 'navyblue')
abline(v = mean, lty = 'dashed', lwd = 1.2, col = 'green')
abline(v = median, lty = 'dashed', lwd = 1.2, col = 'red')
abline(v = variance , lty = 'dashed' , lwd = 1.2 , col = 'pink')
abline(v = ci[1], lty = 'dashed', lwd = 1.2, col = 'orange')
abline(v = ci[2], lty = 'dashed', lwd = 1.2, col = 'orange')

# Legend
legend("topleft", legend = c( "Mean", "Median", "Variance"), 
       lty = c("dashed", "dashed", "dashed"),
       lwd = c(1.2, 1.2, 1.2),
       col = c( "green", "red", "pink" ))






#ex2


# Parameters:
# func : a function whose first argument is a real vector of parameters
# func returns a log10 of the likelihood function
# theta.init : the initial value of the Markov Chain (and of func)
# n.sample: number of required samples
# sigma : standar deviation of the gaussian MCMC sampling pdf

metropolis.1dim <- function(func , lambda.init , n.sample , sigma) {
  theta.cur <- lambda.init
  func.Cur <- func(theta.cur)
  func.Samp <- matrix(data=NA, nrow=n.sample , ncol=2+1)
  n.accept <- 0
  rate.accept <- 0.0
  
  for (n in 1:n.sample) {
    theta.prop <- rnorm(n=1, mean = theta.cur, sigma)
    func.Prop <- func(theta.prop)
    logMR <- func.Prop - func.Cur # Log10 of the Metropolis ratio
    if ( logMR >=0 || logMR >log10(runif(1)) ) {
      theta.cur <- theta.prop
      func.Cur <- func.Prop
      n.accept <- n.accept + 1
    }
    func.Samp[n, 1] <- func.Cur
    func.Samp[n, 2] <- theta.cur
    func.Samp[n, 3] <- n
  }
  return(func.Samp)
}


#
# Our test function
#
testfunc <- function(lambda) {
  return(dgamma(lambda , shape = alpha , rate = mu))
}
#
# - interface for the metropolis function , gets the log10 of test function
testfunc.metropolis <- function(lambda) {
  return(log10(testfunc(lambda )))
}



lambda.init <- 0.1
sample.sig <- 0.1
n.sample <- 100000
demo <- TRUE

set.seed(20190513)
chain <- metropolis.1dim(func=testfunc.metropolis ,
                         lambda.init = lambda.init ,
                         n.sample = n.sample ,
                         sigma = sample.sig)



str(chain)
n <- nrow(chain)

mcmc.data <- chain[1000:n , 2]
str(mcmc.data)

hist(mcmc.data , breaks = 1000 , freq = F , main = 'Histogram of the Samples'  , xlab = 'sample_val')

#In this context, chain is a matrix where each row represents a sample
#and each column represents a different variable or parameter. 
#The third column (chain[, 3]) contains the iteration numbers,
#while the second column (chain[, 2]) contains the corresponding lambda values.
plot(chain[, 3], chain[, 2], type = 'l', ylim = c(0.4, 1), xlab = 'Iteration', ylab = 'Lambda', xlim = c(0, 1000), main = 'MCMC First 1000 Iterations')


mean<- mean(mcmc.data)
variance<- var(mcmc.data)
median<- median(mcmc.data)

#print the results
cat("mean: ", round(mean, 5), "\n")
cat("variance: ", round(variance, 5), "\n")
cat("median: ", round(median, 5), "\n")



#ex3

y <- 11
n <- 116 
frequentist_estimator <- y/n
frequentist_estimator 





alpha_prior <- 1
beta_prior <- 10

alpha_posterior <- alpha_prior + y
beta_posterior <- beta_prior + (n - y)
p = seq(from = 0 , by = 0.001 , length.out = 1000)

posterior_samples <- dbeta(p, alpha_posterior, beta_posterior)

# Plotting the posterior distribution
plot(p, posterior_samples, type = 'n', xlab = "p", ylab = "Density", main = "Posterior Distribution of p")
polygon(c(p, rev(p)), c(posterior_samples, rep(0, length(p))), col = "white", border = NA)
lines(p, posterior_samples,lwd=2, col = "blue")




n <- nrow(chain)

sum <- sum( posterior_samples * p)
mean <- sum / n

variance = 0.001 * sum(p ^ 2 * posterior_samples) - mean ^ 2
sigma = sqrt(variance)

cat("mean: ", round(mean, 5), "\n")
cat("variance: ", round(variance, 5), "\n")
print(c(mean - 2 * sigma , mean + 2 * sigma))





p_value <- pbinom(11 , n , 0.1 , lower.tail = T) + pbinom(105 , n , 0.1 , lower.tail = F)
p_null <- 0.1

# Compare the p-value with the significance level
alpha <- 0.05
if (p_value < alpha) {
  cat("Reject the null hypothesis H???: p =", p_null, "\n")
} else {
  cat("Fail to reject the null hypothesis H???: p =", p_null, "\n")
}

cat("p-value:", p_value, "\n")



y <- 9
n <- 165
frequentist_estimator <- y/n
frequentist_estimator



alpha_prior <- 1
beta_prior <- 10

alpha_posterior <- alpha_prior + y
beta_posterior <- beta_prior + (n - y)
p = seq(from = 0 , by = 0.001 , length.out = 1000)

posterior_samples <- dbeta(p, alpha_posterior, beta_posterior)

# Plotting the posterior distribution
plot(p, posterior_samples, type = 'n', xlab = "p", ylab = "Density", main = "Posterior Distribution of p")
polygon(c(p, rev(p)), c(posterior_samples, rep(0, length(p))), col = "white", border = NA)
lines(p, posterior_samples,lwd=2, col = "blue")




#The shape parameters are 1 + 11 + 9, representing the sum of prior successes, 
#current successes, and future successes. 
#The rate parameters are 165 - 9 + 116 - 11 + 10, 
#representing the sum of prior failures, current failures, 
#and future failures. 
posterior_samples = dbeta(p, 1 + 11 + 9 ,165 - 9 +116 -11 + 10)
plot(p , posterior_samples , type = 'l', lwd = 2 ,  col ="blue")



n <- nrow(chain)
sum <- sum( posterior_samples * p)
mean <- sum / n

variance = 0.001 * sum(p ^ 2 * posterior_samples) - mean ^ 2
sigma = sqrt(variance)

cat("mean: ", round(mean, 5), "\n")
cat("variance: ", round(variance, 5), "\n")
print(c(mean - 2 * sigma , mean + 2 * sigma))





p_value <- pbinom(9 , n , 0.1 , lower.tail = T) + pbinom(107 , n , 0.1 , lower.tail = F)
p_null <- 0.1

# Compare the p-value with the significance level
alpha <- 0.05
if (p_value < alpha) {
  cat("Reject the null hypothesis H???: p =", p_null, "\n")
} else {
  cat("Fail to reject the null hypothesis H???: p =", p_null, "\n")
}

cat("p-value:", p_value, "\n")



#ex4


# First, we load the required packages for performing Bayesian inference with JAGS.
#library(rjags)  # For running JAGS models
#library(coda)   # For analyzing and visualizing MCMC samples

# Next, we specify the data that will be used in the analysis.
# In this case, we have 'n' observations and 'y' successes.
data <- list()
data$n <- 116
data$y <- 11

# We create the JAGS model by calling the 'jags.model()' function.
# The model specification is provided in a separate file called "model.txt".
model <- jags.model("model.txt", data = data)

#model {
# Prior distribution for p
#p ~ dbeta(1, 10)

# Likelihood
#y ~ dbin(p, n)
#}

# We generate samples from the model using the 'coda.samples()' function.
# The 'variable.names' argument specifies the parameters of interest, in this case, 'p'.
# The 'n.iter' argument determines the number of iterations to run the sampler.
samples <- coda.samples(model, variable.names = "p", n.iter = 10000)

# We can then summarize the samples using the 'summary()' function to obtain
# various statistics such as mean, median, standard deviation, and quantiles.

# Next, we convert the samples to a data frame using the 'as.data.frame()' function
# from the 'coda' package. This allows for easier analysis and visualization.
p.df <- as.data.frame(as.mcmc(samples))

# We can plot a histogram of the samples using the 'hist()' function.
# The histogram provides an estimate of the probability distribution of the parameter 'p'.
hist(p.df$p, breaks = 100, freq = FALSE, main = "Histogram of the Samples",
     xlab = "p", col = "brown", cex.main = 2, cex.lab = 2)
# Calculate mean, standard deviation, and 95% credible interval
mean <- mean(p.df$p)
std <- sd(p.df$p)
ci <- c(mean - 2 * std, mean + 2 * std)

cat("Mean: ", mean, "\n")
cat("Standard Deviation: ", std, "\n")
cat("95% Credible Interval: [", ci[1], ", ", ci[2], "]\n")


