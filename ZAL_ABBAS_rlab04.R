#ex1

# observed number of claims
obs <- c(5, 8, 4, 6, 11, 6, 6, 5, 6, 4)
#sum obs  = 61
alpha <- 62 #61+1
lambda<- length(obs) # number of observations
n <- 100

# prior distribution (uniform)
prior <- function(mu) {
  return(1)
}

# likelihood function (Poisson)
likelihood <- function(mu, obs) {
  return(prod(dpois(obs, lambda = mu)))
}

# posterior distribution
posterior <- function(mu, obs) {
  return(likelihood(mu, obs) * prior(mu))
}

# define a grid of values for mu
mu_vals <- seq(0, 10, by = 0.01)

# calculate the unnormalized posterior at each value of mu
post_vals <- sapply(mu_vals, posterior, obs = obs)

# normalize the posterior
post_vals <- post_vals / sum(post_vals)

# plot the posterior distribution
plot(mu_vals, post_vals, type = "l", lwd = 1.7 ,  xlab = "mu", ylab = "post- mu" , col = "navyblue")




# compute posterior mean
post_mean <- sum(mu_vals * post_vals)
cat("Posterior mean:", round(post_mean, 5), "\n")

# compute posterior median
post_median <- mu_vals[which.min(abs(cumsum(post_vals) - 0.5))]
cat("Posterior median:", round(post_median, 5), "\n")

# compute posterior variance
post_var <- sum(post_vals * (mu_vals - post_mean)^2)
cat("Posterior variance:", round(post_var, 2), "\n")



#- plot the posterior distribution and the 95% credibility interval
xi = qgamma(c(0.025 , 0.975) , alpha, lambda )
plot(mu_vals, post_vals, type = "l", lwd = 1.7 ,  xlab = "mu", ylab = "post- mu" , col = "navyblue")
abline(v = xi[1], lty = "dashed", lwd = 1.2 , col = "brown")
abline(v = xi[2], lty = "dashed", lwd = 1.2 , col = "brown")



#suppose to use a Jeffreys’ prior for µ (g(µ) ∝ 1/sqrtµ)

alpha <- 61.5 #sum(obs) +0.5

alpha <- 61 + 0.5

delta.mu <- 1/n

post.mu <- dgamma(mu_vals , alpha, lambda )

plot(mu_vals , post.mu , type = 'l' , lwd = 1.7 , col = 'navyblue')


post_mean <- delta.mu * sum(mu_vals * post.mu)
post_var <- delta.mu * sum(mu_vals^2 * post.mu) - post_mean^2
post_median <- qgamma(0.5 , alpha, lambda )


# print the results
cat("Posterior mean: ", round(post_mean, 5), "\n")
cat("Posterior median: ", round(post_median, 5), "\n")
cat("Posterior variance: ", round(post_var, 5), "\n")


xi <- qgamma(c(0.025 , 0.975) , alpha, lambda )

plot(mu_vals , post.mu , type = 'l' , lwd = 1.7 , col = 'navyblue')

abline(v = xi[1], lty = "dashed",lwd = 1.2, col = "red")
abline(v = xi[2], lty = "dashed",lwd = 1.2, col = "red")







#ex2



p <- 0.15
n <- 75
y <- 0:n
prob_y <- dbinom(y, n, p)

df <- data.frame(y = y, prob_y = prob_y)

ggplot(df, aes(x = y, y = prob_y)) +
  geom_bar(stat = "identity", fill = "orange", color = "brown") +
  labs(title = "Probability Distribution of Y", x = "Number of failures", y = "Probability") +
  theme_minimal()



p <- 0.85
n <- 75
y <- 0:n
prob_y <- dbinom(y, n, p)

df <- data.frame(y = y, prob_y = prob_y)

ggplot(df, aes(x = y, y = prob_y)) +
  geom_bar(stat = "identity", fill = "blue", color = "black") +
  labs(title = "Probability Distribution of Y", x = "Number of succes", y = "Probability") +
  theme_minimal()

#b
n <- 75
y <- 6
freq_estimator <- y/n
freq_estimator


#c

library(ggplot2)

# Set parameter values
p <- seq(from = 0.01, by = 0.001, length.out = 1000)
m <- 0.15
sigma <- 0.14

# Calculate posterior parameters
a <- m^2 * (1 - m) / sigma^2 - m
b <- -(1 - 1 / m) * a

# Calculate posterior distribution and summary statistics
p_post <- dbeta(p, a + 6, b + 69)
p_mean <- mean(p * p_post)
p_sd <- sqrt(mean(p^2 * p_post) - p_mean^2)

# Create plot using ggplot2
df <- data.frame(p = p, p_post = p_post)
plot <- ggplot(df, aes(x = p, y = p_post)) +
  geom_line(color = "navyblue", size = 1) +
  geom_vline(xintercept = p_mean, linetype = "dashed", color = "red") +
  geom_vline(xintercept = p_mean + p_sd, linetype = "dashed", color = "red") +
  geom_vline(xintercept = p_mean - p_sd, linetype = "dashed", color = "red") +
  labs(x = "p", y = "Posterior Density", title = "Posterior Distribution of p") +
  theme(plot.title = element_text(hjust = 0.5))

plot  # display plot


#d
p <- 0.15
1 - pbeta( p , a + y , b + 69  )



#E
pbinom(y , n , p)




#ex3
loglike_beta = function(alpha, beta, data){
  
  logL = 0.0 
  
  for (x in data) {
    logL = logL - log(beta^2 + (x - alpha) ^ 2) + log(beta)
  }
  return (logL)
}

loglike_matrix = function(alpha_values, beta_values, data) {
  logL = matrix(0, nrow = length(alpha_values), ncol = length(beta_values))  # Matrix to store log-likelihood values
  
  for (i in 1:length(alpha_values)) {
    for (j in 1:length(beta_values)) {
      logL[i, j] = loglike_beta(alpha_values[i], beta_values[j], data)  # Calculate log-likelihood for each combination
    }
  }
  
  return(logL)
}

n.sample.x = 300
n.sample.y = 200

x.min = 0 ; x.max = 3 
y.min = 1 ; y.max = 3
h.x = (x.max - x.min)/n.sample.x 
h.y = (y.max - y.min)/n.sample.y

alpha_values = seq(from = x.min , by = h.x , length.out = n.sample.x + 1)
beta_values = seq(from = y.min , by = h.y , length.out = n.sample.y + 1)

a = 2
b = 1.5

n = 200

data = rcauchy(n,  a ,  b)


#p.log.star = loglike_matrix(alpha_values , beta_values , data)
#index.max = which(p.log.star == max(p.log.star), arr.ind = TRUE)

str(alpha_values)
str(beta_values)




#ex4


#options(repr.plot.width=20, repr.plot.height=8)
par(mar=c(2,2,2,2))
set.seed(205)
signal <- function(x, A, B, x0, w, t) {t*(A*exp(-(x-x0)**2/(2*w**2)) + B)}
x0 <- 0 # Signal peak
w <-1 # Signal width
A.true <- 2 # Signal amplitude
B.true <- 1 # Background amplitude
Delta.t <- 5 # Exposure time


xdat <- seq(from = -7*w, to = 7*w, by = 0.5*w)
s.true <- signal(xdat, A.true, B.true, x0, w, Delta.t)
ddat <- rpois(length(s.true),s.true)


log.post <- function(d, x, a, b, x0, w, t) {
  if(a<0 || b <0){return(-Inf)}
  sum(dpois(d, lambda=signal(x, a, b, x0, w, t), log=TRUE))
}

As <- seq(0,5,0.01); Bs <- seq(0,5,0.01)
z <- matrix(data = NA,nrow = length(As),ncol = length(Bs))
for(i in 1:length(As)){
  for(j in 1:length(Bs)){
    z[i,j] <- log.post(ddat,xdat,As[i],Bs[j],x0,w,Delta.t)
  }
}
z <- z-max(z)

par(mfrow = c(1,2))

x.plot <- seq(from = -7*w, to = 7*w, by = 0.05*w)
s.plot <- signal(x.plot, A.true, B.true, x0, w, Delta.t)
plot(xdat,ddat,type = 's', lwd = 3,col = 'black',
     main = 'Photon counts',
     xlab ='x', ylab = 'N',
     cex.axis = 1.25, cex.lab = 1.25, cex.main = 1.25)
lines(x.plot,s.plot, type = 'l', lwd = 2, col ='blue')

contour(As, Bs, exp(z),
        main = "Posterior for amplitude and brackground",
        xlab="Amplitude A", ylab="Background B", xlim = c(1,3),ylim=c(0.5,1.5),
        labcex = 1.25, lwd = 1, cex.axis = 1.25, cex.lab = 1.25, cex.main =1.25 )
abline(v=A.true,h=B.true,col="navyblue")


ws = c(0.1,0.25,1,2,3)
#options(repr.plot.width=16, repr.plot.height=10*length(ws))
par(mfrow = c(length(ws),2))

for(w in ws){
  xdat <- seq(from = -7*w, to = 7*w, by = 0.5*w)
  s.true <- signal(xdat, A.true, B.true, x0, w, Delta.t)
  ddat <- rpois(length(s.true),s.true)
  z <- outer(As,Bs,Vectorize(function(A,B){log.post(ddat,xdat,A,B,x0,w,Delta.t)}))
  z <- z-max(z)
  
  x.plot <- seq(from = -7*w, to = 7*w, by = 0.05*w)
  s.plot <- signal(x.plot, A.true, B.true, x0, w, Delta.t)
  plot(xdat,ddat,type = 's', lwd = 3,col = 'black',
       main = paste('Photon counts for w = ',w),
       xlab ='x', ylab = 'N',
       cex.axis = 1.25, cex.lab = 1.25, cex.main = 1.25)
  lines(x.plot,s.plot, type = 'l', lwd = 2, col ='blue')
  
  contour(As, Bs, exp(z),
          main = paste("Posterior for amplitude and brackground for w = ",w),
          xlab="Amplitude A", ylab="Background B", xlim = c(1,3),ylim=c(0.5,1.5),
          labcex = 1.25, lwd = 1, cex.axis = 1.25, cex.lab = 1.25, cex.main =1.25 )
  abline(v=A.true,h=B.true,col="navyblue")
}


A0 <- seq(1,8,1)
Bs <- seq(2,6,0.01)
B.true <- 4
x0 <- 0
Delta.t <- 5
w <- 1

# Plot settings
#options(repr.plot.width=20, repr.plot.height=8)

# Analysis
for(A.true in A0){
  par(mfrow = c(1,2))
  As <- seq(max(0,A.true-2),A.true+2,0.01)
  r <- A.true/B.true
  xdat <- seq(from = -7*w, to = 7*w, by = 0.5*w)
  s.true <- signal(xdat, A.true, B.true, x0, w, Delta.t)
  ddat <- rpois(length(s.true),s.true)
  z <- outer(As,Bs,Vectorize(function(A,B){log.post(ddat,xdat,A,B,x0,w,Delta.t)}))
  z <- z-max(z)
  
  x.plot <- seq(from = -7*w, to = 7*w, by = 0.05*w)
  s.plot <- signal(x.plot, A.true, B.true, x0, w, Delta.t)
  plot(xdat,ddat,type = 's', lwd = 3,col = 'black',
       main = paste('Photon counts for A/B = ',r),
       xlab ='x', ylab = 'N',
       cex.axis = 1.25, cex.lab = 1.25, cex.main = 1.25)
  lines(x.plot,s.plot, type = 'l', lwd = 2, col ='blue')
  
  contour(As, Bs, exp(z),
          main = paste("Posterior for amplitude and brackground for A/B = ",r),
          xlab="Amplitude A", ylab="Background B", xlim = c(max(A.true-2,0),A.true+2),ylim=c(B.true-1,B.true+1),
          labcex = 1.25, lwd = 1, cex.axis = 1.25, cex.lab = 1.25, cex.main =1.25 )
  abline(v=A.true,h=B.true,col="navyblue")
}


