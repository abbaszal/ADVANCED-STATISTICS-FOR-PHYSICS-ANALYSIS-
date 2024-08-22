

# Define probability distribution function
pk <- function(k) {
  if (k >= 1 & k <= 5) {
    return(k/15)
  } else {
    return(0)
  }
}


# Define probability density function
dprob <- Vectorize(pk)

# Define cumulative distribution function
pprob <- function(k) {
  if(k >= 1 & k < 2) {
    return(pk(1))
  } else if (k >= 2 & k < 3) {
    return(pk(1) + pk(2))
  } else if (k >= 3 & k < 4) {
    return(pk(1) + pk(2) + pk(3))
  } else if (k >= 4 & k < 5) {
    return(pk(1) + pk(2) + pk(3) + pk(4))
  } else if (k == 5) {
    return(1)
  } else{
    return(0)
  }
}









#2

# Load required libraries for plotting
library(ggplot2)

# Define range of values for k
k <- 1:5

# Calculate probabilities for PDF and CDF
pdf <- dprob(k)
cdf <- cumsum(pdf)

# Create data frames for PDF and CDF
pdf_df <- data.frame(k = k, pdf = pdf)
cdf_df <- data.frame(k = k, cdf = cdf)

# Create PDF plot
pdf_plot <- ggplot(pdf_df, aes(x = k, y = pdf)) +
  geom_bar(stat = "identity", fill = "lightblue") +
  labs(title = "Probability Density Function (PDF)", x = "k")

# Create CDF plot
cdf_plot <- ggplot(cdf_df, aes(x = k, y = cdf)) +
  geom_bar(stat = "identity", fill = "blue") +
  labs(title = "Cumulative Distribution Function (CDF)", x = "k")

# Display both plots side by side
library(gridExtra)
grid.arrange(pdf_plot, cdf_plot, ncol = 2)

#3

mean_pk = sum(pdf * k)
variance_pk = sum(pdf * k^2 - (pdf * k)^2)
mean_pk
variance_pk


#4
expected_value <- 0
for(k in 1:5){
  r <- k * (6 - k) * pk(k)
  expected_value <- r + expected_value
}
expected_value

#5



rand_function <- function(n){
  k <- 1:5
  samples <- sample(k, size = n, replace = TRUE, prob = sapply(1:5, pk))
  return(samples)
}

#6

set.seed(123)
n <- 100000
y<- rand_function(n)
x <- numeric(n)
for (i in 1:n) {
  x[i] <- pk(y[i])
}
plot(y, dprob(y), type = "h", col = "brown", lwd = 10, xlim = c(0,6),
     ylab = "Probability", xlab = "Values", main = "Sampled Data vs Uniform Discrete Distribution")
hist(y, breaks = seq(0 - 0.5 , 6+ 0.5, by = 1), col = "yellow", border = "white" , freq = FALSE ,add = TRUE)




#ex2


# Define triangular distribution function
triangular <- function(x, a, b, c) {
  ifelse(x < a | x > b, 0,
         ifelse(x < c, 2*(x-a)/((b-a)*(c-a)),
                2*(b-x)/((b-a)*(b-c))))
}

a <- 10
b <- 30
c <- 15

# Plot density curve
curve(triangular(x, a, b, c), from = a, to = b, n = 1000, col = "lightblue",
      xlab = "x", ylab = "Density", main = "Triangular Distribution")
curve(triangular(x, a, b, c), col = "brown", add = TRUE, from = c, to = b, n = 1000)
#segments(x_coord, 0, x_coord, y_coord, col = "green", lty = "dashed")
legend("topright", legend=c("a ≤ x < c", "c ≤ x ≤ b"),
       col=c("lightblue","brown"), lty=c(1,1),cex = 0.7)



random_triangular <- function(n, a, b, c) {
  U <- runif(n)
  x <- ifelse(U < (c - a) / (b - a), 
              a + sqrt(U * (b - a) * (c - a)), 
              b - sqrt((1 - U) * (b - a) * (b - c)))
  return(x)
}

set.seed(123)
n <- 10000
x <- random_triangular(n,a,b,c)

hist(x, prob = TRUE, main = "Triangular Distribution",
     xlab = "x", ylab = "Density")
curve(triangular(x, a, b, c), add = TRUE, col = "red",from = a, to = b, n = 10000, lwd = 2)




#ex3
set.seed(123)

# Generate 60 waiting times from the exponential distribution
waiting_times <- rexp(60, rate = 1/30)
waiting_times
breaks = seq(-0.5 , max(waiting_times) + 1 + 0.5, by = 1.0)
# Plot the relative histogram of the waiting times
hist(waiting_times,breaks = breaks, freq = FALSE, main = "Waiting Times at Doctor's Office", col = "blue")


t <- 12
waiting_times[which(waiting_times <= 12)]
p <-length(which(waiting_times <= 12)) / length(waiting_times)

# Print the probability
cat("The probability that a person will wait for less than", t, 
    "minutes is", round(p, 4), ".")




# Calculate the average waiting time
avg_waiting_time <- mean(waiting_times)

# Print the results
cat("The average waiting time from the simulated data is", round(avg_waiting_time, 2), "minutes.")

# Set the parameters
rate <- 1/30 # rate parameter for exponential distribution

# Calculate the expected value
expected_value <- 1/rate

# Print the results
cat("The expected value of the waiting time is", expected_value, "minutes.")


t <- 60
waiting_times[which(waiting_times <= 60)]
p <- 1 - length(which(waiting_times <= 60)) / length(waiting_times)

# Print the probability
cat("The probability of waiting more than", t, "minutes is", round(p, 4), ".")



#ex4


p_knows <- 0.7
p_doesnt_know <- 1 - p_knows
p_correct_given_knows <- 1
p_correct_given_doesnt_know <- 1/5
p_correct <- p_correct_given_knows * p_knows + p_correct_given_doesnt_know * p_doesnt_know
p_knows_given_correct <- p_correct_given_knows * p_knows / p_correct
p <- p_knows_given_correct
# Print the probability
cat("The probability that the student really knew the correct answer is", round(p, 4)*100, "%.")



#ex5
#For section (a) if we want to consider waiting time at most 10 minutes, person must arrive at 10:50 for 11:00 train or 11:20 for 11:30 train. It means the overall time is equal with 20 minutes from 10:45 to 11:45.

p <- 20 /60
# Print the result
cat("The probability that she has to wait at most 10 minutes is", round(p, 4)*100 , "%")




p <- 30 /60
# Print the result
cat("The probability that she has to wait at least 15 minutes is", round(p, 4)*100 , "%")







#ex6

# Define the parameters
mean <- 0.1  # Mean annual return rate
sd <- 0.12   # Standard deviation of annual return rate
n_shares <- 200  # Number of shares
price_per_share <- 85  # Price per share



# Calculate the probability that the net profit is at least 800 euros
prob <- 1 - pnorm(800/ (n_shares*price_per_share)  , mean , sd )


# Print the result
cat("The probability that Mr. X's net profit is at least 800 EUR is", round(prob, 4)*100 , "%")



