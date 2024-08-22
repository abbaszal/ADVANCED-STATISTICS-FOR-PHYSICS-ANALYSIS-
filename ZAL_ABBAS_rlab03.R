# Set the initial values
N <- 0
prob <- rep(1/6, 6)
results <- data.frame(N = 0, H0 = 1/6, H1 = 1/6, H2 = 1/6, H3 = 1/6, H4 = 1/6, H5 = 1/6)

# Create a function to update the probabilities
update_prob <- function(color, j, prob){
  if (color == 'white') {
    prob <- (j/5 * prob) / sum(j/5 * prob)
  } else {
    prob <- ((5-j)/5 * prob) / sum((5-j)/5 * prob)
  }
  prob
}

# Simulate the process 20 times
for (i in 1:60){
  
  # Draw a ball randomly from the box
  box <- c(rep('white', sample(6, 1)), rep('black', 5))
  color <- sample(box, 1)
  
  # Update the probabilities
  j <- 0:5
  prob <- update_prob(color, j, prob)
  
  # Increment N and save the results
  N <- N + 1
  results[nrow(results) + 1,] <- c(N, prob)
}

# Print the final results
results

ggplot(data = results, aes(x = N)) + 
  geom_line(aes(y = H0, color = "H0"), size = 1, show.legend = TRUE) +
  geom_line(aes(y = H1, color = "H1"), size = 1, show.legend = TRUE) +
  geom_line(aes(y = H2, color = "H2"), size = 1, show.legend = TRUE) +
  geom_line(aes(y = H3, color = "H3"), size = 1, show.legend = TRUE) +
  geom_line(aes(y = H4, color = "H4"), size = 1, show.legend = TRUE) +
  geom_line(aes(y = H5, color = "H5"), size = 1, show.legend = TRUE) +
  labs(title = "Probability of each hypothesis over time",
       x = "Number of draws",
       y = "Probability") +
  scale_color_manual(values = c("H0" = "red", "H1" = "orange", "H2" = "yellow", 
                                "H3" = "green", "H4" = "blue", "H5" = "purple"),
                     name = "Hypothesis",
                     labels = c("H0", "H1", "H2", "H3", "H4", "H5"))


library(ggplot2)

# Plot the results
ggplot(data = results, aes(x = N, y = H0)) + 
  geom_line(color = "red", size = 1) +
  geom_line(aes(y = H1), color = "orange", size = 1) +
  geom_line(aes(y = H2), color = "yellow", size = 1) +
  geom_line(aes(y = H3), color = "green", size = 1) +
  geom_line(aes(y = H4), color = "blue", size = 1) +
  geom_line(aes(y = H5), color = "purple", size = 1) +
  labs(title = "Probability of each hypothesis over time",
       x = "Number of draws",
       y = "Probability",
       color = "Hypothesis") +
  scale_color_manual(values = c("red", "orange", "yellow", "green", "blue", "purple"))

