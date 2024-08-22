library(ggplot2)
library(lubridate)
library(coda)
library(dplyr)
library(rjags)


#ex1

#func
g <- function(theta) {
  numerator <- exp(-(theta + 3)^2 / 2) + exp(-(theta - 3)^2 / 2)
  denominator <- 2
  result <- numerator / denominator
  return(result)
}




# Parameters:
# func : a function whose first argument is a real vector of parameters
# func returns a log10 of the likelihood function
# theta.init : the initial value of the Markov Chain (and of func)
# n.sample: number of required samples
# sigma : standar deviation of the gaussian MCMC sampling pdf

metropolis.Hasting <- function(func , theta.init , n.sample) {
  theta.cur <- theta.init
  func.Cur <- func(theta.cur)
  func.Samp <- matrix(data=NA, nrow=n.sample , ncol=2+1)
  n.accept <- 0
  rate.accept <- 0.0
  
  for (n in 1:n.sample) {
    theta.prop <- rnorm(n=1, mean = 0, 1)
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


g.metropolis <- function(lambda) {
  return(log10(g(lambda)))}


theta.init <- 0.1
n.sample <- 100000

set.seed(20190513)
chain <- metropolis.Hasting(func=g.metropolis ,
                            theta.init = theta.init ,
                            n.sample = n.sample )


n <- nrow(chain)


mcmc.data = chain[1000:n , 2]

hist(mcmc.data , breaks = 100 , freq = F , main = 'Histogram of the Samples'  , xlab = 'sample values', col = "navyblue")




# Convert your chain data to a CODA object
mcmc.data <- as.mcmc(mcmc.data)
# Compute the autocorrelation
chain_autocorr <- autocorr(mcmc.data)
# Plot the chain autocorrelation
plot(chain_autocorr, main = "Chain Autocorrelation" , col = 'blue')



burn_ins <- c(20, 200, 2000, 10000)
legend_labels <- paste("Burn-in =", burn_ins)
colors <- c("red", "blue", "gray", "brown")
# Create an empty plot
plot(NULL, xlim = range(chain[, 2]), ylim = c(0, 0.3), type = "n", main = 'Histogram of the Samples')
     for (i in 1:length(burn_ins)){
       bi <- burn_ins[i]
       mcmc.data <- chain[(bi+1):nrow(chain), 2] # Exclude burn-in samples
       # Compute density estimate
       dens <- density(mcmc.data)
       # Plot density as a line
       lines(dens$x, dens$y, col = colors[i], lwd = 2)
     }
     # Add legend
     legend("topright", legend = legend_labels, col = colors, lwd = 2)
     
     
     
     
     
     dif_burn_ins <- c(1, 2, 3, 4, 5)
     colors <- c("red", "blue", "gray", "brown","skyblue")
     legend_labels <- paste("Thinning =", dif_burn_ins)
     # Create an empty plot
     plot(NULL, xlim = c(0, length(chain_autocorr)), ylim = c(0, 1), type = "n", main = "Chain Autocorrelation")
     for (i in 1:length(dif_burn_ins)){
       thin <- dif_burn_ins[i]
       mcmc.data <- chain[seq(from = 1000, to = nrow(chain), by = thin), 2]
       mcmc.data <- as.mcmc(mcmc.data)
       chain_autocorr <- autocorr(mcmc.data)
       # Plot autocorrelation with different colors
       lines(chain_autocorr, col = colors[i])
     }
     # Add legend
     legend("topright", legend = legend_labels, col = colors, lwd = 2)
     
     
     
#ex2
     

     # Prepare the data
     vaccine_cases <- 11
     vaccine_population <- 14134
     placebo_cases <- 185
     placebo_population <- 14073
     data <- list(vaccine_cases = vaccine_cases,
                  vaccine_population = vaccine_population,
                  placebo_cases = placebo_cases,
                  placebo_population = placebo_population)
     # Compile the model
     model <- jags.model("model_ex2.txt", data = data)
     
     #model_ex2 {
     # Priors
     #p_vaccine ~ dbeta(1, 1)
     #p_placebo ~ dbeta(1, 1)
     
     # Likelihood
     #vaccine_cases ~ dbin(p_vaccine, vaccine_population)
     #placebo_cases ~ dbin(p_placebo, placebo_population)
     
     # Derived quantities
     #efficacy <- (p_placebo - p_vaccine) / p_placebo * 100
     #}
   
     # Define parameters to monitor
     parameters <- c("p_vaccine", "p_placebo", "efficacy")
     # Run the MCMC
     iterations <- 10000
     burn_in <- 20000
     thin <- 1
     chains <- 2
     samples <- coda.samples(model, variable.names = parameters,
                             n.iter = iterations, thin = thin, n.chains = chains)
     
     # Summarize the results
     summary(samples)
       #Jcovden
 #as i saw in link:
 # Prepare the data
     vaccine_cases <- 116
     vaccine_population <- 19630
     placebo_cases <- 348
     
     placebo_population <- 19691
     data <- list(vaccine_cases = vaccine_cases,
                  vaccine_population = vaccine_population,
                  placebo_cases = placebo_cases,
                  placebo_population = placebo_population)
     # Compile the model
     model <- jags.model("model_ex2.txt", data = data)   
     
     
     # Define parameters to monitor
     parameters <- c("p_vaccine", "p_placebo", "efficacy")
     # Run the MCMC
     iterations <- 10000
     burn_in <- 20000
     thin <- 1
     chains <- 2
     samples <- coda.samples(model, variable.names = parameters,
                             n.iter = iterations, thin = thin, n.chains = chains)
     # Summarize the results
     summary(samples)
     
     
     
     
#ex3
     #Loading the data
     url <- "https://covid.ourworldindata.org/data/owid-covid-data.csv"
     data <- read.csv(url)
     #str(global_data)
     global_data <- data
     
     # Load required libraries
     library(dplyr)
     library(ggplot2)
     
     # Filter the data to include only the relevant columns
     filtered_data <- subset(global_data, select = c("date", "total_vaccinations"))
     
     # Filter out rows with missing or zero total_vaccinations
     filtered_data <- filtered_data[!is.na(filtered_data$total_vaccinations) & filtered_data$total_vaccinations > 0, ]
     
     # Calculate the cumulative vaccinations worldwide
     worldwide_data <- filtered_data %>%
       group_by(date) %>%
       summarise(total_vaccinations_worldwide = sum(total_vaccinations))
     
     # Convert date column to date format
     worldwide_data$date <- as.Date(worldwide_data$date)
     
     # Plot the total number of vaccinated people worldwide over time
     plot_cumulative <- ggplot(worldwide_data, aes(x = date, y = total_vaccinations_worldwide)) +
       geom_line(color='green') +
       labs(x = "Date", y = "Total Vaccinations Worldwide") +
       scale_x_date(date_labels = "%b\n%Y", date_breaks = "3 month") +
       theme_minimal()
     
     # Display the cumulative plot
     print(plot_cumulative)
     
     
     
     
     # Filter the global_data to include only the relevant columns and remove missing or zero new_vaccinations
     worldwide_data <- global_data %>%
       select(date, new_vaccinations) %>%
       filter(!is.na(new_vaccinations), new_vaccinations != 0) %>%
       mutate(date = as.Date(date, format = "%Y-%m-%d")) %>%
       group_by(date) %>%
       summarise(average_vaccinations_worldwide = mean(new_vaccinations))
     
     # Plot the daily average vaccinations worldwide over time
     plot <- ggplot(worldwide_data, aes(x = date, y = average_vaccinations_worldwide)) +
       geom_line(color='green') +
       labs(x = "Date", y = "Daily Average Vaccinations Worldwide") +
       scale_x_date(date_labels = "%b\n%Y", date_breaks = "3 month") +
       theme_minimal()
     
     # Display the plot
     print(plot)
     
     
     
     # Filter the data to include only the relevant columns and remove missing or zero new_vaccinations
     filtered_data <- subset(data, select = c("date", "new_vaccinations"))
     filtered_data$date <- as.Date(filtered_data$date, format = "%Y-%m-%d")
     filtered_data <- filtered_data[!is.na(filtered_data$new_vaccinations) & filtered_data$new_vaccinations != 0, ]
     
     # Extract the year and week from the date
     filtered_data$year <- year(filtered_data$date)
     filtered_data$week <- week(filtered_data$date)
     
     # Calculate the weekly average vaccinations worldwide
     worldwide_data <- filtered_data %>%
       group_by(year, week) %>%
       summarise(average_vaccinations_worldwide = mean(new_vaccinations))
     
     # Create a new date column representing the start of each week
     worldwide_data$date <- as.Date(paste(worldwide_data$year, worldwide_data$week, "1", sep = "-"), format = "%Y-%U-%u")
     
     # Plot the weekly average vaccinations worldwide over time (with green color)
     plot <- ggplot(worldwide_data, aes(x = date, y = average_vaccinations_worldwide)) +
       geom_line(color = "green") +
       labs(x = "Date", y = "Weekly Average Vaccinations Worldwide") +
       scale_x_date(date_labels = "%b\n%Y", date_breaks = "3 months") +
       theme_minimal()
     
     # Display the plot
     print(plot)
     
     
     
     
     
     # Filter the data to include only the relevant columns and remove missing or zero total_deaths
     filtered_data <- subset(data, select = c("date", "total_deaths"))
     filtered_data$date <- as.Date(filtered_data$date, format = "%Y-%m-%d")
     filtered_data <- filtered_data[!is.na(filtered_data$total_deaths) & filtered_data$total_deaths != 0, ]
     
     # Calculate the cumulative number of deaths worldwide
     worldwide_data <- filtered_data %>%
       group_by(date) %>%
       summarise(cumulative_deaths_worldwide = sum(total_deaths))
     
     # Plot the cumulative number of deaths worldwide over time (with red color)
     plot <- ggplot(worldwide_data, aes(x = date, y = cumulative_deaths_worldwide)) +
       geom_line(color = "red") +
       labs(x = "Date", y = "Cumulative Deaths Worldwide") +
       scale_x_date(date_labels = "%b\n%Y", date_breaks = "3 months") +
       theme_minimal()
     
     # Display the plot
     print(plot)
     
     
     
     # Filter the data to include only the relevant columns and remove missing or zero new_deaths
     filtered_data <- subset(data, select = c("date", "new_deaths"))
     filtered_data$date <- as.Date(filtered_data$date, format = "%Y-%m-%d")
     filtered_data <- filtered_data[!is.na(filtered_data$new_deaths) & filtered_data$new_deaths != 0, ]
     
     # Calculate the weekly average number of deaths worldwide
     worldwide_data <- filtered_data %>%
       group_by(date = as.Date(floor_date(date, "week"))) %>%
       summarise(average_deaths_worldwide = mean(new_deaths))
     
     # Plot the weekly average number of deaths worldwide over time (with red color)
     plot <- ggplot(worldwide_data, aes(x = date, y = average_deaths_worldwide)) +
       geom_line(color = "red") +
       labs(x = "Date", y = "Weekly Average Deaths Worldwide") +
       scale_x_date(date_labels = "%b\n%Y", date_breaks = "3 months") +
       theme_minimal()
     
     # Display the plot
     print(plot)
     
     
     
     
     
     
     
     
     
     
     
     
     
     
     
     
     
     
     
     
     
     
     
     
  