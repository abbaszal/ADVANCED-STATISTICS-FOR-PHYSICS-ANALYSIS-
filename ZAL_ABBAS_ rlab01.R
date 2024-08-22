# Exercise 1 - American Airlines Employees
# 
# In this exercise, we have four .txt files that are included of information about four American airlines from 1990 to 2023. 
# 
# 1) First we load the data.
# 
# 2) Then,merge the four data tibble in a common tibble.
# 3) After that, We produce a plot of the behavior of the employees as a function of time for all four companies.
# 
# 4) Then, we see when each company reach the minimum and maximum number of employees.
# 
# 5) Again we plot the fraction of part-time worker over the total employees as a function of time.
# 
# 6) At the end,We discuss about the COVID-19 pandemic and its influences in the employed workers of the airline companies.
# 

#Loading Data
#exercise 1_1
#setwd("C:/Users/Home/Desktop")
dt_1<- read.delim("american_airline_empl.txt")
dt_2 <- read.delim("delta_airline_empl.txt")
dt_3 <- read.delim("federal_express_empl.txt")
dt_4 <- read.delim("united_airline_empl.txt")

library(dplyr)
tibble1 <- tibble(dt_1)
tibble2 <- tibble(dt_2)
tibble3 <- tibble(dt_3)
tibble4 <- tibble(dt_4)
common_tibble <- bind_rows(tibble1, tibble2, tibble3, tibble4)
common_tibble

#exercise 1_3
total <- rbind(data.frame(dt_1) , data.frame(dt_2) , data.frame(dt_3) , data.frame(dt_4) )
full <- gsub("[^[:digit:].]", "", total[,3])
full <- as.integer(full)
part <- gsub("[^[:digit:].]", "", total[,4])
part <- as.integer(part)
colors <- c("red", "blue", "green", "orange", "black")


x <- rep(c(1990:2022),each=12)
y1 <- c(full[1:396])
y2 <- c(part[1:396])
# create a line graph
plot(x, y1, type = "l", col = "blue",lwd=0.5, ylim = c(1000,100000), 
     main = "Comparison of full and part time - American_airline", xlab = "Year", ylab = "The number of workers", cex.main=0.8 , cex.lab=0.8)
options(scipen = 10)
# add a second line to the plot
lines(x, y2, col = "orange",lwd=2)
legend("right", legend=c("Full_time", "Part_time"), 
       col= c(colors[2],colors[4]), lty=c(1,1),cex = 0.7)



x <- rep(c(1990:2022),each=12)
y1 <- c(full[398:793])
y2 <- c(part[398:793])
# create a line graph
plot(x, y1, type = "l", col = "blue",lwd=0.5, ylim = c(1000,100000), 
     main = "Comparison of full and part time - Delta_airline", xlab = "Year", ylab = "The number of workers", cex.main=0.8 , cex.lab=0.8)
options(scipen = 10)
# add a second line to the plot
lines(x, y2, col = "orange",lwd=2)
legend("right", legend=c("Full_time", "Part_time"), 
       col= c(colors[2],colors[4]), lty=c(1,1),cex = 0.7)

x <- rep(c(1990:2022),each=12)
y1 <- c(full[795:1190])
y2 <- c(part[795:1190])
# create a line graph
plot(x, y1, type = "l", col = "blue",lwd=0.5, ylim = c(1000,200000), 
     main = "Comparison of full and part time - Federal_express", xlab = "Year", ylab = "The number of workers", cex.main=0.8 , cex.lab=0.8)
options(scipen = 10)
# add a second line to the plot
lines(x, y2, col = "orange",lwd=2)
legend("right", legend=c("Full_time", "Part_time"), 
       col= c(colors[2],colors[4]), lty=c(1,1),cex = 0.7)

x <- rep(c(1990:2022),each=12)
y1 <- c(full[1192:1587])
y2 <- c(part[1192:1587])
# create a line graph
plot(x, y1, type = "l", col = "blue",lwd=0.5, ylim = c(1000,100000), 
     main = "Comparison of full and part time - United_airline", xlab = "Year", ylab = "The number of workers", cex.main=0.8 , cex.lab=0.8)
options(scipen = 10)
# add a second line to the plot
lines(x, y2, col = "orange",lwd=2)
legend("right", legend=c("Full_time", "Part_time"), 
       col= c(colors[2],colors[4]), lty=c(1,1),cex = 0.7)



meanfull_1 <- numeric(1)
for (i in seq(from = 1, to = 396, by = 12)) {
  if(i<12 ){
    meanfull_1[length(meanfull_1)] <- mean(full[i:(i+11)])
  } else{
    meanfull_1[length(meanfull_1) + 1] <- mean(full[i:(i+11)])
  }
}
meanpart_1 <- numeric(1)
for (i in seq(from = 1, to = 396, by = 12)) {
  if(i<12 ){
    meanpart_1[length(meanpart_1)] <- mean(part[i:(i+11)])
  } else{
    meanpart_1[length(meanpart_1) + 1] <- mean(part[i:(i+11)])
  }
}
meanfull_2 <- numeric(1)
for (i in seq(from = 398, to = 793, by = 12)) {
  if(i<409 ){
    meanfull_2[length(meanfull_2)] <- mean(full[i:(i+11)])
  } else{
    meanfull_2[length(meanfull_2) + 1] <- mean(full[i:(i+11)])
  }
}
meanpart_2 <- numeric(1)
for (i in seq(from = 398, to = 793, by = 12)) {
  if(i<409 ){
    meanpart_2[length(meanpart_2)] <- mean(part[i:(i+11)])
  } else{
    meanpart_2[length(meanpart_2) + 1] <- mean(part[i :(i+11)])
  }
}


meanfull_3 <- numeric(1)
for (i in seq(from = 795, to = 1190, by = 12)) {
  if(i<806){
    meanfull_3[length(meanfull_3)] <- mean(full[i:(i+11)])
  } else{
    meanfull_3[length(meanfull_3) + 1] <- mean(full[i:(i+11)])
  }
}
meanpart_3 <- numeric(1)
for (i in seq(from = 795, to = 1190, by = 12)) {
  if(i<806 ){
    meanpart_3[length(meanpart_3)] <- mean(part[i:(i+11)])
  } else{
    meanpart_3[length(meanpart_3) + 1] <- mean(part[i:(i+11)])
  }
}  


meanfull_4 <- numeric(1)
for (i in seq(from = 1192, to = 1587, by = 12)) {
  if(i<1203 ){
    meanfull_4[length(meanfull_4)] <- mean(full[i:(i+11)])
  } else{
    meanfull_4[length(meanfull_4) + 1] <- mean(full[i:(i+11)])
  }
}
meanpart_4 <- numeric(1)
for (i in seq(from = 1192, to = 1587, by = 12)) {
  if(i<1203 ){
    meanpart_4[length(meanpart_4)] <- mean(part[i:(i+11)])
  } else{
    meanpart_4[length(meanpart_4) + 1] <- mean(part[i:(i+11)])
  }
}  
colors <- c("red", "blue", "green", "orange", "black","brown")

x <- c(1990:2022)
y1 <- c(meanfull_1)
y2 <- c(meanpart_1)


# create a line graph with one line
plot(x, y1, type = "l", col = "blue",lwd=2, ylim = c(1000,100000),
     main = "Comparison of full and part time - American_airline", xlab = "Year", ylab = "Mean number of workers", cex.main=0.8 , cex.lab=0.8)
options(scipen = 10)
# add a second line to the plot
lines(x, y2, col = "blue",lwd=2,lty=2)

legend("right", legend=c("Full_time", "Part_time"),
       col=colors[2], lty=c(1,2),cex = 0.7)


x <- c(1990:2022)
y3 <- c(meanfull_2)
y4 <- c(meanpart_2)
plot(x, y3, type = "l", col = "blue",lwd=2, ylim = c(0,100000),
     main = "Comparison of full and part time - Delta_airline", xlab = "Year", ylab = "Mean number of workers", cex.main=0.8 , cex.lab=0.8)
options(scipen = 10)
# add a second line to the plot
lines(x, y4, col = "blue",lwd=2,lty=2)

legend("right", legend=c("Full_time", "Part_time"),
       col=colors[2], lty=c(1,2),cex = 0.7)


x <- c(1990:2022)
y5 <- c(meanfull_3)
y6 <- c(meanpart_3)

plot(x, y5, type = "l", col = "blue",lwd=2, ylim = c(0,200000),
     main = "Comparison of full and part time - Federal_express", xlab = "Year", ylab = "Mean number of workers", cex.main=0.8 , cex.lab=0.8)
options(scipen = 10)
# add a second line to the plot
lines(x, y6, col = "blue",lwd=2,lty=2)

legend("right", legend=c("Full_time", "Part_time"),
       col=colors[2], lty=c(1,2),cex = 0.7)


x <- c(1990:2022)
y7 <- c(meanfull_4)
y8 <- c(meanpart_4)

plot(x, y7, type = "l", col = "blue",lwd=2, ylim = c(0,100000),
     main = "Comparison of full and part time - United_airline", xlab = "Year", ylab = "Mean number of workers", cex.main=0.8 , cex.lab=0.8)
options(scipen = 10)
# add a second line to the plot
lines(x, y8, col = "blue",lwd=2,lty=2)

legend("right", legend=c("Full_time", "Part_time"),
       col=colors[2], lty=c(1,2),cex = 0.7)
x1 <- c(1990:2022)
y1 <- c(meanfull_1)
y2 <- c(meanpart_1)
y3 <- c(meanfull_2)
y4 <- c(meanpart_2)
y5 <- c(meanfull_3)
y6 <- c(meanpart_3)
y7 <- c(meanfull_4)
y8 <- c(meanpart_4)

plot(x1, y1, type = "l", col = colors[1], ylim = c(0,350000), 
     main = "Comparison of full and part time - All airlines", xlab = "Year", ylab = "Mean number of workers", cex.main=0.8 , cex.lab=0.8)
options(scipen = 10)
lines(x, y2, col=colors[1], lwd=2, lty=2)
lines(x, y3, col=colors[2])
lines(x, y4, col=colors[2], lwd=2, lty=2)
lines(x, y5, col=colors[3])
lines(x, y6, col=colors[3], lwd=2, lty=2)
lines(x, y7, col=colors[4])
lines(x, y8, col=colors[4], lwd=2, lty=2)
legend("topright", legend=c("Full_time_American_airline", "Part_time_American_airline","Full_time_Delta_airline", "Part_time_Delta_airline","Full_time_Federal_express", "Part_time_Federal_express","Full_time_United_airline", "Part_time_United_airline"), 
       col=rep(c(colors[1:4]),each=2), lty=c(1,2,1,2,1,2,1,2),cex = 0.7)




full_1 <- gsub("[^[:digit:].]", "", dt_1[,3])
part_1 <- gsub("[^[:digit:].]", "", dt_1[,4])
full_1<- as.numeric(full_1)
part_1 <- as.numeric(part_1)
full_1<- as.integer(full_1)
part_1<- as.integer(part_1)


full_2 <- gsub("[^[:digit:].]", "", dt_2[,3])
part_2 <- gsub("[^[:digit:].]", "", dt_2[,4])
full_2 <- as.numeric(full_2)
part_2 <- as.numeric(part_2)
full_2<- as.integer(full_2)
part_2<- as.integer(part_2)


full_3 <- gsub("[^[:digit:].]", "", dt_3[,3])
part_3 <- gsub("[^[:digit:].]", "", dt_3[,4])
full_3 <- as.numeric(full_3)
part_3 <- as.numeric(part_3)
full_3<- as.integer(full_3)
part_3<- as.integer(part_3)


full_4 <- gsub("[^[:digit:].]", "", dt_4[,3])
part_4 <- gsub("[^[:digit:].]", "", dt_4[,4])
full_4 <- as.numeric(full_4)
part_4 <- as.numeric(part_4)
full_4<- as.integer(full_4)
part_4<- as.integer(part_4)


sum_1 <- numeric(length(full_1))
sum_2 <- numeric(length(full_1))
sum_3 <- numeric(length(full_1))
sum_4 <- numeric(length(full_1))

for (i in seq_along(full_1)) {
  sum_1[i]  <-  full_1[i] + part_1[i]
}


for (i in seq_along(full_1)) {
  sum_2[i]  <-  full_2[i] + part_2[i]
}


for (i in seq_along(full_1)) {
  sum_3[i]  <-  full_3[i] + part_3[i]
}


for (i in seq_along(full_1)) {
  sum_4[i]  <-  full_4[i] + part_4[i]
}


#exercise1_3


total <- rbind(data.frame(dt_1) , data.frame(dt_2) , data.frame(dt_3) , data.frame(dt_4) )


i <- data.frame(0, nrow(dt_1) , nrow(dt_2) + nrow(dt_1) , nrow(dt_3)+ nrow(dt_2) + nrow(dt_1) )

max_index <- numeric(4)
max_i <- numeric(4)

for (j in 1:4) {
  full_j_name <- paste0("full_", j)
  part_j_name <- paste0("part_", j)
  
  full_j <- get(full_j_name)
  part_j <- get(part_j_name)
  
  sum_j <- full_j + part_j
  max_i[j] <- max(sum_j)
  max_index[j] <- which(sum_j == max_i[j])
}
maximums <- data.frame(matrix(0, nrow=4, ncol=4))
names(maximums) <- c("Month", "Year", "Full_time", "Part_time")
rownames(maximums) <- c("american_airline", "delta_airline", "federal_express", "united_airline")
for(j in 1:4){ 
  maximums[j,1:4] <- total[max_index[j] + i[[j]], 1:4]
}
print(" Maximum number of employess")
maximums



min_index <- numeric(4)
min_i <- numeric(4)

# Calculate max_i and max_index for each sum_i
for (j in 1:4) {
  full_j_name <- paste0("full_", j)
  part_j_name <- paste0("part_", j)
  
  full_j <- get(full_j_name)
  part_j <- get(part_j_name)
  
  sum_j <- full_j + part_j
  min_i[j] <- min(sum_j)
  min_index[j] <- which(sum_j == min_i[j])
}

minimums <- data.frame(matrix(0, nrow=4, ncol=4))
names(minimums) <- c("Month", "Year", "Full_time", "Part_time")
rownames(minimums) <- c("american_airline", "delta_airline", "federal_express", "united_airline")
for(j in 1:4){ 
  minimums[j,1:4] <- total[min_index[j] + i[[j]], 1:4]
}
print(" Minimum number of employess")
minimums


frac_1 <- numeric(length(part_1))
frac_2 <- numeric(length(part_1))
frac_3 <- numeric(length(part_1))
frac_4 <- numeric(length(part_1))

for (i in 1:length(part_1)){
  frac_1[i] <- part_1[i] / sum_1[i]
}
for (i in 1:length(part_2)){
  frac_2[i] <- part_2[i] / sum_2[i]
}
for (i in 1:length(part_3)){
  frac_3[i] <- part_3[i] / sum_3[i]
}
for (i in 1:length(part_1)){
  frac_4[i] <- part_4[i] / sum_4[i]
}



vec <- 1990:2023
vec <- vec[-34]
x <- rep(vec, each=12)
x <- c(x, 2023)

y1 <- c(frac_1)
y2 <- c(frac_2)
y3 <- c(frac_3)
y4 <- c(frac_4)

# create a line graph
plot(x, y1, type = "l", col = "blue", ylim = c(0,0.5), 
     main = "Comparison of all airlines", xlab = "Year", ylab = "The fraction of part-time worker over the total employess", cex.main=0.8 , cex.lab=0.8)

# add a second line to the plot
lines(x, y2, col = "red")

lines(x, y3, col = "green")

lines(x, y4, col = "black")
legend("topright", legend=c("American_airline","Delta_airline", "Federal_express", "United_airline"), 
       col=rep(c(colors[2],colors[1],colors[3],colors[5])), lty=c(1,1,1,1),cex = 0.8)


meanfrac_1 <- numeric(1)
meanfrac_2 <- numeric(1)
meanfrac_3 <- numeric(1)
meanfrac_4 <- numeric(1)
for (i in seq(from = 1, to = 396, by = 12)) {
  if(i<12 ){
    meanfrac_1[length(meanfrac_1)] <- mean(frac_1[i:(i+11)])
  } else{
    meanfrac_1[length(meanfrac_1) + 1] <- mean(frac_1[i:(i+11)])
  }
}
for (i in seq(from = 1, to = 396, by = 12)) {
  if(i<12 ){
    meanfrac_2[length(meanfrac_2)] <- mean(frac_2[i:(i+11)])
  } else{
    meanfrac_2[length(meanfrac_2) + 1] <- mean(frac_2[i:(i+11)])
  }
}  
for (i in seq(from = 1, to = 396, by = 12)) {
  if(i<12 ){
    meanfrac_3[length(meanfrac_3)] <- mean(frac_3[i:(i+11)])
  } else{
    meanfrac_3[length(meanfrac_3) + 1] <- mean(frac_3[i:(i+11)])
  }
}  
for (i in seq(from = 1, to = 396, by = 12)) {
  if(i<12 ){
    meanfrac_4[length(meanfrac_4)] <- mean(frac_4[i:(i+11)])
  } else{
    meanfrac_4[length(meanfrac_4) + 1] <- mean(frac_4[i:(i+11)])
  }
}  




x <- c(1990:2022)
y1 <- c(meanfrac_1)
y2 <- c(meanfrac_2)
y3 <- c(meanfrac_3)
y4 <- c(meanfrac_4)

# create a line graph
plot(x, y1, type = "l", col = "blue", ylim = c(0,0.5), 
     main = "Comparison of all airlines", xlab = "Year", ylab = "Mean of  fraction of part-time worker over the total employess", cex.main=0.8 , cex.lab=0.8)

# add a second line to the plot
lines(x, y2, col = "red")

lines(x, y3, col = "green")

lines(x, y4, col = "black")
legend("topright", legend=c("American_airline","Delta_airline", "Federal_express", "United_airline"), 
       col=rep(c(colors[2],colors[1],colors[3],colors[5])), lty=c(1,1,1,1),cex = 0.8)

#exercise1_6
x <- rep(c(1990:2022),each=12)
y1 <- c(full[1:396])
y2 <- c(part[1:396])
plot(x, y1, type = "l", col = "blue", lwd = 2, ylim = c(1000, 95000), 
     main = "COVID-19 impact on American_airline", xlab = "Year", ylab = "number of workers ", cex.main=0.8)
options(scipen = 10)


lines(x, y2, col = "orange", lwd = 2)
#2020
point_to_label <- 361
x_coord <- x[point_to_label]
y_coord <- y1[point_to_label]
#after 12 months 2021
# point_to_label <- 373
# x_coord1 <- x[point_to_label]
# y_coord1 <- y1[point_to_label]

segments(x_coord, 0, x_coord, y_coord, col = "brown", lty = "dashed")
legend("right", legend=c("Full_time", "Part_time"),
       col=c(colors[2],colors[4]), lty=c(1,1),cex = 0.7)


x <- rep(c(1990:2022),each=12)
y1 <- c(full[1:396])
y2 <- c(part[1:396])
plot(x, y1, type = "l", col = "blue", lwd = 2, ylim = c(1000, 95000), 
     main = "COVID-19 impact on American_airline between 2020 to 2021", xlab = "Year", ylab = "number of workers ", cex.main=0.8)
options(scipen = 10)


lines(x, y2, col = "orange", lwd = 2)
#2020
point_to_label <- 361
x_coord <- x[point_to_label]
y_coord <- y1[point_to_label]
#after 12 months 2021
point_to_label <- 373
x_coord1 <- x[point_to_label]
y_coord1 <- y1[point_to_label]

segments(x_coord, 0, x_coord, y_coord, col = "brown", lty = "dashed")
segments(x_coord1, 0, x_coord1, y_coord1, col = "green", lty = "dashed")
legend("right", legend=c("Full_time", "Part_time"),
       col=c(colors[2],colors[4]), lty=c(1,1),cex = 0.7)

#The number of American airline workers in 2020
dt_1[361:372,]

x <-  c(1:12)
y1 <- c(full[361:372])
y2 <- c(part[361:372])
plot(x, y1, type = "l", col = "blue", lwd = 2, ylim = c(1000, 110000), 
     main = "COVID-19 impact on American_airline_2020", xlab = "Month", ylab = "number of workers ", cex.main=0.7)
options(scipen = 1)
lines(x, y2, col = "orange")
legend("right", legend=c("Full_time", "Part_time"),
       col=c(colors[2],colors[4]), lty=c(1,1),cex = 0.7)


#The number of Delta airline workers in 2020
dt_2[361:372,]

x <- rep(c(1990:2022),each=12)
y1 <- c(full_2[1:396])
y2 <- c(part_2[1:396])
plot(x, y1, type = "l", col = "blue", lwd = 2, ylim = c(1000, 95000), 
     main = "COVID-19 impact on Delta_airline between 2020 to 2021", xlab = "Year", ylab = "number of workers ", cex.main=0.8)
options(scipen = 10)


lines(x, y2, col = "orange", lwd = 2)
#2020
point_to_label <- 361
x_coord <- x[point_to_label]
y_coord <- y1[point_to_label]
#after 12 months 2021
point_to_label <- 373
x_coord1 <- x[point_to_label]
y_coord1 <- y1[point_to_label]

segments(x_coord, 0, x_coord, y_coord, col = "brown", lty = "dashed")
segments(x_coord1, 0, x_coord1, y_coord1, col = "green", lty = "dashed")
legend("topright", legend=c("Full_time", "Part_time"),
       col=c(colors[2],colors[4]), lty=c(1,1),cex = 0.7)
x <-  c(1:12)
y1 <- c(full[758:769])
y2 <- c(part[758:769])
plot(x, y1, type = "l", col = "blue", lwd = 2, ylim = c(1000, 110000), 
     main = "COVID-19 impact on Delta_airline_2020", xlab = "Month", ylab = "number of workers ", cex.main=0.7)
options(scipen = 1)
lines(x, y2, col = "orange")
legend("topright", legend=c("Full_time", "Part_time"),
       col=c(colors[2],colors[4]), lty=c(1,1),cex = 0.7)



x <- rep(c(1990:2022),each=12)
y1 <- c(full_4[1:396])
y2 <- c(part_4[1:396])
plot(x, y1, type = "l", col = "blue", lwd = 2, ylim = c(1000, 95000), 
     main = "COVID-19 impact on United_airline between 2020 to 2021", xlab = "Year", ylab = "number of workers ", cex.main=0.8)
options(scipen = 10)


lines(x, y2, col = "orange", lwd = 2)
#2020
point_to_label <- 361
x_coord <- x[point_to_label]
y_coord <- y1[point_to_label]
#after 12 months 2021
point_to_label <- 373
x_coord1 <- x[point_to_label]
y_coord1 <- y1[point_to_label]

segments(x_coord, 0, x_coord, y_coord, col = "brown", lty = "dashed")
segments(x_coord1, 0, x_coord1, y_coord1, col = "green", lty = "dashed")
legend("topright", legend=c("Full_time", "Part_time"),
       col=c(colors[2],colors[4]), lty=c(1,1),cex = 0.7)


x <- rep(c(1990:2022),each=12)
y1 <- c(full_3[1:396])
y2 <- c(part_3[1:396])
plot(x, y1, type = "l", col = "blue", lwd = 2, ylim = c(1000, 220000), 
     main = "COVID-19 impact on Fed-ex between 2020 to 2021", xlab = "Year", ylab = "number of workers ", cex.main=0.8)
options(scipen = 10)


lines(x, y2, col = "orange", lwd = 2)
#2020
point_to_label <- 361
x_coord <- x[point_to_label]
y_coord <- y1[point_to_label]
#after 12 months 2021
point_to_label <- 373
x_coord1 <- x[point_to_label]
y_coord1 <- y1[point_to_label]

segments(x_coord, 0, x_coord, y_coord, col = "brown", lty = "dashed")
segments(x_coord1, 0, x_coord1, y_coord1, col = "green", lty = "dashed")
legend("bottomright", legend=c("Full_time", "Part_time"),
       col=c(colors[2],colors[4]), lty=c(1,1),cex = 0.7)


vec <- 1990:2023
vec <- vec[-34]
x <- rep(vec, each=12)
x <- c(x, 2023)

y1 <- c(sum_1)
y2 <- c(sum_2)
y3 <- c(sum_3)
y4 <- c(sum_4)


plot(x, y1, type = "l", col = "blue", ylim = c(1000,400000), 
     main = "Comparison of all airlines", xlab = "Year", ylab = "The number of total employess", cex.main=0.8 , cex.lab=0.8)


lines(x, y2, col = "red")

lines(x, y3, col = "green")

lines(x, y4, col = "black")
legend("topright", legend=c("American_airline","Delta_airline", "Federal_express", "United_airline"), 
       col=rep(c(colors[2],colors[1],colors[3],colors[5])), lty=c(1,1,1,1),cex = 0.8)

# 
# Exercise 2 -  Data Frames and Tibble
# 
# the nycflights13 R package contains data on all flights departing from New Your City airports
# in 2013. All available data is organized into four tibbles:
#   
#   airlines: contains metadata on airlines names and corresponding carrier codes
# 
# airports: contains metadata on all airports connected to NYC
# 
# flights: has data of all flights departing from the three NYC airports (JFK, LGA and EWR) in
# 2013
# 
# planes: Plane metadata for all plane numbers found in the FAA aircraft registry.
# 
# 
# 
# 1.First of all we Plot the total number of flights departed from each of the three NYC airports as a function of time.
# 
# 
# 2.secondly, Plot the average number of flights computed over the first five working days of each week as a function of the week number of the year.
# 
# 
# 3.Then, For each flight in the data frame we compute the departure delay and extract the following pieces of information.
# 
# 4.We plot of the average plane speed as a function of departure day of the year.
# 
# 5.After that,we analyze the flights offered by each airline company and determine.


# Load the packages
library(nycflights13)
library(dplyr)
library(ggplot2)

str(flights)

#exercise2-1

flights$date <- as.Date(paste(flights$year, flights$month, flights$day), "%Y %m %d")
str(flights$date)
# group flights by date and origin airport, and count the number of flights for each group
flights_by_date_origin <- flights %>%
  group_by(date, origin) %>%
  summarize(num_flights = n())

flights_by_date_origin
flights_by_date_origin[12,]
# plot the data
ggplot(flights_by_date_origin, aes(x = date, y = num_flights, color = origin)) +
  geom_line() +
  scale_x_date(date_labels = "%b %d", date_breaks = "1 day") +
  labs(title = "Total Number of Flights Departed by Origin Airport",
       x = "Date", y = "Number of Flights", color = "Origin") +
  theme_minimal()

#exercise2-2
flights$day_of_week <- weekdays(flights$date)
str(flights$day_of_week)

# define working days from Monday to Friday
working_days <- c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday")

# group flights by week number, day of the week, and origin airport
flights_by_week <- flights %>%
  mutate(week_num = format(date, "%U")) %>%
  filter(day_of_week %in% working_days) %>%
  group_by(week_num, day_of_week, origin) %>%
  summarize(num_flights = n()) %>%
  group_by(week_num, origin) %>%
  summarize(avg_flights = mean(num_flights))

flights_by_weekend <- flights %>%
  mutate(week_num = format(date, "%U")) %>%
  filter(day_of_week %in% c("Saturday", "Sunday")) %>%
  group_by(week_num, origin) %>%
  summarize(num_flights = n()) %>%
  group_by(week_num, origin) %>%
  summarize(avg_flights = mean(num_flights))

# plot for working days
ggplot(flights_by_week, aes(x = as.numeric(week_num), y = avg_flights, color = origin)) +
  geom_line() +
  scale_x_continuous(breaks = seq(1, 52, by = 1)) +
  labs(title = "Average Number of Flights Departed on Working Days",
       x = "Week Number", y = "Average Number of Flights", color = "Origin") +
  theme_minimal()

# plot for weekends
ggplot(flights_by_weekend, aes(x = as.numeric(week_num), y = avg_flights, color = origin)) +
  geom_line() +
  scale_x_continuous(breaks = seq(1, 52, by = 1)) +
  labs(title = "Average Number of Flights Departed on Weekends",
       x = "Week Number", y = "Average Number of Flights", color = "Origin") +
  theme_minimal()

#exercise2-3
# Compute departure delay for each flight
flights <- flights %>%
  mutate(dep_delay = dep_time - sched_dep_time)
flights

# Compute min, max, and average delay for each day of the year for each airport
delay_summary <- flights %>%
  group_by(year, month, day, origin) %>%
  summarize(min_delay = min(dep_delay, na.rm = TRUE),
            max_delay = max(dep_delay, na.rm = TRUE),
            avg_delay = mean(dep_delay, na.rm = TRUE))

# Plot
ggplot(delay_summary, aes(x = as.Date(paste(year, month, day, sep = "-")), y = avg_delay, color = origin)) +
  geom_line(aes(y = min_delay), linetype = "solid") +
  geom_line(aes(y = max_delay), linetype = "dashed") +
  geom_line() +
  labs(title = "Departure Delay for Each Day of the Year",
       x = "Date", y = "Delay (minutes)", color = "Airport") +
  facet_wrap(~ origin, scales = "free_y") +
  theme_minimal()


#exercise2-4
# Calculate flight duration in minutes
flights$duration <- as.numeric(flights$arr_time - flights$dep_time)

# Calculate average plane speed in miles per minute (v = x/t)
flights$avg_speed <- flights$distance / flights$duration

# Extract the day of the year from the departure time
flights$day_of_year <- (flights$time_hour)

# Calculate the average plane speed for each day of the year
avg_speed_by_day <- flights %>%
  group_by(day_of_year) %>%
  summarise(avg_speed = mean(avg_speed, na.rm = TRUE))

# Plot average plane speed
ggplot(avg_speed_by_day, aes(x = day_of_year, y = avg_speed)) +
  geom_line(color = "blue") +
  labs(x = "Day of the Year", y = "Average Plane Speed (miles per minute)") +
  ggtitle("Average Plane Speed as a Function of Departure Day of the Year") +
  theme(plot.title = element_text(hjust = 0.5))


#exercise2-5
# Group flights by airlines and the number of flights per day
flights_per_day <- flights %>%
  group_by(carrier, year, month, day) %>%
  summarize(n = n())


# row_num_largest <- which.max(flights_per_day[[5]])
# # Exclude the row with the largest value
# flights_per_day_excl_largest <- flights_per_day[-row_num_largest,]
# 
# # Find the row with the second-largest value
# row_num_second_largest <- which.max(flights_per_day_excl_largest[[5]])
# 
# large_1 <- data.frame(matrix(0, nrow=2, ncol=5))
# names(large_1) <- c("carrier" ,   "year",  "month"  , "day" ,    "n")
# rownames(large_1) <- c("largest", "second_largest")
# large_1 <- data.frame(
#   flights_per_day[row_num_largest,],
# flights_per_day[row_num_second_largest ,])



# Initialize an empty data frame to store the largest and second largest number of flights per day for each airline
large_1 <- data.frame(matrix(0, nrow=0, ncol=6))
names(large_1) <- c("carrier", "year", "largest_num_flights_per_day", "second_largest_num_flights_per_day", "largest_date", "second_largest_date")

# Loop over each unique airline
for (airline in unique(flights_per_day$carrier)) {
  # Subset the data for the current airline
  airline_data <- filter(flights_per_day, carrier == airline)
  
  # Find the row with the largest value
  row_num_largest <- which.max(airline_data[[5]])
  
  # Exclude the row with the largest value
  airline_data_excl_largest <- airline_data[-row_num_largest,]
  
  # Find the row with the second-largest value
  row_num_second_largest <- which.max(airline_data_excl_largest[[5]])
  
  # Create a data frame with the largest and second-largest values for the current airline
  airline_large_1 <- data.frame(
    carrier = airline,
    year = airline_data[row_num_largest, "year"],
    largest_num_flights_per_day = airline_data[row_num_largest,5],
    second_largest_num_flights_per_day = airline_data_excl_largest[row_num_second_largest,5],
    largest_date = as.Date(paste(airline_data[row_num_largest, "year"], airline_data[row_num_largest, "month"], airline_data[row_num_largest, "day"], sep="-")),
    second_largest_date = as.Date(paste(airline_data_excl_largest[row_num_second_largest, "year"], airline_data_excl_largest[row_num_second_largest, "month"], airline_data_excl_largest[row_num_second_largest, "day"], sep="-"))
  )
  
  # Add the data for the current airline to the overall data frame
  large_1 <- rbind(large_1, airline_large_1)
}


#it is sorted
sorted_large_1 <- large_1[order(large_1[,3], decreasing = TRUE),]
sorted_large_1


# Create a data frame for the data you provided
data <- data.frame(n = c(63, 96, 2, 167, 153, 181, 4, 11, 1, 80, 1, 187, 69, 17, 38, 3),
                   n_1 = c(62, 96, 2, 167, 152, 180, 3, 11, 1, 80, 1, 182, 67, 16, 38, 3),
                   carrier = c(large_1[,1]))

# Define the colors for the bars
colors <- c("#3366CC", "#DC3912")

# Create a bar plot with two bars for each airline
ggplot(data, aes(x = factor(carrier), y = n, fill = "largest")) +
  geom_bar(stat = "identity", position = "dodge", color = "black") +
  geom_bar(aes(y = n_1, fill = "second_largest"), stat = "identity", position = "dodge", color = "black") +
  scale_fill_manual(values = colors, name = "") +
  theme_classic() +
  labs(title = "The number of Largest and Second Largest Flights for each Airline per day", x = "Airline", y = "Number of Flights") +
  guides(fill = guide_legend(title = ""))


# Calculate the number of flights per week for each airline
# Group flights by carrier, year, and week
flights_per_week <- flights_per_day %>%
  group_by(carrier, year, week = format(as.Date(paste(year, month, day, sep = "-")), "%U")) %>%
  summarize(n = sum(n))


# Initialize data frame
large_1_week <- data.frame(matrix(0, nrow=0, ncol=4))
names(large_1_week) <- c("carrier", "year",  "largest_num_flights_per_week", "second_largest_num_flights_per_week")

# Loop over each unique airline
for (airline in unique(flights_per_week$carrier)) {
  # Subset the data for the current airline
  airline_data <- filter(flights_per_week, carrier == airline)
  
  # Find the row with the largest value
  row_num_largest <- which.max(airline_data[[4]])
  
  # Exclude the row with the largest value
  airline_data_excl_largest <- airline_data[-row_num_largest,]
  
  # Find the row with the second-largest value
  row_num_second_largest <- which.max(airline_data_excl_largest[[4]])
  
  # Create a data frame with the largest and second-largest values for the current airline
  airline_large_1_week <- data.frame(
    carrier = airline,
    year = airline_data[row_num_largest, "year"],
    largest_num_flights_per_week = airline_data[row_num_largest, 4],
    second_largest_num_flights_per_week = airline_data_excl_largest[row_num_second_largest, 4]
  )
  
  # Add the data for the current airline to the overall data frame
  large_1_week <- rbind(large_1_week, airline_large_1_week)
}
 

#it is sorted
sorted_large_1_week <- large_1_week[order(large_1_week[,3], decreasing = TRUE),]
sorted_large_1_week


flights_per_month <- flights %>%
  group_by(carrier, year, month) %>%
  summarize(n = n())
smallest_flights_per_month <- flights_per_month %>%
  group_by(carrier) %>%
  summarize(smallest_n = min(n)) %>%
  arrange(smallest_n)
smallest_flights_per_month

distance_per_day <- flights %>%
  group_by(carrier, year, month, day) %>%
  summarize(total_distance = sum(distance))
longest_distance_per_month <- distance_per_day %>%
  group_by(carrier, year, month) %>%
  arrange(desc(total_distance)) %>%
  slice(1)
#for example:To see the longest distance flights for all airlines in the fourth month
longest_distance_april <- longest_distance_per_month %>%
  filter(month == 4) %>%
  arrange(desc(total_distance))
longest_distance_april 

ggplot(longest_distance_april, aes(x = carrier, y = total_distance)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  ggtitle("Longest Distance Flights by Airline in April 2013") +
  xlab("Airline") +
  ylab("Total Distance (miles)") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_y_continuous(labels = scales::comma)