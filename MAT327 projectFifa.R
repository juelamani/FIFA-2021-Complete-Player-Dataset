library(readr)
data <- read_csv("data.csv")
View(data)


#The code below will define the variables to be used in the project
name <- data$name
nationality <- data$nationality
position <- data$position
overall <- data$overall
age <- data$age
hits <- data$hits
potential <- data$potential
team <- data$team


#The code below will output a histogram of the player overall skill
hist(overall, main = "Histogram Showing the Overall Player Skill", xlab = "Overal", ylab = "Amount of Players with said overall")

#The code below will output the distribution of player's age
hist(age, main = "Histogram showing the Distribution of Player's Age", xlab = "Player Age", ylab = "Number of players with x age")

#the code below will output a histogram showing the player's potential
hist(potential, main = "Histogram Showing Player Potential", xlab = "Player Potential", ylab = "Number of Players with This potential")

#The code below will plot a scatterplot of player age vs player overall
plot(age, overall, main = "Scatterplot Showing Age and Player Overall", xlab = "Player's Age", ylab = "Player Overall Rating")

#The code below will compute the mean age of the players in the dataset
mean(age)
#The mean age is 26.31144 years of age

#The code below will print the median age of the players
median(age)
#The median is 26 years of age

#The code below will compute the standard deviation of the age of the players
sd(age)
#The standard deviation of the age of the players is 4.556077

#The code below will compute the variance of the age of the players
var(age)
#The variance of the age of the players is 20.75784


#The code below will calculate mean, median, standard deviation and variance for player overall and player potential
mean(potential)
median(potential)
sd(potential)
var(potential)

mean(overall)
median(overall)
sd(overall)
var(overall)

#The code below will create a confidence interval for the data set, starting by creating a subset

ageSubser <- c(27, 32, 25, 20, 21, 30, 22, 36, 33, 35, 31, 24, 27, 29, 36, 37,22,29,27,25)

xbar <- mean(ageSubser)
s <- sd(ageSubser)
n <- 20
t <- qt(0.995,20-1)
L <- xbar - t*s/sqrt(n)
U <- xbar + t*s/sqrt(n)

cor(age,overall)
#The correlation is 0.3419951

agePotetial.Lm <- lm(overall ~ age, data=data)
agePotetial.Lm

resid(agePotetial.Lm)

hist(resid(agePotetial.Lm), main = "Histogram of Residuals", xlab = "Residuals", ylab = "Frequencies")

plot(agePotetial.Lm)

