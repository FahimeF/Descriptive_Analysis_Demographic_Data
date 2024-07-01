library(corrplot)
library(dplyr)
library(ggplot2)
library(reshape2)

# Set working directory to the script's location (this is useful for portability)
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

# Load data from the working directory
census_2022_2002 <- read.csv("census_2022_2002.csv", header = TRUE)
census_2022_2002 <- census_2022_2002[order(census_2022_2002$life.expectancy.both.sexes),]
census_2022_2002 <- na.omit(census_2022_2002)

# Histogram of Fertility rate in 2022
fertility.rate_2022 <- census_2022_2002$total.fertility.rate[census_2022_2002$year == 2022]
hist(fertility.rate_2022, col = 'red', main = "Frequency Distribution of Fertility Rate in 2022",
     freq = TRUE, ylim = c(0, 100), xlab = "Fertility Rate")
abline(v = mean(fertility.rate_2022), col = "green", lwd = 2)
abline(v = median(fertility.rate_2022), col = "blue", lwd = 2)
legend("topright", legend = c("Mean", "Median"), col = c("green", "blue"), lty = 1, cex = 0.8, bty = "n")
summary(fertility.rate_2022)

# Histogram of life expectancy in 2022
life.expectancy_2022 <- census_2022_2002$life.expectancy.both.sexes[census_2022_2002$year == 2022]
hist(life.expectancy_2022, col = 'red', main = "Frequency Distribution of Life Expectancy in 2022", 
     freq = TRUE, xlab = "Life Expectancy")
abline(v = mean(life.expectancy_2022), col = "green", lwd = 2)
abline(v = median(life.expectancy_2022), col = "blue", lwd = 2) 
legend("topleft", legend = c("Mean", "Median"), col = c("green", "blue"), lty = 1, cex = 0.8, bty = "n")
summary(life.expectancy_2022)

# Histogram of life expectancy of male in 2022
life.expectancy_male_2022 <- census_2022_2002$life.expectancy.males[census_2022_2002$year == 2022]
hist(life.expectancy_male_2022, col = 'red', main = "Frequency Distribution of Male's Life Expectancy in 2022",
     freq = TRUE , xlab = "Male Life Expectancy")
abline(v = mean(life.expectancy_male_2022), col = "green", lwd = 2)
abline(v = median(life.expectancy_male_2022), col = "blue", lwd = 2) 
legend("topleft", legend = c("Mean", "Median"), col = c("green", "blue"), lty = 1, cex = 0.8, bty = "n")
summary(life.expectancy_male_2022)

# Histogram of life expectancy of female in 2022
life.expectancy_female_2022 <- census_2022_2002$life.expectancy.females[census_2022_2002$year == 2022]
hist(life.expectancy_female_2022, col = 'red', main = "Frequency Distribution of Female's Life Expectancy in 2022",
     freq = TRUE, xlab = "Female Life Expectancy")
abline(v = mean(life.expectancy_female_2022), col = "green", lwd = 2)
abline(v = median(life.expectancy_female_2022), col = "blue", lwd = 2) 
legend("topleft", legend = c("Mean", "Median"), col = c("green", "blue"), lty = 1, cex = 0.8, bty = "n")
summary(life.expectancy_female_2022)

# Histogram of difference in life expectancy in 2022
life.expectancy_diff <-  life.expectancy_female_2022 - life.expectancy_male_2022 
hist(life.expectancy_diff, col = 'red', main = "Male and Female Life Expectancy Difference in 2022",
     freq = TRUE, xlab = "Male & Female Life Expectancy Difference", ylim = c(0, 120))
abline(v = mean(life.expectancy_diff), col = "green", lwd = 2)
abline(v = median(life.expectancy_diff), col = "blue", lwd = 2)
legend("topright", legend = c("Mean", "Median"), col = c("green", "blue"), lty = 1, cex = 0.8, bty = "n")
summary(life.expectancy_diff)


# Plot male and female life expectancy for all countries as indices
plot(life.expectancy_male_2022, type = "o", col = "red", ylab = "Life Expectancy")
lines(life.expectancy_female_2022, type = "o", col = "blue")
legend("bottomright", legend = c("Male", "Female"), col = c("red", "blue"), lty = 1:1, cex = 1.2)

# Scatter matrix for pairs of variables to observe relationships between variables
pairs(census_2022_2002[census_2022_2002$year == 2022, 5:8])

# Compute the correlation matrices
pearson_corr <- cor(census_2022_2002[census_2022_2002$year == 2022, 5:8], method = "pearson")
spearman_corr <- cor(census_2022_2002[census_2022_2002$year == 2022, 5:8], method = "spearman")

# Plot Pearson correlation matrix
par(mfrow = c(1, 2), mar = c(2, 2, 2, 2))
corrplot(pearson_corr, method = "number", type = "upper", title = "Pearson Coefficients", 
         mar = c(0, 0, 2, 0), number.cex = 0.7, cl.ratio = 0.6, cl.align.text = 'r')

# Plot Spearman correlation matrix
corrplot(spearman_corr, method = "number", type = "upper", title = "Spearman Coefficients", 
         mar = c(0, 0, 2, 0), number.cex = 0.7, cl.ratio = 0.6, cl.align.text = 'r') 



# Boxplot for total fertility rate
data_fer <- data.frame(
  region = census_2022_2002[census_2022_2002$year == 2022, 'region'],
  subregion = census_2022_2002[census_2022_2002$year == 2022, 'subregion'],
  fertility = census_2022_2002[census_2022_2002$year == 2022, 'total.fertility.rate']
)
data_fer <- data_fer[order(data_fer$region, data_fer$subregion), ]
data_fer$subregion <- factor(data_fer$subregion, levels = rev(unique(data_fer$subregion)), ordered = TRUE)
ggplot(data_fer, aes(subregion, fertility, fill = region)) + 
  geom_boxplot() + 
  coord_flip() + 
  theme_minimal() + 
  labs(title = "Total Fertility Rate by Subregion in 2022", x = "Subregion", y = "Fertility Rate") +
  theme(plot.title = element_text(hjust = 0.5))

# Boxplot for life expectancy of both sexes
data_exp <- data.frame(
  region = census_2022_2002[census_2022_2002$year == 2022, 'region'],
  subregion = census_2022_2002[census_2022_2002$year == 2022, 'subregion'],
  life_expectancy = census_2022_2002[census_2022_2002$year == 2022, 'life.expectancy.both.sexes']
)
data_exp <- data_exp[order(data_exp$region, data_exp$subregion), ]
data_exp$subregion <- factor(data_exp$subregion, levels = rev(unique(data_exp$subregion)), ordered = TRUE)
ggplot(data_exp, aes(subregion, life_expectancy, fill = region)) + 
  geom_boxplot() + 
  coord_flip() + 
  theme_minimal() + 
  labs(title = "Life Expectancy (Both Sexes) by Subregion in 2022", x = "Subregion", y = "Life Expectancy") +
  theme(plot.title = element_text(hjust = 0.5))

# Boxplot for life expectancy of males
data_male <- data.frame(
  region = census_2022_2002[census_2022_2002$year == 2022, 'region'],
  subregion = census_2022_2002[census_2022_2002$year == 2022, 'subregion'],
  life_expectancy = census_2022_2002[census_2022_2002$year == 2022, 'life.expectancy.males']
)
data_male <- data_male[order(data_male$region, data_male$subregion), ]
data_male$subregion <- factor(data_male$subregion, levels = rev(unique(data_male$subregion)), ordered = TRUE)
ggplot(data_male, aes(subregion, life_expectancy, fill = region)) + 
  geom_boxplot() + 
  coord_flip() + 
  theme_minimal() + 
  labs(title = "Life Expectancy (Males) by Subregion in 2022", x = "Subregion", y = "Life Expectancy for Males") +
  theme(plot.title = element_text(hjust = 0.5))

# Boxplot for life expectancy of females
data_female <- data.frame(
  region = census_2022_2002[census_2022_2002$year == 2022, 'region'],
  subregion = census_2022_2002[census_2022_2002$year == 2022, 'subregion'],
  life_expectancy = census_2022_2002[census_2022_2002$year == 2022, 'life.expectancy.females']
)
data_female <- data_female[order(data_female$region, data_female$subregion), ]
data_female$subregion <- factor(data_female$subregion, levels = rev(unique(data_female$subregion)), ordered = TRUE)
ggplot(data_female, aes(subregion, life_expectancy, fill = region)) + 
  geom_boxplot() + 
  coord_flip() + 
  theme_minimal() + 
  labs(title = "Life Expectancy (Females) by Subregion in 2022", x = "Subregion", y = "Life Expectancy for Females") +
  theme(plot.title = element_text(hjust = 0.5))

# Fertility rate 2002 vs 2022
data_fert <- dcast(census_2022_2002, country + subregion ~ factor(year), value.var = "total.fertility.rate")
ggplot(data = data_fert, mapping = aes(x = `2002`, y = `2022`)) +
  geom_point() + 
  facet_wrap(. ~ subregion) + 
  geom_abline() + 
  theme_minimal() + 
  labs(title = "Fertility Rate 2002 vs 2022", x = "Fertility Rate in 2002", y = "Fertility Rate in 2022") +
  theme(plot.title = element_text(hjust = 0.5))

# Life expectancy for both sexes 2002 vs 2022
data_life_exp <- dcast(census_2022_2002, country + subregion ~ factor(year), value.var = "life.expectancy.both.sexes")
ggplot(data = data_life_exp, mapping = aes(x = `2002`, y = `2022`)) +
  geom_point() + 
  facet_wrap(. ~ subregion) + 
  geom_abline() + 
  theme_minimal() + 
  labs(title = "Life Expectancy for Both Sexes 2002 vs 2022", x = "Life Expectancy in 2002", y = "Life Expectancy in 2022") +
  theme(plot.title = element_text(hjust = 0.5))

