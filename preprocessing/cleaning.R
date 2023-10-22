install.packages("ggplot2")
install.packages("dplyr")
library(ggplot2)
library(dplyr)


data_file <- read.csv("data-analysis/data.csv")
data_file

# Function to calculate mode
mode <- function(x) {
  uniq_x <- unique(x)  # Get unique values in the vector
  freq <- tabulate(match(x, uniq_x))  # Count the frequency of each unique value
  mode_value <- uniq_x[which.max(freq)]  # Find the unique value with the highest frequency
  return(mode_value)
}

mean_energy <- mean(data_file$energy)
median_energy <- median(data_file$energy)
mode_energy <- mode(data_file$energy)


missing_values <- sum(is.na(data_file$duration))
print(missing_values)

# Print the variables in a single statement
print(paste("Mean Energy:", mean_energy, ", Median Energy:", median_energy, ", Mode Energy:", mode_energy))

# plot for energy vs duration
ggplot(data = data_file,
       aes(x = duration, y = energy, color = app)) +
  xlim(0, NA) +
  ylim(0, NA) +
  geom_smooth(method = lm , color = "gray", se = FALSE) +
  geom_jitter() +
  labs(x = "Duration [min]",
       y = "Energy consumption [J]",
       color = "App")

