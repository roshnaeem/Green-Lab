install.packages("ggplot2")
install.packages("dplyr")
library(ggplot2)
library(dplyr)

#access the data file, data_file is global variable, which are gonna fuck me at some point. 
data_file

energy_data = data_file$energy

# get the data for each electon app
skype_electron_data <- data_file %>% filter(app == 1)
slack_electron_data <- data_file %>% filter(app == 2)
discord_electron_data <- data_file %>% filter(app == 3)


# Summaries
summary(skype_electron_data$energy)
summary(slack_electron_data$energy)
summary(discord_electron_data$energy)

# Boxplot for the different apps
ggplot(data = data_file, aes(x = as.factor(app), y = energy)) +
  ylim(0, NA) +
  xlab("App") +
  ylab("Energy consumption [J]") +
  geom_violin() + 
  geom_boxplot(width=.1, alpha=.5, position=position_dodge(.9)) +
  stat_summary(fun=mean, geom="point", shape="diamond", size=3, color="black")

# Scatterplot for the different apps
ggplot(data = data_file, aes(x = as.factor(app), y = energy)) +
  ylim(0, NA) +
  xlab("App") +
  ylab("Energy consumption [J]") +
  geom_jitter()

