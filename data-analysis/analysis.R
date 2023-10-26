install.packages("ggplot2")
install.packages("dplyr")
install.packages("stats")
library(ggplot2)
library(dplyr)
library(stats)


#access the data file, data_file is global variable, which are gonna fuck me up at some point. 
data_file


# Summaries for web apps
# 1st Quartile, that 255of values fall below this point, 25% data is less than or equal to first quartile
summary(skype_web_data$energy)
summary(slack_web_data$energy)
summary(discord_web_data$energy)

summary(skype_web_data$cpu)
summary(slack_web_data$cpu)
summary(discord_web_data$cpu)

summary(skype_web_data$memory)
summary(slack_web_data$memory)
summary(discord_web_data$memory)

summary(skype_web_data$network)
summary(slack_web_data$network)
summary(discord_web_data$network)


# Summaries for electron app
# 1st Quartile, that 255of values fall below this point, 25% data is less than or equal to first quartile
summary(skype_electron_data$energy)
summary(slack_electron_data$energy)
summary(discord_electron_data$energy)

summary(skype_electron_data$cpu)
summary(slack_electron_data$cpu)
summary(discord_electron_data$cpu)

summary(skype_electron_data$memory)
summary(slack_electron_data$memory)
summary(discord_electron_data$memory)

summary(skype_electron_data$network)
summary(slack_electron_data$network)
summary(discord_electron_data$network)

#make histograms
# energy - 2 mins
hist_energy_2 <- ggplot(data = avg_config_data_file_2, aes(x = app, y = energy, fill = factor(app_type))) +
  geom_bar(stat = "identity", position = "dodge") +
  xlab("Apps") +
  ylab("Energy") +
  ggtitle("Energy Consumption for Duration = 2") +
  scale_fill_manual(values = c("electron" = "#5DADE2", "web" = "#7DCEA0")) +
  theme_minimal()

hist_energy_2

#energy - 8 mins
hist_energy_8 <- ggplot(data = avg_config_data_file_8, aes(x = app, y = energy, fill = factor(app_type))) +
  geom_bar(stat = "identity", position = "dodge") +
  xlab("Apps") +
  ylab("Energy") +
  ggtitle("Energy Consumption for Duration = 8") +
  scale_fill_manual(values = c("electron" = "#C39BD3", "web" = "#E59866")) +
  theme_minimal()

hist_energy_8

# cpu - 2 mins
hist_cpu_2 <- ggplot(data = avg_config_data_file_2, aes(x = app, y = cpu, fill = factor(app_type))) +
  geom_bar(stat = "identity", position = "dodge") +
  xlab("Apps") +
  ylab("Energy") +
  ggtitle("CPU Usage for Duration = 2") +
  scale_fill_manual(values = c("electron" = "#5DADE2", "web" = "#7DCEA0")) +
  theme_minimal()

hist_cpu_2

# cpu - 8 mins
hist_cpu_8 <- ggplot(data = avg_config_data_file_8, aes(x = app, y = cpu, fill = factor(app_type))) +
  geom_bar(stat = "identity", position = "dodge") +
  xlab("Apps") +
  ylab("Energy") +
  ggtitle("CPU Usage for Duration = 8") +
  scale_fill_manual(values = c("electron" = "#C39BD3", "web" = "#E59866")) +
  theme_minimal()

hist_cpu_8

# mem - 2 mins
hist_mem_2 <- ggplot(data = avg_config_data_file_2, aes(x = app, y = memory, fill = factor(app_type))) +
  geom_bar(stat = "identity", position = "dodge") +
  xlab("Apps") +
  ylab("Energy") +
  ggtitle("Memory Usage for Duration = 2") +
  scale_fill_manual(values = c("electron" = "#5DADE2", "web" = "#7DCEA0")) +
  theme_minimal()
hist_mem_2

# mem - 8 mins
hist_mem_8 <- ggplot(data = avg_config_data_file_8, aes(x = app, y = memory, fill = factor(app_type))) +
  geom_bar(stat = "identity", position = "dodge") +
  xlab("Apps") +
  ylab("Energy") +
  ggtitle("Memory Usage for Duration = 8") +
  scale_fill_manual(values = c("electron" = "#C39BD3", "web" = "#E59866")) +
  theme_minimal()
hist_mem_8

# network - 2 mins
hist_net_2 <- ggplot(data = avg_config_data_file_2, aes(x = app, y = network, fill = factor(app_type))) +
  geom_bar(stat = "identity", position = "dodge") +
  xlab("Apps") +
  ylab("Energy") +
  ggtitle("Network Usage for Duration = 2") +
  scale_fill_manual(values = c("electron" = "#5DADE2", "web" = "#7DCEA0")) +
  theme_minimal()
hist_net_2

# mem - 8 mins
hist_net_8 <- ggplot(data = avg_config_data_file_8, aes(x = app, y = network, fill = factor(app_type))) +
  geom_bar(stat = "identity", position = "dodge") +
  xlab("Apps") +
  ylab("Energy") +
  ggtitle("Network Usage for Duration = 8") +
  scale_fill_manual(values = c("electron" = "#C39BD3", "web" = "#E59866")) +
  theme_minimal()
hist_net_8

# Save the plot to the "plots" directory
ggsave("graphs/hist_energy_2.png", hist_energy_2)
ggsave("graphs/hist_energy_8.png", hist_energy_8)
ggsave("graphs/hist_cpu_2.png", hist_cpu_2)
ggsave("graphs/hist_cpu_8.png", hist_cpu_8)
ggsave("graphs/hist_mem_2.png", hist_mem_2)
ggsave("graphs/hist_mem_8.png", hist_mem_8)
ggsave("graphs/hist_net_2.png", hist_net_2)
ggsave("graphs/hist_net_8.png", hist_net_8)


print(avg_config_data_file_web_2)
print(avg_config_data_file_electron_2)
print(avg_config_data_file_web_8)
print(avg_config_data_file_electron_8)

# Perform Mann-Whitney U Test
# for electron nd web apps for 2 mins and 8 mins - energy
mwu_result_energy_2 <- wilcox.test(avg_config_data_file_web_2$energy, avg_config_data_file_electron_2$energy, alternative = "two.sided")
# Print the test result
print(mwu_result_energy_2)

# for electron and web for 8 mins
mwu_result_energy_8 <- wilcox.test(avg_config_data_file_web_8$energy, avg_config_data_file_electron_8$energy, alternative = "two.sided")
# Print the test result
print(mwu_result_energy_8)

# for electron nd web apps for 2 mins and 8 mins - cpu
mwu_result_cpu_2 <- wilcox.test(avg_config_data_file_web_2$cpu, avg_config_data_file_electron_2$cpu, alternative = "two.sided")
# Print the test result
print(mwu_result_cpu_2)

# for electron and web for 8 mins
mwu_result_cpu_8 <- wilcox.test(avg_config_data_file_web_8$cpu, avg_config_data_file_electron_8$cpu, alternative = "two.sided")
# Print the test result
print(mwu_result_cpu_8)

# For electron and web apps for 2 mins - memory
mwu_result_memory_2 <- wilcox.test(avg_config_data_file_web_2$memory, avg_config_data_file_electron_2$memory, alternative = "two.sided")
# Print the test result
print(mwu_result_memory_2)

# For electron and web apps for 8 mins - memory
mwu_result_memory_8 <- wilcox.test(avg_config_data_file_web_8$memory, avg_config_data_file_electron_8$memory, alternative = "two.sided")
# Print the test result
print(mwu_result_memory_8)

# For electron and web apps for 2 mins - network
mwu_result_network_2 <- wilcox.test(avg_config_data_file_web_2$network, avg_config_data_file_electron_2$network, alternative = "two.sided")
# Print the test result
print(mwu_result_network_2)

# For electron and web apps for 8 mins - network
mwu_result_network_8 <- wilcox.test(avg_config_data_file_web_8$network, avg_config_data_file_electron_8$network, alternative = "two.sided")
# Print the test result
print(mwu_result_network_8)


# # Boxplot for the different apps
# ggplot(data = data_file, aes(x = as.factor(app), y = energy)) +
#   ylim(0, NA) +
#   xlab("App") +
#   ylab("Energy consumption [J]") +
#   geom_violin() + 
#   geom_boxplot(width=.1, alpha=.5, position=position_dodge(.9)) +
#   stat_summary(fun=mean, geom="point", shape="diamond", size=3, color="black")
# 
# # Scatterplot for the different apps
# ggplot(data = avg_config_data_file, aes(x = as.factor(app), y = energy)) +
#   ylim(0, NA) +
#   xlab("App") +
#   ylab("Energy consumption [J]") +
#   geom_jitter()
# 
