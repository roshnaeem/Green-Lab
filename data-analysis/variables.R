install.packages("dplyr")
library(dplyr)

#access the data file
data_file <- read.csv("data-analysis/data.csv")
avg_config_data_file <- read.csv("data-analysis/avg-config-data.csv")
transform_data_file <- read.csv("data-analysis/transformed-data.csv")
data_file
avg_config_data_file
transform_data_file


#electron and web apps data
web_data <- data_file %>% filter(app_type == "web")
electron_data <- data_file %>% filter(app_type == "electron")


# web apps data
web_energy_data <- web_data$energy
web_cpu_data <- web_data$cpu
web_network_data <- web_data$network
web_memory_data <- web_data$memory

#electron apps data
electron_energy_data <- electron_data$energy
electron_cpu_data <- electron_data$cpu
electron_network_data <- electron_data$network
electron_memory_data <- electron_data$memory

# get the data for each electon app
skype_electron_data <- data_file %>% filter(app == 1 & app_type == "electron")
slack_electron_data <- data_file %>% filter(app == 2   & app_type == "electron")
discord_electron_data <- data_file %>% filter(app == 3 &  app_type == "electron")

# get the data for each electon app
skype_web_data <- data_file %>% filter(app == 1 & app_type == "web")
slack_web_data <- data_file %>% filter(app == 2   & app_type == "web")
discord_web_data <- data_file %>% filter(app == 3 &  app_type == "web")

data_file_web_2 <- data_file %>% filter(duration == 2 & app_type == "web")
data_file_web_8 <- data_file %>% filter(duration == 8 & app_type == "web")
data_file_electron_1 <- data_file %>% filter(duration == 2 & app_type == "electron" & app == 1)
data_file_electron_2 <- data_file %>% filter(duration == 2 & app_type == "electron" & app == 2)
data_file_electron_3 <- data_file %>% filter(duration == 2 & app_type == "electron" & app == 3)

data_file_electron_81 <- data_file %>% filter(duration == 8 & app_type == "electron" & app == 1)
data_file_electron_82 <- data_file %>% filter(duration == 8 & app_type == "electron" & app == 2)
data_file_electron_83 <- data_file %>% filter(duration == 8 & app_type == "electron" & app == 3)

print(data_file_web_2)

avg_config_data_file <- read.csv("data-analysis/avg-config-data.csv") %>%
  mutate(app = ifelse(app == 1, "Skype", 
                      ifelse(app == 2, "Slack",
                             ifelse(app == 3, "Discord", "Unknown"))))

avg_config_data_file_2 <- avg_config_data_file %>% filter(duration == 2)
avg_config_data_file_8 <- avg_config_data_file %>% filter(duration == 8)

avg_config_data_file_web_2 <- avg_config_data_file %>% filter(duration == 2 & app_type == "web")
avg_config_data_file_web_8 <- avg_config_data_file %>% filter(duration == 8 & app_type == "web")
avg_config_data_file_electron_2 <- avg_config_data_file %>% filter(duration == 2 & app_type == "electron")
avg_config_data_file_electron_8 <- avg_config_data_file %>% filter(duration == 8 & app_type == "electron")

