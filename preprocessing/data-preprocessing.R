install.packages("pROC")
library(pROC)

## for powerjoular, 2 mins meeting should have 120 lines, and for 8 mins meeting, should have 480 lines
## right now are 119 and 476 for 2 an d4 mins duration
## put the mean in missing value

# Create an empty data frame with the column names you need
empty_data <- data.frame(run_id = character(), 
                         microphone = character(), 
                         camera = character(), 
                         screen = character(), 
                         duration = character(), 
                         app = character(), 
                         network = character(), 
                         memory = character(),
                         energy =  character(),
                         cpu = character(),
                         stringsAsFactors = FALSE)

# Set the path to the directory containing subdirectories
data_folder <- "data-analysis/data/ExperimentData"

# List all directories inside the 'data' folder
directories <- list.dirs(path = data_folder, full.names = TRUE, recursive = FALSE)

# Loop through each directory and append data to empty_data
for (dir in directories) {
  
  base_dir <- basename(dir)

  # Split the string based on underscores and convert to numeric
  split_values <- unlist(strsplit(base_dir, "_"))
  
  # Convert the split values to a list and reverse them
  rev_digits <- rev(as.list(split_values))
  
  # Reset vector for each directory
  network <- character()
  memory <- character()
  energy <- character()
  cpu <- character()

  # Check if there are at least 6 digits in the directory name
  if (length(rev_digits) >= 6) {

    #Extract the elements from list
    run_id <-as.numeric(sub("Iter", "", rev_digits[6]))
    microphone <- as.numeric(rev_digits[5])
    camera <- as.numeric(rev_digits[4])
    screen <- as.numeric(rev_digits[3])
    duration <- as.numeric(rev_digits[2])
    app <- as.numeric(rev_digits[1])

    # Construct the path to the packet_counts.csv file in the current directory
    packets_file <- file.path(dir, "packet_counts.csv")
    memory_file <- file.path(dir, "ps.csv")
    power_file <- file.path(dir, "powerjoular.csv")
    
    #network from packets_counts file
    if (length(packets_file) > 0) {
      packets_data <- read.csv(packets_file, header = TRUE, stringsAsFactors = FALSE)
      
      # Extract the 'total packets' column
      total_packets <- packets_data$Total.Packets
      
      # Add 'total packets' column to the network vector
      network <- c(network, total_packets)
      
    }
    if (length(memory_file) > 0) {
      memory_data <- read.csv(memory_file, header = TRUE, stringsAsFactors = FALSE)
      
      mem_col <- memory_data$X.mem
      timestamp_col <- memory_data$timestamp
      
      # Convert timestamp to POSIXct for proper grouping
      timestamp_format <- as.POSIXct(timestamp_col, format="%Y-%m-%d %H:%M:%S")
      
      # Group mem by timestamp and create a list of mem values
      grouped_data <- aggregate(mem_col ~ timestamp, data = memory_data, FUN = sum)
      
      # Calculate the mean of the "%mem" column, grouped by timestamp
      mem_avg <- round(mean(grouped_data$mem_col, na.rm = TRUE), 5)

      memory <- c(memory, mem_avg)
    }
    if (length(power_file) > 0) {
      
      power_data <- read.csv(power_file, header = TRUE, stringsAsFactors = FALSE)
      
      # # Convert Date column to proper date-time format
      # power_data$Date <- as.POSIXct(power_data$Date, format = "%Y-%m-%d %H:%M:%S")
      # 
      # # Calculate AUC for Total Power over time
      # roc_curve <- roc(power_data$Date, power_data$Total.Power)
      # auc_value <- auc(roc_curve)
      # print(auc_value)
      
      # Calculate the sum of the "energy" column
      energy_sum <- sum(power_data$Total.Power, na.rm = TRUE)
      energy <- c(energy, energy_sum)
      
      # Calculate the mean of the "cpu utilization" column
      cpu_average <- round(mean(power_data$CPU.Utilization, na.rm = TRUE), 5)
      cpu <- c(cpu, cpu_average)
    }
    
    # Create a data frame with the extracted digits
    extracted_data <- data.frame(run_id, microphone, camera, screen, duration, app, network, memory, energy, cpu, stringsAsFactors = FALSE)
    
    # Append the extracted data to the empty_data data frame
    empty_data <- rbind(empty_data, extracted_data)

  }
}

# Write the appended data to the existing CSV file
write.csv(empty_data, file = "data-analysis/data.csv", row.names = FALSE, quote = FALSE)

# Check the updated contents of the CSV file
read.csv("data-analysis/data.csv")
