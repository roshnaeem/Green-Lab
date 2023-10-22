install.packages("pROC")
library(pROC)

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
  
  print(dir)
  
  # Extract digits from the directory name
  digits <- gsub("\\D", "", basename(dir)) # Using 'basename' to get the name of the directory
  
  print(digits)
  
  
  # Check if there are at least 6 digits in the directory name
  if (nchar(digits) >= 6) {
    
    print(digits)
    
    rev_digits <- rev(strsplit(digits, '')[[1]])
    
    print(rev_digits)
    
    
    # Reset network vector for each directory
    network <- character()
    memory <- character()
    energy <- character()
    cpu <- character()
    
    # Extract the first six digits for run_id, microphone, camera, screen, duration, and app
    app <- rev_digits[1]
    duration <- rev_digits[2]
    screen <- rev_digits[3]
    camera <- rev_digits[4]
    microphone <- rev_digits[5]
    run_id <- rev_digits[6]
    
    #print(paste(run_id, app, duration))
    
    # Construct the path to the packet_counts.csv file in the current directory
    packets_file <- file.path(dir, "packet_counts.csv")
    memory_file <- file.path(dir, "ps.csv")
    power_file <- file.path(dir, "powerjoular.csv")

    #network from packets_counts file
    if (length(packets_file) > 0) {
      packets_data <- read.csv(packets_file, header = TRUE, stringsAsFactors = FALSE)
      #print(packets_data)
      
      # Extract the 'total packets' column
      total_packets <- packets_data$Total.Packets
      #print(total_packets)
      
      # Add 'total packets' column to the network vector
      network <- c(network, total_packets)
      
    }
    if (length(memory_file) > 0) {
      memory_data <- read.csv(memory_file, header = TRUE, stringsAsFactors = FALSE)

      # Calculate the sum of the "%mem" column
      mem_sum <- sum(memory_data$X.mem, na.rm = TRUE)

      memory <- c(memory, mem_sum)
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
      
      # Calculate the sum of the "cpu utilization" column
      cpu_average <- mean(power_data$CPU.Utilization, na.rm = TRUE)
      print(cpu_average)

      cpu <- c(cpu, cpu_average)
    }
    
    # Create a data frame with the extracted digits and network data
    extracted_data <- data.frame(run_id, microphone, camera, screen, duration, app, network, memory, energy, cpu, stringsAsFactors = FALSE)
    
    # Append the extracted data to the empty_data data frame
    empty_data <- rbind(empty_data, extracted_data)
    
  }
}

# Remove duplicate rows
#empty_data <- unique(empty_data)

# Write the appended data to the existing CSV file
#write.csv(empty_data, file = "data-analysis/data.csv", row.names = FALSE, quote = FALSE)

# Check the updated contents of the CSV file
#read.csv("data-analysis/data.csv")

