# Define the root directory where your folders are located
root_dir <- "\\\\storage1.ris.wustl.edu\\prwillia\\Active\\Alsu\\AC3_Rootletin_Rbpms"

if (file.exists(root_dir)) {
  print("Directory exists!")
} else {
  print("Directory does not exist!")
}


read_csv_in_folders <- function(root_dir) {
  # Initialize four empty lists to store data frames for different folder categories
  cilia_visible_green_data <- list()
  includes_invisible_rgccilia_data <- list()
  onlyrgcall_data <- list()
  onlyrgcvisible_data <- list()
  
  # Get all directories (subfolders) within the root directory, including nested ones
  all_folders <- list.dirs(root_dir, recursive = TRUE, full.names = TRUE)
  
  # Loop through each folder
  for (folder in all_folders) {
    # Get the list of CSV files in the current folder matching the given pattern
    csv_files <- list.files(folder, pattern = "Ac3_488_RootlrtinRRX_Rbpms408_Segment_Length.*\\.csv$", full.names = TRUE)
    
    # If there are any CSV files in the current folder, process them
    if (length(csv_files) > 0) {
      for (csv_file in csv_files) {
        # Read the CSV file
        data <- read.csv(csv_file, fill = TRUE, header = FALSE, check.names = FALSE, na.strings = c("", "NA"))
        
        # Check the folder name and categorize the data accordingly
        if (grepl("Cilia_visible_green", folder)) {
          cilia_visible_green_data[[csv_file]] <- data
        } else if (grepl("Includes_invisible_RGCcilia", folder)) {
          includes_invisible_rgccilia_data[[csv_file]] <- data
        } else if (grepl("OnlyRGCall", folder)) {
          onlyrgcall_data[[csv_file]] <- data
        } else if (grepl("OnlyRGCvisible", folder)) {
          onlyrgcvisible_data[[csv_file]] <- data
        }
      }
    }
  }
  
  # Return the four lists of data frames
  return(list(
    cilia_visible_green = cilia_visible_green_data,
    includes_invisible_rgccilia = includes_invisible_rgccilia_data,
    onlyrgcall = onlyrgcall_data,
    onlyrgcvisible = onlyrgcvisible_data
  ))
} 

# Call the function to process the files
result_lists <- read_csv_in_folders(root_dir)


### Data clean-up

# Function to remove the first two rows from multiple data frames and set the new first row as column names
remove_first_two_rows_and_set_colnames <- function(result_lists) {
  # Iterate through each list in result_lists (i.e., each category of data frames)
  for (list_name in names(result_lists)) {
    # Get the list of data frames for this category
    dfs <- result_lists[[list_name]]
    
    # Iterate over the data frames within the current category
    for (i in seq_along(dfs)) {
      # Remove the first two rows
      dfs[[i]] <- dfs[[i]][-c(1, 2), ]
      
      # Set the first row as the column names
      colnames(dfs[[i]]) <- as.character(dfs[[i]][1, ])
      
      # Remove the first row now that it has been set as the column names
      dfs[[i]] <- dfs[[i]][-1, ]
    }
    
    # Update the result_lists to hold the modified data frames within the same category
    result_lists[[list_name]] <- dfs
  }
  
  # Return the modified result_lists with the original categories preserved
  return(result_lists)
}

# Call the function on the result_lists
modified_lists <- remove_first_two_rows_and_set_colnames(result_lists)


# Empty lists to store the results
amacrine_result_list <- list()
rgc_result_list <- list()

# Initialize the result lists
amacrine_result_list <- list()
rgc_result_list <- list()

# Initialize the result lists
amacrine_result_list <- list()
rgc_result_list <- list()

# Iterate over the files in modified_lists$cilia_visible_green
for (i in 1:length(modified_lists$cilia_visible_green)) {
  # Check if the index exists in both lists
  if (i <= length(modified_lists$onlyrgcvisible)) {
    # Extract corresponding data frames from each list
    green_data <- modified_lists$cilia_visible_green[[i]]
    visible_data <- modified_lists$onlyrgcvisible[[i]]
    
    # Check if both green_data and visible_data are non-empty data frames
    if (is.data.frame(green_data) && is.data.frame(visible_data)) {
      
      # Ensure "Segment Length" is numeric in both data frames
      green_data$`Segment Length` <- as.numeric(green_data$`Segment Length`)
      visible_data$`Segment Length` <- as.numeric(visible_data$`Segment Length`)
      
      # Remove rows with NA values in "Segment Length" (if necessary)
      green_data_clean <- green_data[!is.na(green_data$`Segment Length`), ]
      visible_data_clean <- visible_data[!is.na(visible_data$`Segment Length`), ]
      
      # Find the matched rows based on "Segment Length" using merge
      matched_rows <- merge(green_data_clean, visible_data_clean, by = "Segment Length", all = FALSE)
      
      # Find the remaining rows in green_data that are not present in visible_data
      remaining_data <- green_data_clean[!green_data_clean$`Segment Length` %in% matched_rows$`Segment Length`, ]
      
      # If remaining_data has rows left, add it to the amacrine_result_list
      if (nrow(remaining_data) > 0) {
        amacrine_result_list[[length(amacrine_result_list) + 1]] <- remaining_data
      }
      
      # If matched_rows has rows, add them to the rgc_result_list
      if (nrow(matched_rows) > 0) {
        rgc_result_list[[length(rgc_result_list) + 1]] <- matched_rows
      }
      
    } else {
      # Handle the case where the data is not a data frame
      message(paste("Data at index", i, "is not a data frame. Skipping."))
    }
  } else {
    # If there is no corresponding file in onlyrgcvisible, skip the current file
    message(paste("No corresponding file in 'onlyrgcvisible' for file at index", i, ". Skipping."))
  }
}


# Combine all resulting data frames into a single data frame if there are any valid results
if (length(amacrine_result_list) > 0) {
  amacrine_result <- do.call(rbind, amacrine_result_list)
  # Optionally, view the amacrine_result
  print(amacrine_result)
} else {
  print("No valid data to combine for amacrine_result.")
}

# Combine the matched rows into a single data frame for rgc_result
if (length(rgc_result_list) > 0) {
  rgc_result <- do.call(rbind, rgc_result_list)
  # Optionally, view the rgc_result
  print(rgc_result)
} else {
  print("No valid data to combine for rgc_result.")
}


# Load the necessary library for plotting
library(ggplot2)


# Plot histogram for "Segment Length" in rgc_result (temporary numeric conversion and remove NA)
ggplot(rgc_result_clean, aes(x = as.numeric(`Segment Length`))) +
  geom_histogram(binwidth = 1, fill = "skyblue", color = "black", alpha = 0.7) +
  labs(title = "Histogram of Segment Length in rgc_result", x = "Segment Length", y = "Frequency") +
  theme_minimal()


# Plot histogram for "Segment Length" in amacrine_result (temporary numeric conversion and remove NA)
ggplot(amacrine_result_clean, aes(x = as.numeric(`Segment Length`))) +
  geom_histogram(binwidth = 1, fill = "lightgreen", color = "black", alpha = 0.7) +
  labs(title = "Histogram of Segment Length in amacrine_result", x = "Segment Length", y = "Frequency") +
  theme_minimal()

# Load necessary library
library(ggplot2)

# First, combine amacrine_result and rgc_result directly into a single dataframe

# Add a 'Category' column to distinguish between Amacrine and RGC
amacrine_result$Category <- "Amacrine"
rgc_result$Category <- "RGC"

# Now, combine the two datasets into one for easier plotting
combined_data <- rbind(amacrine_result, rgc_result)

# Make sure "Segment Length" is numeric
combined_data$`Segment Length` <- as.numeric(combined_data$`Segment Length`)

# Remove rows with NA values in "Segment Length"
combined_data_clean <- combined_data[!is.na(combined_data$`Segment Length`), ]

# T-test to compare means of Segment Length between Amacrine and RGC
t_test_result <- t.test(`Segment Length` ~ Category, data = combined_data_clean)

# Plot 1: Boxplot showing the distribution of Segment Length by Category
boxplot <- ggplot(combined_data_clean, aes(x = Category, y = `Segment Length`, fill = Category)) +
  geom_boxplot() +
  labs(title = "Distribution of Segment Length by Category", x = "Category", y = "Segment Length") +
  theme_minimal() +
  annotate("text", x = 1.5, y = max(combined_data_clean$`Segment Length`, na.rm = TRUE), 
           label = paste("t-test p-value:", format(t_test_result$p.value, digits = 3)), size = 5)

# Plot 2: Jitter plot showing the distribution of Segment Length by Category
jitterplot <- ggplot(combined_data_clean, aes(x = Category, y = `Segment Length`, color = Category)) +
  geom_jitter(width = 0.2, size = 2, alpha = 0.7) +
  stat_summary(fun = "mean", geom = "point", shape = 18, size = 3, color = "black") +  # Add the mean points
  stat_summary(fun.data = "mean_cl_normal", geom = "errorbar", width = 0.2) +  # Add error bars (confidence interval around the mean)
  labs(title = "Jitter Plot of Segment Length by Category", x = "Category", y = "Segment Length") +
  theme_minimal() +
  annotate("text", x = 1.5, y = max(combined_data_clean$`Segment Length`, na.rm = TRUE), 
           label = paste("t-test p-value:", format(t_test_result$p.value, digits = 3)), size = 5)


# Display the plots
print(boxplot)
print(jitterplot)

