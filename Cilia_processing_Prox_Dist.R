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

#install.packages("dplyr")

library(dplyr)

merge_by_keywords <- function(result_lists) {
  # Define the keywords for categories (Updated to Proximal and Distal)
  keywords <- c("Proximal", "Distal")
  
  # Initialize an empty list to store the merged data frames for each category
  merged_results <- list()
  
  # Loop through each category in result_lists (cilia_visible_green, includes_invisible_rgccilia, etc.)
  for (list_name in names(result_lists)) {
    category_data <- result_lists[[list_name]]
    
    # Initialize empty lists for each keyword category
    merged_results[[list_name]] <- list(Proximal = list(), Distal = list())
    
    # Loop through each data frame in the category
    for (i in seq_along(category_data)) {
      # Get the file name (key) for the data frame
      file_name <- names(category_data)[i]
      
      # Loop through each keyword (Proximal, Distal)
      for (keyword in keywords) {
        # Check if the keyword is present in the file name (case-insensitive)
        if (grepl(keyword, file_name, ignore.case = TRUE)) {
          # If a match is found, add the data frame to the corresponding list for that keyword
          merged_results[[list_name]][[keyword]] <- append(merged_results[[list_name]][[keyword]], list(category_data[[i]]))
        }
      }
    }
    
    # After categorizing, merge data frames within each keyword list by columns (matching column names)
    for (keyword in keywords) {
      # If there are multiple data frames to merge, merge them by columns (stacking the rows)
      if (length(merged_results[[list_name]][[keyword]]) > 1) {
        merged_results[[list_name]][[keyword]] <- bind_rows(merged_results[[list_name]][[keyword]])
      } else if (length(merged_results[[list_name]][[keyword]]) == 1) {
        # If only one data frame exists, no merge is needed, just assign it
        merged_results[[list_name]][[keyword]] <- merged_results[[list_name]][[keyword]][[1]]
      }
      
      # After merging, convert "Segment Length" to numeric if it exists
      if ("Segment Length" %in% colnames(merged_results[[list_name]][[keyword]])) {
        merged_results[[list_name]][[keyword]]$`Segment Length` <- as.numeric(merged_results[[list_name]][[keyword]]$`Segment Length`)
      }
    }
  }
  
  # Return the merged data frames for all categories
  return(merged_results)
}

# Call the function to merge data frames based on the keywords
merged_lists <- merge_by_keywords(modified_lists)


### Plotting

#install.packages("ggplot2", "tidyr", "gridExtra", "FSA")

# Load necessary libraries
library(ggplot2)
library(dplyr)
library(tidyr)
library(gridExtra)
library(FSA)  # For Dunn's test

# Function to plot distribution histograms for each category
plot_histograms <- function(merged_results) {
  
  # Define the updated keywords for categories
  keywords <- c("Proximal", "Distal")
  
  # Loop through each category in merged_results
  for (list_name in names(merged_results)) {
    category_data <- merged_results[[list_name]]
    
    # Create an empty list to store histograms
    plot_list <- list()
    
    # Loop through each keyword (Proximal, Distal)
    for (keyword in keywords) {
      if (keyword %in% names(category_data)) {
        # Get the current data frame for the keyword
        df <- category_data[[keyword]]
        
        # Add a new column to indicate the keyword (Proximal, Distal)
        df$Category <- keyword
        
        # Create a histogram for 'Segment Length' for this keyword
        plot_list[[keyword]] <- ggplot(df, aes(x = `Segment Length`)) +
          geom_histogram(binwidth = 0.5, fill = "skyblue", color = "black", alpha = 0.7) +
          labs(title = paste(list_name, "-", keyword, "Segment Length Distribution"),
               x = "Segment Length", y = "Frequency") +
          theme_minimal()
      }
    }
    
    # Arrange the histograms in a 2x2 grid for the current category
    if (length(plot_list) > 0) {
      grid.arrange(grobs = plot_list, ncol = 2)  # Arrange 4 plots in a 2x2 grid
    } else {
      cat("No data found for", list_name, "\n")
    }
  }
}

# Call the function (assuming 'merged_results' is available and correctly formatted)
plot_histograms(merged_lists)


# Function to plot Segment Length for each category, with separate boxplot and jitter plot
plot_segment_length_wilcox <- function(merged_results) {
  
  # Define the keywords for categories (Proximal and Distal)
  keywords <- c("Proximal", "Distal")
  
  # Create a list to store the plots
  all_plots <- list()
  
  # Loop through each category in merged_results
  for (list_name in names(merged_results)) {
    category_data <- merged_results[[list_name]]
    
    # Create an empty data frame to store the combined data for plotting
    combined_data <- data.frame()
    
    # Loop through each keyword (Proximal, Distal)
    for (keyword in keywords) {
      if (keyword %in% names(category_data)) {
        # Get the current data frame for the keyword
        df <- category_data[[keyword]]
        
        # Add a new column to indicate the keyword (Proximal, Distal)
        df$Category <- factor(keyword)  # Explicitly convert to factor
        
        # Bind the data to the combined_data data frame
        combined_data <- bind_rows(combined_data, df)
      }
    }
    
    # Check if there is any data to plot
    if (nrow(combined_data) > 0) {
      
      # Debugging: Check counts per Category
      print(table(combined_data$Category))
      
      # Pairwise Wilcoxon rank-sum tests for each combination of categories
      wilcox_results <- list()
      category_combinations <- combn(levels(combined_data$Category), 2, simplify = TRUE)
      
      # Run Wilcoxon test for each pair of categories
      for (pair in 1:ncol(category_combinations)) {
        category1 <- category_combinations[1, pair]
        category2 <- category_combinations[2, pair]
        
        # Filter data for the two categories
        subset_data <- combined_data[combined_data$Category %in% c(category1, category2), ]
        
        # Perform Wilcoxon rank-sum test
        wilcox_result <- wilcox.test(`Segment Length` ~ Category, data = subset_data)
        
        # Store the result
        wilcox_results[[paste(category1, "vs", category2)]] <- wilcox_result$p.value
      }
      
      # Format the results for display
      wilcox_text <- ""
      for (comparison in names(wilcox_results)) {
        p_value <- wilcox_results[[comparison]]
        wilcox_text <- paste(wilcox_text, paste(comparison, "p =", format(p_value, digits = 3)), sep = "\n")
      }
      
      # Create a p-value string to display in the plot title
      p_value_text <- paste("Wilcoxon p-values:\n", wilcox_text)
      
      # 1. Boxplot Plot
      p_boxplot <- ggplot(combined_data, aes(x = Category, y = `Segment Length`, color = Category)) +
        geom_boxplot(outlier.shape = NA, width = 0.6) +  # Box plot, no outliers shown by default
        labs(title = paste("Boxplot of Segment Length -", list_name),
             subtitle = p_value_text,  # Add Wilcoxon test results
             x = "Category (Proximal, Distal)", 
             y = "Segment Length, µm") +
        theme_minimal() + 
        theme(axis.text.x = element_text(angle = 45, hjust = 1))  # Rotate x-axis labels for better readability
      
      # 2. Jitter Plot with Mean and Error Bars (SD)
      p_jitter <- ggplot(combined_data, aes(x = Category, y = `Segment Length`, color = Category)) +
        geom_jitter(width = 0.2, size = 2, alpha = 0.6) +  # Jittered points to avoid overlap
        stat_summary(fun = "mean", geom = "point", shape = 18, size = 4, color = "black") +  # Mean points
        stat_summary(fun.data = "mean_cl_normal", geom = "errorbar", color = "red", width = 0.2) +  # Error bars (CI)
        labs(title = paste("Jitter Plot with Mean and Error Bars -", list_name),
             subtitle = p_value_text,  # Add Wilcoxon test results
             x = "Category (Proximal, Distal)", 
             y = "Segment Length") +
        theme_minimal() + 
        theme(axis.text.x = element_text(angle = 45, hjust = 1))  # Rotate x-axis labels for better readability
      
      # Add both plots to the all_plots list
      all_plots[[paste(list_name, "boxplot", sep = "_")]] <- p_boxplot
      all_plots[[paste(list_name, "jitter", sep = "_")]] <- p_jitter
      
    } else {
      cat("No data found for", list_name, "\n")
    }
  }
  
  # Arrange the plots in a 4x4 grid using grid.arrange
  plots1 <- all_plots[1:4]  # First 4 plots (2x2 grid)
  plots2 <- all_plots[5:8]  # Next 4 plots (another 2x2 grid)
  
  # Arrange the first 2x2 grid
  grid.arrange(grobs = plots1, ncol = 2)
  
  # Arrange the second 2x2 grid
  grid.arrange(grobs = plots2, ncol = 2)
}


# 'merged_lists' is the result from the merge_by_keywords function
# Call the plot function to create the plots for each category
plot_segment_length_wilcox(merged_lists)


### Checking the number of data

# Function to read CSV files and organize them by category
read_csv_in_folders <- function(root_dir) {
  # Initialize four empty lists to store file names by categories
  cilia_visible_green_data <- list()
  includes_invisible_rgccilia_data <- list()
  onlyrgcall_data <- list()
  onlyrgcvisible_data <- list()
  
  # Get all directories (subfolders) within the root directory, including nested ones
  all_folders <- list.dirs(root_dir, recursive = TRUE, full.names = TRUE)
  
  # Loop through each folder
  for (folder in all_folders) {
    # Get the list of CSV files in the current folder
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
  
  # Return the lists of file names categorized by their folders
  return(list(
    cilia_visible_green = cilia_visible_green_data,
    includes_invisible_rgccilia = includes_invisible_rgccilia_data,
    onlyrgcall = onlyrgcall_data,
    onlyrgcvisible = onlyrgcvisible_data
  ))
}

# Function to plot the number of files containing keywords (Proximal, Distal) for each category
plot_keywords_count_per_category <- function(root_dir) {
  # Define the updated keywords for categories
  keywords <- c("Proximal", "Distal")
  
  # Get the categorized data by calling the read_csv_in_folders function
  result_lists <- read_csv_in_folders(root_dir)
  
  # Create an empty data frame to store the results
  keyword_counts <- data.frame(Category = character(),
                               Keyword = character(),
                               Count = numeric())
  
  # Loop through each category in result_lists
  for (category_name in names(result_lists)) {
    category_data <- result_lists[[category_name]]
    
    # Ensure category_data is a character vector of file names
    category_files <- names(category_data)
    
    # Loop through each keyword (Proximal, Distal)
    for (keyword in keywords) {
      # Count how many files in this category contain the keyword in the file name
      count <- sum(sapply(category_files, function(file) grepl(keyword, file, ignore.case = TRUE)))  # Case insensitive search for keywords in filenames
      
      # Add the count to the keyword_counts data frame
      keyword_counts <- rbind(keyword_counts, 
                              data.frame(Category = category_name, 
                                         Keyword = keyword, 
                                         Count = count))
    }
  }
  
  # Check if there is any data to plot
  if (nrow(keyword_counts) > 0) {
    # 1. Bar plot for keyword counts per category
    p_barplot <- ggplot(keyword_counts, aes(x = Keyword, y = Count, fill = Keyword)) +
      geom_bar(stat = "identity", position = "dodge") +  # Bar plot with counts for each keyword
      facet_wrap(~ Category, scales = "free_y") +  # Facet by Category
      labs(title = "Keyword Count in Each Category",
           x = "Keyword",
           y = "Number of Files") +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))  # Rotate x-axis labels for better readability
    
    # Display the plot
    print(p_barplot)
  } else {
    print("No valid data to plot.")
  }
}

# Assuming result_lists is your list containing data frames for the categories
plot_keywords_count_per_category(root_dir)
