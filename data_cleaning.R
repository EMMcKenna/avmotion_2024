install.packages("readr")
library(readr)
library(dplyr)

#### Data import ####
##C:\Users\Eimea\OneDrive\Documents\Desktop\csv_files
getwd()
setwd("C:/Users/Eimea/OneDrive/Documents/Desktop/csv_files")

#load in datafiles to global environment from working directory
temp = list.files(pattern="*.csv")
for (i in 1:length(temp)) {
  # Read each CSV file into the global environment
  assign(temp[i], read.csv(temp[i], fill = T))
}

dfs <- mget(ls(envir = globalenv())[sapply(ls(envir = globalenv()), function(x) is.data.frame(get(x)))])

####from chatty / not working tho
for (file in temp) {
  # Read each CSV file into a data frame
  df <- read.csv(file, fill = TRUE)
  
  # Check if the variable of interest is present in the column names
  if ("cat_resp.corr"%in% colnames(df)) {
    colnames(df)[1] <- "stimulus"
    # Add the data frame to the dfs list
    dfs[[file]] <- df
  }
}


# Filter dfs to include only data frames with "trials_to_criterion" column
dfs <- lapply(dfs, function(df) {
  if ("cat_resp.corr" %in% colnames(df)) {
    # Add additional renaming or adjustments if needed
    return(df)
  } else {
    # If the specified column is not present, return NULL (or any other indicator)
    return(NULL)
  }
})

# Now dfs contains only data frames with the variable of interest



# Filter out NULL values from the list
dfs <- dfs[sapply(dfs, Negate(is.null))]
# Now, dfs contains only data frames with the specified column

# Specify columns to extract
columns_to_extract <- c("participant",
                        "stimulus","moviesound", "stimulus_cat","object_type",
                        "category", "exposure_resp.keys", "exposure_resp.corr",
                        "exposure_resp.rt", "learning_resp.corr",
                        "learn_prop_correct","learning_resp.rt", "learn_average_rt", "trials_to_criterion",                        'cat_resp.corr', 'cat_resp.rt', "main_prop_correct", "main_average_rt",
                        "gender","class", "age", "group")  # Add your column names





#glimpse(dfs)
# Extract specified columns from each data frame
dfs_extracted <- lapply(dfs, function(df) {
  df_subset <- df[, columns_to_extract, drop = FALSE]
  return(df_subset)
})

# Print out column names of each data frame in dfs
lapply(dfs, function(df) {
  print(colnames(df))
})

# Extract specified columns from each data frame
dfs_extracted <- lapply(dfs, function(df) {
  df_subset <- df[, columns_to_extract, drop = FALSE]
  return(df_subset)
})


# Convert gender, class, age, and group columns to character type if they are not already character
dfs_extracted <- lapply(dfs_extracted, function(df) {
  if ("gender" %in% colnames(df) && is.logical(df$gender)) {
    df$gender <- as.character(df$gender)
  }
  if ("class" %in% colnames(df) && is.logical(df$class)) {
    df$class <- as.character(df$class)
  }
  if ("class" %in% colnames(df) && is.integer(df$class)) {
    df$class <- as.character(df$class)
  }
  if ("age" %in% colnames(df) && is.logical(df$age)) {
    df$age <- as.character(df$age)
  }
  if ("group" %in% colnames(df) && is.logical(df$group)) {
    df$group <- as.numeric(df$group)
  }
  df_subset <- df[, columns_to_extract, drop = FALSE]
  return(df)
})

# Perform a full join on the list of data frames with type conversion
merged <- purrr::reduce(dfs_extracted, full_join)
merged$participant <- as.factor(merged$participant)
table(merged$participant)
merged_data <- as.data.frame(merged)

merged_data 
write.csv(merged_data, "mergedpass_data.csv", row.names = FALSE)
