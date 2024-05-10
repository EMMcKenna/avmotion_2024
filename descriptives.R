library(readr)
library(dplyr)
library(psych)

mergedpassclean_data <- as.data.frame(mergedpassclean_data)

mergedpassclean_data$participant <- as.factor(mergedpassclean_data$participant)

mergedpassclean_data$age <- as.numeric(mergedpassclean_data$age)

mergedpassclean_data$class <- as.numeric(mergedpassclean_data$class)

mergedpassclean_data$gender <- as.factor(mergedpassclean_data$gender)

# Create a new factor called class_group
mergedpassclean_data$class_group <- factor(
  # Use ifelse to create the groups based on the values in the class column
  ifelse(mergedpassclean_data$class %in% c(0, 1, 2), "group_1", 
         ifelse(mergedpassclean_data$class %in% c(3, 4), "group_2", "group_3")),
  levels = c("group_1", "group_2", "group_3")
)


####new attempt####
# Calculate descriptive statistics using base R
age_summary <- mergedpassclean_data %>%
  group_by(class_group, participant) %>%
  summarise(
    Mean_Age = mean(age, na.rm = TRUE),
    SD_Age = sd(age, na.rm = TRUE),
    Median_Age = median(age, na.rm = TRUE),
    Q1_Age = quantile(age, 0.25, na.rm = TRUE),
    Q3_Age = quantile(age, 0.75, na.rm = TRUE)
  ) %>%
  group_by(class_group) %>%
  summarise(
    Count = n_distinct(participant), # Count unique participants
    Mean_Age = mean(Mean_Age),
    SD_Age = mean(SD_Age),
    Median_Age = mean(Median_Age),
    Q1_Age = mean(Q1_Age),
    Q3_Age = mean(Q3_Age)
  )




# Calculate quartiles and standard deviation
group_summary <- mergedpassclean_data %>%
  group_by(class_group) %>%
  summarise(
    mean = mean(age),
    Q1 = quantile(age, 0.25, na.rm = TRUE),
    Q3 = quantile(age, 0.75, na.rm = TRUE),
    Median = quantile(age, 0.5, na.rm = TRUE),
    SD = sd(age, na.rm = TRUE),
    Min = min(age, na.rm = TRUE),
    Max = max(age, na.rm = TRUE)
  )

# Print the summary
print(group_summary)


