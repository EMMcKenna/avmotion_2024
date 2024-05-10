library(readr)
library(dplyr)
library(psych)

setwd("C:/Users/Eimea/OneDrive/Documents/Desktop/csv_files/data_analysis")

mergedpassclean_data <- as.data.frame(read.csv("mergedpassclean_data.csv"))


mergedpassclean_data <- as.data.frame(mergedpassclean_data)

mergedpassclean_data$participant <- as.factor(mergedpassclean_data$participant)

mergedpassclean_data$age <- as.numeric(mergedpassclean_data$age)

mergedpassclean_data$gender <- as.factor(mergedpassclean_data$gender)

mergedpassclean_data$class <- as.numeric(mergedpassclean_data$class)

# Create a new factor called class_group
mergedpassclean_data$class_group <- factor(
  # Use ifelse to create the groups based on the values in the class column
  ifelse(mergedpassclean_data$class %in% c(0, 1, 2), "group_1", 
         ifelse(mergedpassclean_data$class %in% c(3, 4), "group_2", "group_3")),
  levels = c("group_1", "group_2", "group_3")
)

mergedpassclean_data$movement_cat <- as.factor(mergedpassclean_data$movement_cat)
levels(mergedpassclean_data$movement_cat)
mergedpassclean_data$sound_cat <- as.factor(mergedpassclean_data$sound_cat)


mergedpassclean_data$trials_to_criterion <- as.numeric(mergedpassclean_data$trials_to_criterion)

mergedpassclean_data$learn_prop_correct <- as.numeric(mergedpassclean_data$learn_prop_correct)

mergedpassclean_data$cat_resp.corr <- as.numeric(mergedpassclean_data$cat_resp.corr)

mergedpassclean_data$cat_resp.rt <- as.numeric(mergedpassclean_data$cat_resp.rt)

####removing outliers-source marti####

#Remove outliers - exclusion criteria- RTs and cut off 
#a - GROUP cut_off - at least 3 SDs above the mean 


library(dplyr)
mergedpassclean_data <- as.data.frame(mergedpassclean_data)
cols <- c('participant', 'class_group', 'movement_cat', 'sound_cat', 'cat_resp.corr', 'cat_resp.rt', "object")
cols_analysis <- mergedpassclean_data[cols]

cols_analysis <- cols_analysis[complete.cases(cols_analysis), ]


df_rt <- cols_analysis
upper_cut_off <- mean(df_rt$cat_resp.rt) + 2.5*(sd(df_rt$cat_resp.rt))
lower_cut_off <- mean(df_rt$cat_resp.rt) - 2.5*(sd(df_rt$cat_resp.rt))
d_groupcutoff<- subset(df_rt, cat_resp.rt < upper_cut_off & cat_resp.rt > lower_cut_off)
df_rt$outlier_RT <- ifelse(df_rt$cat_resp.rt > upper_cut_off |
                             df_rt$cat_resp.rt < lower_cut_off, 'Yes', 'No')
table(df_rt$outlier_RT)
df_rt_noGroupoutliers <- subset(df_rt, outlier_RT == 'No')

#b - INDIVIDUAL cut-off - 4 SDs above the individual mean
df_rt_Indcutoff <- df_rt_noGroupoutliers %>% group_by(participant, class_group) %>%
  filter(cat_resp.rt < mean(cat_resp.rt) + (4 * sd(cat_resp.rt)), cat_resp.rt > mean(cat_resp.rt) - (4 * sd(cat_resp.rt)))
boxplot(df_rt_Indcutoff$cat_resp.rt)
hist(df_rt_Indcutoff$cat_resp.rt)
range(df_rt_Indcutoff$cat_resp.rt)

#c - check people with RT below 500 ms 
d_below250 <- subset(df_rt_Indcutoff, cat_resp.rt < 0.250) # 50 participant with RT fastest of 150 ms
summary(d_below250)

#final dataset -with no outliers
df_rt_noout <- subset(df_rt_Indcutoff, cat_resp.rt > 0.250)

write.csv(df_rt_noout, "overview_movexsound.csv", row.names = FALSE)



cat_rt_clean <- df_rt_noout 



####movementxclass=rt####
###summarise data per participant, per class group, per object movement_cat

categorisation_object_type_means <- overview_movexsound %>%
  group_by(class_group, object_type) %>%
  summarise(mean_acc = mean(cat_resp.corr), 
            mean_RT = mean(cat_resp.rt),
            sd_acc = sd(cat_resp.corr),
            sd_RT = sd(cat_resp.rt),
            se_acc = sd_acc /sqrt(n()),
            se_RT = sd_RT / sqrt(n()))

cat_object_acc_mean <- overview_movexsound %>%
  group_by(class_group, movement_cat) %>%
  summarise(mean_acc = mean(cat_resp.corr),
            sd_acc = sd(cat_resp.corr),
            se_acc = sd_acc / sqrt(n()))


write.csv(categorisation_object_type_means, "categorisation_object_type_means", row.names = FALSE)



# Load necessary package
library(ggplot2)


####attempt2####
library(dplyr)
library(tidyr)
library(ggplot2)

# Assuming clean_categorisation is your data frame

# Calculate mean and standard error for each age group and accuracy variable
mean_rt_class <- categorisation_object_type_means %>%
  group_by(class_group, object_type) %>%
  summarise(meanobj_RT = mean(mean_RT),
            sdobj_RT = sd(sd_RT),
            seobj_RT = sd_RT / sqrt(n()))

#ATTEMPT 2#
mean_rt_class <- categorisation_object_type_means %>%
  group_by(class_group, object_type) %>%
  summarise(mean_RT = mean(mean_RT, na.rm = TRUE),  # Calculate mean RT excluding NA values
            sd_RT = sd(mean_RT),  # Calculate SD of mean RT
            se_RT = sd_RT / sqrt(n()))  # Calculate SE of mean RT

# Load necessary packages
library(dplyr)
library(ggplot2)

# Calculate mean and standard error by age_group and movement_cat
summary_data <- categorisation_object_type_means %>%
  group_by(class_group, object_type) %>%
  summarise(mean_RT = mean(mean_RT),
            se_RT = sd(sd_RT) / sqrt(n()))
categorisation_object_type_means$object_type <- as.factor(categorisation_object_type_means$object_type)

# Assuming "object_type" is your factor variable
levels_order <- levels(categorisation_object_type_means$object_type)

# Print the order of levels
print(levels_order)


# Create the bar plot with error bars representing SE
ggplot(categorisation_object_type_means, aes(x = class_group, y = mean_RT, fill = object_type)) +
  geom_bar(stat = "identity", position = "dodge", colour = 'black', size = 1) +
  geom_errorbar(aes(ymin = mean_RT - se_RT, ymax = mean_RT + se_RT),
                position = position_dodge(width = 0.9), width = 0.25) +
  labs(x = "Class Group", y = "Mean RT (cat_resp.RT)", fill = "Movement Category") +
  scale_fill_manual(values = c(
    "familiarcorrelated" = "#00868B",
    "familiaruncorrelated" = "blue",
    "staticcorrelated" = "#87CEFF",
    "staticuncorrelated" = "darkblue",
    "unfamiliarcorrelated" = "#6959CD",
    "unfamiliaruncorrelated" = "#9A32CD"))+
  theme_classic() +
  theme(legend.position = "top")  # Optional: change legend position

write.csv(categorisation_object_type_means, "cat_objecttype_means.csv", row.names = FALSE)

####accuracy####
cat_movement_acc_mean <- df_rt_noout %>%
  group_by(class_group, movement_cat) %>%
  summarise(mean_acc = mean(cat_resp.corr),
            sd_acc = sd(cat_resp.corr),
            se_acc = sd_acc / sqrt(n()))


####didnt work
mean_accuracy_class <- categorisation_movement_means %>%
  group_by(class_group, movement_cat) %>%
  summarise(meancat_acc = mean(mean_acc),
            se_acc = sd_acc / sqrt(n()))



# Load necessary packages
library(dplyr)
library(ggplot2)

# Create the bar plot with error bars representing SE
ggplot(cat_movement_acc_mean, aes(x = class_group, y = mean_acc, fill = movement_cat)) +
  geom_bar(stat = "identity", position = "dodge", colour = 'black', size = 1) +
  geom_errorbar(aes(ymin = mean_acc - se_acc, ymax = mean_acc + se_acc),
                position = position_dodge(width = 0.9), width = 0.25) +
  labs(x = "Class Group", y = "Mean Accuracy (cat_resp.corr)", fill = "Movement Category") +
  scale_fill_manual(values = c(
    "familiar" = "#00868B",
    "static" = "#87CEFF",
    "unfamiliar" = "#6959CD"))+
  theme_classic() +
  theme(legend.position = "top")  # Optional: change legend position

write.csv(cat_movement_acc_mean, "cat_movementacc_mSE", row.names = FALSE)



####sound####
###summarise data per participant, per class group, per object movement_cat

categorisation_sound_means <- df_rt_noout %>%
  group_by(participant, class_group, sound_cat) %>%
  summarise(mean_acc = mean(cat_resp.corr), 
            mean_RT = mean(cat_resp.rt),
            sd_acc = sd(cat_resp.corr),
            sd_RT = sd(cat_resp.rt))

cat_sound_acc_mean <- df_rt_noout %>%
  group_by(class_group, sound_cat) %>%
  summarise(mean_acc = mean(cat_resp.corr),
            sd_acc = sd(cat_resp.corr),
            se_acc = sd_acc / sqrt(n()))


write.csv(categorisation_sound_means, "categorisation_sound_means", row.names = FALSE)



# Load necessary package
library(ggplot2)


####attempt2####
library(dplyr)
library(tidyr)
library(ggplot2)

categorisation_sound_means <- df_rt_noout %>%
  group_by(participant, class_group, sound_cat) %>%
  summarise(mean_acc = mean(cat_resp.corr), 
            mean_RT = mean(cat_resp.rt),
            sd_acc = sd(cat_resp.corr),
            sd_RT = sd(cat_resp.rt))



# Calculate mean and standard error for each age group and accuracy variable
mean_rt_class <- categorisation_sound_means %>%
  group_by(class_group, sound_cat) %>%
  summarise(mean_RT = mean(mean_RT),
            sd_RT = 
            se_RT = sd_RT / sqrt(n()))
# Load necessary packages
library(dplyr)
library(ggplot2)

# Calculate mean and standard error by age_group and movement_cat
summary_data <- categorisation_sound_means %>%
  group_by(class_group, sound_cat) %>%
  summarise(mean_RT = mean(mean_RT),
            sd_RT = sd(sd_RT),
            se_RT = sd_RT/ sqrt(n()))  # Calculate standard error

# Create the bar plot with error bars representing SE
ggplot(summary_data, aes(x = class_group, y = mean_RT, fill = sound_cat)) +
  geom_bar(stat = "identity", position = "dodge", colour = 'black', size = 1) +
  geom_errorbar(aes(ymin = mean_RT - se_RT, ymax = mean_RT + se_RT),
                position = position_dodge(width = 0.9), width = 0.25) +
  labs(x = "Class Group", y = "Mean RT (cat_resp.RT)", fill = "Sound Category") +
  scale_fill_manual(values = c(
    "correlated" = "#00868B",
    "uncorrelated" = "#87CEFF"))+
  theme_classic() +
  theme(legend.position = "top")  # Optional: change legend position

write.csv(summary_data, "cat_sound_mSE", row.names = FALSE)

####accuracy####
cat_sound_acc_mean <- df_rt_noout %>%
  group_by(class_group, sound_cat) %>%
  summarise(mean_acc = mean(cat_resp.corr),
            sd_acc = sd(cat_resp.corr),
            se_acc = sd_acc / sqrt(n()))


####didnt work
mean_accuracy_class <- categorisation_sound_means %>%
  group_by(class_group, sound_cat) %>%
  summarise(meancat_acc = mean(mean_acc),
            se_acc = sd_acc / sqrt(n()))



# Load necessary packages
library(dplyr)
library(ggplot2)

# Create the bar plot with error bars representing SE
ggplot(cat_sound_acc_mean, aes(x = class_group, y = mean_acc, fill = sound_cat)) +
  geom_bar(stat = "identity", position = "dodge", colour = 'black', size = 1) +
  geom_errorbar(aes(ymin = mean_acc - se_acc, ymax = mean_acc + se_acc),
                position = position_dodge(width = 0.9), width = 0.25) +
  labs(x = "Class Group", y = "Mean Accuracy (cat_resp.corr)", fill = "Sound Category") +
  scale_fill_manual(values = c(
    "correlated" = "#00868B",
    "uncorrelated" = "#87CEFF"))+
  theme_classic() +
  theme(legend.position = "top")  # Optional: change legend position

write.csv(cat_sound_acc_mean, "cat_soundacc_mSE", row.names = FALSE)


####both####



####didnt work
mean_accuracy_class <- categorisation_object_type_means %>%
  group_by(class_group, object_type) %>%
  summarise(meancat_acc = mean(mean_acc),
            sdcat_acc = sd(mean_acc),
            se_acc = sd(mean_acc) / sqrt(n()))

mean_accuracy_class <- categorisation_object_type_means %>%
  na.omit() %>%
  group_by(class_group, object_type) %>%
  summarise(meancat_acc = mean(mean_acc, na.rm = TRUE),  # Calculate mean accuracy excluding NA values
            sdcat_acc = sd(mean_acc),  # Calculate SD of mean accuracy
            se_acc = sd(mean_acc) / sqrt(n()))  # Calculate SE of mean accuracy


# Load necessary packages
library(dplyr)
library(ggplot2)

# Create the bar plot with error bars representing SE
ggplot(categorisation_object_type_means, aes(x = class_group, y = mean_acc, fill = object_type)) +
  geom_bar(stat = "identity", position = "dodge", colour = 'black', size = 1) +
  geom_errorbar(aes(ymin = mean_acc - se_acc, ymax = mean_acc + se_acc),
                position = position_dodge(width = 0.9), width = 0.25) +
  labs(x = "Class Group", y = "Mean Accuracy (cat_resp.corr)", fill = "Object Type") +
  scale_fill_manual(values = c(
    "familiarcorrelated" = "#00868B",
    "familiaruncorrelated" = "blue",
    "staticcorrelated" = "#87CEFF",
    "staticuncorrelated" = "darkblue",
    "unfamiliarcorrelated" = "#6959CD",
    "unfamiliaruncorrelated" = "#9A32CD"))+
  theme_classic() +
  theme(legend.position = "top")  # Optional: change legend position

write.csv(cat_movement_acc_mean, "cat_objectacc_mSE.csv", row.names = FALSE)



####sound####
###summarise data per participant, per class group, per object movement_cat

categorisation_sound_means <- df_rt_noout %>%
  group_by(participant, class_group, sound_cat) %>%
  summarise(mean_acc = mean(cat_resp.corr), 
            mean_RT = mean(cat_resp.rt),
            sd_acc = sd(cat_resp.corr),
            sd_RT = sd(cat_resp.rt))

cat_sound_acc_mean <- df_rt_noout %>%
  group_by(class_group, sound_cat) %>%
  summarise(mean_acc = mean(cat_resp.corr),
            sd_acc = sd(cat_resp.corr),
            se_acc = sd_acc / sqrt(n()))


write.csv(categorisation_sound_means, "categorisation_sound_means", row.names = FALSE)



# Load necessary package
library(ggplot2)
# Assuming your data frame is called "data" and has the relevant columns
# Replace "data" with the actual name of your data frame
####regression model####
# Load the necessary package if not already loaded
install.packages("MASS")  # Uncomment and run if you haven't installed the package yet
library(MASS)

categorisation_overall_means <- as.data.frame(categorisation_overall_means)
# Perform stepwise regression after removing rows with NA values
model <- lm(mean_RT ~ class_group + movement_cat + sound_cat, data = categorisation_overall_means)

# Stepwise selection
step_model <- step(model, direction = "both")

# Print the final model
summary(step_model)

categorisation_mean <- ggplot(categorisation_overall_means, aes(x = class_group, y = mean_acc, fill = movement_cat)) +
  geom_bar(stat = "identity", position = 'stack', width = 0.7, colour = 'black' ) +
  #geom_bar(aes(pattern = variable), 
  #pattern_density = 0.1, pattern_spacing = 0.05, 
  #position = "stack", 
  #width = 0.7) +
  scale_fill_manual(values = c( 'group_1' = "#E69F00", 'group_2' = "#56B4E9", "group_3" = "#009E73")) +    #cbbPalette,
                   # breaks = legend_order) +
  facet_wrap(~ sound_cat, scales = 'free_y') +
  theme_classic() +
  labs(title = "Accuracy across Object types and Age groups",
       x = "Age Group", 
       y = "mean_acc") +
  theme(legend.position = 'right', axis.text.x = element_text(angle = 40, hjust = 1)) 
#geom_hline(yintercept = 0.5, linetype = 'dashed', col = 'black')
categorisation_mean


#####overall-not by participant
cat2_movement_means <- cols_analysis %>%
  group_by(class_group, movement_cat) %>%
  summarise(mean_acc = mean(cat_resp.corr), 
            mean_RT = mean(cat_resp.rt))


cat2_sound_means <- cols_analysis %>%
  group_by(class_group, sound_cat) %>%
  summarise(mean_acc = mean(cat_resp.corr), 
            mean_RT = mean(cat_resp.rt))

cat2_means <- cols_analysis %>%
  group_by(class_group, sound_cat, movement_cat) %>%
  summarise(mean_acc = mean(cat_resp.corr), 
            mean_RT = mean(cat_resp.rt))



