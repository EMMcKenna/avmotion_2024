#location of data: C:\Users\Eimea\Documents\Validation_stimuli\data
#"C:\\Users\\Eimea\\Documents\\Validation_stimuli\\data"
set.wd('C:\\Users\\Eimear\\Documents\\Validation_stimuli')
install.packages("stargazer")

library(tidyverse)
library(stargazer)
library(psych)
library(ggthemes)
library(jtools)
library(ggpubr)
library(qwraps2)
library(reshape2)

#to make this run, you will need to sort the stimuli by type 
#make the stimuli fall into 2 categories first
#correlated / uncorrelated
#within uncorrelated need - sr jumble ft
#so there will be a variable called correlation which will be a factor and within this will be another factor stimulus type: corr, jum, ft, sr


val_stim <- as.data.frame(val_stim)
data <- val_stim %>%
  select(participant,
         ratings_task.response,
         ratings_task.rt,
         correlation,
         stimulus_type,
         age,
         gender
         )


class(valstim$participant)  #potentially not needed?
valstim$participant <- as.factor(valstim$participant) #potentially not relevant

class(valstim$participant)
valstim$ratings_task.response <- as.numeric(valstim$ratings_task.response)

class(valstim$ratings_task.rt)
valstim$ratings_task.rt <- as.numeric(val_stim$ratings_task.rt)

class(valstim$correlation)
valstim$correlation <- as.factor(valstim$correlation)
#data should be coded corr as 0 and uncorr as 1
levels(valstim$correlation) <- c(correlated, uncorrelated)

#levels(prob_a$age_group) <- c('4-6 years', '7-9 years','10+ years')
#summary(features_qual$age_group)

class(valstim$stimulus_type)
valstim$stimulus_type <- as.factor(valstim$stimulus_type)
#data should be coded corr as 0, jumble as 1, ft as 2, sr as 3
levels(valstim$stimulus_type) <- c(corr, jumble, fourier, slowreversed)



class(valstim$age)
valstim$age <- as.numeric(valstim$age)

class(valstim$gender)
valstim$gender <- as.factor(valstim$gender)

#######to analyse KN
#location of data: C:\Users\Eimea\Documents\Validation_stimuli\data
#"C:\\Users\\Eimea\\Documents\\Validation_stimuli\\data"
setwd('C:\\Users\\Eimear\\Documents\\Validation_stimuli\\data')
install.packages("stargazer")

library(tidyverse)
library(stargazer)
library(psych)
library(ggthemes)
library(jtools)
library(ggpubr)
library(qwraps2)
library(reshape2)

#to make this run, you will need to sort the stimuli by type 
#make the stimuli fall into 2 categories first
#correlated / uncorrelated
#within uncorrelated need - sr jumble ft
#so there will be a variable called correlation which will be a factor and within this will be another factor stimulus type: corr, jum, ft, sr


KN <- as.data.frame(KN)
colnames(KN)[1] <- 'participant'

KN_result <- KN %>%
  select(participant,
         ratings_task.response,
         ratings_task.rt,
         object,
         correlation,
         stimulus_type,
         age,
         gender
  )


class(KN_result$participant)  #potentially not needed?
KN_result$participant <- as.factor(KN_result$participant) #potentially not relevant

class(KN_result$participant)
KN_result$ratings_task.response <- as.numeric(KN_result$ratings_task.response)

class(KN_result$ratings_task.rt)
KN_result$ratings_task.rt <- as.numeric(KN_result$ratings_task.rt)

class(KN_result$correlation)
KN_result$correlation <- as.factor(KN_result$correlation)
#data should be coded corr as 0 and uncorr as 1
KN_result$correlation
levels(KN_result$correlation) <- c('correlated', 'uncorrelated')

#levels(prob_a$age_group) <- c('4-6 years', '7-9 years','10+ years')
#summary(features_qual$age_group)

class(KN_result$stimulus_type)
KN_result$stimulus_type <- as.factor(KN_result$stimulus_type)
#data should be coded corr as 0, jumble as 1, ft as 2, sr as 3
levels(KN_result$stimulus_type) <- c('corrav', 'fourier', 'jumble', 'slowreversed')

KN_result$object <- as.factor(KN_result$object)
levels(KN_result$object)

class(KN_result$age)
KN_result$age <- as.numeric(KN_result$age)

class(KN_result$gender)
KN_result$gender <- as.factor(KN_result$gender)


KN_result_mean <- KN_result %>%
  group_by(object, correlation, stimulus_type) %>%
  summarize(
    mean_rating = mean(ratings_task.response, na.rm = TRUE),
    mean_rt = mean(ratings_task.rt, na.rm = TRUE)
  )

KN_result_mean <- as.data.frame(KN_result_mean) #datafile

write.csv(KN_result_mean, "C:\\Users\\Eimea\\Documents\\Validation_stimuli.csv", row.names = FALSE)

ggplot(KN_result_mean, aes(x = interaction(correlation, stimulus_type), y = mean_rating, fill = interaction(stimulus_type, correlation))) +
  geom_bar(stat = "identity", position = "dodge") +
  facet_wrap(~ object, scales = 'free_y') +
  labs(title = "Mean Similarity between movement and sound",
       x = "Sound Type",
       y = "Mean Rating") +
  coord_cartesian(ylim = c(1,7)) +
  #scale_fill_manual(values = c(corr, ft, jumble, sr))+
  theme_minimal()        
KN_result_mean

#analysing Grace
grace <- as.data.frame(grace)
summary(grace)
colnames(grace)[1] <- 'participant'

grace_result <- grace %>%
  select(participant,
         ratings_task.response,
         ratings_task.rt,
         object,
         correlation,
         stimulus_type,
         age,
         gender
  )


class(grace_result$participant)  #potentially not needed?
grace_result$participant <- as.factor(grace_result$participant) #potentially not relevant

class(grace_result$participant)
grace_result$ratings_task.response <- as.numeric(grace_result$ratings_task.response)

class(grace_result$ratings_task.rt)
grace_result$ratings_task.rt <- as.numeric(grace_result$ratings_task.rt)

class(grace_result$correlation)
grace_result$correlation <- as.factor(grace_result$correlation)
#data should be coded corr as 0 and uncorr as 1
grace_result$correlation
levels(grace_result$correlation) <- c('correlated', 'uncorrelated')

#levels(prob_a$age_group) <- c('4-6 years', '7-9 years','10+ years')
#summary(features_qual$age_group)

class(grace_result$stimulus_type)
grace_result$stimulus_type <- as.factor(grace_result$stimulus_type)
summary(grace_result$stimulus_type)
#data should be coded corr as 0, jumble as 1, ft as 2, sr as 3
levels(grace_result$stimulus_type) <- c('corrav', 'fourier', 'jumble', 'slowreversed')

grace_result$object <- as.factor(grace_result$object)
levels(grace_result$object)

class(grace_result$age)
grace_result$age <- as.numeric(grace_result$age)

class(grace_result$gender)
grace_result$gender <- as.factor(grace_result$gender)


grace_result_mean <- grace_result %>%
  group_by(object, correlation, stimulus_type) %>%
  summarize(
    mean_rating = mean(ratings_task.response, na.rm = TRUE),
    mean_rt = mean(ratings_task.rt, na.rm = TRUE)
  )

grace_result_mean <- as.data.frame(grace_result_mean) #datafile

write.csv(grace_result_mean, "C:\\Users\\Eimea\\Documents\\Validation_stimuli.csv", row.names = FALSE)

ggplot(grace_result_mean, aes(x = interaction(correlation, stimulus_type), y = mean_rating, fill = interaction(stimulus_type, correlation))) +
  geom_bar(stat = "identity", position = "dodge") +
  facet_wrap(~ object, scales = 'free_y') +
  labs(title = "Mean Similarity between movement and sound",
       x = "Sound Type",
       y = "Mean Rating") +
  #scale_fill_manual(values = c(corr, ft, jumble, sr))+
  theme_minimal()        
grace_result_mean

#analysing Alan

AOD <- as.data.frame(AOD)
summary(AOD)
colnames(AOD)[1] <- 'participant'
colnames(AOD)[2] <- 'gender'
colnames(AOD)[3] <- 'age'
colnames(AOD)[4] <- 'ratings_task.response'
colnames(AOD)[5] <- 'ratings_task.rt'
colnames(AOD)[6] <- 'object'
colnames(AOD)[7] <- 'correlation'
colnames(AOD)[8] <- 'stimulus_type'

AOD <- AOD[-1, , drop = FALSE]

AOD_result <- AOD %>%
  select(participant,
         ratings_task.response,
         ratings_task.rt,
         object,
         correlation,
         stimulus_type,
         age,
         gender
  )


class(AOD_result$participant)  #potentially not needed?
AOD_result$participant <- as.factor(AOD_result$participant) #potentially not relevant

class(AOD_result$participant)
AOD_result$ratings_task.response <- as.numeric(AOD_result$ratings_task.response)

class(AOD_result$ratings_task.rt)
AOD_result$ratings_task.rt <- as.numeric(AOD_result$ratings_task.rt)

class(AOD_result$correlation)
AOD_result$correlation <- as.factor(AOD_result$correlation)
#data should be coded corr as 0 and uncorr as 1
AOD_result$correlation
levels(AOD_result$correlation) <- c('correlated', 'uncorrelated')

AOD_result$object <- as.factor(AOD_result$object)

#levels(prob_a$age_group) <- c('4-6 years', '7-9 years','10+ years')
#summary(features_qual$age_group)

class(AOD_result$stimulus_type)
AOD_result$stimulus_type <- as.factor(AOD_result$stimulus_type)
summary(AOD_result$stimulus_type)
#data should be coded corr as 0, jumble as 1, ft as 2, sr as 3
levels(AOD_result$stimulus_type) <- c('corrav', 'fourier', 'jumble', 'slowreversed')



class(AOD_result$age)
AOD_result$age <- as.numeric(AOD_result$age)

class(AOD_result$gender)
AOD_result$gender <- as.factor(AOD_result$gender)


AOD_result_mean <- AOD_result %>%
  group_by(object, correlation, stimulus_type) %>%
  summarize(
    mean_rating = mean(ratings_task.response, na.rm = TRUE),
    mean_rt = mean(ratings_task.rt, na.rm = TRUE)
  )

AOD_result_mean <- as.data.frame(AOD_result_mean) #datafile

write.csv(AOD_result_mean, "C:\\Users\\Eimea\\Documents\\Validation_stimuli.csv", row.names = FALSE)

ggplot(AOD_result_mean, aes(x = interaction(correlation, stimulus_type), y = mean_rating, fill = interaction(stimulus_type, correlation))) +
  geom_bar(stat = "identity", position = "dodge") +
  facet_wrap(~ object, scales = 'free_y') +
  labs(title = "Mean Similarity between movement and sound",
       x = "Sound Type",
       y = "Mean Rating") +
  #scale_fill_manual(values = c(corr, ft, jumble, sr))+
  theme_minimal()        
AOD_result_mean

#analysing Marti

marti <- as.data.frame(marti)
summary(marti)
colnames(marti)[1] <- 'participant'
#colnames(AOD)[2] <- 'gender'
#colnames(AOD)[3] <- 'age'
#colnames(AOD)[4] <- 'ratings_task.response'
#colnames(AOD)[5] <- 'ratings_task.rt'
#colnames(AOD)[6] <- 'object'
#colnames(AOD)[7] <- 'correlation'
#colnames(AOD)[8] <- 'stimulus_type'



marti_result <- marti %>%
  select(participant,
         ratings_task.response,
         ratings_task.rt,
         object,
         correlation,
         stimulus_type,
         age,
         gender
  )


class(marti_result$participant)  #potentially not needed?
marti_result$participant <- as.factor(marti_result$participant) #potentially not relevant

class(marti_result$participant)
marti_result$ratings_task.response <- as.numeric(marti_result$ratings_task.response)

class(marti_result$ratings_task.rt)
marti_result$ratings_task.rt <- as.numeric(marti_result$ratings_task.rt)

class(marti_result$correlation)
marti_result$correlation <- as.factor(marti_result$correlation)
#data should be coded corr as 0 and uncorr as 1
marti_result$correlation
levels(marti_result$correlation) <- c('correlated', 'uncorrelated')
summary(marti_result$correlation)
marti_result$object <- as.factor(marti_result$object)

#levels(prob_a$age_group) <- c('4-6 years', '7-9 years','10+ years')
#summary(features_qual$age_group)

class(marti_result$stimulus_type)
marti_result$stimulus_type <- as.factor(marti_result$stimulus_type)
summary(marti_result$stimulus_type)
#data should be coded corr as 0, jumble as 1, ft as 2, sr as 3
levels(marti_result$stimulus_type) <- c('corrav', 'fourier', 'jumble', 'slowreversed')



class(marti_result$age)
marti_result$age <- as.numeric(marti_result$age)

class(marti_result$gender)
marti_result$gender <- as.factor(marti_result$gender)


marti_result_mean <- marti_result %>%
  group_by(object, correlation, stimulus_type) %>%
  summarize(
    mean_rating = mean(ratings_task.response, na.rm = TRUE),
    mean_rt = mean(ratings_task.rt, na.rm = TRUE)
  )

marti_result_mean <- as.data.frame(marti_result_mean) #datafile

write.csv(marti_result_mean, "C:\\Users\\Eimea\\Documents\\Validation_stimuli.csv", row.names = FALSE)

ggplot(marti_result_mean, aes(x = interaction(correlation, stimulus_type), y = mean_rating, fill = interaction(stimulus_type, correlation))) +
  geom_bar(stat = "identity", position = "dodge") +
  facet_wrap(~ object, scales = 'free_y') +
  labs(title = "Mean Similarity between movement and sound",
       x = "Sound Type",
       y = "Mean Rating") +
  #scale_fill_manual(values = c(corr, ft, jumble, sr))+
  theme_minimal()        
marti_result_mean









---------------------------------------------------------------------------
# Assuming you have three data frames: participant1, participant2, participant3
  
# Install and load the dplyr package if not already installed
# install.packages("dplyr")
library(dplyr)

# Combine data frames into one
combined_mean <- bind_rows(
  mutate(grace_result_mean, Participant = "P1"),  # Adding a column to identify the participant
  mutate(AOD_result_mean, Participant = "P2"),
  mutate(KN_result_mean, Participant = "P3"),
  mutate(marti_result_mean, Participant = 'P4')
)
class(combined_mean$Participant)  #potentially not needed?
combined_mean$Participant <- as.factor(combined_mean$Participant) #potentially not relevant

class(combined_mean$participant)
combined_mean$mean_rating<- as.numeric(combined_mean$mean_rating)

class(combined_mean$mean_rt)
combined_mean$mean_rt <- as.numeric(combined_mean$mean_rt)

class(combined_mean$correlation)
combined_mean$correlation <- as.factor(combined_mean$correlation)
#data should be coded corr as 0 and uncorr as 1
combined_mean$correlation
levels(combined_mean$correlation) <- c('correlated', 'uncorrelated')

combined_mean$object <- as.factor(combined_mean$object)

#levels(prob_a$age_group) <- c('4-6 years', '7-9 years','10+ years')
#summary(features_qual$age_group)

class(combined_mean$stimulus_type)
combined_mean$stimulus_type <- as.factor(combined_mean$stimulus_type)
summary(combined_mean$stimulus_type)
#data should be coded corr as 0, jumble as 1, ft as 2, sr as 3
levels(combined_mean$stimulus_type) <- c('corrav', 'fourier', 'jumble', 'slowreversed')



#class(combined_mean$age)
#combined_mean$age <- as.numeric(combined_mean$age)

#class(combined_mean$gender)
#combined_mean$gender <- as.factor(combined_mean$gender)


combined_mean_result <- combined_mean %>%
  group_by(object, correlation, stimulus_type) %>%
  summarize(
    mean_rating_total = mean(mean_rating, na.rm = TRUE),
    mean_rt_total = mean(mean_rt, na.rm = TRUE)
  )

combined_mean_result <- as.data.frame(combined_mean_result) #datafile

write.csv(combined_mean_result, "C:\\Users\\Eimea\\Documents\\Validation_stimuli.csv", row.names = FALSE)

ggplot(combined_mean_result, aes(x = interaction(correlation, stimulus_type), y = mean_rating_total, fill = interaction(stimulus_type, correlation))) +
  geom_bar(stat = "identity", position = "dodge") +
  facet_wrap(~ object, scales = 'free_y') +
  labs(title = "Mean Similarity between movement and sound",
       x = "Sound Type",
       y = "Mean Rating") +
  #scale_fill_manual(values = c(corr, ft, jumble, sr))+
  theme_minimal()        
combined_mean_result

--------------------------------------------
  
# Install and load necessary packages
install.packages("readxl")
install.packages("dplyr")

library(readxl)
library(dplyr)

setwd('C:\\Users\\Eimea\\Documents\\Validation_stimuli\\data\\prolific')


install.packages("stargazer")

library(tidyverse)
library(stargazer)
library(psych)
library(ggthemes)
library(jtools)
library(ggpubr)
library(qwraps2)
library(reshape2)
# Create a list of file names

file_names <- c("p1.csv", "p2.csv", "p3.csv", "p4.csv", "p5.csv", 
                "p6.csv", "p7.csv", "p8.csv", "p9.csv", "p10.csv")

# Read each Excel file into a list of dataframes
list_of_dfs <- lapply(file_names, read.csv)

# Combine the list of dataframes into one dataframe
sound_val <- bind_rows(list_of_dfs)

# Print the combined dataframe
print(sound_val)

colnames(sound_val)[1] <- 'participant'

class(sound_val$participant)  #potentially not needed?
sound_val$participant <- as.factor(sound_val$participant) #potentially not relevant

class(sound_val$participant)
sound_val$ratings_task.response<- as.numeric(sound_val$ratings_task.response)

class(sound_val$ratings_task.rt)
sound_val$ratings_task.rt <- as.numeric(sound_val$ratings_task.rt)

class(sound_val$correlation)
sound_val$correlation <- as.factor(sound_val$correlation)
#data should be coded corr as 0 and uncorr as 1
sound_val$correlation

###to fix contents

sound_val$recoded_correlation<- sound_val$correlation %>%
  recode_factor("corrav" = "correlated", "correlated" = "correlated", "correlation" = "correlated",
                "uncorrav" = "uncorrelated", "uncorrelated" = "uncorrelated", "uncorrelation" = "correlation")

levels(sound_val$recoded_correlation) <- c('correlated', 'uncorrelated')

sound_val$object <- as.factor(sound_val$object)
summary(sound_val$object)

#levels(prob_a$age_group) <- c('4-6 years', '7-9 years','10+ years')
#summary(features_qual$age_group)

class(sound_val$stimulus_type)
sound_val$stimulus_type <- as.factor(sound_val$stimulus_type)
summary(sound_val$stimulus_type)
#data should be coded corr as 0, jumble as 1, ft as 2, sr as 3
levels(sound_val$stimulus_type) <- c('corrav', 'fourier', 'jumble')



#class(combined_mean$age)
#combined_mean$age <- as.numeric(combined_mean$age)

#class(combined_mean$gender)
#combined_mean$gender <- as.factor(combined_mean$gender)


combined_mean_result <- sound_val %>%
  group_by(object, correlation, stimulus_type) %>%
  summarize(
    mean_rating_total = mean(ratings_task.response, na.rm = TRUE),
    mean_rt_total = mean(ratings_task.rt, na.rm = TRUE)
  )

combined_mean_result <- as.data.frame(combined_mean_result) #datafile

write.csv(combined_mean_result, "C:\\Users\\Eimea\\Documents\\Validation_stimuli.csv", row.names = FALSE)

library(ggplot2)
ggplot(combined_mean_result, aes(x = stimulus_type, y = mean_rating_total, fill = stimulus_type)) +
  geom_bar(stat = "identity", position = "dodge") +
  facet_wrap(~ object, scales = 'free_y') +
  labs(title = "Mean Similarity between movement and sound",
       x = "Sound Type",
       y = "Mean Rating") +
  #scale_fill_manual(values = c(corr, ft, jumble, sr))+
  theme_minimal()        
combined_mean_result




