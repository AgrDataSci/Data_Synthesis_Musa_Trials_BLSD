#This script generates a table template to be manully filled with
#information about 
#first load data

source("scripts/utils/load_data.R")

variable_x_trial <- data.frame("trial_id" = row.names(musa_trials_data), 
                               "blsd_variable" = rep("", nrow(musa_trials_data)),
                               "evaluation_period" = rep("", nrow(musa_trials_data)))



variable_x_trial

write.csv(variable_x_trial, file = "output/variable_x_trial_template.csv")
