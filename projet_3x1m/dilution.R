library(readxl)
library(tidyverse)

dilution <- read_excel("resultats/results_october_3x1m.xlsx", 
                       sheet = "dilution_calculation")

dilution <- select(dilution, strain, replicate, chl_wanted, chl_real)

dilution$chl_wanted <- as.numeric(dilution$chl_wanted)

dilution <- dilution[!is.na(dilution$chl_wanted),]

names_dil <- data.frame('chl_wanted' = c(20, 5, 1, 0.3, 0.1, 0.05),
                        'dil_name' = c('dilution_0', 'dilution_1', 'dilution_2', 'dilution_3', 'dilution_4', 'dilution_5'))

dilution <- left_join(dilution, names_dil)

write_csv(dilution, 'projet_3x1m/dashboard/dilution.csv')
