library(readxl)
library(openxlsx)
library(tidyverse)

results <- read_excel("resultats/results_october_3x1m.xlsx", skip = 1)
hplc <- read_excel("resultats/hplc_roscoff_022021.xlsx") %>% janitor::clean_names()


#make common strain names
hplc <- hplc %>% rename('strain' = code_culture) %>% 
  mutate(strain = case_when(strain == 'RCC0076' ~ 'RCC76',
                            strain == 'RCC9511' ~ 'PCC9511',
                            strain == 'RCC0156' ~ 'RCC156',
                            strain == 'RCC0100' ~ 'RCC100',
                            TRUE ~ strain))

common <- intersect(hplc$strain, results$strain)
unique(hplc$strain[which(!hplc$strain %in% common)]) #those are the mixing exp

hplc_simple <- filter(hplc, strain %in% common)


#make common replicate names

common <- intersect(hplc_simple$replicate, results$replicate)
unique(hplc_simple$replicate[which(!hplc_simple$replicate %in% common)])
unique(hplc_simple$replicate) #looks good

#make common dilution name
common <- intersect(hplc_simple$dilution, results$dilution) #0 match
unique(hplc_simple$dilution)
unique(results$dilution)

hplc_simple$dilution <- gsub('Dilution ', '', hplc_simple$dilution)
common <- intersect(hplc_simple$dilution, results$dilution) #0 match

results$dilution <- gsub('Cmère', 'Cmere', results$dilution)

hplc_simple$dilution <- gsub('Mere', 'Cmere', hplc_simple$dilution)
hplc_simple$dilution <- gsub('0b', '0bis', hplc_simple$dilution)
common <- intersect(hplc_simple$dilution, results$dilution) #0 match



unique(hplc_simple$dilution[which(!hplc_simple$dilution %in% common)]) #only mixing left

hplc_simple_clean <- filter(hplc_simple, dilution %in% common) %>% select(-date_of_analysis)
hplc_simple_clean[hplc_simple_clean == "LOD"] <- '0'
results_merge <- left_join(results, hplc_simple_clean, by = c('strain', 'replicate', 'dilution'))

write_csv(results_merge, "resultats/results_reshape.csv")
