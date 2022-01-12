library(readxl)
library(openxlsx)
library(tidyverse)

results <- read_excel("resultats/results_october_3x1m.xlsx", skip = 1)
hplc <- read_excel("resultats/hplc_roscoff_022021.xlsx") %>% janitor::clean_names()

#filter melange hplc values
hplc_melange <- filter(hplc, dilution == "Melange")

#make common strain names
hplc_melange <- hplc_melange %>% separate(code_culture, into = c("strain_a", "strain_b"), sep = " et ") %>% 
  mutate(strain_a = case_when(strain_a == 'RCC0076' ~ 'RCC76',
                            strain_a == 'RCC9511' ~ 'PCC9511',
                            strain_a == 'RCC0156' ~ 'RCC156',
                            strain_a == 'RCC0100' ~ 'RCC100',
                            TRUE ~ strain_a),
         strain_b = case_when(strain_b == 'RCC0076' ~ 'RCC76',
                              strain_b == 'RCC9511' ~ 'PCC9511',
                              strain_b == 'RCC0156' ~ 'RCC156',
                              strain_b == 'RCC0100' ~ 'RCC100',
                              TRUE ~ strain_b))

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
