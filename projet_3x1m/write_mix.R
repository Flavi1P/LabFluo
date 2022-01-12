library(tidyverse)
library(lubridate)
library(readxl)


strain_mix <- read_csv('projet_3x1m/dashboard/strain_mix.csv')
mix_info <- read_excel("resultats/results_october_3x1m.xlsx", 
                       sheet = "Melanges") %>% janitor::clean_names() %>% select(melange:concentration_vrai) %>% na.omit()
mix_files <- list.files('Resultats/3x1m', full.names = TRUE)
mix_files <- mix_files[grep('melange', mix_files)]


dat_final <- data.frame('date' = NA, 'time' = NA, 'e440' = NA, 'e470' = NA, 'e532' = NA , 'jsp' = NA , 'replicate' = NA, 'file' = NA, 'melange' = NA )
mel_data <- data.frame('date' = NA, 'time' = NA, 'e440' = NA, 'e470' = NA, 'e532' = NA , 'jsp' = NA , 'replicate' = NA, 'file' = NA )



for(i in mix_files){
  
  subfiles <- list.files(i, full.names = TRUE)
  for(j in subfiles){
    
    echo_3x1m <- read_table2(j, col_names = FALSE)
    
    echo_3x1m <- select(echo_3x1m, -X3, -X4, -X5, -X7, -X9)
    
    
    names(echo_3x1m) <- c('date', 'time', 'e440', 'e470', 'e532', 'jsp')
    
    echo_3x1m$time <- gsub(']', '', echo_3x1m$time)
    echo_3x1m$date <- gsub('\\[', '', echo_3x1m$date)
    
    echo_3x1m$time <- substr(echo_3x1m$time,1,8)
    echo_3x1m$hms <- hms(echo_3x1m$time)
    echo_3x1m$second <- seconds(echo_3x1m$hms) - min(seconds(echo_3x1m$hms))
    
    echo_3x1m$file <- gsub('.*\\/', '', j) %>% gsub('.txt', '', .)
    
    echo_3x1m$replicate <- ifelse(echo_3x1m$second < 60, 1, ifelse(echo_3x1m$second > 180 & echo_3x1m$second < 240, 2, ifelse(echo_3x1m$second > 6*60, 3, NA)))
    
    echo_3x1m <- na.omit(echo_3x1m)
    mel_data <- bind_rows(mel_data, echo_3x1m)
    mel_data$melange <- gsub('.*\\/', '', i)
    
  }
 
  dat_final <- bind_rows(dat_final, mel_data)
}

data <- left_join(dat_final, strain_mix)

data$melange_full <- paste(data$melange, gsub('.*_', '', data$file), sep = '')
mix_info %<>% rename('melange_full' = melange)

data_full <- left_join(data, mix_info)
data_full$concentration_vrai[data_full$file == 'blanc'] <- 0

write_csv(data_full, 'projet_3x1m/dashboard/mix_data.csv')
