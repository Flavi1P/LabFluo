library(tidyverse)
library(lubridate)

strain <- 'rcc3006'
replicat <- 'c'
dat_final <- data.frame('date' = NA, 'time' = NA, 'e440' = NA, 'e470' = NA, 'e532' = NA , 'jsp' = NA , 'replicate' = NA, 'file' = NA )
for( i in (list.files(paste('resultats/3x1m/', strain, '/' , replicat, sep = '')))){
  echo_3x1m <- read_table2(paste('resultats/3x1m/', strain, '/', replicat, '/', i, sep = ''), 
                           col_names = FALSE)
  
  echo_3x1m <- select(echo_3x1m, -X3, -X4, -X5, -X7, -X9)
  
  
  names(echo_3x1m) <- c('date', 'time', 'e440', 'e470', 'e532', 'jsp')
  
  echo_3x1m$file <- i
  
  echo_3x1m$time <- gsub(']', '', echo_3x1m$time)
  echo_3x1m$date <- gsub('\\[', '', echo_3x1m$date)
  
  echo_3x1m$time <- substr(echo_3x1m$time,1,8)
  echo_3x1m$hms <- hms(echo_3x1m$time)
  echo_3x1m$second <- seconds(echo_3x1m$hms) - min(seconds(echo_3x1m$hms))
  
  echo_3x1m$replicate <- ifelse(echo_3x1m$second < 60, 1, ifelse(echo_3x1m$second > 180 & echo_3x1m$second < 240, 2, ifelse(echo_3x1m$second > 6*60, 3, NA)))
  
  echo_3x1m <- na.omit(echo_3x1m)
  
  dat_final <- bind_rows(dat_final, echo_3x1m)
}

dat_final <- na.omit(dat_final)
write_csv(dat_final, paste('resultats/3x1m/replicat_', replicat,'/', strain, '.csv', sep = ''))
