
data <-  data.frame('date' = NA, 'time' = NA, 'e440' = NA, 'e470' = NA, 'e532' = NA , 'jsp' = NA , 'replicate' = NA, 'file' = NA )
for(i in list.files('resultats/3x1m/rcc3006/c', full.names = TRUE)){
  echo_3x1m <- read_table2(i, 
                           col_names = FALSE)
  if(i == 'resultats/3x1m/rcc3006/c/dilution_1.txt'){
    echo_3x1m <- read_table2(i , col_names = FALSE)
    
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
  }
    else{
    echo_3x1m <- select(echo_3x1m, -X1, -X2, -X3, -X5, -X7)
    
    names(echo_3x1m) <- c('e440', 'e470', 'e532', 'jsp')
    
    echo_3x1m$second <- time(echo_3x1m$e440)
    echo_3x1m$replicate <- ifelse(echo_3x1m$second < 60, 1, ifelse(echo_3x1m$second > 60 & echo_3x1m$second < 60*2, 2, ifelse(echo_3x1m$second > 2*60, 3, NA)))
    
    echo_3x1m <- na.omit(echo_3x1m)
    echo_3x1m$date <- '2020-10-31'
    echo_3x1m$time <- '14:30:00'
    echo_3x1m$file <- gsub('^.*/', '', i)
    echo_3x1m$hms <- hms('00:00:00')
  }
  
  data <- bind_rows(data, echo_3x1m)
  
  }

write_csv(data, 'resultats/3x1m/replicat_b/rcc76.csv')
