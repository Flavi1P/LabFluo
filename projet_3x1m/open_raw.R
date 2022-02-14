library(tidyverse)
library(lubridate)
library(patchwork)

for( i in (list.files(paste('resultats/3x1m/', strain, '/' , replicat, sep = '')))){
  
  echo_3x1m <- read_table2("resultats/3x1m/melange_1/melange_b.txt", 
                           col_names = FALSE)
  
  echo_3x1m <- select(echo_3x1m, -X3, -X4, -X5, -X7, -X9)
  
  names(echo_3x1m) <- c('date', 'time', 'e440', 'e470', 'e532', 'jsp')
  
  echo_3x1m$time <- gsub(']', '', echo_3x1m$time)
  echo_3x1m$date <- gsub('\\[', '', echo_3x1m$date)
  
  echo_3x1m$time <- substr(echo_3x1m$time,1,8)
  echo_3x1m$hms <- hms(echo_3x1m$time)
  echo_3x1m$second <- seconds(echo_3x1m$hms) - min(seconds(echo_3x1m$hms))
  
  echo_3x1m$replicate <- ifelse(echo_3x1m$second < 60, 1, ifelse(echo_3x1m$second > 60*3 & echo_3x1m$second < 60*4, 2, ifelse(echo_3x1m$second > 6*60, 3, NA)))
  
  echo_3x1m <- na.omit(echo_3x1m)
  
  echo_3x1m <- echo_3x1m %>% mutate('chl' = 0.0073*(e470-50))
  
  chl_plot <- ggplot(echo_3x1m)+
    geom_path(aes(x = second, y = chl, group = date))+
    theme_bw()
  
  echo_long <- pivot_longer(echo_3x1m, e440:e532, names_to = 'wavelength', values_to = 'dc')

}

fluo_plot <- ggplot(echo_long)+
  geom_path(aes(x = second, y = dc, colour = wavelength, group = wavelength ), size = 1)+
  theme_bw()+
  scale_color_brewer(palette = 'Set2')+
  facet_wrap(.~replicate, scales = 'free_x')

fluo_plot
chl_fluo <- filter(echo_long, wavelength %in% c('e470', 'e440'))
two_fluo_plot <- ggplot(chl_fluo)+
  geom_path(aes(x = time, y = dc, colour = wavelength, group = wavelength ), size = 1)+
  theme_bw()+
  scale_color_brewer(palette = 'Set2')

fluo_plot + chl_plot + two_fluo_plot

#white <- echo_long %>% group_by(wavelength) %>% summarise('white_value' = mean(dc), 'white_sd' = sd(dc))
