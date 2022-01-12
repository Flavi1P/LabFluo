library(tidyverse)
library(stringr)


read_abs <- function(path_abs){
  df <- read_lines(path_abs)
  
  start <- grep('#DATA', df)+1
  end <- max(grep('300.000000', df))
  
  df <- df[start:end]
  df2 <- as.numeric(str_extract(df, '-?0.[0-9]{6}$'))
  return(df2)
}




write_abs_data <- function(path){
  files <- list.files(path, full.names = TRUE)
  files <- files[grep('PETIT_[0-9]{2}', files)]
  full_data <- data.frame('abs' = NULL, id = NULL)
  for(i in files){
    df <- read_abs(i)
    id <- str_extract(i, 'PETIT_[0-9]{2}')
    data <- data.frame('abs' = df, 'id' = id)
    full_data <- bind_rows(full_data, data)
    
  }
  
  full_data$wl <- c(800:300)
  
  log <- read_delim(paste(path, "Sample Table.csv", sep = ''), delim = ',') %>% 
    janitor::clean_names() %>% select(-type)
  
  full_data <- left_join(full_data, log, by = c('id'= 'sample_id')) %>% 
    mutate(strain = str_extract(description, 'RCC[0-9]+'),
           dilution = str_extract(description, 'D[0-9]?M?|_[0-9]+_'),
           replicate = str_extract(description, '_[A-D]_|C[a-b]?$'),
           replicate = gsub('_', '', replicate))
  
  #substract white 
  
  white <- full_data %>% filter(is.na(dilution)) %>% group_by(wl) %>% select(wl, abs) %>% 
    group_by(wl) %>% summarise_all(mean) %>% rename("white" = abs)
  
  full_data <- left_join(full_data, white) %>% mutate(abs = abs - white) %>% filter(!is.na(dilution)) 
  return(full_data)
}

df1 <- write_abs_data("resultats/Spectro_PETIT_Roscoff/Boussole-MethodfromDox2 lundi 14 dВcembre 2020 09_51 Paris, Madrid/")
df2 <- write_abs_data("resultats/Spectro_PETIT_Roscoff/Boussole-MethodfromDox2 mercredi 16 dВcembre 2020 08_52 Paris, Madrid/")
df3 <- write_abs_data("resultats/Spectro_PETIT_Roscoff/Boussole-MethodfromDox2 mardi 15 dВcembre 2020 08_08 Paris, Madrid/")

full_df <- bind_rows(df1, df2, df3)

#substract the 750 value

wl750 <- filter(full_df, wl == 750) %>% rename('white_750' = abs) %>% select(-wl, -white)

full_df <- left_join(full_df, wl750) %>% mutate(abs = abs - white_750)


ggplot(full_df)+
  geom_path(aes(x = wl, y = abs, colour = strain, group = description))+
  facet_grid(dilution~replicate)

ggplot(full_df)+
  geom_path(aes(x = wl, y = abs, colour = strain, group = description))+
  geom_vline(aes(xintercept = 470), colour = 'blue')+
  geom_vline(aes(xintercept = 440), colour = 'green')
  
