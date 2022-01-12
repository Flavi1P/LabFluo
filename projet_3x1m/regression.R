library(tidyverse)
library(broom)

data <- data.frame('date' = NA, 'time' = NA, 'e440' = NA, 'e470' = NA, 'e532' = NA , 'jsp' = NA , 'replicate' = NA, 'file' = NA, 'strain' = NA )
dilution <- read_csv('projet_3x1m/dashboard/dilution.csv') %>% rename('replicat_bio' = replicate)


for(i in list.files('resultats/3x1m/replicat_a')){
  t <- read_csv(paste('resultats/3x1m/replicat_a/', i, sep = ''))
  t$strain <- i
  data <- bind_rows(data, t)
}
data$replicat <- 'a'

datab <- data.frame('date' = NA, 'time' = NA, 'e440' = NA, 'e470' = NA, 'e532' = NA , 'jsp' = NA , 'replicate' = NA, 'file' = NA, 'strain' = NA )

for(i in list.files('resultats/3x1m/replicat_b')){
  t <- read_csv(paste('resultats/3x1m/replicat_b/', i, sep = ''))
  t$strain <- i
  datab <- bind_rows(datab, t)
}
datab$replicat <- 'b'
datac <- data.frame('date' = NA, 'time' = NA, 'e440' = NA, 'e470' = NA, 'e532' = NA , 'jsp' = NA , 'replicate' = NA, 'file' = NA, 'strain' = NA )

for(i in list.files('resultats/3x1m/replicat_c')){
  t <- read_csv(paste('resultats/3x1m/replicat_c/', i, sep = ''))
  t$strain <- i
  datac <- bind_rows(datac, t)
}
datac$replicat <- 'c'
datad <- data.frame('date' = NA, 'time' = NA, 'e440' = NA, 'e470' = NA, 'e532' = NA , 'jsp' = NA , 'replicate' = NA, 'file' = NA, 'strain' = NA )

for(i in list.files('resultats/3x1m/replicat_d')){
  t <- read_csv(paste('resultats/3x1m/replicat_d/', i, sep = ''))
  t$strain <- i
  datad <- bind_rows(datad, t)
}
datad$replicat <- 'd'


data_stat <- data %>% bind_rows(datab, datac, datad) %>% rename('replicat_bio' = replicat) %>% 
  mutate(dil_name = gsub('.txt', '', .$file),
         strain = toupper(gsub('.csv', '', .$strain)),
         replicat_bio = toupper(.$replicat_bio)) %>% 
  left_join(dilution) %>%
  select(e440, e470, e532, strain, replicate, replicat_bio, dil_name, chl_real) %>% 
  filter(dil_name != 'blanc')

data_stat_xcel <- data_stat %>% group_by(strain, replicat_bio, dil_name) %>% 
  summarise_all(list(max, mean))

write_excel_csv(data_stat_xcel, '3x1m_xcel')

blanc <- data %>% bind_rows(datab, datac, datad) %>%
  filter(file == 'blanc.txt') %>% ungroup() %>%
  select(strain, e440, e470, e532, replicat) %>%
  mutate(replicat_bio = toupper(replicat),
         strain = toupper(gsub('.csv', '', .$strain))) %>% 
  select(-replicat) %>% 
  pivot_longer(e440:e532, names_to = 'wavelength', values_to = 'dc') %>% 
  group_by(strain, replicat_bio, wavelength) %>% 
  summarise_all(mean) %>% 
  rename('blanc' = dc)

data_stat_lon <- pivot_longer(data_stat, e440:e532, names_to='wavelength', values_to = 'dc') 

data_stat_clean <- left_join(data_stat_lon, blanc) %>% 
  mutate(dc_zero = dc - blanc) %>% 
  group_by(strain, wavelength, replicat_bio, dil_name, replicate) %>% 
  summarise_all(mean)

stat_lm <- data_stat_clean %>% 
  group_by(wavelength, strain, replicat_bio) %>% 
  do(fitdc = tidy(lm(chl_real~dc_zero + 0, data = .))) %>% 
  unnest(fitdc) %>%
  right_join(data_stat_clean) %>% 
  ungroup() %>% 
  group_by(strain, replicat_bio) %>% 
  mutate(y_pos = max(dc_zero) - mean(dc_zero))

labels <- select(stat_lm, strain, replicat_bio, wavelength, estimate) %>%
  mutate('coeff' = round(estimate, 3)) %>% select(-estimate)
labels <- labels[!duplicated(labels),]

stat_lm <- left_join(stat_lm, labels)

write_csv(stat_lm, 'projet_3x1m/dashboard/lm.csv')

ggplot(filter(stat_lm, strain == 'RCC3006'))+
  geom_smooth(aes(x = chl_real, y = dc_zero, colour = wavelength), method = 'lm', se = TRUE)+
  geom_point(aes(x = chl_real, y = dc_zero, fill = wavelength), shape = 21)+
  geom_text(aes(x = 2, y =  y_pos, label = coeff, colour = wavelength), hjust = 0.5, position = ggstance::position_dodgev(60),size = 4)+
  scale_x_reverse()+
  scale_color_brewer(palette = 'Set2')+
  scale_fill_brewer(palette = 'Set2', labels = c('440 nm', '470 nm', '532 nm'), name = 'excitation\nwavelength')+
  ylab('dc - white')+
  theme_minimal()+
  guides(color = FALSE)+
  facet_wrap(~replicat_bio)

