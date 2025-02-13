library(tidyverse)
library(broom)

data <- data.frame('date' = NA, 'time' = NA, 'e440' = NA, 'e470' = NA, 'e532' = NA , 'jsp' = NA , 'replicate' = NA, 'file' = NA, 'strain' = NA )

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
datab$replicat <- 'c'

datad <- data.frame('date' = NA, 'time' = NA, 'e440' = NA, 'e470' = NA, 'e532' = NA , 'jsp' = NA , 'replicate' = NA, 'file' = NA, 'strain' = NA )

for(i in list.files('resultats/3x1m/replicat_d')){
  t <- read_csv(paste('resultats/3x1m/replicat_d/', i, sep = ''))
  t$strain <- i
  datac <- bind_rows(datad, t)
}
datab$replicat <- 'd'


data_stat <- data %>% bind_rows(datab, datac, datad) %>%  group_by(file, strain) %>% summarise_all(c(mean, sd)) %>% select(-replicat_fn1, -replicat_fn2)

data_plot <- data_stat %>% pivot_longer(c(e440_fn1:e532_fn1, e440_fn2:e532_fn2),  names_to = 'wavelength', values_to = 'dc') %>% na.omit()
data_plot$fn <- substr(data_plot$wavelength, 6, 8)
data_plot$wavelength <- substr(data_plot$wavelength, 1, 4)
data_plot <- data_plot %>% pivot_wider(c(file, strain, dc, wavelength, fn), names_from = fn, values_from = dc)

dilution <- tibble('file' = c('dilution_5.txt', 'dilution_4.txt', 'dilution_3.txt', 'dilution_3b.txt', 'dilution_2.txt', 'dilution_1'), 'dilution' = c(0.05, 0.1, 0.3, 0.3, 1, 5))
data_plot <- left_join(data_plot, dilution)

blanc <- filter(data_plot, file == 'blanc.txt') %>% ungroup() %>%  select(strain, wavelength, fn1)
colnames(blanc) <- c('strain', 'wavelength', 'blanc')

data_plot <- filter(data_plot, file != 'blanc.txt') %>% left_join(blanc)

write_csv(data_plot, 'projet_3x1m/dashboard/data_plot.csv')

ggplot(data_plot)+
  geom_bar(aes(x = file, y = fn1, fill = wavelength), position = position_dodge(), stat = 'identity')+
  geom_errorbar(aes(x = file, ymin = fn1 - fn2, ymax = fn1 + fn2, group = wavelength), position = position_dodge())+
  facet_wrap(.~strain, scales = 'free')+
  scale_fill_brewer(palette = 'Set2')

data_white <- data_plot %>% mutate(dc = fn1 - blanc,
                                   dc_sd = fn2)


ggplot(data_white)+
  geom_bar(aes(x = file, y = dc, fill = wavelength), position = position_dodge(), stat = 'identity')+
  geom_errorbar(aes(x = file, ymin = dc - dc_sd, ymax = dc + dc_sd, group = wavelength), position = position_dodge())+
  facet_wrap(.~strain, scales = 'free')+
  scale_fill_brewer(palette = 'Set2')

dilution <- read_csv('scipts/dashboard/dilution.csv') %>% rename('replicat_bio' = replicate)

data_white$file <- gsub('.txt', '', data_white$file)

data_white <- left_join(data_white, dilution, by = c('file' = 'dil_name'))

ggplot(data_white)+
  geom_smooth(aes(x = chl_real, y = dc, colour = wavelength), sd = TRUE, method = 'lm')+
  facet_wrap(.~strain.x, scales = 'free_x')+
  scale_x_reverse()+
  scale_fill_brewer(palette = 'Set2')+
  theme_minimal()


