data <- data.frame('date' = NA, 'time' = NA, 'e440' = NA, 'e470' = NA, 'e532' = NA, 'jsp' = NA, 'replicate' = NA, 'file' = NA, 'hms' = NA, 'second' = NA, 'replicat' = NA, 'strain' = NA)
for(i in list.files('resultats/3x1m/replicat_a', full.names = TRUE)){
  t <- read_csv(i)
  t$strain <- gsub('^.*/', '', i)
  t$strain <- gsub('.csv', '', t$strain)
  data <- bind_rows(data, t)
}

datb <- data.frame('date' = NA, 'time' = NA, 'e440' = NA, 'e470' = NA, 'e532' = NA, 'jsp' = NA, 'replicate' = NA, 'file' = NA, 'hms' = NA, 'second' = NA, 'replicat' = NA, 'strain' = NA)

for(i in list.files('resultats/3x1m/replicat_b', full.names = TRUE)){
  t <- read_csv(i)
  t$strain <- gsub('^.*/', '', i)
  t$strain <- gsub('.csv', '', t$strain)
  datb <- bind_rows(datb, t)
}

datc <- data.frame('date' = NA, 'time' = NA, 'e440' = NA, 'e470' = NA, 'e532' = NA, 'jsp' = NA, 'replicate' = NA, 'file' = NA, 'hms' = NA, 'second' = NA, 'replicat' = NA, 'strain' = NA)

for(i in list.files('resultats/3x1m/replicat_c', full.names = TRUE)){
  t <- read_csv(i)
  t$strain <- gsub('^.*/', '', i)
  t$strain <- gsub('.csv', '', t$strain)
  datc <- bind_rows(datc, t)
}
data$replicat <- 'A'
datb$replicat <- 'B'
datc$replicat <- 'C'
data_full <- bind_rows(data, datb, datc)
data_stat <- data_full %>%  group_by(strain, file, replicat) %>% summarise_all(c(mean, sd))

data_plot <- data_stat %>% pivot_longer(c(e440_fn1:e532_fn1, e440_fn2:e532_fn2),  names_to = 'wavelength', values_to = 'dc') %>% na.omit()
data_plot$fn <- substr(data_plot$wavelength, 6, 8)
data_plot$wavelength <- substr(data_plot$wavelength, 1, 4)
data_plot <- data_plot %>% pivot_wider(c(strain, file, replicat, dc, wavelength, fn), names_from = fn, values_from = dc)

dilution <- tibble('file' = c('dilution_5.txt', 'dilution_4.txt', 'dilution_3.txt', 'dilution_3b.txt', 'dilution_2.txt', 'dilution_1', 'dilution_0'), 'dilution' = c(0.05, 0.1, 0.3, 0.3, 1, 5, 20))
data_plot <- left_join(data_plot, dilution)

blanc <- filter(data_plot, file == 'blanc.txt') %>% ungroup() %>%  select( wavelength, fn1)
colnames(blanc) <- c('wavelength', 'blanc')

data_plot <- filter(data_plot, file != 'blanc.txt') %>% left_join(blanc)

write_csv(data_plot, 'projet_3x1m/dashboard/data_rep.csv')

ggplot(data_plot)+
  geom_bar(aes(x = file, y = fn1, fill = wavelength), position = position_dodge(), stat = 'identity')+
  geom_errorbar(aes(x = file, ymin = fn1 - fn2, ymax = fn1 + fn2, group = wavelength), position = position_dodge())+
  scale_fill_brewer(palette = 'Set2')+
  facet_wrap(.~strain + replicat, scale = 'free_x')

             