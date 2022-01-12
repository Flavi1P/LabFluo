data <- data.frame('date' = NA, 'time' = NA, 'e440' = NA, 'e470' = NA, 'e532' = NA, 'jsp' = NA, 'replicate' = NA, 'file' = NA, 'hms' = NA, 'second' = NA, 'replicat_bio' = NA, 'strain' = NA)

for(i in list.files('resultats/3x1m/replicat_a', full.names = TRUE)){
  t <- read_csv(i)
  t$strain <- gsub('^.*/', '', i)
  t$strain <- gsub('.csv', '', t$strain)
  t$replicat_bio <- 'A'
  data <- bind_rows(data,t)
}
for(i in list.files('resultats/3x1m/replicat_b', full.names = TRUE)){
  t <- read_csv(i)
  t$strain <- gsub('^.*/', '', i)
  t$strain <- gsub('.csv', '', t$strain)
  t$replicat_bio <- 'B'
  data <- bind_rows(data,t)
}
for(i in list.files('resultats/3x1m/replicat_c', full.names = TRUE)){
  t <- read_csv(i)
  t$strain <- gsub('^.*/', '', i)
  t$strain <- gsub('.csv', '', t$strain)
  t$replicat_bio <- 'C'
  data <- bind_rows(data,t)
}

data_long <- pivot_longer(data, e440:e532, names_to = 'wavelength', values_to = 'dc')

write_csv(data_long, 'projet_3x1m/dashboard/time_series.csv')
ggplot(filter(data_long, strain == 'rcc156' & file == 'dilution_2.txt'))+
  geom_path(aes(x = second, y = dc, colour = wavelength))+
  facet_wrap(.~ replicate+ replicat_bio, scale = 'free_x')



