library(tidyverse)

data <- read_csv('projet_3x1m/dashboard/lm.csv')

taxa <- tibble('strain' = c('PCC9511',
                                'RCC162',
                                'RCC156',
                                'RCC2379',
                                'RCC2374',
                                'RCC2319',
                                'RCC76',
                                'RCC4213',
                                'RCC100',
                                'RCC1717',
                                'RCC3006'),
                   'taxa' = c('Proch',
                              'Proch',
                              'Proch',
                              'Syn',
                              'Syn',
                              'Syn',
                              'Diatom',
                              'Diatom',
                              'Pelagophyte',
                              'Diatom',
                              'Dino'))

data_large <- pivot_wider(data, c('strain', 'replicat_bio', 'replicate', 'dil_name', 'dc_zero', 'wavelength', 'chl_real', 'estimate'), values_from = c('dc_zero', 'estimate'), names_from = 'wavelength') %>% 
  mutate('d470_d440' = estimate_e470/estimate_e440,
         'd532_d440' = estimate_e532/estimate_e440) %>% 
  left_join(taxa)

write_csv(data_large, 'projet_3x1m/dashboard/taxa_df.csv')

ggplot(data_large)+
  geom_point(aes(x = d532_d440, y = d470_d440, colour = strain, shape = taxa) , size = 4)+
  ylab('slope_470/slope_440')+
  xlab('slope_532/slope_440')+
  scale_x_continuous(trans = 'log')+
  scale_color_manual(values = c('#ae017e',
                                '#fd8d3c',
                                '#f768a1',
                                '#c2e699',
                                '#a1dab4',
                                '#41b6c4',
                                '#225ea8',
                                '#969696',
                                '#78c679',
                                '#238443'))

ggplot(filter(data_large, taxa != 'Syn' & taxa != 'Proch'))+
  geom_density(aes(x = d532_d440, fill = taxa))

ggplot(data_large)+
  geom_density(aes(x = ratio_diatom, fill = taxa))+
  scale_x_continuous(trans = 'log')


stat_lm_large <- data %>% ungroup() %>% left_join(taxa) %>% select(strain, taxa, wavelength, dc_zero, chl_real, replicat_bio, replicate) %>% pivot_wider(names_from = 'wavelength', values_from = 'dc_zero') %>% 
  mutate(ratio532_440 = e532 / e440,
         ratio470_440 = e470 / e440)



ggplot(stat_lm_large)+
  geom_point(aes(x = e532/e440, y = e470/e440, colour = taxa))+
  xlim(-5, 5)+
  ylim(-1,1)
