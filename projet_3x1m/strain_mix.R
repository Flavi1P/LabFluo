strain_mix <- tibble('melange' = paste('melange_', c(1:8), sep = ''),
                         'x_strain' = c('RCC3006',
                                         'RCC1717',
                                         'PCC9511',
                                         'RCC2319',
                                         'RCC1717',
                                        'RCC9511',
                                        'RCC1717',
                                         'RCC2319'),
                     
                          'y_strain' = c('RCC1717',
                                         'RCC3006',
                                         'RCC2319',
                                         'PCC9511',
                                         'PCC9511',
                                         'RCC1717',
                                         'RCC2319',
                                         'RCC1717'))

write_csv(strain_mix, 'projet_3x1m/dashboard/strain_mix.csv')
