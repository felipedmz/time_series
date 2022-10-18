library(dplyr)

import = as_tibble(read.csv2("D:/Google Drive/MBA/analise de series temporais/trabalho/walmart-monthly-sales.csv"));
import$period = str_c(import$month, '-', import$year)
names(import)

data = select(import, c(period, monthly_sales))
names(data)
head(data)
