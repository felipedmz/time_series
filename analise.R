library(dplyr)

import = as_tibble(read.csv2("D:/Google Drive/MBA/analise de series temporais/trabalho/wallmart-monthly-sales.csv"));
import$period = str_c(import$year, '-', import$month)
names(import)

data = select(import, c(period, monthly_sales))
data
