library(dplyr)
library(stringr)
import = as_tibble(read.csv2("D:/Google Drive/MBA/analise de series temporais/trabalho/walmart-monthly-sales.csv"));

# fomatando coluna periodo
import$period = str_c(import$month, '-', import$year)

# reduzindo escala de vendas (milhoes de dolares)
import = import %>% mutate(monthly_sales_mm = as.numeric(monthly_sales)/1000000)
names(import)
head(import)

# definindo a serie temporal (fev/2010 a out/2012) 

data <- select(import, c(period, monthly_sales_mm))
data_ts <- ts(data$monthly_sales_mm, start=c(2010, 2), end=c(2012, 10), frequency = 12)

data_ts
summary(data_ts)

plot(data_ts, xlab="Tempo", ylab="Vendas", type="l")

# definindo amostras treinamento e teste

tam_amostra_teste <- 10
tam_amostra_treinamento <- length(data_ts) - tam_amostra_teste

treinamento_ts <- window(data_ts, 
                         start=c(2010, 1), 
                         end=c(2010, tam_amostra_treinamento))
validacao_ts <- window(data_ts, 
                       start=c(2010, tam_amostra_treinamento + 1), 
                       end=c(2010, tam_amostra_treinamento + tam_amostra_teste))

# vermelho destaca a base de teste/validacao
plot(treinamento_ts, 
     xlab="Tempo", 
     ylab="Vendas", 
     xaxt="n",
     xlim=c(2010, 2013.25), type="l")

axis(1, at=seq(2010, 2013, 1), labels=format(seq(2010, 2013,1)))
lines(validacao_ts, bty="l", col="red")


# modelo naive

modelo_naive <- naive(treinamento_ts, level=0, h=tam_amostra_teste)
plot(modelo_naive, 
     xlab="Tempo", 
     ylab="Vendas", 
     xaxt="n",
     xlim=c(2010, 2013.25), bty="l", flty=2)

axis(1, at=seq(2010, 2013, 1), labels=format(seq(2010, 2013, 1)))
lines(validacao_ts)



