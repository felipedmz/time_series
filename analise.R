library(dplyr)
library(stringr)
library(forecast)
import = as_tibble(read.csv2("D:/workspace/r_time_series/data/walmart-monthly-sales.csv"));

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

title(main="Amostras de Treinamento e Teste")
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
lines(validacao_ts, bty="l", col="red")

# modelo baseline = Naive
accuracy(modelo_naive, validacao_ts)


### A) modelo de tendência exponencial

modelo_tendencia_exp <- tslm(treinamento_ts ~ trend, lambda=0)
summary(modelo_tendencia_exp)
checkresiduals(modelo_tendencia_exp, test="LB")

# observacao geral do modelo 

plot(treinamento_ts, xlab="Tempo", ylab="Vendas", bty="l")
lines(modelo_tendencia_exp$fitted.values, lwd=2, col='green')

modelo_tendencia_exp_proj <- forecast(modelo_tendencia_exp, 
                                      h = tam_amostra_teste, 
                                      level=0)

# modelo sobre a base de validacao

plot(modelo_tendencia_exp_proj, 
     xlab="Tempo", 
     ylab="Vendas", 
     xaxt="n", 
     ylim=c(150, 300),
     xlim=c(2010, 2013.25), 
     bty="l", 
     flty=2,
     main="Projeção - Modelo de Regressão Exponencial")

axis(1, at=seq(2010, 2013, 1), labels=format(seq(2010, 2013,1)))
lines(validacao_ts, bty="l", col="red")

accuracy(modelo_tendencia_exp_proj, validacao_ts)

# resultado final para projecao da realidade

modelo_tendencia_exp_final <- tslm(data_ts ~ trend, lambda = 0)
summary(modelo_tendencia_exp_final)
modelo_tendencia_exp_final_proj <- forecast(modelo_tendencia_exp_final, 
                                            h=3, 
                                            level=0.95)

plot(modelo_tendencia_exp_final_proj, 
     xlab="Tempo", 
     ylab="Vendas", 
     ylim=c(150, 300),
     xlim=c(2010, 2013), 
     bty="l", 
     flty=2, 
     main="Projeção Futura - Regressão Exponencial")
lines(modelo_tendencia_exp_final_proj$fitted, lwd=2, col="blue")

lines(validacao_ts, bty="l", col="red")


### B) modelo de tendência polinomial

modelo_tendencia_poli <- tslm(treinamento_ts ~ trend + I(trend^2))
summary(modelo_tendencia_poli)
checkresiduals(modelo_tendencia_poli, test="LB")

# observacao geral do modelo 

plot(treinamento_ts, xlab="Tempo", ylab="Vendas", bty="l")
lines(modelo_tendencia_poli$fitted.values, lwd=2, col='green')

modelo_tendencia_poli_proj <- forecast(modelo_tendencia_poli, h = tam_amostra_teste, level=0.95)


# modelo sobre a base de validacao

plot(modelo_tendencia_poli_proj, 
     xlab="Tempo", 
     ylab="Vendas", 
     xaxt="n",
     #ylim=c(150, 300),
     xlim=c(2010, 2013.25), 
     bty="l", 
     flty=2,
     main="Projeção - Modelo de Regressão Polinomial")

axis(1, at=seq(2010, 2013, 1), labels=format(seq(2010, 2013,1)))
lines(validacao_ts, bty="l", col="red")

accuracy(modelo_tendencia_exp_proj, validacao_ts)

# resultado final para projecao da realidade

modelo_tendencia_poli_final <- tslm(data_ts ~ trend + I(trend^2))
summary(modelo_tendencia_poli_final)
modelo_tendencia_poli_final_proj <- forecast(modelo_tendencia_poli_final, 
                                             h=3, 
                                             level=0.95)

plot(modelo_tendencia_poli_final_proj, 
     xlab="Tempo", 
     ylab="Vendas",
     ylim=c(150, 300),
     xlim=c(2010, 2013), 
     bty="l", 
     flty=2, 
     main="Projeção Futura - Regressão Polinomial")
lines(modelo_tendencia_exp_final_proj$fitted, lwd=2, col="blue")
lines(validacao_ts, bty="l", col="red")

### C) modelo sazonal

ggseasonplot(data_ts)
dummies_mensais <- seasonaldummy(data_ts)

modelo_sazonalidade_linear <- tslm(treinamento_ts ~ season)
summary(modelo_sazonalidade_linear)
checkresiduals(modelo_sazonalidade_linear, test="LB")

# observacao geral do modelo 

plot(treinamento_ts, xlab="Tempo", ylab="Vendas", bty="l")
lines(modelo_sazonalidade_linear$fitted.values, lwd=2, col='green')

modelo_sazonalidade_linear_proj <- forecast(modelo_sazonalidade_linear, 
                                            h = tam_amostra_teste, 
                                            level=0.95)
# modelo sobre a base de validacao

plot(modelo_sazonalidade_linear_proj, 
     xlab="Tempo", 
     ylab="Vendas", 
     xaxt="n",
     #ylim=c(150, 300),
     xlim=c(2010, 2013.25), 
     bty="l", 
     flty=2,
     main="Projeção - Modelo Regressão Sazonal")

axis(1, at=seq(2010, 2013, 1), labels=format(seq(2010, 2013,1)))
lines(validacao_ts, bty="l", col="red")

accuracy(modelo_sazonalidade_linear_proj, validacao_ts)


# resultado final para projecao da realidade

modelo_sazonalidade_linear_final <- tslm(data_ts ~ season)
summary(modelo_sazonalidade_linear_final)
modelo_sazonalidade_linear_final_proj <- forecast(modelo_sazonalidade_linear_final, 
                                                  h=3, 
                                                  level=0.95)

plot(modelo_sazonalidade_linear_final_proj, 
     xlab="Tempo", 
     ylab="Vendas",
     #ylim=c(150, 300),
     xlim=c(2010, 2013), 
     bty="l", 
     flty=2, 
     main="Projeção Futura - Regressão Sazonal")
lines(modelo_sazonalidade_linear_final_proj$fitted, lwd=2, col="blue")
lines(validacao_ts, bty="l", col="red")


### D) modelo sazonal com tendencia

ggseasonplot(data_ts)
dummies_mensais <- seasonaldummy(data_ts)

modelo_sazonal_tend_linear <- tslm(treinamento_ts ~ season + trend + I(trend^2))
summary(modelo_sazonal_tend_linear)
checkresiduals(modelo_sazonal_tend_linear, test="LB")

# observacao geral do modelo 

plot(treinamento_ts, xlab="Tempo", ylab="Vendas", bty="l")
lines(modelo_sazonal_tend_linear$fitted.values, lwd=2, col='green')

modelo_sazonal_tend_linear_proj <- forecast(modelo_sazonal_tend_linear, 
                                            h = tam_amostra_teste, 
                                            level=0.95)

# modelo sobre a base de validacao

plot(modelo_sazonal_tend_linear_proj, 
     xlab="Tempo", 
     ylab="Vendas", 
     xaxt="n",
     #ylim=c(150, 300),
     xlim=c(2010, 2013.25), 
     bty="l", 
     flty=2,
     main="Projeção - Modelo Regressão Sazonal+Tendência")

axis(1, at=seq(2010, 2013, 1), labels=format(seq(2010, 2013,1)))
lines(validacao_ts, bty="l", col="red")

accuracy(modelo_sazonal_tend_linear_proj, validacao_ts)

# resultado final para projecao da realidade

modelo_sazonal_tend_linear_final <- tslm(data_ts ~ season + trend + I(trend^2))
summary(modelo_sazonal_tend_linear_final)
modelo_sazonal_tend_linear_final_proj <- forecast(modelo_sazonal_tend_linear_final, 
                                                  h=3, 
                                                  level=0.95)

plot(modelo_sazonal_tend_linear_final_proj, 
     xlab="Tempo", 
     ylab="Vendas",
     ylim=c(150, 300),
     xlim=c(2010, 2013), 
     bty="l", 
     flty=2, 
     main="Projeção Futura - Regressão Sazonal+Tendência")
lines(modelo_sazonal_tend_linear_final_proj$fitted, lwd=2, col="blue")
lines(validacao_ts, bty="l", col="red")

### E) Média Móvel
### F) Suavização exponencial
### G) Suavização exponencial complexa
### H) ARIMA



