# Wallmart Sales

### > Data Source

Walmart Sales Dataset of 45stores: https://www.kaggle.com/datasets/varsharam/walmart-sales-dataset-of-45stores

### > Steps

1. Definição da serie temporal (fev/2010 a out/2012) 
2. Definição amostras treinamento e teste
3. Modelagem
	- A) modelo de tendência exponencial
	- B) modelo de tendência polinomial
	- C) modelo sazonal
	- D) modelo sazonal com tendencia
	- E) Média Móvel Simples
	- F1) Suavização exponencial complexa - ADITIVA (AAA)
	- F2) Suavização exponencial complexa - MULTIPLICATIVA 
	- F3) Suavização exponencial complexa - Automatica 
	- G-zero) definindo DIFERENCA das amostras treinamento e teste para metricas KPSS e ADF 
	- G1) ARIMA
	- G2) AUTO-ARIMA
4. Estudo de Acurácia
5. Escolha modelo vencedor
