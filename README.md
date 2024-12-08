# Redes-Neuronales

## Objetivo del Proyecto:
El objetivo es realizar un análisis exploratorio de los datos de admisión a universidades para comprender mejor las relaciones entre las variables y luego construir un modelo predictivo utilizando redes neuronales para predecir la probabilidad de admisión a un programa académico. La aplicación Shiny proporcionará una interfaz interactiva para que los usuarios ingresen sus datos y obtengan una predicción de su probabilidad de ser admitidos.

Este código proporciona una herramienta útil tanto para el análisis exploratorio de datos como para la predicción interactiva, utilizando modelos de redes neuronales y visualizaciones para evaluar el rendimiento del modelo.
### Análisis Exploratorio de Datos:
Lectura y Exploración de los Datos:
Se carga un archivo CSV con datos de estudiantes y se exploran las variables, eliminando la columna "Serial.No." y mostrando un resumen de los datos.
Gráficos de Correlación:
Se crea una función para generar gráficos de dispersión que muestran la correlación entre diferentes variables numéricas del dataset (como GRE Score, TOEFL Score, y CGPA).
Gráficos de Barras:
Se realiza una discretización de las variables "Chance.of.Admit", "GRE.Score" y "TOEFL.Score" para crear categorías y se visualizan las frecuencias de estas categorías mediante gráficos de barras.
Gráfico 3D de Dispersión:
Se genera un gráfico tridimensional para explorar la relación entre GRE Score, CGPA y TOEFL Score.
Gráfico de Densidad:
Se crea un gráfico de densidad para visualizar la distribución de una variable numérica, como "GRE.Score".
Normalización de los Datos:
Se aplica una normalización Min-Max a las columnas numéricas del dataset para prepararlas para el modelado.
División de Datos en Conjuntos de Entrenamiento y Prueba:
Se divide el dataset normalizado en un conjunto de entrenamiento (80%) y uno de prueba (20%) para futuros modelos predictivos.
Modelos de Redes Neuronales para Predicción:
Entrenamiento de Modelos Neuronales:

Se entrenan dos modelos de redes neuronales usando los paquetes neuralnet y nnet para predecir la probabilidad de admisión (Chance.of.Admit) basada en características como GRE Score, TOEFL Score y CGPA.
El modelo nn1 usa la función neuralnet() con 5 neuronas en la capa oculta.
El modelo nn2 usa la función nnet() con 5 neuronas en la capa oculta.

Métricas de Rendimiento: Se calculan métricas como RMSE, MAE, R² y correlación de Pearson para cada modelo, y se muestran en tablas para evaluar su precisión.
Histogramas de Residuos: Se generan histogramas de los residuos (diferencias entre las predicciones y los valores reales).
Gráfico de Predicciones vs Valores Reales: Se comparan las predicciones con los valores reales para cada modelo mediante gráficos.
Predicción Interactiva:

Los usuarios pueden ingresar datos (como puntajes GRE, TOEFL, CGPA, etc.) a través de una interfaz interactiva.
La aplicación realiza una predicción de la probabilidad de admisión usando el modelo nn1.
La predicción se muestra en un valueBox, y un GIF visualiza la predicción en función del valor obtenido (si la probabilidad es alta o baja).
Manejo de Errores:

Se utiliza tryCatch() para manejar posibles errores durante la predicción, mostrando un mensaje de error si algo falla.

