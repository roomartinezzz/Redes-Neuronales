library(NeuralNetTools)

library(shiny)
library(shinythemes)
library(rpart)
library(rpart.plot)
library(dplyr)
library(ggplot2)
library(corrplot)
library(liver)
library(dplyr)
library(plotly)
library(ggcorrplot)
library(DT)
library(tidyr)
library(shinydashboard)
library(stringr) 
library(caret)
library(cluster)
library(factoextra)
library(reshape2)
library(shinythemes)
library(shinyBS)  
library(NeuralNetTools)
library(nnet)



library(visNetwork)

library(neuralnet)



data1 <- read.csv("Admission_Predict.csv", sep = ",")
#Exploracion de los datos
data1$Serial.No. <- NULL

data1
str(data1)

data_summary <- summary(data1)
data_str <- capture.output(str(data1))
data_table <- datatable(data1)

color <- c(
  "#A4D8E1",  # Cyan claro
  "#4DA6C4",  # Cyan medio
  "#2E93B8",  # Cyan oscuro
  "#1C7A92",  # Cyan muy oscuro
  "#0B4462" , # Cyan profundo,
  "#DAF7A6",  # Verde claro,
  "#FF6F61",  # Coral
  "#F7CAC9",  # Rosa claro
  "#B9D3C1",  # Verde agua
  "#92A8D1",  # Azul suave
  "#88B04B" ,# Verde manzana
  "#B9D3C1",  # Verde agua
  "#92A8D1",  # Azul suave
  "#82CCDD",  # Aqua
  "#C5E1A5",  # Verde claro (Mate)
  "#FFB142",  # Naranja suave
  "#BFACE2"   # Lavanda
  
)


# Función para generar gráfico de correlación
correlation_plot <- function(data1, var1, var2, color) {
  
  # Calculamos la correlación entre las dos variables
  cor_value <- cor(data1[[var1]], data1[[var2]], use = "complete.obs")
  
  # Título del gráfico
  plot_title <- paste( var1, "y", var2, ":", round(cor_value, 2))
  
  # Gráfico de dispersión con la correlación
  cor_plot <- plot_ly(
    data = data1,
    x = ~data1[[var1]],
    y = ~data1[[var2]],
    type = 'scatter',
    mode = 'markers',
    marker = list(color = color),
    text = ~paste0("Correlación: ", round(cor_value, 2)),
    hoverinfo = 'text'
  ) %>%
    layout(
      xaxis = list(title = var1),
      yaxis = list(title = var2)
    )
  
  return(cor_plot)  
}

# Definir las combinaciones de variables
numeric_vars <- c("GRE.Score", "TOEFL.Score", "University.Rating", "SOP", "LOR", "CGPA")

# Crear una lista de combinaciones de dos variables utilizando combn
var_combinations <- combn(numeric_vars, 2, simplify = FALSE)

# Selecciona las variables de la combinación
selected_combination <- var_combinations[[1]]  # Primer par de variables (por ejemplo, "GRE.Score" y "TOEFL.Score")

# Llamar a la función de gráfico de correlación con la combinación seleccionada
correlation_plot(data1, selected_combination[1], selected_combination[2], color = "#0B4462")

#__________________________________________________________________________________________________________________


# Función para crear gráficos de barras
barrasFrec <- function(data1, column, color) {
  
  # Frecuencia de cada categoría
  filtered_data <- data1 %>%
    count(!!sym(column)) %>%
    mutate(Percentage = n / sum(n) * 100)
  
  # Gráfico
  barFrec <- plot_ly(
    filtered_data,
    x = ~filtered_data[[column]],  # Utilizar la columna como x
    y = ~Percentage,               # Utilizar el porcentaje como y
    type = 'bar',
    text = ~paste0(round(Percentage, 2), "%"),  # Texto con porcentaje
    textposition = 'auto',
    marker = list(color = color)
  ) %>%
    layout(
      xaxis = list(title = column),
      yaxis = list(title = "Frecuencia (%)")
    )
  
  return(barFrec)
}


# Ejemplo de cómo llamar a la función para variables categóricas
color <- "#1C7A92"  # Color para los gráficos

# Variables categóricas

# Discretizamos "Chance.of.Admit" en categorías
data1$ChanceCategory <- cut(data1$Chance.of.Admit,
                            breaks = seq(0, 1, by = 0.1),
                            include.lowest = TRUE,
                            labels = paste(seq(0, 0.9, by = 0.1), "-", seq(0.1, 1, by = 0.1)))

data1$GRE.Category <- cut(data1$GRE.Score, 
                          breaks = seq(300, 340, by = 5),  # Rango de 5 puntos
                          include.lowest = TRUE, 
                          labels = paste(seq(300, 335, by = 5), "-", seq(305, 340, by = 5)))


data1$TOEFL.Category <- cut(data1$TOEFL.Score, 
                            breaks = seq(100, 120, by = 5),  # Rango de 5 puntos
                            include.lowest = TRUE, 
                            labels = paste(seq(100, 115, by = 5), "-", seq(105, 120, by = 5)))




# Gráficos de barras para las variables de data1
barChance <- barrasFrec(data1, "ChanceCategory", color)
barGRE <- barrasFrec(data1, "GRE.Category", color)  
barTOEFL <- barrasFrec(data1, "TOEFL.Category", color)  
barRating <- barrasFrec(data1, "University.Rating", color)  # Ya es categórica


#_______

scatter3d_plot<-plot_ly(data = data1, 
        x = ~GRE.Score, 
        y = ~CGPA, 
        z = ~TOEFL.Score, 
        type = 'scatter3d', 
        mode = 'markers', 
        marker = list(color = '#0B4462')) %>%
  layout(
         scene = list(xaxis = list(title = 'GRE Score'),
                      yaxis = list(title = 'CGPA'),
                      zaxis = list(title = 'TOEFL Score')))


###-------

# Crear la función para el gráfico de densidad con línea de densidad
density_plot <- function(data1, response_var) {
  
  # Calcular la densidad utilizando la función density() de R
  dens <- density(data1[[response_var]], na.rm = TRUE, bw = 0.02)  # Ajuste el parámetro 'bw' para suavizar
  
  # Crear el histograma de densidad
  plot <- plot_ly(data = data1, 
                  x = ~get(response_var),  # Usa la variable de respuesta
                  type = 'histogram',
                  histnorm = 'density',  # Normalizar a densidad
                  marker = list(color = "#4DA6C4", opacity = 0.7), 
                  rug = list(alpha = 0.1)) %>%
    layout(
           xaxis = list(title = response_var),
           yaxis = list(title = 'Densidad'))
  

  return(plot)
}

### Normalizacion de datos
# Función de normalización Min-Max
min_max_scaling <- function(x) {
  return((x - min(x)) / (max(x) - min(x)))
}

# Aplicar normalización a todas las columnas numéricas (excluyendo 'Research' y 'Chance.of.Admit' si no deseas normalizarlas)
data_normalized <- as.data.frame(lapply(data1[, 1:6], min_max_scaling))

# Incluir las columnas que no quieres normalizar
data_normalized$Research <- data1$Research
data_normalized$Chance.of.Admit <- data1$Chance.of.Admit

##Entrenamiento y prueba
set.seed(123)

# Dividir el dataset en entrenamiento (80%) y testeo (20%)
train_indices <- createDataPartition(data_normalized$Chance.of.Admit, p = 0.8, list = FALSE)

# Conjuntos de entrenamiento y testeo
train_normalized <- data_normalized[train_indices, ]
test_normalized <- data_normalized[-train_indices, ]












# Define UI for application that draws a histogram
# Define UI
ui <- navbarPage(
  title = div(style = "font-size: 30px;", "Redes Neuronales"),  # Título más grande
  tags$head(
    tags$link(
      href = "https://fonts.googleapis.com/css2?family=Montserrat:wght@300;400;700&display=swap",  # Cargar la fuente Montserrat
      rel = "stylesheet"
    ),
    tags$style(HTML("
          /* Fondo animado con un degradado sutil */
      body {
        background: linear-gradient(45deg, #f0f9ff, #dff9fb, #d1f2eb, #fef9e7, #f5c6aa);
        background-size: 400% 400%;
        animation: gradientAnimation 15s ease infinite;
        font-family: 'Montserrat', sans-serif;
      }
      
      /* Animación del fondo para cambiar el degradado suavemente */
      @keyframes gradientAnimation {
        0% {background-position: 0% 50%;}
        50% {background-position: 100% 50%;}
        100% {background-position: 0% 50%;}
      }

      .navbar { 
        background-color: #0B4462;  /* Color  */
        border-color: #C3E4A6;      /* Borde ligeramente más oscuro */
        font-family: 'Montserrat', sans-serif;  /* Aplicar la fuente Montserrat */
      }
      .navbar-default .navbar-nav > li > a { 
        color: white;  /* Texto blanco */
        font-size: 20px;  /* Tamaño de la letra de las pestañas */
        font-weight: 400;  /* Peso normal */
      }
      .navbar-default .navbar-brand {
        color: white;  /* Título blanco */
        font-weight: 500;  /* Título con peso en negrita */
      }
      .navbar-default .navbar-nav > li > a:hover {
        background-color: #C3E4A6;  /* Color ligeramente más oscuro al pasar el cursor */
      }
      
        /* Cambiar estilo para los encabezados de todos los boxes */
        .box-header {
          background-color: #A4D8E1 !important;  /* Color de fondo del header */
          color: #333 !important;                /* Color del texto */
          padding: 3px !important;              /* Espaciado dentro del header */
          border-radius: 2px 2px 0 0 !important; /* Bordes redondeados solo arriba */
        }

        .box-custom {
          height: 200px; /* Ajusta la altura según lo necesario */
          overflow-y: auto; /* Permitir desplazamiento si el contenido es demasiado largo */
          border: 2px solid black; /* Borde negro alrededor del box */
          background-color: transparent !important; /* Fondo transparente */
        }

        /* Eliminar el borde de los boxes */
        .box {
          border: none !important;
          box-shadow: none !important;
        }

        /* Eliminar el borde inferior del header de los boxes */
        .box-header {
          border-bottom: none !important;
        }

        /* Cambiar el color de fondo y el borde superior */
        .main-header .navbar {
          background-color: #6DB3C8 !important; /* Color de fondo de la barra superior */
        }



        /* Cambiar el color de los ítems del menú al pasar el mouse */
        .skin-blue .main-sidebar .sidebar-menu > li > a:hover {
          background-color: #6DB3C8 !important;  /* Cambia el color de fondo al hacer hover en el menú */
          color: white !important;  /* Cambia el color del texto en hover */
        }



        .content {
          position: relative; /* Permite que el contenido se muestre sobre el fondo */
          z-index: 1;
          color: #333; /* Color del texto principal */
          padding: 20px; /* Añade algo de espacio */
        }

        /* Estilo específico para el box en el panel Interactivo */
        .interactive-box {
          background-color: rgba(249, 249, 249, 0.9); /* Fondo suave */
          border-radius: 5px; /* Bordes redondeados */
        }

        .large-input-container {
          font-size: 30px;  /* Aumenta el tamaño de la fuente */
          height: 50px;  /* Aumenta la altura del contenedor */
        }
        
        .large-input-container input {
          font-size: 30px;  /* Aumenta el tamaño de la fuente del input */
          height: 50px;  /* Aumenta la altura del input */
        }
        .bs-collapse-panel {
  border: 1px solid #A4D8E1; /* Color del borde */
  border-radius: 5px;        /* Bordes redondeados */
  margin-bottom: 10px;       /* Espacio entre paneles */
}
  .nav-tabs > li > a {
  background-color: #A4D8E1; /* Color de fondo de las pestañas */
  color: #333; /* Color del texto de las pestañas */
}

.nav-tabs > li > a:hover {
  background-color: #89C2D8; /* Color de fondo al pasar el mouse */
}

      ")),
    
    # Sección para el mensaje que cambia al pasar el mouse sobre los enlaces
    tags$script(HTML("
        $(document).ready(function() {
          $('a').hover(
            function() {
              $('#message').text($(this).attr('title')); // Cambiar el texto al pasar el mouse
            }, 
            function() {
              $('#message').text(''); // Limpiar el texto al salir
            }
          );
        });
      "))
  ),
  
  
  
  
  
  navbarMenu("Teoría", icon = icon('book'),
             tabPanel("Información", 
                      icon = icon('lightbulb'),
                      fluidRow(
                        box(title='Introducción a las Redes Neuronales', width = 12,status='warning',
                            tags$p("Las redes neuronales o redes neuronales simuladas son un subconjunto del aprendizaje automático que se inspira en el cerebro humano. Imitan cómo las neuronas biológicas se comunican entre sí para tomar una decisión.",style = "margin-bottom: 10px; text-align: justify;"),
                            tags$p("Toda red neuronal consta de capas de nodos o neuronas artificiales: una capa de entrada, una o varias capas ocultas y una capa de salida. Cada nodo se conecta a los demás y tiene su propia ponderación y umbral asociados. Si la salida de cualquier nodo individual está por encima del valor umbral especificado, ese nodo se activa y envía datos a la siguiente capa de la red. De lo contrario, no se pasa ningún dato a la siguiente capa de la red.",style = "margin-bottom: 10px; text-align: justify;"),
                            tags$p("Las redes neuronales se basan en datos de entrenamiento para aprender y mejorar su precisión con el tiempo. Una perfeccionadas, se convierten en potentes herramientas en informática e inteligencia artificial, que nos permiten clasificar y agrupar datos a gran velocidad",style = "margin-bottom: 10px; text-align: justify;"),
                            tags$br(),
                            img(src='red.gif', style = "width:100%; height: 500px;margin-bottom: 20px;")),
                      tabsetPanel(
                        tabPanel('Estructura de la red',
                                 tags$div(
                                   style = "padding-left: 10px; padding-right: 10px;",  # Añade márgenes laterales
                                   
                                   tags$br(),
                                   tags$p("Las redes neuronales son modelos creados al ordenar operaciones matemáticas siguiendo una determinada estructura. La forma más común de representar la estructura de una red neuronal es mediante el uso de capas (layers), formadas a su vez por neuronas (unidades, units o neurons). Cada neurona, realiza una operación sencilla y está conectada a las neuronas de la capa anterior y de la capa siguiente mediante pesos, cuya función es regular la información que se propaga de una neurona a otra.", 
                                          style = "margin-bottom: 10px; text-align: justify;"),
                                   
                                   img(src='red2.jpeg', style = "width:70%; height: 500px; display: block; margin-left: auto; margin-right: auto; margin-bottom: 20px;"),
                                   
                                   tags$p("La primera capa de la red neuronal (color rojo) se conoce como capa de entrada o input layer y recibe los datos en bruto, es decir, el valor de los predictores. La capa intermedia (color azul), conocida como capa oculta o hidden layer, recibe los valores de la capa de entrada, ponderados por los pesos (flechas grises). La última capa, llamada output layer, combina los valores que salen de la capa intermedia para generar la predicción.", 
                                          style = "margin-bottom: 10px; text-align: justify;"),
                                   
                                   tags$p("Para facilitar la comprensión de la estructura de las redes, es útil representar una red equivalente a un modelo de regresión lineal:", 
                                          style = "margin-bottom: 10px; text-align: justify;"),
                                   
                                   tags$div(
                                     style = "display: flex; align-items: flex-start; margin-bottom: 20px;",
                                     img(src='reg.png', style = "width: 30%; height:400px; margin-right: 20px;"),
                                     tags$div(
                                       style = "width: 70%; text-align: justify;",
                                       tags$br(),
                                       tags$br(),
                                       tags$p("Cada neurona de la capa de entrada representa el valor de uno de los predictores. Las flechas representan los coeficientes de regresión, que en términos de redes se llaman pesos, y la neurona de salida representa el valor predicho. Para que esta representación equivalga a la ecuación de un modelo lineal, faltan dos cosas:"),
                                       tags$br(),
                                       tags$ul(
                                         tags$li("El bias del modelo."),
                                         tags$li("Las operaciones de multiplicación y suma que combinan el valor de los predictores con los pesos del modelo.")
                                       ),
                                       tags$br(),
                                       tags$p("Cada neurona de la capa intermedia tiene un valor de bias, pero suele omitirse en las representaciones gráficas. En cuanto a las operaciones matemáticas, es el elemento clave que ocurre dentro de las neuronas y conviene verlo con detalle.")

                            
                            )))),
                          tabPanel("La neurona (unidad)",
                                   tags$div(
                                     style = "padding-left: 10px; padding-right: 10px;",
                                   tags$br(),
                                   tags$p('La neurona es la unidad funcional de los modelos de redes. Dentro de cada neurona ocurren simplemente dos operaciones: la suma ponderada de sus entradas y la aplicación de una función de activación.', style = "margin-bottom: 10px; text-align: justify;"),
                                  tags$p("En la primera parte, se multiplica cada valor de entrada xi por su peso asociado wi y se suman junto con el bias. Este es el valor neto de entrada a la neurona. A continuación, este valor se pasa por una función, conocida como función de activación, que transforma el valor neto de entrada en un valor de salida.",style = "margin-bottom: 10px; text-align: justify;"),
                                  tags$br(),
                                  tags$p("Si bien el valor que llega a la neurona, siempre es una combinación lineal, gracias a la función de activación, se pueden generar salidas muy diversas. Es en la función de activación donde reside el potencial de los modelos de redes para aprender relaciones no lineales.",style = "margin-bottom: 10px; text-align: justify;"),
                                  img(src='red3.png', style = "width:70%; height: 500px; display: block; margin-left: auto; margin-right: auto; margin-bottom: 20px;"),
                          )),
                          
                          tabPanel("Función de activación",
                                   tags$div(
                                     style = "padding-left: 10px; padding-right: 10px;",
                                   tags$br(),
                                   tags$p("Las funciones de activación controlan en gran medida qué información se propaga desde una capa a la siguiente (forward propagation). Estas funciones convierten el valor neto de entrada a la neuronal (combinación de los input, pesos y bias) en un nuevo valor. Gracias a combinar funciones de activación no lineales con múltiples capas (ver más adelante), los modelos de redes son capaces de aprender relaciones no lineales", style = "margin-bottom: 10px; text-align: justify;"),
                                   tags$p("En la primera parte, se multiplica cada valor de entrada xi por su peso asociado wi y se suman junto con el bias. Este es el valor neto de entrada a la neurona. A continuación, este valor se pasa por una función, conocida como función de activación, que transforma el valor neto de entrada en un valor de salida.",style = "margin-bottom: 10px; text-align: justify;"),
                                    bsCollapse(
                                       bsCollapsePanel("Rectified linear unit (ReLU)",
                                                       tags$p("La función de activación ReLu aplica una transformación no lineal muy simple, activa la neurona solo si el input está por encima de cero. Mientras el valor de entrada está por debajo de cero, el valor de salida es cero, pero cuando es superior, el valor de salida aumenta de forma lineal con el de entrada."),
                                                       img(src='relu.png', style = "width:30%; height: 400px; display: block; margin-left: auto; margin-right: auto; margin-bottom: 20px;"),
                                                       tags$p("De esta forma, la función de activación retiene únicamente los valores positivos y descarta los negativos dándoles una activación de cero.")),
                                       bsCollapsePanel("Sigmoide",
                                                       tags$p("La función sigmoide transforma valores en el rango de (-inf, +inf) a valores en el rango (0, 1)."),
                                                       img(src='sig.png', style = "width:30%; height: 400px; display: block; margin-left: auto; margin-right: auto; margin-bottom: 20px;"),
                                                       tags$p("Un caso en el que la función de activación sigmoide sigue siendo la función utilizada por defecto es en las neuronas de la capa de salida de los modelos de clasificación binaria, ya que su salida puede interpretarse como probabilidad."))
                                       
                                      
                                     
                                     ))),
                        
                          tabPanel("Función de coste (loss function)",
                                 tags$div(
                                   style = "padding-left: 10px; padding-right: 10px;",
                                   tags$br(),
                                   tags$p("La función de coste (l), también llamada función de pérdida, loss function o cost function, es la encargada de cuantificar la distancia entre el valor real y el valor predicho por la red, en otras palabras, mide cuánto se equivoca la red al realizar predicciones. En la mayoría de casos, la función de coste devuelve valores positivos. Cuanto más próximo a cero es el valor de coste, mejor son las predicciones de la red (menor error), siendo cero cuando las predicciones se corresponden exactamente con el valor real.", style = "margin-bottom: 10px; text-align: justify;"),
                                   tags$br(),
                                   tags$p("La función de coste puede calcularse para una única observación o para un conjunto de datos (normalmente promediando el valor de todas las observaciones). Es el segundo caso el que se utiliza para dirigir el entrenamiento de los modelos.",style = "margin-bottom: 10px; text-align: justify;"),
                                   tags$br(),
                                   tags$p("Dependiendo del tipo de problema, regresión o clasificación, es necesario utilizar una función de coste u otra. En problemas de regresión, las más utilizadas son el error cuadrático medio y el error absoluto medio. En problemas de clasificación suele emplearse la función log loss, también llamada logistic loss o cross-entropy loss."),
                                   bsCollapse(
                                     bsCollapsePanel("Error cuadrático medio",
                                                     tags$p("El error cuadrático medio (mean squared error, MSE) es con diferencia la función de coste más utilizada en problemas de regresión. Para una determinada observación i, el error cuadrático se calcula como la diferencia al cuadrado entre el valor predicho y^ y el valor real y."),
                                                     tags$p("De esta forma, la función de activación retiene únicamente los valores positivos y descarta los negativos dándoles una activación de cero.")),
                                     bsCollapsePanel("Log loss, logistic loss o cross-entropy loss",
                                                     tags$p("En problemas de clasificación, la capa de salida utiliza como función de activación la función softmax. Gracias a esta función, la red devuelve una serie de valores que pueden interpretarse como la probabilidad de que la observación predicha pertenezca a cada una de las posibles clases.Cuando la clasificación es de tipo binaria, donde la variable respuesta es 1 o 0, y p=Pr(y=1), la función de coste log-loss se define como:", style = "margin-bottom: 10px; text-align: justify;"),
                                                     img(src='fomr.png', style = "width:35%; height: 200px; display: block; margin-left: auto; margin-right: auto; margin-bottom: 20px;"))
                     
                        
                        ))),
                        
                        tabPanel("Tipos de redes neuronales",
                                 tags$div(
                                   style = "padding-left: 10px; padding-right: 10px;",
                                   tags$br(),
                                   bsCollapse(
                                     bsCollapsePanel("Feedforward",
                                                     tags$p("Constan de una capa de entrada, capas ocultas y una capa de salida. Se llama feedforward porque los datos fluyen en la dirección hacia adelante, y no hay retropropagación.", style = "margin-bottom: 10px; text-align: justify;"),
                                                     img(src='for.gif', style = "width:35%; height: 200px; display: block; margin-left: auto; margin-right: auto; margin-bottom: 20px;"),
                                                     tags$p(" Se utiliza sobre todo en clasificación, reconocimiento de voz, reconocimiento facial y reconocimiento de patrones.", style = "margin-bottom: 10px; text-align: justify;")),
                                     bsCollapsePanel("Perceptrón multicapa",
                                                     tags$p("Los perceptrones multicapa (MLP) resuelven las deficiencias de las redes neuronales feedforward al no poder aprender mediante retropropagación. Es bidireccional y consta de múltiples capas ocultas y funciones de activación. Los MLP utilizan la propagación hacia delante para las entradas y la retropropagación para actualizar los pesos. Son redes neuronales básicas que han sentado las bases de la visión por ordenador, la tecnología del lenguaje y otras redes neuronales.", style = "margin-bottom: 10px; text-align: justify;")),

                                     bsCollapsePanel("Redes neuronales convolucionales (CNN)",
                                                     tags$p("Las redes neuronales de convolución (CNN) se utilizan generalmente en visión por ordenador, reconocimiento de imágenes y reconocimiento de patrones. Se utiliza para extraer características importantes de la imagen mediante múltiples capas convolucionales. La capa convolucional de la CNN utiliza una matriz (filtro) personalizada para convolucionar sobre las imágenes y crear un mapa.", style = "margin-bottom: 10px; text-align: justify;"),
                                                     tags$p("En general, las redes neuronales de convolución constan de una capa de entrada, una capa de convolución, una capa de agrupación, una capa totalmente conectada y una capa de salida ", style = "margin-bottom: 10px; text-align: justify;")),
                                     bsCollapsePanel("Redes neuronales recurrentes (RNN)",
                                                     tags$p("Las redes neuronales recurrentes (RNN) se utilizan habitualmente para datos secuenciales como textos, secuencias de imágenes y series temporales. Son similares a las redes feed-forward, salvo que obtienen las entradas de secuencias anteriores mediante un bucle de realimentación. Las RNN se utilizan en PNL, predicciones de ventas y previsiones meteorológicas.", style = "margin-bottom: 10px; text-align: justify;"),
                                                     tags$p("Las RNN presentan problemas de gradiente evanescente, que se resuelven mediante versiones avanzadas de las RNN denominadas redes de memoria a corto plazo (LSTM) y redes de unidades recurrentes controladas (GRU).", style = "margin-bottom: 10px; text-align: justify;"))
                                     
                                     
                                     )

                        ))))))
                      
            ,
                      
                      
  tabPanel("Principales librerías:",
           icon = icon('cogs'),
           
           p("Se describen las principales librerías utilizadas en R para implementar redes neuronales. Estas herramientas permiten desde la creación de modelos simples hasta redes neuronales profundas y avanzadas.", style = "color: #0B4462;"),
           
           # Usamos bsCollapse para los paneles
           bsCollapse(
             id = "paneles",  # ID para el grupo de paneles
             multiple = FALSE,  # Sólo uno a la vez puede estar abierto
             open = c("panel1", "panel2"),  # Paneles por defecto abiertos
             
             # Panel 1
             bsCollapsePanel(
               title = "Neuralnet", 
               value = "panel1",  # Identificador único para este panel
               p("Descripción: Permite entrenar redes neuronales feedforward (MLP). Ideal para tareas de clasificación y regresión con una estructura simple.", style = "color: #0B4462;"),
               p("Características principales:", style = "color: #0B4462;"),
               tags$ul(
                 tags$li(style = "color: #0B4462;", "Entrenamiento de redes neuronales con múltiples capas ocultas."),
                 tags$li(style = "color: #0B4462;", "Funciones de activación como sigmoide, tangente hiperbólica y lineales."),
                 tags$li(style = "color: #0B4462;", "Optimización mediante el algoritmo de descenso del gradiente.")
               ),
               p("Instalación:", style = "color: #0B4462;"),
               verbatimTextOutput("code1")
             ),
             
             # Panel 2
             bsCollapsePanel(
               title = "Keras", 
               value = "panel2",  # Identificador único para este panel
               p("Descripción: Una interfaz de R para TensorFlow. Permite construir redes neuronales profundas, convolucionales y recurrentes con facilidad.", style = "color: #0B4462;"),
               p("Características principales:", style = "color: #0B4462;"),
               tags$ul(
                 tags$li(style = "color: #0B4462;", "Soporta redes neuronales profundas (DNNs), convolucionales (CNNs) y recurrentes (RNNs)."),
                 tags$li(style = "color: #0B4462;", "Optimización de entrenamiento con soporte para GPU."),
                 tags$li(style = "color: #0B4462;", "Ideal para modelos complejos como procesamiento de imágenes o texto.")
               ),
               p("Instalación:", style = "color: #0B4462;"),
               verbatimTextOutput("code2")
             ),
             
             # Panel 3
             bsCollapsePanel(
               title = "Tensorflow", 
               value = "panel3",  # Identificador único para este panel
               p("Descripción: Proporciona una interfaz completa para trabajar con TensorFlow. Ideal para diseñar redes complejas desde cero.", style = "color: #0B4462;"),
               p("Características principales:", style = "color: #0B4462;"),
               tags$ul(
                 tags$li(style = "color: #0B4462;", "Control total sobre el entrenamiento y la optimización."),
                 tags$li(style = "color: #0B4462;", "Modelos complejos como redes profundas, auto-codificadores, etc."),
                 tags$li(style = "color: #0B4462;", "Entrenamiento distribuido y en paralelo.")
               ),
               p("Instalación:", style = "color: #0B4462;"),
               verbatimTextOutput("code3")
             ),
             
             # Panel 4
             bsCollapsePanel(
               title = "Nnet", 
               value = "panel4",  # Identificador único para este panel
               p("Descripción: Permite entrenar redes neuronales feedforward con una sola capa oculta. Ideal para modelos simples de clasificación y regresión.", style = "color: #0B4462;"),
               p("Características principales:", style = "color: #0B4462;"),
               tags$ul(
                 tags$li(style = "color: #0B4462;", "Implementación del algoritmo de retropropagación."),
                 tags$li(style = "color: #0B4462;", "Sencillo y eficiente para problemas pequeños.")
               ),
               p("Instalación:", style = "color: #0B4462;"),
               verbatimTextOutput("code4")
             ))
             
             
             
           ),
  
  
  

  
  # Pestaña Datos con subpestañas
  navbarMenu("Datos", icon = icon('database'),
             tabPanel("Dataset", 
                      icon = icon('table'),
                      fluidRow(
                        box(
                          title = 'Descripción de los Datos',
                          width = 12,
                          status = 'warning',
                          tags$h5("Este conjunto de datos contiene 6 variables y 500 observaciones."),
                          tags$br(),
                          tags$p(tags$b("Nombre de columnas:")),
                          tags$ul(
                            tags$br(),
                            tags$li(tags$b("GRE Score:"), "Puntaje en el examen GRE (Graduate Record Examination), generalmente entre 260 y 340."),
                            tags$li(tags$b("TOEFL Score:"), "Puntaje en el TOEFL, normalmente en un rango de 0 a 120."),
                            tags$li(tags$b("University Rating:"), "Calificación de la universidad en un rango de 1 a 5."),
                            tags$li(tags$b("SOP:"), "Puntaje del Statement of Purpose, en un rango de 1 a 5."),
                            tags$li(tags$b("LOR:"), "Puntuación de las cartas de recomendación (Letter of Recommendation), también en un rango de 1 a 5."),
                            tags$li(tags$b("CGPA:"), "Promedio acumulativo de calificaciones, generalmente en una escala de 0 a 10."),
                            tags$li(tags$b("Research:"), "Indicador binario (0 o 1) que indica si el candidato ha realizado investigaciones."),
                            tags$li(tags$b("Chance of Admit:"), "Variable objetivo que varía entre 0 y 1 y representa la probabilidad de ser admitido."),
                            tags$br(),
                            
                      
                          ),
                          
                          
                          fluidRow(
                            box(title = 'Tabla de Datos', width = 12, status = 'warning', DTOutput('data_table'), class="box_custom")
                          ),
                          tags$br(),
                          fluidRow(
                            box(title = 'Forma de los Datos', width = 6, status = 'warning', verbatimTextOutput('data_str'), class="box_custom"),
                            box(title = 'Estadísticas Descriptivas', width = 6, status = 'warning', verbatimTextOutput('data_summary'), class="box_custom")
                          )
                          
                        )
                      ))),
  
  # Pestañas EDA, Reglas de asociación y Simulación
  # Panel de Análisis Exploratorio de Datos
  tabPanel("EDA", 
           icon = icon('search'),
           fluidRow(
             # Caja para Correlación entre variables
             column(
               width = 12,  # Ocupa todo el ancho
               style = "margin-bottom: 30px;",  # Separación con margen inferior
               box(
                 title = "Correlación entre:", 
                 width = 12, 
                 status = 'warning', 
                 height = "500px", 
                 solidHeader = TRUE,
                 # Selector para elegir las variables a correlacionar
                 selectInput(
                   "selected_combination", 
                   label = "Selecciona las variables:",
                   choices = sapply(combn(numeric_vars, 2, simplify = FALSE), function(x) paste(x[1], "vs", x[2])),
                   selected = sapply(combn(numeric_vars, 2, simplify = FALSE), function(x) paste(x[1], "vs", x[2]))[1]
                 ),
                 # Espacio para el gráfico de correlación
                 plotlyOutput("correlation_plot")
               )
             ),
             
             # Espacio de separación con una columna vacía
             column(
               width = 12,
               style = "height: 20px;"  # Columna vacía para separar
             ),
             
             # Caja para Gráfico de Barras
             column(
               width = 12,
               style = "margin-bottom: 30px;",  # Espacio entre las cajas
               box(
                 title = "Frecuencia para la variable:", 
                 width = 12, 
                 status = 'warning', 
                 height = "500px", 
                 solidHeader = TRUE,
                 selectInput("selected_column", 
                             label = "Selecciona la variable:",
                             choices = c("ChanceCategory", "GRE.Category", "TOEFL.Category", "University.Rating"),
                             selected = "ChanceCategory"),
                 # Espacio para mostrar el gráfico de barras
                 plotlyOutput("bar_plot")
               )
             ),
             
             # Espacio de separación con una columna vacía
             column(
               width = 12,
               style = "height: 20px;"  # Columna vacía para separar
             ),
             
             # Caja para Gráfico 3D
             column(
               width = 6,
               style = "margin-bottom: 30px;",  # Espacio entre las cajas
               box(
                 tags$br(),
                 title = "Grafico 3D: GRE vs CGPA VS TOEFL" , 
                 width = 12, 
                 status = 'warning', 
                 height = "400px", 
                 solidHeader = TRUE,
                 plotlyOutput("scatter3d_plot"))
          ),
          

          # Caja para Gráfico 3D
          column(
            width = 6,
            style = "margin-bottom: 30px;",  # Espacio entre las cajas
            box(
              tags$br(),
              title = "Distribución de Chance of Admit" , 
              width = 12, 
              status = 'warning', 
              height = "400px", 
              solidHeader = TRUE,
              plotlyOutput("density_plot")
          )
           
            
          )))  ,
  navbarMenu("Aplicación", 
             tabPanel("Red Neuronal: Neuralnet", 
                      icon = icon('layer-group'),
                      fluidRow(
                        box(
                          title = "Primer paso: Normalizacion de los datos:",
                          width = 12, 
                          status = 'warning', 
                          height = "500px", 
                          solidHeader = TRUE,
                          DTOutput('final_data_normalized'), class="box_custom"
                        ),
                        tags$hr(),  # Línea horizontal para separación visual
                        column(
                          width = 6,
                          style = "margin-bottom: 30px;", 
                          box(
                            title = "Red Neuronal: Neuralnet:",
                            width = 12, 
                            status = 'warning', 
                            height = "500px", 
                            solidHeader = TRUE,
                            plotOutput("nn_plot1", height = "500px", width = "100%"), 
                            class = "box_custom"
                          )
                        ),
                        column(
                          width = 6,
                          style = "margin-bottom: 30px;", 
                          box(
                            title = "Red Neuronal: con pesos",
                            width = 12,   
                            status = 'warning', 
                            height = "500px", 
                            solidHeader = TRUE,
                            img(src = 'pesos.png', 
                                style = "width:100%; height: 500px; display: block; margin-left: auto; margin-right: auto; margin-bottom: 20px;"), 
                            class = "box_custom"
                          )
                        ),
                        tags$hr(),  # Línea horizontal para separación visual
                        column(
                          width = 12,
                          style = "height: 30px;"  # Se usa para separación de elementos
                        ),
                        column(
                          width = 6,
                          style = "margin-bottom: 30px;", 
                          box(
                            title = "Performance del Modelo 1:",
                            width = 12,   
                            status = 'warning', 
                            height = "500px", 
                            solidHeader = TRUE,
                            DTOutput("model_performance"), 
                            class = "box_custom"
                          )
                        ),
                        column(
                          width = 6,
                          style = "margin-bottom: 30px;", 
                          box(
                            title = "Predicciones vs. Valores Reales",
                            width = 12,   
                            status = 'warning', 
                            height = "500px", 
                            solidHeader = TRUE,
                            plotOutput("pred_vs_real"), 
                            class = "box_custom"
                          )
                        )
                      )
             ),
             
             # Aquí agregamos un segundo tabPanel para nnet
             tabPanel("Red Neuronal: Nnet",
                      icon = icon('cogs'),
                      fluidRow(
                        tags$hr(),  # Línea horizontal para separación visual
                        column(
                          width = 6,
                          style = "margin-bottom: 30px;", 
                          box(
                            title = "Red Net",
                            width = 12, 
                            status = 'warning', 
                            height = "500px", 
                            solidHeader = TRUE,
                            plotOutput("nn_plot2", height = "500px", width = "100%"), 
                            class = "box_custom"
                          )
                        ),
                        column(
                          width = 6,
                          style = "margin-bottom: 30px;", 
                          box(
                            title = "Performance del Modelo 2:",
                            width = 12,   
                            status = 'warning', 
                            height = "500px", 
                            solidHeader = TRUE,
                            DTOutput("model_performance2"),
                            class = "box_custom"
                          )
                        ),
                        tags$hr(),  # Línea horizontal para separación visual
                        column(
                          width = 12,
                          style = "height: 30px;"  # Se usa para separación de elementos
                        ),
                        column(
                          width = 6,
                          style = "margin-bottom: 30px;", 
                          box(
                            title = "Predicciones vs. Valores Reales",
                            width = 12,   
                            status = 'warning', 
                            height = "500px", 
                            solidHeader = TRUE,
                            plotOutput("pred_vs_real_nn2"),  
                            class = "box_custom"
                          )
                        ),
                        column(
                          width = 6,
                          style = "margin-bottom: 30px;", 
                          box(
                            title = "Histograma de residuoss",
                            width = 12,   
                            status = 'warning', 
                            height = "500px", 
                            solidHeader = TRUE,
                            plotOutput("residual_histogram"),  
                            class = "box_custom")
                          
                        )
                      )
             )
  )
  ,
  

  
  
  
  
  
  
  tabPanel("Simulación", 
           icon = icon('project-diagram'),
           
           box(
             titlePanel("Predicción de Probabilidad de Admision"),
             
             fluidRow(
               column(12, 
                      sidebarPanel(
                        numericInput("GRE.Score", "GRE Score:", value = 300, min = 290, max = 340),
                        numericInput("TOEFL.Score", "TOEFL Score:", value = 100, min = 90, max = 120),
                        selectInput("University.Rating", "University Rating:", choices = 1:5, selected = 3),
                        numericInput("SOP", "Statement of Purpose (SOP):", value = 4, min = 1, max = 5),
                        numericInput("LOR", "Letter of Recommendation (LOR):", value = 3, min = 1, max = 5),
                        numericInput("CGPA", "Cumulative Grade Point Average (CGPA):", value = 5, min = 5, max = 10),
                        selectInput("Research", "Research Experience:", choices = c(0, 1), selected = 0),
                        actionButton("predict", "Predecir")
                      )
               )
               
               
             )
             
             
           ), 
           box(
             valueBoxOutput("predicted_value"), 
             uiOutput("prediction_gif")
  )))


## Define server logic
server <- function(input, output) {
  
  observe({
    rmd_file <- "Redes_neuronales1.Rmd"
    html_file <- "www/report.html"
    
    # Verifica si el archivo RMD existe
    if (!file.exists(rmd_file)) {
      print("El archivo RMD no se encuentra.")
    }
    
    # Renderiza el archivo HTML si no existe
    if (!file.exists(html_file)) {
      tryCatch({
        rmarkdown::render(rmd_file, output_format = "html_document", output_file = html_file)
        print(paste("HTML generado exitosamente:", html_file))
      }, error = function(e) {
        print(paste("Error al renderizar el archivo RMD:", e$message))
      })
    }
  })
  output$download_rmd <- downloadHandler(
    filename = function() {
      paste("Redes_neuronales1", ".Rmd", sep = "")
    },
    content = function(file) {
      # Verifica si el archivo existe antes de copiar
      if (file.exists("Redes_neuronales1.Rmd")) {
        file.copy("Redes_neuronales1.Rmd", file)
        print("Archivo RMD descargado correctamente.")
      } else {
        print("El archivo RMD no se encuentra.")
      }
    }
  )
  
  output$data_str <- renderPrint({str(data1)})
  output$data_summary <- renderPrint({summary(data1)})
  output$data_table <- renderDT({
    datatable(data1, options = list(
      scrollX = TRUE, pageLength = 5, searchHighlight = TRUE,
      autoWidth = TRUE, dom = 'Bfrtip', buttons = c('copy', 'csv', 'excel', 'pdf', 'print')
    ), rownames = FALSE, extensions = 'Buttons')  })
  
  output$final_datatable<-renderDT({
    datatable(final_dataset, options = list(
      scrollX = TRUE, pageLength =5, searchHighlight = TRUE,
      autoWidth = TRUE, dom = 'Bfrtip', buttons = c('copy', 'csv', 'excel', 'pdf', 'print')
    ), rownames = FALSE, extensions = 'Buttons')
  })
  
  #Librerias
  # Asegúrate de incluir estos bloques de código dentro de tu aplicación Shiny
  output$code1 <- renderText({
    "install.packages('neuralnet')\nlibrary(neuralnet)"
  })
  
  output$code2 <- renderText({
    "install.packages('keras')\nlibrary(keras)\ninstall_keras()"
  })
  
  output$code3 <- renderText({
    "install.packages('tensorflow')\nlibrary(tensorflow)\ninstall_tensorflow()"
  })
  
  output$code4 <- renderText({
    "install.packages('nnet')\nlibrary(nnet)"
  })
  
  output$code5 <- renderText({
    "install.packages('caret')\nlibrary(caret)"
  })
  
  output$code6 <- renderText({
    "install.packages('h2o')\nlibrary(h2o)"
  })
  
  output$code7 <- renderText({
    "install.packages('deepnet')\nlibrary(deepnet)"
  })
  
  
  
  
  
  
  
  
  

  output$correlation_plot <- renderPlotly({
    selected_combination <- input$selected_combination  # Obtiene la selección del usuario
    
    # Separar las dos variables seleccionadas
    vars <- strsplit(selected_combination, " vs ")[[1]]
    var1 <- vars[1]
    var2 <- vars[2]
    
    # Generar el gráfico de correlación
    plot <- correlation_plot(data1, var1, var2, color = "#0B4462")
    
    return(plot)
  })
  
  # Llamada a la función de barrasFrec cuando el usuario selecciona una variable
  output$bar_plot <- renderPlotly({
    selected_var <- input$selected_column  # Obtener la variable seleccionada
    
    # Llamar a la función de barrasFrec y pasar la variable seleccionada
    barFrec_plot <- barrasFrec(data1, selected_var, "#1C7A92")
    
    return(barFrec_plot)
  })  
  

  
    output$scatter3d_plot <- renderPlotly({
      scatter3d_plot})
      
    output$density_plot <- renderPlotly({
      density_plot(data1, 'Chance.of.Admit')
    })
    
    #Normalizacion de datos
    
    output$final_data_normalized <- renderDT({
      datatable(data_normalized, options = list(pageLength = 10))
    })
    
    
    
    
    
    
    
    
    # Crear el modelo de red neuronal nn1
    nn1 <- neuralnet(Chance.of.Admit ~ ., 
                     data = train_normalized, 
                     hidden = 5, 
                     linear.output = TRUE)
    
    # Renderizar el gráfico de la red neuronal nn1
    output$nn_plot1 <- renderPlot({
      plotnet(nn1, 
              circle_cex = 5,     # Tamaño de los nodos
              pos_col = "#0B4462",  # Color de los pesos positivos
              neg_col = "red",    # Color de los pesos negativos
              alpha = 0.7)        # Transparencia de los enlaces
    })
    
    # Crear el modelo nn2
    nn2 <- nnet(Chance.of.Admit ~ ., 
                data = train_normalized, 
                size = 5, 
                linout = TRUE)
    
    # Renderizar el gráfico de la red neuronal nn2
    output$nn_plot2 <- renderPlot({
      plotnet(nn2)
    })
    
    # Calcular las predicciones y las métricas del modelo
    metrics_nn1 <- reactive({
      # Predicciones para nn1
      pred_nn1 <- predict(nn1, test_normalized)
      verdadero_nn1 <- test_normalized$Chance.of.Admit
      
      # Métricas
      rmse <- sqrt(mean((pred_nn1 - verdadero_nn1)^2))
      mae <- mean(abs(pred_nn1 - verdadero_nn1))
      sst <- sum((verdadero_nn1 - mean(verdadero_nn1))^2)
      sse <- sum((pred_nn1 - verdadero_nn1)^2)
      r_squared <- 1 - (sse / sst)
      pearson_corr <- cor(pred_nn1, verdadero_nn1)
      
      return(list(rmse = rmse, mae = mae, r_squared = r_squared, pearson_corr = pearson_corr))
    })
    
    metrics_nn2 <- reactive({
      # Predicciones para nn2
      pred_nn2 <- predict(nn2, test_normalized)
      verdadero_nn2 <- test_normalized$Chance.of.Admit
      
      # Métricas
      rmse_nn2 <- sqrt(mean((pred_nn2 - verdadero_nn2)^2))
      mae_nn2 <- mean(abs(pred_nn2 - verdadero_nn2))
      sst_nn2 <- sum((verdadero_nn2 - mean(verdadero_nn2))^2)
      sse_nn2 <- sum((pred_nn2 - verdadero_nn2)^2)
      r_squared_nn2 <- 1 - (sse_nn2 / sst_nn2)
      pearson_corr_nn2 <- cor(pred_nn2, verdadero_nn2)
      
      return(list(rmse = rmse_nn2, mae = mae_nn2, r_squared = r_squared_nn2, pearson_corr = pearson_corr_nn2))
    })
    
    # Mostrar las métricas de nn1
    output$model_performance <- renderDT({
      metrics1 <- metrics_nn1()
      data.frame(
        Metric = c("RMSE", "MAE", "R²", "Correlación de Pearson"),
        Value = c(round(metrics1$rmse, 4), round(metrics1$mae, 4), round(metrics1$r_squared, 4), round(metrics1$pearson_corr, 4))
      )
    })
    
    # Mostrar las métricas de nn2
    output$model_performance2 <- renderDT({
      metrics2 <- metrics_nn2()
      data.frame(
        Metric = c("RMSE", "MAE", "R²", "Correlación de Pearson"),
        Value = c(round(metrics2$rmse, 4), round(metrics2$mae, 4), round(metrics2$r_squared, 4), round(metrics2$pearson_corr, 4))
      )
    })
    
    # Histograma de residuos para nn1
    output$residual_histogram <- renderPlot({
      residuos_nn1 <- predict(nn1, test_normalized) - test_normalized$Chance.of.Admit
      hist(residuos_nn1, xlab = "Residuos", col = "#4DA6C4", breaks = 30)
    })
    
    # Graficar predicciones vs valores reales para nn1
    output$pred_vs_real <- renderPlot({
      plot(test_normalized$Chance.of.Admit, predict(nn1, test_normalized),
           xlab = "Valores Reales", ylab = "Predicciones",
           pch = 19, col = "#4DA6C4")
      abline(a = 0, b = 1, col = "#DAF7A6", lwd = 2)
    })
    
    # Graficar predicciones vs valores reales para nn2
    output$pred_vs_real_nn2 <- renderPlot({
      test_comparison_nn2 <- data.frame(
        prediccion = predict(nn2, test_normalized),
        verdadero = test_normalized$Chance.of.Admit
      )
      
      plot(test_comparison_nn2$prediccion, test_comparison_nn2$verdadero,
           main = "Predicciones vs. Valores Reales (Modelo nn2)",
           xlab = "Predicciones", ylab = "Valores Reales",
           pch = 19, col = "#4DA6C4")
      abline(a = 0, b = 1, col = "#DAF7A6", lwd = 2)
    })
    
    
    # Histograma de residuos para nn2
    residuos_nn2 <- predict(nn2, test_normalized) - test_normalized$Chance.of.Admit
    output$residual_histogram <- renderPlot({
      residuos_nn1 <- predict(nn2, test_normalized) - test_normalized$Chance.of.Admit
      hist(residuos_nn2, xlab = "Residuos", col = "#4DA6C4", breaks = 30)
    })
    observeEvent(input$predict, {
      tryCatch({
        # Crear los datos de entrada del usuario
        input_data <- data.frame(
          GRE.Score = input$GRE.Score,
          TOEFL.Score = input$TOEFL.Score,
          University.Rating = as.numeric(input$University.Rating),
          SOP = input$SOP,
          LOR = input$LOR,
          CGPA = input$CGPA,
          Research = as.numeric(input$Research)
        )
        
        print(input_data)
        
        # Realizar la predicción utilizando el modelo neural
        prediction <- predict(nn1, input_data)
        
        # Renderizar el valueBox con la predicción
        output$predicted_value <- renderValueBox({
          valueBox(
            value = round(prediction, 4),  # Predicción de la probabilidad de admisión
            subtitle = "Probabilidad de Admisión",
            icon = icon("graduation-cap"),
            color = "blue"
          )
        })
        
        # Renderizar el GIF según el valor de la predicción
        output$prediction_gif <- renderUI({
          if (prediction < 0.6) {
            tags$img(src = "yes.gif", height = "400px", width = "400px")  # Más grande
          } else {
            tags$img(src = "yes.gif", height = "400px", width = "400px")  # Más grande
          }
        })
        
      }, error = function(e) {
        # Capturar el error y mostrar un mensaje
        print(paste("Error en la predicción:", e$message))
      })
    })
    
}
    

    
    
    
    

    


    


# Run the application
shinyApp(ui = ui, server = server)