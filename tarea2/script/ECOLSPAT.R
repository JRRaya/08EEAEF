#######################
# escritorio de trabajo
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

# librerias
library(spatstat)
library(tidyverse)

# cargamos datos
data.quercus <- read.csv("datos_quercus.csv", sep = ",")
summary(data.quercus[,2:3]) # necesitamos los valores max y min para establecer la cuadricula de analisis

# filtramos los datos por especies
QUEILE <- data.quercus %>%
  filter(species == "Quercus ilex")
QUEFAG <- data.quercus %>%
  filter(species == "Quercus faginea")

# formateamos datos
# creamos objetos de clase "ppp" desde una matriz de datos 
# ppp para analisis univariente = Q. ilex
ppp1 <- with(QUEILE, ppp(x, y, 
                         c(0.040,19.950), c(0.040,19.840)) # ventana rectangular
)

# grafico por especies
plot(ppp1)

# ppp para analisis univariente = Q. faginea
ppp2 <- with(QUEFAG, ppp(x, y, 
                         c(0.040,19.950), c(0.040,19.840))
)

# grafico por especies
plot(ppp2)

# ppp de tipo multitype para analisis multivariante
ppp12 <- with(data.quercus, ppp(x, y, c(0.040,19.950), c(0.040,19.840), 
                                  marks = species) # variable categorica indicando las especies
)

# la variable especie debe estar formateado como factor
marks(ppp12) <- factor(marks(ppp12), 
                       levels = c("Quercus ilex","Quercus faginea")
)

# grafico para ambas especies
plot(ppp12)

###################### 
# analisis de patron espacial de puntos univariante usando la funcion "Lest"
# calculamos significancia usando simulaciones de montecarlo

# analisis para Q. ilex
L1_A <- envelope(ppp1, Lest, nsim = 999, ranL = 1, global = TRUE)
L1_A
# grafico
plot(L1_A, . -r ~ r, 
     main=NULL,
     #legend = F,
     las=1 
)

# analisis para Q. faginea
L1_B <- envelope(ppp2, Lest, nsim = 999, ranL = 1, global = TRUE)
L1_B
# grafico
plot(L1_B, . -r ~ r, 
     main=NULL,
     #legend = F,
     las=1 
)

########################
# Analisis multivariante 
# Lcross funcion para evaluar la relacion espacial entre Q. ilex y Q. faginea
# calculamos significancia usando simulaciones de montecarlo
L12 <- envelope(ppp12, Lcross, nsim = 999, 
               i = "Quercus ilex", j = "Quercus faginea",
               rank = 1, global = TRUE)
L12
# grafico
plot(L12, . -r ~ r, 
     main=NULL,
     #legend = F,
     las=1 
)



# Cargar la librería ggplot2
library(ggplot2)
library(png)
library(grid)

# Crear el gráfico
img <- readPNG("Quercus.png")
ggplot(data.quercus, aes(x = x, y = y, color = species)) +
  geom_point(size = 3, alpha = 0.6) +
  labs(x = "Coordenada X", y = "Coordenada Y", color = "Especie") +
  scale_color_manual(values = c("Quercus ilex" = "#006400", "Quercus faginea" = "brown")) +  # Define los colores de la leyenda
  theme_minimal() +  # Usa un tema mínimo sin fondo
  theme(panel.grid.major = element_blank(),  # Elimina la rejilla mayor
        panel.grid.minor = element_blank(),  # Elimina la rejilla menor
        panel.background = element_rect(fill = NA, color = "black"),  # Establece el fondo transparente y el borde en negro
        axis.text = element_text(size = 16),  # Ajusta el tamaño del texto de los ejes
        axis.title = element_text(size = 16),  # Ajusta el tamaño y el estilo del texto de los títulos de los ejes
        legend.title = element_text(color = "black", size = 14),  # Ajusta el tamaño y el estilo del texto del título de la leyenda
        legend.text = element_text(size = 12, face = "italic"))  
  
  # Obtener coordenadas para la esquina inferior derecha debajo de la leyenda
  coords <- ggplot_build(fig)$layout$layout$panel_params[[1]][c("x.range", "y.range")]
x <- coords[1, 2]
y <- coords[2, 1]

# Ajustar el gráfico para agregar la imagen
(fig +
  annotation_custom(rasterGrob(img, interpolate = TRUE),
                    xmin = x - unit(1, "in"), xmax = x, ymin = y, ymax = y + unit(1, "in")))

