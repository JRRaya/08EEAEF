# 1. Carga de paquetes y limpieza de entorno
# 1.1. Limpiamos la RAM
rm(list = ls(all.names = TRUE))
pacman::p_unload(pacman::p_loaded(), character.only = TRUE)
gc(full = TRUE)

# 1.2. Cargamos las librerías a emplear
pacman::p_load(sf, terra, tidyverse, data.table, readxl, spatstat)

# 2. Carga de datos
lista_dfs <- setNames(
  lapply(
<<<<<<< HEAD
    excel_sheets("tarea2/data/tarea2.xlsx")[2:3], # Filtramos para leer solo hojas 2 y 3
    function(x) {
      df <- readxl::read_excel("tarea2/data/tarea2.xlsx", sheet = x, col_names = FALSE)
      setNames(df, c("x", "y"))
    }
  ), 
  excel_sheets("tarea2/data/tarea2.xlsx")[2:3] # Filtramos también los nombres para el setNames
)

# 3. Definir la ventana de observación
# 3.1. Rango de observaciones en el eje X
xrange <- range(c(lista_dfs$Q_faginea$x, lista_dfs$Q_ilex$x))

# 3.2. Rengo de observaciones en el eje Y
yrange <- range(c(lista_dfs$Q_faginea$y, lista_dfs$Q_ilex$y))

# 3.3. Contrucción de la ventana (subconjunto de la parcela muestreada) a partir de los rangos de los ejes
ventana <- owin(xrange = xrange, yrange = yrange)

# 4. Crear los objetos de patrones de puntos ("point pattern")
# 4.1. Patrón de puntos de Q. ilex
ppp_ilex <- ppp(
  x = lista_dfs$Q_ilex$x,
  y = lista_dfs$Q_ilex$y,
  window = ventana
)

plot(ppp_ilex)

# 4.2. Patrón de puntos de Q. faginea
ppp_faginea <- ppp(
  x = lista_dfs$Q_faginea$x,
  y = lista_dfs$Q_faginea$y,
  window = ventana
)

plot(ppp_faginea)

# 4.3. Patrón de puntos combinado ("marcado")
ppp_marcado <- ppp(
  x = c(lista_dfs$Q_faginea$x, lista_dfs$Q_ilex$x),
  y = c(lista_dfs$Q_faginea$y, lista_dfs$Q_ilex$y),
  window = ventana,
  marks = factor(
    c(
      rep("Plantulas", length(lista_dfs$Q_faginea$x)),
      rep("Rebrotes", length(lista_dfs$Q_ilex$x))
    )
  )
)

plot(ppp_marcado)

# 5. Análisis univariante
# 5.1. Cálculo de la K de Ripley para cada grupo de puntos
# 5.1.1. K de Riley de Q. ilex
# 5.1.1.1. Cálculo de la K con todas las correcciones 
k_ilex <- Kest(
  X = ppp_ilex, 
  correction = "all"
)

# 5.1.1.2. Comprobación visual con la corrección isotrópica
k_ilex_iso <- plot(k_ilex$iso)

# 5.1.2. K de Ripley de Q. faginea
# 5.1.2.1. Cálculo de la K con todas las correcciones 
k_faginea <- Kest(
  X = ppp_faginea, 
  correction = "all"
)

# 5.1.2.2. Comprobación visual con la corrección isotrópica
k_faginea_iso <- plot(k_faginea$iso)

# 5.2. Cálculo de la "función de correlación por pares" ('g')
# 5.2.1. Función de correlación por pares de Q. ilex
# 5.2.1.1. Cálculo de la 'g' con todas las correcciones
g_ilex <- pcf(
  X = ppp_ilex, 
  correction = "all"
)

# 5.2.1.2. Comprobación visual con la corrección isotrópica
g_ilex_iso <- plot(g_ilex$iso)

# 5.2.2. Función de correlación por pares de Q. faginea
# 5.2.2.1. Cálculo de la 'g' con todas las correcciones
g_faginea <- pcf(
  X = ppp_faginea,
  correction = "all"
)

# 5.2.2.2. Comprobación visual con la corrección isotrópica
g_faginea_iso <- plot(g_faginea$iso)

# 6. Evaluación de la significancia mediante simulaciones de Monte Carlo
>>>>>>>>> Temporary merge branch 2
