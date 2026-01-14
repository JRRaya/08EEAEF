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
    excel_sheets("data/tarea2.xlsx")[2:3], # Filtramos para leer solo hojas 2 y 3
    function(x) {
      df <- readxl::read_excel("data/tarea2.xlsx", sheet = x, col_names = FALSE)
      setNames(df, c("x", "y", "z"))
    }
  ), 
  excel_sheets("data/tarea2.xlsx")[2:3] # Filtramos también los nombres para el setNames
)