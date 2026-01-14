# 1. Carga de paquetes y limpieza de entorno
# 1.1. Limpiamos la RAM
rm(list = ls(all.names = TRUE))
pacman::p_unload(pacman::p_loaded(), character.only = TRUE)
gc(full = TRUE)

# 1.2. Cargamos las librerías a emplear
pacman::p_load(dplyr, tidyr, readxl, epiphy, parallel, RcppParallel, data.table)


# 1.3. Establecemos el número de núcleos a emplear
setDTthreads(4)
getDTthreads()

# 2. Carga de archivos
lista_dfs_prueba <- setNames(
  lapply(
    excel_sheets("data/DATOS_EJEMPLO.xls"), # Nombres de las hojas del excel
    function(x) {
      df <- read_excel("data/DATOS_EJEMPLO.xls", sheet = x, col_names = FALSE) # Carga de cada uno de los df
      setNames(df, c("x", "y", "z")) # Asignamos nombres de las columnas
    }
  ), 
  excel_sheets("data/DATOS_EJEMPLO.xls")
)

# 3. Cálculo de SADIE
# 3.1. Dataframe 'hum jul04 autoc'
df1 <- sadie(data = lista_dfs[[1]], index = "all", nperm = 100, threads = 3)

df1_plot <- plot(df1)

summary(df1)

# 3.2. Dataframe 'luz autoc'
df2 <- sadie(data = lista_dfs[[2]], index = "all", nperm = 100, threads = 3)

df2_plot_isoclines <- plot(df2, isoclines = TRUE)

summary(df2)

# 3.3. Dataframe 'Sup Pistacea ab98'
df3 <- sadie(data = lista_dfs_prueba[[3]], index = "all", nperm = 100, threads = 3)

df3_plot_isoclines <- plot(df3, isoclines = TRUE)

summary(df3)

# 3.4. Dataframe 'Sup Pistacia se98'
df4 <- sadie(data = lista_dfs[[4]], index = "all", nperm = 100, threads = 3)

df4_plot_isoclines <- plot(df4, isoclines = TRUE)

summary(df4)

# 3.5. Dataframe 'hum sep04 autoc'
df5 <- sadie(data = lista_dfs[[5]], index = "all", nperm = 100, threads = 3)

df5_plot <- plot(df5)