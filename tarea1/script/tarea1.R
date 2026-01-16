# 1. Carga de paquetes y limpieza de entorno
# 1.1. Limpiamos la RAM
rm(list = ls(all.names = TRUE))
pacman::p_unload(pacman::p_loaded(), character.only = TRUE)
gc(full = TRUE)

# 1.2. Cargamos las librerías a emplear
pacman::p_load(dplyr, tidyr, readxl, writexl, epiphy, parallel, RcppParallel, data.table)


# 1.3. Establecemos el número de núcleos a emplear
setDTthreads(4)
getDTthreads()

# 2. Carga de datos a emplear
lista_dfs <- setNames(
  lapply(
    excel_sheets("tarea1/data/SADIE.xlsx"), # Nombres de las hojas del excel
    function(x) {
      df <- read_excel("tarea1/data/SADIE.xlsx", sheet = x, col_names = FALSE) # Carga de cada uno de los df
      setNames(df, c("x", "y", "z")) # Asignamos nombres de las columnas
    }
  ), 
  excel_sheets("tarea1/data/SADIE.xlsx")
)

# 3. Calculo de SADIE
# 3.1. 1º base de datos
base1 <- lista_dfs[[1]] %>% 
  sadie(
    index = "all",
    nperm = 100,
    seed = 123,
    threads = 3,
    verbose = FALSE
  )

summary(base1)

# 3.2. 2º base de datos
base2 <- lista_dfs[[2]] %>% 
  sadie(
    index = "all",
    nperm = 100,
    seed = 123,
    threads = 3,
    verbose = FALSE
  )

summary(base2)

# 4. Contrucción de las tablas de datos
# 4.1. 1º base de datos
df1 <- lista_dfs[[1]] %>% 
  mutate(
    v = base1$info_clust$idx_P,
    c = base1$info_clust$idx_LMX,
    p_value = base2$info_clust$prob
  )

# 4.2. 2º base de datos
df2 <- lista_dfs[[2]] %>% 
  mutate(
    v = base2$info_clust$idx_P,
    c = base2$info_clust$idx_LMX,
    p_value = base2$info_clust$prob
  )

# 5. Representación gráfica y guardado de los índices 'v' y 'c'
# 5.1. Mapas del índice de agrupación 'v'
# 5.1.1. 1º base de datos
png("outputs/perry_base1.png", width = 2100, height = 2100, res = 300)
base1_plot <- plot(base1, index = "Perry")
dev.off()

png("outputs/perry_isoclines_base1.png", width = 2100, height = 2100, res = 300)
base1_plot_isoclines <- plot(base1, isoclines = TRUE, index = "Perry")
dev.off()

# 5.1.2. 2º base de datos
png("outputs/perry_base2.png", width = 2100, height = 2100, res = 300)
base2_plot <- plot(base2, index = "Perry")
dev.off()

png("outputs/perry_isoclines_base2.png", width = 2100, height = 2100, res = 300)
base2_plot_isoclines <- plot(base2, isoclines = TRUE, index = "Perry")
dev.off()

# 5.2. Mapas del índice de agrupación 'c'
# 5.2.1. 1º base de datos
png("outputs/lmx_base1.png", width = 2100, height = 2100, res = 300)
base1_plot <- plot(base1, index = "Li-Madden-Xu")
dev.off()

png("outputs/lmx_isoclines_base1.png", width = 2100, height = 2100, res = 300)
base1_plot_isoclines <- plot(base1, isoclines = TRUE, index = "Li-Madden-Xu")
dev.off()

# 5.2.2. 2º base de datos
png("outputs/lmx_base2.png", width = 2100, height = 2100, res = 300)
base2_plot <- plot(base2, index = "Li-Madden-Xu")
dev.off()

png("outputs/lmx_isoclines_base2.png", width = 2100, height = 2100, res = 300)
base2_plot_isoclines <- plot(base2, isoclines = TRUE, index = "Li-Madden-Xu")
dev.off()

# 6. Guardado de los resultados
# 6.1. 1º base de datos
write_xlsx(
  x = df1,
  path = "outputs/df1.xlsx",
  col_names = TRUE,
  format_headers = TRUE
)

# 6.2. 2º base de datos
write_xlsx(
  x = df2,
  path = "outputs/df2.xlsx",
  col_names = TRUE,
  format_headers = TRUE
)
