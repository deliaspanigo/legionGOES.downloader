# 1. Cargar librerías
library(readxl)
library(usethis)
library(dplyr)

# 2. Leer y asignar al nombre FINAL (tree_data)
# Importante: El nombre de la variable aquí será el nombre del dataset en el paquete
tree_data <- readxl::read_excel("data-raw/arbol_estadistico.xlsx", sheet = 1) %>%
  rename_with(tolower)

# 3. Guardar (ahora sí, sin comillas y sin el argumento 'name')
usethis::use_data(tree_data, overwrite = TRUE)
