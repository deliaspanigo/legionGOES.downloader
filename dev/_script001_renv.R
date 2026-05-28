# 1. Limpieza
if ("renv" %in% loadedNamespaces()) unloadNamespace("renv")

# 2. Instalación en la librería base del R-Portable
# Asegúrate de ejecutar esto desde el R que está en .\App\R-Portable
install.packages("renv", lib = .Library)

# 3. Configuración para PORTABILIDAD (Crucial)
Sys.setenv(RENV_CONFIG_SANDBOX_ENABLED = FALSE)
Sys.setenv(RENV_CONFIG_CACHE_ENABLED = FALSE)



# 2. Reconfigurar renv para que use la carpeta local
renv::settings$snapshot.type("all")
renv::settings$use.cache(FALSE)

# 3. Forzar la reparación/reinstalación local
renv::restore()

# 4. Inicialización
# Entramos a la carpeta del proyecto si no estamos en ella
if (basename(getwd()) != "Rscience2027") setwd("Rscience2027")

renv::init(bare = TRUE, restart = FALSE)

# 5. Configuración de Snapshot
renv::settings$snapshot.type("explicit")

# 6. Instalación de dependencias
# Esto instalará lo que declare tu archivo DESCRIPTION
renv::install()
renv::snapshot()

renv::restore()

# 7. Snapshot Final
renv::snapshot(prompt = FALSE)

# VERIFICACIÓN FINAL
cat("\n--- VERIFICACIÓN DE AISLAMIENTO ---\n")
print(.libPaths()) # Debe mostrar la ruta interna de tu proyecto


renv::rebuild()

# 8. Prueba manual antes de cerrar
# shiny::runApp(launch.browser = TRUE)
