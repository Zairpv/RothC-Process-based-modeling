# Configuración inicial
years2 <- seq(1/12, 1, by = 1/12)  # Simulación mensual durante 1 año
num_years <- 100  # Número de años a simular

library(readxl)
Litterfal_Folio38 <- read_excel("01. ACTIVE PROJECTS/A. PhD - Colpos/A. Thesis/Chapter 3/1. R analysis/3. Final model parameterization/Litterfal_Folio38.xlsx")
View(Litterfal_Folio38)

litterfall_values <- c(2.3383, 4.5223, 4.0218, 3.04245, 4.2984, 4.6976, 3.8229)  # Extiende a 100 valores
litterfall_values=Litterfal_Folio38$`C inputs`

# Lista para almacenar resultados
pool_sizes <- list()

df_results <- data.frame()

# Estado inicial
initial_pools <- as.numeric(poolSize3_F38[, 2:6])

for (year in 1:num_years) {
  
  # Definir el input de hojarasca para el año actual
  litterfall_input <- litterfall_values[year] 
  
  # Correr modelo RothC
  Y_model <- RothCModel(
    t = years2,
    ks = c(10, 0.3, 0.66, 0.02, 0),
    C0 = initial_pools,
    In = litterfall_input,
    clay = clay_F38_2014,
    DR = 0.25,
    xi = xi.frame_F38_ESC
  )
  
  # Obtener los stocks de carbono
  CT_year <- getC(Y_model) 
  total_C <- rowSums(CT_year)  # Suma de todas las fracciones  
  
  # Guardar resultados
  df_year <- data.frame(years2, as.data.frame(CT_year), total_C)
  names(df_year) <- c("Time", "DPM", "RPM", "BIO", "HUM", "IOM", "TotalSOC")
  
  pool_sizes[[year]] <- tail(df_year, 1)  # Último mes del año
  df_results <- rbind(df_results, cbind(Year = year, pool_sizes[[year]]))
  
  # Actualizar pools iniciales para el próximo año
  initial_pools <- as.numeric(pool_sizes[[year]][, 2:6])
}

