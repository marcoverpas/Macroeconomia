# Carica i pacchetti necessari ####
library(plotly)
library(viridis)

# Definisci i coefficienti della funzione Cobb-Douglas ####
alpha <- 0.5  # elasticity of K
beta <- 1-alpha  # elasticity of L
A <- 1 #total factor productivity

# Definisci intervalli di valori per K e L
K <- seq(0, 100, by = 1)
L <- seq(0, 100, by = 1)

# Crea tutte le combinazioni di K e L
grid <- expand.grid(K = K, L = L)

# Calcola Y per ciascuna combinazione di of K e L
grid$Y <- A * (grid$K^alpha) * (grid$L^beta)

# Trasforma i dati in forma matriciale (per consentire i grafici)
z_matrix <- matrix(grid$Y, nrow = length(K), ncol = length(L))

# Definisci gli isoquanti
isoquant_levels <- seq(10, 100, by = 10)

# Stampa la f.d.p. in 3D
plot_ly(x = K, y = L, z = z_matrix) %>%
  add_surface(
    colorscale = "Jet",
    contours = list(
    z = list(
      show = TRUE,
      start = min(isoquant_levels),
      end = max(isoquant_levels),
      size = 10,
      color = "white",  
      width = 2  
    )
  )) %>%
  layout(
    title = "a) La funzione di produzione Cobb-Douglas in 3D",
    scene = list(
      xaxis = list(title = "Capitale (K)"),
      yaxis = list(title = "Lavoro (L)"),
      zaxis = list(title = "Prodotto (Y)"),
      camera = list(
        eye = list(x = 1, y = -2, z = 1)  
      )
    )
  )
