# Parámetros del problema
N_defectuosos <- 3  # m
N_buenos <- 5       # n
n_comprados <- 2    # k

# 1. Función de cuantía f_X (P(X = x))
x_vals <- 0:2
f_X <- dhyper(x_vals, m = N_defectuosos, n = N_buenos, k = n_comprados)

# Mostrar la función de cuantía en una tabla
tabla_cuantia <- data.frame(
  x = x_vals,
  Probabilidad = f_X,
  Prob_Fraccion = c("10/28", "15/28", "3/28")
)
print("1. Función de cuantía f_X:")
print(tabla_cuantia)

# 2. Probabilidad de que todos los vasos comprados sean defectuosos (P(X = 2))
p_todos_defectuosos <- dhyper(2, m = N_defectuosos, n = N_buenos, k = n_comprados)
cat("\n2. Probabilidad de que ambos vasos sean defectuosos P(X = 2):", p_todos_defectuosos, "\n")

# 3. Función de distribución F_X (P(X <= x))
F_X <- phyper(x_vals, m = N_defectuosos, n = N_buenos, k = n_comprados)
tabla_distribucion <- data.frame(
  x = x_vals,
  F_X = F_X
)
print("\n3. Función de distribución F_X:")
print(tabla_distribucion)

# Gráficos
par(mfrow = c(1, 2)) # Dividir la ventana gráfica en 2

# Gráfico de la función de cuantía
plot(x_vals, f_X, type = "h", lwd = 3, col = "blue",
     main = "Función de Cuantía f_X",
     xlab = "x (Vasos defectuosos)", ylab = "P(X = x)",
     ylim = c(0, 0.6))
points(x_vals, f_X, pch = 19, col = "blue", cex = 1.5)

# Gráfico de la función de distribución escalonada
# Usamos stepfun para graficar correctamente la función de distribución acumulada
F_step <- stepfun(x_vals, c(0, F_X))
plot(F_step, verticals = FALSE, col.points = "red", lwd = 2,
     main = "Función de Distribución F_X",
     xlab = "x", ylab = "F_X(x)")
