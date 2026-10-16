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

# #Ejercicio 2

# Probabilidades de ventas diarias para un día (Y)
p_diaria <- c(0.7, 0.2, 0.1)  # Para 0, 1 y 2 ventas respectivamente
valores_diarios <- 0:2

# 1. Hallar la función de cuantía f_X para dos días independientes (convolución)
f_X <- convolve(p_diaria, rev(p_diaria), type = "full")
x_vals <- 0:4

# Mostrar la función de cuantía en una tabla
tabla_cuantia <- data.frame(
  x = x_vals,
  Probabilidad = f_X
)
print("1. Función de cuantía f_X (Ventas en dos días):")
print(tabla_cuantia)

# 2. Calcular la probabilidad de que se formalice por lo menos una venta P(X >= 1)
# Esto es equivalente a 1 - P(X = 0)
p_al_menos_una <- 1 - f_X[1]
cat("\n2. Probabilidad de al menos una venta P(X >= 1):", p_al_menos_una, "\n")

# 3. Función de distribución F_X (P(X <= x))
F_X <- cumsum(f_X)
tabla_distribucion <- data.frame(
  x = x_vals,
  F_X = F_X
)
print("\n3. Función de distribución F_X:")
print(tabla_distribucion)

# Gráficos
par(mfrow = c(1, 2)) # Dividir la ventana gráfica en 2

# Gráfico de la función de cuantía
plot(x_vals, f_X, type = "h", lwd = 3, col = "darkgreen",
     main = "Función de Cuantía f_X",
     xlab = "x (Ventas en 2 días)", ylab = "P(X = x)",
     ylim = c(0, 0.6))
points(x_vals, f_X, pch = 19, col = "darkgreen", cex = 1.5)

# Gráfico de la función de distribución acumulada
F_step <- stepfun(x_vals, c(0, F_X))
plot(F_step, verticals = FALSE, col.points = "darkorange", lwd = 2,
     main = "Función de Distribución F_X",
     xlab = "x", ylab = "F_X(x)")
