## Dependencias pertinentes
z_new.packages <- c("foreign", "dplyr")[!(c("foreign", "dplyr") %in% installed.packages()[,"Package"])]

if (length(z_new.packages)) {
  install.packages(z_new.packages)
}

require("foreign")
require("dplyr")

library("foreign")
library("dplyr")

## Data del estudio
data <- read.spss("./db/data.sav",
                  use.value.labels = TRUE,
                  to.data.frame = TRUE)

## Función auxiliar truncar
trunc <- function(x, ..., prec = 3) {
  base::trunc(x*10^prec, ...)/10^prec
}

## Media, Desviación Estándar y Varianza
# Total de Medallas
media_total_med = trunc(mean(data$Total_medallas, na.rm = TRUE))
desv_est_total_med = trunc(sd(data$Total_medallas, na.rm = TRUE))
varianza_total_med = trunc(var(data$Total_medallas, na.rm = TRUE))

# Medallas de Oro
media_med_oro = trunc(mean(data$Med_oro, na.rm = TRUE))
desv_est_med_oro = trunc(sd(data$Med_oro, na.rm = TRUE))
varianza_med_oro = trunc(var(data$Med_oro, na.rm = TRUE))

# PIB
media_PIB = trunc(mean(data$PIB, na.rm = TRUE))
desv_est_PIB = trunc(sd(data$PIB, na.rm = TRUE))
varianza_PIB = trunc(var(data$PIB, na.rm = TRUE))

## Histogramas
# Histograma del Total de Medallas
par(mfrow = c(2, 2))
hist(data$Total_medallas,
     main = "Histograma - Total de Medallas",
     xlab = "Total Medallas",
     ylab = "Frecuencia",
     col = "brown")

# Histograma de las Medallas de Oro
hist(data$Med_oro,
     main = "Histograma - Medallas de Oro",
     xlab = "Medallas de Oro",
     ylab = "Frecuencia",
     col = "yellow")

# Histograma del PIB
hist(data$PIB,
     main = "Histograma - PIB",
     xlab = "PIB",
     ylab = "Frecuencia",
     col = "blue")

## Tabla de Frecuencias
data %>%
  group_by(País, Total_medallas, Med_oro) %>%
  summarise(count=n()) %>%
  arrange(desc(Total_medallas)) %>%
  print(n = 200)

## Gráfico de puntos
plot(data$PIB,
     data$Total_medallas,
     main = "Total Medallas vs PIB",
     xlab = "PIB",
     ylab = "Total Medallas")
