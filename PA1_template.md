## Carga y exploración de datos

``` r
# Cambia la ruta si es necesario
datos <- read.csv("C:/Users/lenovo/Documents/COURSERA/datasciencecoursera/RepData_PeerAssessment1/activity.csv", stringsAsFactors = FALSE)

# Convertimos fecha a formato Date
datos$fecha <- as.Date(datos$date, format = "%Y-%m-%d")

# Vemos un resumen inicial
head(datos)
```

    ##   steps       date interval      fecha
    ## 1    NA 2012-10-01        0 2012-10-01
    ## 2    NA 2012-10-01        5 2012-10-01
    ## 3    NA 2012-10-01       10 2012-10-01
    ## 4    NA 2012-10-01       15 2012-10-01
    ## 5    NA 2012-10-01       20 2012-10-01
    ## 6    NA 2012-10-01       25 2012-10-01

## Total de pasos por día y su distribución

``` r
pasos_por_dia <- datos %>%
  group_by(fecha) %>%
  summarise(total_pasos = sum(steps, na.rm = TRUE))

# Histograma
ggplot(pasos_por_dia, aes(x = total_pasos)) +
  geom_histogram(binwidth = 1000, fill = "skyblue", color = "black") +
  labs(title = "Histograma del número total de pasos dados por día", x = "Total pasos por día", y = "Frecuencia")
```

![](PA1_template_files/figure-markdown_github/unnamed-chunk-2-1.png)

``` r
# Media y mediana
media_pasos <- mean(pasos_por_dia$total_pasos)
mediana_pasos <- median(pasos_por_dia$total_pasos)

media_pasos
```

    ## [1] 9354.23

``` r
mediana_pasos
```

    ## [1] 10395

## Patrón medio por intervalo de 5 minutos

``` r
promedio_por_intervalo <- datos %>%
  group_by(interval) %>%
  summarise(media_pasos = mean(steps, na.rm = TRUE))

# Gráfico de series temporales
ggplot(promedio_por_intervalo, aes(x = interval, y = media_pasos)) +
  geom_line(color = "blue") +
  labs(title = "Patrón medio de pasos por intervalo de 5 minutos", x = "Intervalo de 5 minutos", y = "Número medio de pasos")
```

![](PA1_template_files/figure-markdown_github/unnamed-chunk-3-1.png)

``` r
# Intervalo con máximo promedio de pasos
max_intervalo <- promedio_por_intervalo$interval[which.max(promedio_por_intervalo$media_pasos)]
max_intervalo
```

    ## [1] 835

## Manejo de valores faltantes (NA)

``` r
# Número de valores NA
num_na <- sum(is.na(datos$steps))
num_na
```

    ## [1] 2304

``` r
# Función para imputar valores NA con la media del intervalo
imputar_pasos <- function(steps, interval) {
  if (is.na(steps)) {
    return(promedio_por_intervalo$media_pasos[promedio_por_intervalo$interval == interval])
  } else {
    return(steps)
  }
}

datos_imputados <- datos
datos_imputados$steps <- mapply(imputar_pasos, datos_imputados$steps, datos_imputados$interval)
```

## Análisis con datos imputados

``` r
# Recalcular total pasos por día con datos imputados
pasos_dia_imputados <- datos_imputados %>%
  group_by(fecha) %>%
  summarise(total_pasos = sum(steps))

# Histograma con datos imputados
ggplot(pasos_dia_imputados, aes(x = total_pasos)) +
  geom_histogram(binwidth = 1000, fill = "lightgreen", color = "black") +
  labs(title = "Histograma total de pasos por día (datos imputados)", x = "Total pasos por día", y = "Frecuencia")
```

![](PA1_template_files/figure-markdown_github/unnamed-chunk-5-1.png)

``` r
# Media y mediana con datos imputados
media_imputados <- mean(pasos_dia_imputados$total_pasos)
mediana_imputados <- median(pasos_dia_imputados$total_pasos)

num_na
```

    ## [1] 2304

``` r
media_imputados
```

    ## [1] 10766.19

``` r
mediana_imputados
```

    ## [1] 10766.19

## Comparación de actividad entre días laborables y fines de semana

``` r
# Configurar localización para obtener días en español
Sys.setlocale("LC_TIME", "es_ES.UTF-8") # Ajusta según sistema operativo, por ejemplo "Spanish_Spain" en Windows
```

    ## [1] "es_ES.UTF-8"

``` r
datos_imputados$diasemana <- weekdays(datos_imputados$fecha)

datos_imputados$tipo_dia <- ifelse(datos_imputados$diasemana %in% c("sábado", "domingo"), "Fin de semana", "Día laborable")
datos_imputados$tipo_dia <- factor(datos_imputados$tipo_dia, levels = c("Día laborable", "Fin de semana"))

promedio_intervalo_tipo <- datos_imputados %>%
  group_by(interval, tipo_dia) %>%
  summarise(media_pasos = mean(steps))

ggplot(promedio_intervalo_tipo, aes(x = interval, y = media_pasos, color = tipo_dia)) +
  geom_line() +
  facet_wrap(~ tipo_dia, ncol = 1) +
  labs(title = "Patrón de actividad: días laborables vs fines de semana", x = "Intervalo de 5 minutos", y = "Número medio de pasos") +
  theme(legend.position = "none")
```

![](PA1_template_files/figure-markdown_github/unnamed-chunk-6-1.png)
