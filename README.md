# trabajo_personal
## Trabajo Personal

### Autor: Ines Garcia

### Día: 24/1/2022

---

```{r}
library(nycflights13)
library(tidyverse)
vuelos <- nycflights13::flights
```


#### 1.Encuentra todos los vuelos que llegaron más de una hora tarde de lo previsto.
```{r}
a <-filter(vuelos, arr_delay >60)
dim(a)
```



Los vuelos que llegaron más de una tarde de lo previsto fueron 27789

#### 2.Encuentra todos los vuelos que volaron hacia San Francisco (aeropuertos SFO y OAK) 
```{r}
b <- filter(vuelos, dest == "SFO" | dest== "OAK")
dim(b)
```


Los vuelos que volaron hacia los aeropuertos de San Francisco fueron 13643.

#### 3. Encuentra todos los vuelos operados por United American (UA) o por American Airlines (AA) 

```{r}
c <-filter(vuelos, carrier == "UA" | carrier == "AA")
dim(c)
```



Los vuelos operados por United American o por American Airlines fueron 91394.

#### 4.Encuentra todos los vuelos que salieron los meses de primavera (Abril, Mayo y Junio) 
```{r}
d <- filter(vuelos, month == 4 | month == 5 |month == 6)
dim(d)
```



Los vuelos que salieron los meses de Abril, Mayo y Junio fueron 85369.

#### 5. Encuentra todos los vuelos que llegaron más de una hora tarde pero salieron con menos de una hora de retraso. 

```{r}
e <-filter(vuelos, arr_delay > 60 & dep_delay <60)
dim(e)
```



Los vuelos que salieron más de una hora tarde pero salieron con menos de una hora de retraso fueron 4956.


#### 6.Encuentra todos los vuelos que salieron con más de una hora de retraso pero consiguieron llegar con menos de 30 minutos de retraso (el avión aceleró en el aire) 
```{r}
f <-filter(vuelos, arr_delay <30 & dep_delay >60)
dim(f)
```



Los vuelos que salieron con más de una hora de retraso pero consiguieron llegar con menos de 30 min de retraso fueron 181.

#### 7.  Encuentra todos los vuelos que salen entre medianoche y las 7 de la mañana (vuelos nocturnos). 
```{r}
g <-filter(vuelos, dep_time <=700)
dim(g)
```



Los vuelos que salieron entre medianoche y las 7 de la mañana fueron 31958.

#### 8. ¿Cuántos vuelos tienen un valor desconocido de dep_time? 
```{r}
h <- which(is.na(vuelos$dep_time))
length(h)
```



El número de vuelos que tienen un valor desconocido es de 8255.

#### 9.  ¿Qué variables del dataset contienen valores desconocidos? 
```{r}
apply(X = is.na(vuelos), MARGIN = 2, FUN = sum)
```




Las variables que tienen valores NA son: tailnum, dep_time, de_delay, arr_time, arr_delay y air_time.

#### 10. Ordena los vuelos de flights para encontrar los vuelos más retrasados en la salida. ¿Qué vuelos fueron los que salieron los primeros antes de lo previsto? 
```{r}
arrange(vuelos, desc(dep_delay))
arrange(vuelos, dep_delay)
```



El vuelo que más temprano salío antes de lo previsto es el que tiene un tailnum de N592JB y salío 43 minutos antes.
El segundo vuelo que más temprano salío es el que tiene un tailnum de 	N612DL y salío 33 minutos antes.


#### 11. Ordena los vuelos de flights para encontrar los vuelos más rápidos. Usa el concepto de rapidez que consideres. 
```{r}
vuelos$velocidad <- vuelos$distance/vuelos$air_time
arrange(vuelos, desc(velocidad))
```



#### 12. ¿Qué vuelos tienen los trayectos más largos? 
```{r}
arrange(vuelos, desc(distance))
```



El trayecto más largo es el que sale del aeropuerto JFK y aterriza en el aeropuerto HNL, cuya distancia total es de 4983 millas.


#### 13. ¿Qué vuelos tienen los trayectos más cortos? 
```{r}
arrange(vuelos, distance)
```



El trayecto más corto es de 80 millas, sale del aeropuerto EWR y aterriza en PHL, el primer vuelo cuyo trayecto es de 17 millas corresponde al de un vuelo cancelado, es por este motivo que no lo tengo en cuenta como trayecto más corto.


#### 14. El dataset de vuelos tiene dos variables, dep_time y sched_dep_time muy útiles pero difíciles de usar por cómo vienen dadas al no ser variables continuas. Fíjate que cuando pone 559, se refiere a que el vuelo salió a las 5:59... Convierte este dato en otro más útil que represente el número de minutos que pasan desde media noche.
```{r}
vuelos_min <-mutate(vuelos, salida_programada_min = (sched_dep_time %/% 100 * 60 + sched_dep_time %% 100) %% 1440, salida_min = (dep_time %/% 100 * 60 + dep_time %% 100) %% 1440)
```



#### 15. Compara los valores de dep_time, sched_dep_time y dep_delay. ¿Cómo deberían relacionarse estos tres números? Compruébalo y haz las correcciones numéricas que necesitas. 
```{r}
vuelos_min$comprobacion1 <- vuelos_min$sched_dep_time + vuelos_min$dep_delay
vuelos_min$comprobacion2 <- vuelos_min$salida_programada_min + vuelos_min$dep_delay
```
La relación existente entre estas variable debería ser que la suma de sched_dep_time y dep_delay es igual al valor de dep_time. El problema es que las unidades de estas variables no coinciden, es por este motivo que utilizariamos las nuevas variables en min creadas en el ejercicio 14.
Cómo podemos ver en las nuevas columnas creadas, la de comprobacion1 sería errónea puesto que los valores no coinciden con salida_min, ya que las unidades no son adecuadas. Sin embargo, los resultados obtenidos en la columna comprobacion2 si coincide con la de salida_min.

#### 16. Investiga si existe algún patrón del número de vuelos que se cancelan cada día.
```{r}
vueloscancelados <-  vuelos %>%
  mutate(cancelado = (is.na(arr_delay) | is.na(arr_delay))) %>%
  group_by(year, month, day) %>%
  summarise(num_cancelado = sum(cancelado), num_vuelo = n(),)
library(ggplot2)
ggplot(vueloscancelados) +
  geom_line(mapping = aes(x = num_vuelo, y = num_cancelado, col=num_vuelo, )) 
```




Cómo podemos observar en el plot el número de vuelos cancelados es directamente proporcional al número de vuelos que salgan cada día, es decir, a mayor número de vuelos, mayor número de vuelos cancelados.


#### 17. Investiga si la proporción de vuelos cancelados está relacionada con el retraso promedio por día en los vuelos. 
```{r}
proporcion_retraso <- vuelos %>%
  mutate(cancelados = (is.na(tailnum))) %>%
  group_by(year, month, day) %>%
  summarise(proporcion_cancelados = mean(cancelados),media_dep_delay = mean(dep_delay, na.rm = TRUE),med_arr_delay = mean(arr_delay, na.rm = TRUE)) %>% ungroup()
ggplot(proporcion_retraso) +
  geom_line(aes(x = media_dep_delay, y = proporcion_cancelados, col=proporcion_cancelados))
```




Observando la gráfica obtenida no podemos llegar a una conclusión clara de que exista o no una relación entre ambas variables.


#### 18. Investiga si la proporción de vuelos cancelados está relacionada con el retraso promedio por aeropuerto en los vuelos. 
```{r}
LGA <- filter(vuelos, origin=="LGA")
Prop_retraso_cancel_LGA <-  LGA %>%
  mutate(cancelados = (is.na(tailnum))) %>%
  group_by(origin, dest) %>%
  summarise(prop_cancelados = mean(cancelados),med_dep_delay = mean(dep_delay, na.rm = TRUE),med_arr_delay = mean(arr_delay, na.rm = TRUE)) %>% ungroup()
ggplot(Prop_retraso_cancel_LGA) +
  geom_line(aes(x = med_dep_delay, y = prop_cancelados, col= prop_cancelados))
```
```{r}
EWR <- filter(vuelos, origin=="EWR")
Prop_retraso_cancel_EWR <- EWR %>%
  mutate(cancelados = (is.na(tailnum))) %>%
  group_by(origin, dest) %>%
  summarise(prop_cancelados = mean(cancelados),med_dep_delay = mean(dep_delay, na.rm = TRUE),med_arr_delay = mean(arr_delay, na.rm = TRUE)) %>% ungroup()
ggplot(Prop_retraso_cancel_EWR) +
  geom_line(aes(x = med_dep_delay, y = prop_cancelados, col= prop_cancelados))
```
```{r}
JFK <- filter(vuelos, origin=="JFK")
Prop_retraso_cancel_JFK <- JFK %>%
  mutate(cancelados = (is.na(tailnum))) %>%
  group_by(origin, dest) %>%
  summarise(prop_cancelados = mean(cancelados),med_dep_delay = mean(dep_delay, na.rm = TRUE),med_arr_delay = mean(arr_delay, na.rm = TRUE)) %>% ungroup()
ggplot(Prop_retraso_cancel_JFK) +
  geom_line(aes(x = med_dep_delay, y = prop_cancelados, col= prop_cancelados))
```




En el caso del aeropuerto JFK existe una relación entre el número de vuelos cancelados y el retraso en la salida de los vuelos. Sin embargo, en el caso del aeropuerto EWR podemos observar que no existe una relación entre las variables. Por último, el caso del aeropuerto LGA es algo más confuso puesto que no podemos determinar si existe o no una relación entre ambos parámetros.

#### 19. ¿Qué compañía aérea sufre los peores retrasos? 
```{r}
vuelos %>%
   group_by(carrier) %>%
   summarise(dep_delay = mean(dep_delay, na.rm = TRUE)) %>%
   arrange(desc(dep_delay))
vuelos %>%
   group_by(carrier) %>%
   summarise(arr_delay = mean(arr_delay, na.rm = TRUE)) %>%
   arrange(desc(arr_delay))
```
La compañia que sufre los peores retrasos es la F9.


#### 20. Queremos saber qué hora del día nos conviene volar si queremos evitar los retrasos en la salida. 
```{r}
vuelos %>%
  group_by(hour) %>%
  summarise(dep_delay = mean(dep_delay, na.rm = TRUE)) %>%
  arrange(dep_delay)
```
La mejor hora para volar sería las 5 de la mañana.


#### 21. Queremos saber qué día de la semana nos conviene volar si queremos evitar los retrasos en la salida. 
```{r}
library(lubridate)
make_dtime <- function(year, month, day, time) {
  make_datetime(year, month, day, time %/% 100, time %% 100)
}
vuelos_dt <- vuelos %>% 
  filter(!is.na(dep_time), !is.na(arr_time)) %>% 
  mutate(
    dep_time = make_dtime(year, month, day, dep_time),
    arr_time = make_dtime(year, month, day, arr_time),
    sched_dep_time = make_dtime(year, month, day, sched_dep_time),
    sched_arr_time = make_dtime(year, month, day, sched_arr_time)) %>% 
  select(origin, dest, ends_with("delay"), ends_with("time"))
vuelos_dt %>%
  mutate(dow = wday(sched_dep_time)) %>%
  group_by(dow) %>%
  summarise(
    dep_delay = mean(dep_delay),
    arr_delay = mean(arr_delay, na.rm = TRUE)
  ) %>%
  print(n = Inf)
vuelos_dt %>%
   mutate(wday = wday(dep_time, label = TRUE)) %>% 
   group_by(wday) %>% 
   summarize(ave_dep_delay = mean(dep_delay, na.rm = TRUE)) %>% 
   ggplot(aes(x = wday, y = ave_dep_delay)) + 
   geom_bar(stat = "identity", col = "pink")
```




El día más conveniente para viajar es el sábado como podemos ver en el gráfico de barras, ya que es el día en el que menos retrasos se producen.


#### 22.  Para cada destino, calcula el total de minutos de retraso acumulado. 
```{r}
retraso_totvuelos <- vuelos %>%
  filter(arr_delay > 0) %>%
  group_by(dest) %>%
  summarise(arr_delay = sum(arr_delay))
retraso_totvuelos
```



#### 23.  Para cada uno de ellos, calcula la proporción del total de retraso para dicho destino. 
```{r}
vuelos %>%
   filter(arr_delay > 0) %>%
   group_by(dest, origin, carrier, flight) %>%
   summarise(arr_delay = sum(arr_delay)) %>%
   group_by(dest) %>%
   mutate(
     arr_delay_prop = arr_delay / sum(arr_delay)
   ) %>%
   arrange(dest, desc(arr_delay_prop)) %>%
   select(carrier, flight, origin, dest, arr_delay_prop) 
```



#### 24. Es hora de aplicar todo lo que hemos aprendido para visualizar mejor los tiempos de salida para vuelos cancelados vs los no cancelados. Recuerda bien qué tipo de dato tenemos en cada caso. ¿Qué deduces acerca de los retrasos según la hora del día a la que está programada el vuelo de salida? 
```{r}
vuelos_dt %>%
  mutate(sched_dep_hour = hour(sched_dep_time)) %>%
  group_by(sched_dep_hour) %>%
  summarise(dep_delay = mean(dep_delay)) %>%
  ggplot(aes(y = dep_delay, x = sched_dep_hour)) +
  geom_point() +
  geom_smooth()
```



Cómo podemos observar en el gráfico el retraso en la salida de los vuelos aumenta a medida que va pasando el día alcanzando su punto máximo aproximadamente entre las 6 y 8 de la tarde. Sin embargo, podemos apreciar como durante la madrugada los retrasos son menores.


#### 25. Subir la carpeta a github y facilitar la url. 
El url de mi carpeta de github es: https://github.com/igarciamart/trabajo_personal.git



#### 26. Al finalizar el documento agrega el comando sessionInfo() 
```{r}
sessionInfo()
```
