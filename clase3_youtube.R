library(tidyverse)
library(pacman)
Contagiados <- read_csv("https://raw.githubusercontent.com/MinCiencia/Datos-COVID19/master/output/producto19/CasosActivosPorComuna.csv")

Contagiados_organizado <- Contagiados %>%
        pivot_longer(cols = starts_with("202"),
                     names_to = "Fecha",
                     values_to = "Infectados"
                     ) %>%
        mutate(Fecha = lubridate::ymd(Fecha), prevalencia = (100000*Infectados/Poblacion)) %>%
        dplyr::filter(Fecha == max(Fecha),!is.na(`Codigo comuna`)) %>%
        arrange(desc(prevalencia))
hist(Contagiados_organizado$prevalencia)

Episodes <- read_csv("https://raw.githubusercontent.com/derek-corcoran-barrios/The_office/master/The_Office_Episodes_per_Character.csv")

words <- read_csv("https://raw.githubusercontent.com/derek-corcoran-barrios/The_office/master/The_office_Words.csv")

stop_words <- read_csv("https://raw.githubusercontent.com/derek-corcoran-barrios/The_office/master/stop_words.csv")

pers_por_temp <- words %>%
        group_by(speaker, season)  %>%
        summarise(n=n())  %>%
        ungroup()  %>%
        group_by(season)  %>%
        slice_max(order_by = n, n=10)  %>%
        ungroup()  %>%
        arrange(season, desc(n)) 

Eps_por_season <-  words  %>%
        dplyr::select(season, episode)  %>%
        distinct() %>%
        group_by(season)  %>%
        summarise(Eps= n())
palabras_por_temp <- pers_por_temp  %>%
        left_join(words) %>%
        group_by(speaker, season)  %>%
        summarise(n= n())  %>%
        ungroup()  %>%
        pivot_wider(names_from = speaker, values_from = n, values_fill = 0)  %>%
        pivot_longer(cols = Andy:Toby, names_to = "speaker", values_to = "words")  %>%
        arrange(season)  %>%
        left_join(Eps_por_season)  %>%
        group_by(speaker)  %>%
        ## La clave para hacer un delta, es crear otra columna con el la funcion Lag
        ## y tener en cuenta que ya tengo organizado cómo realmente lo necesito
        mutate(words = words/Eps, Lag = lag(words), delta = words-Lag)  %>%
        dplyr::filter(!is.na(delta))

G <- ggplot(palabras_por_temp, aes(x = season, y = delta)) + geom_path(aes(color = speaker)) + theme(legend.position = "bottom")

plotly::ggplotly(G)
