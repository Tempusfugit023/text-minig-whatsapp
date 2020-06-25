# WhatsApp Conversacion: 
# Análisis de chats en WhatsApp: 
# Parte 1 - Análisis de texto y visualización de Datos con R
# Luis Fernando Escobar

# 1) Preparación y lectura de datos

install.packages("rwhatsapp")
install.packages("tidytext")
install.packages("kableExtra")


library(rwhatsapp)
library(lubridate)
library(tidyverse)
library(tidytext)
library(kableExtra)
library(RColorBrewer)
library(knitr)

library("dplyr")            # Habilita la sintaxis %>% de tuberIa
library("ggplot2") 

# LEEMOS EL CHAT A TRAVÉS DEL TXT EXPORTADO DESDE LA APP
setwd("C:/Users/FERNANDO/Documents")
miChat <- rwa_read("Mi_Chat1.txt")
miChat
Chat_completo <- miChat  

miChat <- Chat_completo %>%  
  mutate(day = date(time)) %>%  
  filter(day >= "2018-03-12" & day <= "2020-06-02")

# PREPARACIÓN DE DATOS PARA ANÁLISIS POR DATE/TIME
# SEGMENTACIÓN POR MES
Chat_completo1 <- miChat 
miChat <- Chat_completo1 %>% 
  mutate(estacion = case_when(
  day >= ymd("2018-03-12") & day <= ymd("2018-06-20") ~ "Otoño_2018",
  day >= ymd("2018-06-21") & day <= ymd("2018-09-20") ~ "Invierno_2018",
  day >= ymd("2018-09-21") & day <= ymd("2018-12-20") ~ "Primavera_2018",
  day >= ymd("2018-12-21") & day <= ymd("2019-03-20") ~ "Verano_2019",
  day >= ymd("2019-03-21") & day <= ymd("2019-06-20") ~ "Otoño_2019",
  day >= ymd("2019-06-21") & day <= ymd("2019-09-20") ~ "Invierno_2019",
  day >= ymd("2019-09-21") & day <= ymd("2019-12-20") ~ "Primavera_2019",
  day >= ymd("2019-12-20") ~ "Verano_2020",
  T ~ "Fuera_de_rango")) %>% 
  mutate(estacion = factor(estacion)) %>% 
  filter(!is.na(author))

# 2) Frecuencia de mensajes diarios
# PALETA DE COLORES
paleta.estaciones <- brewer.pal(8,"Set1")[c(7,5,1,3,4,2,6,8)]
# VERIFICANDO CUÁNTOS MENSAJES SE ENVIARON DURANTE EL PERIODO DE TIEMPO
miChat %>% 
  group_by(estacion) %>% 
  count(day) %>%
  ggplot(aes(x = day, y = n, fill=estacion)) +
  geom_bar(stat = "identity") +
  scale_fill_manual(values=paleta.estaciones) +
  ylab("Número de mensajes") + xlab("Fecha") +
  ggtitle("Mensajes por día", "Frecuencia por estación del año") +
  theme_minimal() +
  theme( legend.title = element_blank(), 
         legend.position = "bottom")

# Frecuencia de mensajes por día de la semana
# MENSAJES POR DÍA DE LA SEMANA
miChat %>% 
  mutate( wday.num = wday(day),
          wday.name = weekdays(day)) %>% 
  group_by(estacion, wday.num, wday.name) %>% 
  count() %>% 
  ggplot(aes(x = reorder(wday.name, -wday.num), y = n, fill=estacion)) +
  geom_bar(stat = "identity") +
  scale_fill_manual(values=paleta.estaciones) +
  ylab("") + xlab("") +
  coord_flip() +
  ggtitle("Número de mensajes por día de la semana", "Frecuencia por estación del año") +
  theme_minimal() +
 theme( legend.title = element_blank(), 
         legend.position = "bottom")

# Frecuencia de mensajes por hora del día
# MANTENER EL ORDEN DE DÍAS DE LA SEMANA Y RENOMBRARLOS
diasemana <- c("domingo","lunes","martes","miércoles","jueves","viernes","sábado","domingo")
names(diasemana) <- 1:7
# MENSAJES POR HORA DEL DÍA
miChat %>% 
  mutate( hour = hour(time), 
          wday.num = wday(day),
          wday.name = weekdays(day)) %>% 
  count(estacion, wday.num, wday.name, hour) %>% 
  ggplot(aes(x = hour, y = n, fill=estacion)) +
  geom_bar(stat = "identity") +
  scale_fill_manual(values=paleta.estaciones) +
  ylab("Número de mensajes") + xlab("Horario") +
  ggtitle("Número de mensajes por hora del día", "Frecuencia según estación del año") +
  facet_wrap(~wday.num, ncol=7, labeller = labeller(wday.num=diasemana))+
  theme_minimal() +
  theme( legend.title = element_blank(), 
         legend.position = "bottom",
         panel.spacing.x=unit(0.0, "lines"))

# ¿Quién ha enviado mayor cantidad de mensajes?
# CAMBIEMOS EL NOMBRE DE LOS USUARIOS POR CONFIDENCIALIDAD
levels(miChat$author)[2] <- "LFEC"
levels(miChat$author)[1] <- "BKRL"
# MENSAJES POR USUARIO
miChat %>%
  mutate(day = date(time)) %>%
  group_by(estacion) %>% 
  count(author) %>% 
  ggplot(aes(x = reorder(author, n), y = n, fill=estacion)) +
  geom_bar(stat = "identity") +
  scale_fill_manual(values=paleta.estaciones) +
  ylab("Número total de mensajes") + xlab("Usuario") +
  coord_flip() +
  ggtitle("Número total de mensajes por usuario.", "¿Quién es más comunicativo? Frecuencia por estación del año") +
  theme_minimal() +
  theme( legend.title = element_blank(), 
         legend.position = "bottom")

#¿Cuáles son los emojis más usados en el chat?
# LIBRERÍA PARA FETCH DE IMAGEN PNG DE EMOJI DESDE https://abs.twimg.com
  library(ggimage)
# EMOJI RANKING
plotEmojis <- miChat %>% 
  unnest(emoji, emoji_name) %>% 
  mutate(emoji = str_sub(emoji, end = 1)) %>% 
  mutate(emoji_name = str_remove(emoji_name, ":.*")) %>% 
  count(emoji, emoji_name) %>% 
  
  # PLOT TOP 30 EMOJIS
  top_n(30, n) %>% 
  arrange(desc(n)) %>% 
  # CREA UNA URL DE IMAGEN CON EL UNICODE DE EMOJI
  mutate(emoji_url = map_chr(emoji, 
                              ~paste0("https://abs.twimg.com/emoji/v2/72x72/", as.hexmode(utf8ToInt(.x)),".png")) 
  )
# PLOT DEL RANKING DE EMOJIS MÁS USADOS
plotEmojis %>% 
  ggplot(aes(x=reorder(emoji_name, n), y=n)) +
  geom_col(aes(fill=n), show.legend = FALSE, width = .2) +
  geom_point(aes(color=n), show.legend = FALSE, size = 3) +
  geom_image(aes(image=emoji_url), size=.045) +
  scale_fill_gradient(low="#2b83ba",high="#d7191c") +
                      scale_color_gradient(low="#2b83ba",high="#d7191c") +
                                           ylab("Número de veces que el emoji fue usado") +
                                             xlab("Emoji y significado") +
                                             ggtitle("Emojis más utilizados de manera general", "Emojis más usados por todos") +
                                             coord_flip() +
                                             theme_minimal() +
                                             theme()

# Emojis más utilizados en el chat, por usuario
# EMOJI RANK POR USUARIO
plotEmojis <- miChat %>%
  unnest(emoji, emoji_name) %>%
  mutate( emoji = str_sub(emoji, end = 1)) %>% # 
  count(author, emoji, emoji_name, sort = TRUE) %>%
  # PLOT DEL TOP 8 EMOJIS POR USUARIO
  group_by(author) %>%
  top_n(n = 8, n) %>%
  slice(1:8) %>% 
  # CREA UNA URL DE IMAGEN CON EL UNICODE DE EMOJI
  mutate(emoji_url = map_chr(emoji, 
                              ~paste0("https://abs.twimg.com/emoji/v2/72x72/",as.hexmode(utf8ToInt(.x)),".png")) )
# PLOT DE LA DATA
plotEmojis %>% 
  ggplot(aes(x = reorder(emoji, -n), y = n)) +
  geom_col(aes(fill = author, group=author), show.legend = FALSE, width = .20) +
  # USAR PARA HACER FETCH DE UNA IMAGEN PNG DE EMOJI https://abs.twimg.com
  geom_image(aes(image=emoji_url), size=.13) +
  ylab("Número de veces que se usó el emoji") +
  xlab("Emoji") +
  facet_wrap(~author, ncol = 5, scales = "free") +
  ggtitle("Emojis más usados en la conversación, por usuario") +
  theme_minimal() +
  theme(axis.text.x = element_blank())

#¿Cuáles son las palabras más usadas en el chat?
library(tidytext)
library(stopwords)
# REMOVEMOS PALABRAS SIN SIGNIFICADO RELEVANTE, COMO ARTÍCULOS, PRONOMBRES, ETC.
remover_palabras <- c(stopwords(language = "pt"),
                      "multimedia", "y", "jaja", "bueno", "bien", "jejeje", "muy", "estoy", "sé", "buena", "la", "el","en", "es", "si", "lo", "ya", "pero", "esa", "los","yo","mi", "un", "con", "las", "omitido", "más","eso", "al", "una", "del", "qué", "todo", "así", "le", "su", "va", "porque", "todos", "hay", "les", "pue", "ese", "son", "está", "pues", "ahí", "sí","ver", "estás", "algo", "vas", "ir","voy", "creo","fue","solo", "ni","sólo","nada", "aqui", "q", "tú", "haha")

# CONTEO DE PALABRAS
miChat %>%
  unnest_tokens(input = text, output = word) %>%
  filter(!word %in% remover_palabras) %>% 
  count(word) %>% 
  # PLOT DEL TOP 30 DE PALABRAS MÁS USADAS EN CONVERSACIÓN
  top_n(30,n) %>% 
  arrange(desc(n)) %>% 
  ggplot(aes(x=reorder(word,n), y=n, fill=n, color=n)) +
  geom_col(show.legend = FALSE, width = .1) +
  geom_point(show.legend = FALSE, size = 3) +
  scale_fill_gradient(low="#2b83ba",high="#d7191c") +
                      scale_color_gradient(low="#2b83ba",high="#d7191c") +
                                           ggtitle("Palabras más usadas en la conversación de manera general") +
                                             xlab("Palabras") +
                                             ylab("Número de veces que se usó la palabra") +
                                             coord_flip() +
                                             theme_minimal()

# Palabras más usadas en el chat, por usuario
# CONTEO DE PALABRAS POR USUARIO
miChat %>%
  unnest_tokens(input = text,
                output = word) %>%
  filter(!word %in% remover_palabras) %>%
  count(author, word, sort = TRUE) %>%
  
  # TOP 20 PALABRAS MÁS USADAS POR USUARIO
  group_by(author) %>%
  top_n(n = 20, n) %>%
  slice(1:20) %>%
  ungroup() %>% 
  arrange(author, desc(n)) %>% 
  mutate(order=row_number()) %>% 
  ggplot(aes(x = reorder(word, n), y = n, fill = author, color = author)) +
  geom_col(show.legend = FALSE, width = .1) +
  geom_point(show.legend = FALSE, size = 3) +
  xlab("Palabras") +
  ylab("Número de veces que se usó la palabra") +
  coord_flip() +
  facet_wrap(~author, ncol = 3, scales = "free") +
  ggtitle("Palabras más usadas por usuario en la conversación") +
  theme_minimal()

# ¿Quién tiene un léxico más diverso?
  # DIVERSIDAD DE LÉXICO
  miChat %>%
  unnest_tokens(input = text,
                output = word) %>%
  filter(!word %in% remover_palabras) %>%
  group_by(author) %>%
  summarise(lex_diversity = n_distinct(word)) %>%
  arrange(desc(lex_diversity)) %>%
  ggplot(aes(x = reorder(author, lex_diversity),
             y = lex_diversity,
             fill = author)) +
  geom_col(show.legend = FALSE) +
  scale_y_continuous(expand = (mult = c(0, 0, 0, 500))) +
  geom_text(aes(label = scales::comma(lex_diversity)), hjust = -0.1) +
  ylab("Diversidad léxica") +
  xlab("Usuario") +
  ggtitle("Diversidad de léxico en la conversación") +
  coord_flip()
  
#  Palabras únicas por usuario 
  # PALABRAS ÚNICAS POR ELLA
  palabras_unicas_ella <- miChat %>%
    unnest_tokens(input = text,
                  output = word) %>%
    filter(author != "BKRL") %>%  
    count(word, sort = TRUE)
  miChat %>%
    unnest_tokens(input = text,
                  output = word) %>%
    filter(author == "BKRL") %>% 
    count(word, sort = TRUE) %>% 
    filter(!word %in% palabras_unicas_ella$word) %>% 
    # SELECCIONAR SÓLO PALABRAS QUE NADIE MÁS USA
    top_n(n = 15, n) %>%
    ggplot(aes(x = reorder(word, n), y = n)) +
    geom_col(show.legend = FALSE) +
    ylab("Número de veces que se usó la palabra") + xlab("Palabras") +
    coord_flip() +
    ggtitle("Top de palabras únicas usadas por BKRL")
  # PALABRAS ÚNICAS POR ÉL
  palabras_unicas_el <- miChat %>%
    unnest_tokens(input = text,
                  output = word) %>%
    filter(author != "LFEC") %>%  
    count(word, sort = TRUE)
  miChat %>%
    unnest_tokens(input = text,
                  output = word) %>%
    filter(author == "LFEC") %>% 
    count(word, sort = TRUE) %>% 
    filter(!word %in% palabras_unicas_el$word) %>% 
    # SELECCIONAR SÓLO PALABRAS QUE NADIE MÁS USA
    top_n(n = 15, n) %>%
    ggplot(aes(x = reorder(word, n), y = n)) +
    geom_col(show.legend = FALSE) +
    ylab("Número de veces que se usó la palabra") + xlab("Palabras") +
    coord_flip() +
    ggtitle("Top de palabras únicas usadas por LFEC")
  
#  Análisis de sentimientos con Emoji Sentiment Ranking
  # USAMOS EL PAQUETE RVEST
  library(rvest)
  # FETCH DE PÁGINA HTML EMOJI SENTIMENT RANKING 1.0
  url_base <- "http://kt.ijs.si/data/Emoji_sentiment_ranking/index.html"
  doc <- read_html(url_base)
  # BUSCAR TABLA DE EMOJI Y PROCESO
  tabla_emojis <- doc %>% 
    html_node("#myTable") %>% 
    html_table() %>% 
    
  # OBTENER PUNTAJE DE SENTIMIENTO Y LIMPIAR
    sentimiento_emoji <- tabla_emojis %>% 
    select(1,6:9) %>% 
    set_names("char", "negativo","neutral","positivo","sent.score")
  # EXTRAER EMOJI Y UNIR CON SENTIMIENTO
  emoji_chat <- miChat %>% 
    unnest(emoji, emoji_name) %>% 
    mutate( emoji = str_sub(emoji, end = 1)) %>% 
    inner_join(sentimiento_emoji, by=c("emoji"="char"))
  # VISUALIZACIÓN PREVIA 
  emoji_chat %>% 
    select(-source, -day, -estacion) %>% 
    slice(1207:1219) %>% 
    kable() %>% 
    kable_styling(font_size = 10)  
    as_tibble()

    # USAMOS EL PAQUETE RVEST
    library(rvest)
    # FETCH DE PÁGINA HTML EMOJI SENTIMENT RANKING 1.0
    url_base <- "http://kt.ijs.si/data/Emoji_sentiment_ranking/index.html"
    doc <- read_html(url_base)
    # BUSCAR TABLA DE EMOJI Y PROCESO
    tabla_emojis <- doc %>% 
      html_node("#myTable") %>% 
      html_table() %>% 
      as_tibble()

    
    # OCURRENCIAS DE SENTIMIENTOS POR EMOJIS, POR USUARIO
    emoji_sentimiento_usuarios <- emoji_chat %>% 
      group_by(author) %>% 
      summarise(
        positivo=mean(positivo),
        negativo=mean(negativo),
        neutral=mean(neutral),
        balance=mean(sent.score)
      ) %>% 
      arrange(desc(balance))
    # FORMATO DE DATOS PARA REALIZAR PLOT
    emoji_sentimiento_usuarios %>% 
      mutate( negativo  = -negativo,
              neutral.positivo =  neutral/2,
              neutral.negativo = -neutral/2) %>% 
      select(-neutral) %>% 
      gather("sentiment","mean", -author, -balance) %>% 
      mutate(sentiment = factor(sentiment, levels = c("negativo", "neutral.negativo", "positivo", "neutral.positivo"), ordered = T)) %>% 
      ggplot(aes(x=reorder(author,balance), y=mean, fill=sentiment)) +
      geom_bar(position="stack", stat="identity", show.legend = F, width = .5) +
      scale_fill_manual(values = brewer.pal(4,"RdYlGn")[c(1,2,4,2)]) +
      ylab(" - Negativo / Neutral / Positivo +") + xlab("Usuario") +
      ggtitle("Análisis de sentimientos por usuario","Basado en el puntaje promedio de sentimientos por emojis") +
      coord_flip() +
      theme_minimal()    

    
    
    # Análisis de sentimientos con AFINN
    library(textdata)
    # OBTENER LÉXICO POSITIVO/NEGATIVO DEL PACKAGE DE LEXICON 
    lexico_negpos  <- get_sentiments("afinn") # INTENSIDAD DE VALOR
    # PREVIEW DEL FORMATO DE LEXICO
    lexico_negpos %>% 
      head(10) %>% 
      kable() %>%
      kable_styling(full_width = F, font_size = 11)
    # PREVIEW CUÁLES SON LOS VALORES POSIBLES
    table(lexico_negpos$value) %>% 
      head(10) %>% 
      kable() %>%
      kable_styling(full_width = F, font_size = 11)

    
    # EXTRAER EMOJIS
    emoji_sentimiento_score <- miChat %>%
      select( emoji, emoji_name) %>% 
      unnest( emoji, emoji_name) %>% 
      mutate( emoji = str_sub(emoji, end = 1)) %>% 
      mutate( emoji_name = str_remove(emoji_name, ":.*")) %>%  
      distinct() %>% 
      unnest_tokens(input=emoji_name, output=emoji_words) %>% 
      inner_join(lexico_negpos, by=c("emoji_words"="word"))
    # CREAR TABLA DE 3 COLUMNAS
    bind_cols(
      slice(emoji_sentimiento_score, 01:10),
      slice(emoji_sentimiento_score, 11:20),
      slice(emoji_sentimiento_score, 21:30)
    ) %>% 
      kable() %>% 
      kable_styling(full_width = F, font_size = 11)        
    
    
    
    
    # EXTRAER EMOJIS
    emoji_chat <- miChat %>% 
      unnest(emoji, emoji_name) %>% 
      mutate( emoji = str_sub(emoji, end = 1)) %>% 
      mutate( emoji_name = str_remove(emoji_name, ":.*"))
    # TOKENIZAR EL NOMBRE DE EMOJI
    emoji_chat <- emoji_chat %>% 
      select(author, emoji_name) %>% 
      unnest_tokens(input=emoji_name, output=emoji_words)
    # JOIN CON LEXICON
    usuario_summary <- emoji_chat %>% 
      inner_join(lexico_negpos, by=c("emoji_words"="word")) %>% 
      count(author, value) %>% 
      group_by(author) %>% 
      mutate(mean=n/sum(n)) %>% 
      ungroup()
    # COLORES Y GRÁFICA
    reordenar_niveles <- c(-3,-2,-1,3,2,1)
    colores <- c("#d7191c","#fdae61","#ffffbf","#1a9641","#a6d96a","#ffffbf")
    mis_colores <- brewer.pal(5,"RdYlGn")[c(1,2,3,5,4,3)]
    # PLOT DE LA GRÁFICA
    usuario_summary %>% 
      mutate( mean = ifelse(value<0, -mean, mean)) %>% 
      group_by(author) %>% 
      mutate( balance = sum(mean)) %>% 
      ungroup() %>% 
      mutate( value = factor(value, levels = reordenar_niveles, ordered=T)) %>% 
      ggplot(aes(x=reorder(author,balance), y=mean, fill=value)) +
      geom_bar(stat="identity",position="stack", show.legend = F, width = .5) +
      scale_fill_manual(values = mis_colores) +
      xlab("Usuario") + ylab("Escala de netagivo a positivo") +
      coord_flip() +
      ggtitle("Análisis de sentimientos por uso de emojis", "Uso de package Lexicon") +
      theme_minimal()
    
    
    
    # ¿Cuál es la emoción más frecuente? (Con EmoLex)
    # OBTENER OTRO LÉXICO CON NOMBRE DE SENTIMIENTOS
    lexico_sentimientos <- get_sentiments("nrc") 
    # EXTRAER EMOJIS
    emoji_emocion <- miChat %>%
      select( emoji, emoji_name) %>% 
      unnest( emoji, emoji_name) %>% 
      mutate( emoji = str_sub(emoji, end = 1)) %>%  
      mutate( emoji_name = str_remove(emoji_name, ":.*")) %>%  
      unnest_tokens(input=emoji_name, output=emoji_words) %>% 
      inner_join(lexico_sentimientos, by=c("emoji_words"="word")) %>%
      # REMOVER CLASIFICACIÓN NEGATIVA/POSITIVA 
      filter(!sentiment %in% c("negative","positive")) %>% 
      
      # MANTENER SÓLO LOS 4 EMOJI MÁS FRECUENTES PARA CADA SENTIMIENTO
      count(emoji, emoji_words, sentiment) %>% 
      group_by(sentiment) %>% 
      top_n(4,n) %>% 
      slice(1:4) %>% 
      ungroup() %>% 
      select(-n)
    ## PONER TABLAS JUNTAS
    bind_cols(
      slice(emoji_emocion, 01:16),
      slice(emoji_emocion, 17:32)
    ) %>% 
      kable() %>% 
      kable_styling(full_width = F, font_size = 11)
    
    
    # JOIN CON EMOJIS
    sentimiento_chat <- emoji_chat %>% 
      inner_join(lexico_sentimientos, by=c("emoji_words"="word")) %>%
      # REMOVER CLASIFICACIÓN POITIVA/NEGATIVA 
      filter(!sentiment %in% c("negative","positive"))
    # PLOT DE EMOCIONES MAYORMENTE EXPRESADAS
    sentimiento_chat %>% 
      count(sentiment) %>% 
      ggplot(aes(x=reorder(sentiment,n), y=n)) +
      geom_col(aes(fill=n), show.legend = FALSE, width = .1) +
      geom_point(aes(color=n), show.legend = FALSE, size = 3) +
      coord_flip() +
      ylab("Número de veces expresado") + xlab("Emoción") +
      scale_fill_gradient(low="#2b83ba",high="#d7191c") +
      scale_color_gradient(low="#2b83ba",high="#d7191c") +
      ggtitle("Emoción expresada con mayor frecuencia","Expresado por uso de emojis") +
      theme_minimal()
    
    
    
    # ¿Qué emociones expresan con mayor frecuencia cada usuario?
      # PLOT DE EMOCIONES POR USUARIO
      sentimiento_chat %>% 
      count(author, sentiment) %>% 
      left_join(filter(lexico_sentimientos, sentiment %in% c("negative","positive")),by=c("sentiment"="word")) %>% 
      rename( sentimiento = sentiment.y) %>% 
      mutate( sentimiento = ifelse(is.na(sentimiento), "neutral", sentimiento)) %>% 
      mutate( sentimiento = factor(sentimiento, levels = c("negative", "neutral", "positive"), ordered=T) ) %>% 
      group_by(author) %>%
      top_n(n = 8, n) %>%
      slice(1:8) %>% 
      ggplot(aes(x = reorder(sentiment, n), y = n, fill = sentimiento)) +
      geom_col() +
      scale_fill_manual(values = c("#d7191c","#fdae61", "#1a9641")) +
      ylab("Número de veces expresado") +
      xlab("Emoción") +
      coord_flip() +
      facet_wrap(~author, ncol = 3, scales = "free_x") +
      ggtitle("Emociones mayormente expresadas por usuario", "Expresado por uso de emojis") + 
      theme_minimal() + theme(legend.position = "bottom")  
    
    
      
    
      
      
    