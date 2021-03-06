# WhatsApp Conversacion: 
# An�lisis de chats en WhatsApp: 
# Parte 1 - An�lisis de texto y visualizaci�n de Datos con R
# Luis Fernando Escobar

# 1) Preparaci�n y lectura de datos

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

# LEEMOS EL CHAT A TRAV�S DEL TXT EXPORTADO DESDE LA APP
setwd("C:/Users/FERNANDO/Documents")
miChat <- rwa_read("Mi_Chat1.txt")
miChat
Chat_completo <- miChat  

miChat <- Chat_completo %>%  
  mutate(day = date(time)) %>%  
  filter(day >= "2018-03-12" & day <= "2020-06-02")

# PREPARACI�N DE DATOS PARA AN�LISIS POR DATE/TIME
# SEGMENTACI�N POR MES
Chat_completo1 <- miChat 
miChat <- Chat_completo1 %>% 
  mutate(estacion = case_when(
  day >= ymd("2018-03-12") & day <= ymd("2018-06-20") ~ "Oto�o_2018",
  day >= ymd("2018-06-21") & day <= ymd("2018-09-20") ~ "Invierno_2018",
  day >= ymd("2018-09-21") & day <= ymd("2018-12-20") ~ "Primavera_2018",
  day >= ymd("2018-12-21") & day <= ymd("2019-03-20") ~ "Verano_2019",
  day >= ymd("2019-03-21") & day <= ymd("2019-06-20") ~ "Oto�o_2019",
  day >= ymd("2019-06-21") & day <= ymd("2019-09-20") ~ "Invierno_2019",
  day >= ymd("2019-09-21") & day <= ymd("2019-12-20") ~ "Primavera_2019",
  day >= ymd("2019-12-20") ~ "Verano_2020",
  T ~ "Fuera_de_rango")) %>% 
  mutate(estacion = factor(estacion)) %>% 
  filter(!is.na(author))

# 2) Frecuencia de mensajes diarios
# PALETA DE COLORES
paleta.estaciones <- brewer.pal(8,"Set1")[c(7,5,1,3,4,2,6,8)]
# VERIFICANDO CU�NTOS MENSAJES SE ENVIARON DURANTE EL PERIODO DE TIEMPO
miChat %>% 
  group_by(estacion) %>% 
  count(day) %>%
  ggplot(aes(x = day, y = n, fill=estacion)) +
  geom_bar(stat = "identity") +
  scale_fill_manual(values=paleta.estaciones) +
  ylab("N�mero de mensajes") + xlab("Fecha") +
  ggtitle("Mensajes por d�a", "Frecuencia por estaci�n del a�o") +
  theme_minimal() +
  theme( legend.title = element_blank(), 
         legend.position = "bottom")

# Frecuencia de mensajes por d�a de la semana
# MENSAJES POR D�A DE LA SEMANA
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
  ggtitle("N�mero de mensajes por d�a de la semana", "Frecuencia por estaci�n del a�o") +
  theme_minimal() +
 theme( legend.title = element_blank(), 
         legend.position = "bottom")

# Frecuencia de mensajes por hora del d�a
# MANTENER EL ORDEN DE D�AS DE LA SEMANA Y RENOMBRARLOS
diasemana <- c("domingo","lunes","martes","mi�rcoles","jueves","viernes","s�bado","domingo")
names(diasemana) <- 1:7
# MENSAJES POR HORA DEL D�A
miChat %>% 
  mutate( hour = hour(time), 
          wday.num = wday(day),
          wday.name = weekdays(day)) %>% 
  count(estacion, wday.num, wday.name, hour) %>% 
  ggplot(aes(x = hour, y = n, fill=estacion)) +
  geom_bar(stat = "identity") +
  scale_fill_manual(values=paleta.estaciones) +
  ylab("N�mero de mensajes") + xlab("Horario") +
  ggtitle("N�mero de mensajes por hora del d�a", "Frecuencia seg�n estaci�n del a�o") +
  facet_wrap(~wday.num, ncol=7, labeller = labeller(wday.num=diasemana))+
  theme_minimal() +
  theme( legend.title = element_blank(), 
         legend.position = "bottom",
         panel.spacing.x=unit(0.0, "lines"))

# �Qui�n ha enviado mayor cantidad de mensajes?
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
  ylab("N�mero total de mensajes") + xlab("Usuario") +
  coord_flip() +
  ggtitle("N�mero total de mensajes por usuario.", "�Qui�n es m�s comunicativo? Frecuencia por estaci�n del a�o") +
  theme_minimal() +
  theme( legend.title = element_blank(), 
         legend.position = "bottom")

#�Cu�les son los emojis m�s usados en el chat?
# LIBRER�A PARA FETCH DE IMAGEN PNG DE EMOJI DESDE https://abs.twimg.com
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
# PLOT DEL RANKING DE EMOJIS M�S USADOS
plotEmojis %>% 
  ggplot(aes(x=reorder(emoji_name, n), y=n)) +
  geom_col(aes(fill=n), show.legend = FALSE, width = .2) +
  geom_point(aes(color=n), show.legend = FALSE, size = 3) +
  geom_image(aes(image=emoji_url), size=.045) +
  scale_fill_gradient(low="#2b83ba",high="#d7191c") +
                      scale_color_gradient(low="#2b83ba",high="#d7191c") +
                                           ylab("N�mero de veces que el emoji fue usado") +
                                             xlab("Emoji y significado") +
                                             ggtitle("Emojis m�s utilizados de manera general", "Emojis m�s usados por todos") +
                                             coord_flip() +
                                             theme_minimal() +
                                             theme()

# Emojis m�s utilizados en el chat, por usuario
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
  ylab("N�mero de veces que se us� el emoji") +
  xlab("Emoji") +
  facet_wrap(~author, ncol = 5, scales = "free") +
  ggtitle("Emojis m�s usados en la conversaci�n, por usuario") +
  theme_minimal() +
  theme(axis.text.x = element_blank())

#�Cu�les son las palabras m�s usadas en el chat?
library(tidytext)
library(stopwords)
# REMOVEMOS PALABRAS SIN SIGNIFICADO RELEVANTE, COMO ART�CULOS, PRONOMBRES, ETC.
remover_palabras <- c(stopwords(language = "pt"),
                      "multimedia", "y", "jaja", "bueno", "bien", "jejeje", "muy", "estoy", "s�", "buena", "la", "el","en", "es", "si", "lo", "ya", "pero", "esa", "los","yo","mi", "un", "con", "las", "omitido", "m�s","eso", "al", "una", "del", "qu�", "todo", "as�", "le", "su", "va", "porque", "todos", "hay", "les", "pue", "ese", "son", "est�", "pues", "ah�", "s�","ver", "est�s", "algo", "vas", "ir","voy", "creo","fue","solo", "ni","s�lo","nada", "aqui", "q", "t�", "haha")

# CONTEO DE PALABRAS
miChat %>%
  unnest_tokens(input = text, output = word) %>%
  filter(!word %in% remover_palabras) %>% 
  count(word) %>% 
  # PLOT DEL TOP 30 DE PALABRAS M�S USADAS EN CONVERSACI�N
  top_n(30,n) %>% 
  arrange(desc(n)) %>% 
  ggplot(aes(x=reorder(word,n), y=n, fill=n, color=n)) +
  geom_col(show.legend = FALSE, width = .1) +
  geom_point(show.legend = FALSE, size = 3) +
  scale_fill_gradient(low="#2b83ba",high="#d7191c") +
                      scale_color_gradient(low="#2b83ba",high="#d7191c") +
                                           ggtitle("Palabras m�s usadas en la conversaci�n de manera general") +
                                             xlab("Palabras") +
                                             ylab("N�mero de veces que se us� la palabra") +
                                             coord_flip() +
                                             theme_minimal()

# Palabras m�s usadas en el chat, por usuario
# CONTEO DE PALABRAS POR USUARIO
miChat %>%
  unnest_tokens(input = text,
                output = word) %>%
  filter(!word %in% remover_palabras) %>%
  count(author, word, sort = TRUE) %>%
  
  # TOP 20 PALABRAS M�S USADAS POR USUARIO
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
  ylab("N�mero de veces que se us� la palabra") +
  coord_flip() +
  facet_wrap(~author, ncol = 3, scales = "free") +
  ggtitle("Palabras m�s usadas por usuario en la conversaci�n") +
  theme_minimal()

# �Qui�n tiene un l�xico m�s diverso?
  # DIVERSIDAD DE L�XICO
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
  ylab("Diversidad l�xica") +
  xlab("Usuario") +
  ggtitle("Diversidad de l�xico en la conversaci�n") +
  coord_flip()
  
#  Palabras �nicas por usuario 
  # PALABRAS �NICAS POR ELLA
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
    # SELECCIONAR S�LO PALABRAS QUE NADIE M�S USA
    top_n(n = 15, n) %>%
    ggplot(aes(x = reorder(word, n), y = n)) +
    geom_col(show.legend = FALSE) +
    ylab("N�mero de veces que se us� la palabra") + xlab("Palabras") +
    coord_flip() +
    ggtitle("Top de palabras �nicas usadas por BKRL")
  # PALABRAS �NICAS POR �L
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
    # SELECCIONAR S�LO PALABRAS QUE NADIE M�S USA
    top_n(n = 15, n) %>%
    ggplot(aes(x = reorder(word, n), y = n)) +
    geom_col(show.legend = FALSE) +
    ylab("N�mero de veces que se us� la palabra") + xlab("Palabras") +
    coord_flip() +
    ggtitle("Top de palabras �nicas usadas por LFEC")
  
#  An�lisis de sentimientos con Emoji Sentiment Ranking
  # USAMOS EL PAQUETE RVEST
  library(rvest)
  # FETCH DE P�GINA HTML EMOJI SENTIMENT RANKING 1.0
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
  # VISUALIZACI�N PREVIA 
  emoji_chat %>% 
    select(-source, -day, -estacion) %>% 
    slice(1207:1219) %>% 
    kable() %>% 
    kable_styling(font_size = 10)  
    as_tibble()

    # USAMOS EL PAQUETE RVEST
    library(rvest)
    # FETCH DE P�GINA HTML EMOJI SENTIMENT RANKING 1.0
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
      ggtitle("An�lisis de sentimientos por usuario","Basado en el puntaje promedio de sentimientos por emojis") +
      coord_flip() +
      theme_minimal()    

    
    
    # An�lisis de sentimientos con AFINN
    library(textdata)
    # OBTENER L�XICO POSITIVO/NEGATIVO DEL PACKAGE DE LEXICON 
    lexico_negpos  <- get_sentiments("afinn") # INTENSIDAD DE VALOR
    # PREVIEW DEL FORMATO DE LEXICO
    lexico_negpos %>% 
      head(10) %>% 
      kable() %>%
      kable_styling(full_width = F, font_size = 11)
    # PREVIEW CU�LES SON LOS VALORES POSIBLES
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
    # COLORES Y GR�FICA
    reordenar_niveles <- c(-3,-2,-1,3,2,1)
    colores <- c("#d7191c","#fdae61","#ffffbf","#1a9641","#a6d96a","#ffffbf")
    mis_colores <- brewer.pal(5,"RdYlGn")[c(1,2,3,5,4,3)]
    # PLOT DE LA GR�FICA
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
      ggtitle("An�lisis de sentimientos por uso de emojis", "Uso de package Lexicon") +
      theme_minimal()
    
    
    
    # �Cu�l es la emoci�n m�s frecuente? (Con EmoLex)
    # OBTENER OTRO L�XICO CON NOMBRE DE SENTIMIENTOS
    lexico_sentimientos <- get_sentiments("nrc") 
    # EXTRAER EMOJIS
    emoji_emocion <- miChat %>%
      select( emoji, emoji_name) %>% 
      unnest( emoji, emoji_name) %>% 
      mutate( emoji = str_sub(emoji, end = 1)) %>%  
      mutate( emoji_name = str_remove(emoji_name, ":.*")) %>%  
      unnest_tokens(input=emoji_name, output=emoji_words) %>% 
      inner_join(lexico_sentimientos, by=c("emoji_words"="word")) %>%
      # REMOVER CLASIFICACI�N NEGATIVA/POSITIVA 
      filter(!sentiment %in% c("negative","positive")) %>% 
      
      # MANTENER S�LO LOS 4 EMOJI M�S FRECUENTES PARA CADA SENTIMIENTO
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
      # REMOVER CLASIFICACI�N POITIVA/NEGATIVA 
      filter(!sentiment %in% c("negative","positive"))
    # PLOT DE EMOCIONES MAYORMENTE EXPRESADAS
    sentimiento_chat %>% 
      count(sentiment) %>% 
      ggplot(aes(x=reorder(sentiment,n), y=n)) +
      geom_col(aes(fill=n), show.legend = FALSE, width = .1) +
      geom_point(aes(color=n), show.legend = FALSE, size = 3) +
      coord_flip() +
      ylab("N�mero de veces expresado") + xlab("Emoci�n") +
      scale_fill_gradient(low="#2b83ba",high="#d7191c") +
      scale_color_gradient(low="#2b83ba",high="#d7191c") +
      ggtitle("Emoci�n expresada con mayor frecuencia","Expresado por uso de emojis") +
      theme_minimal()
    
    
    
    # �Qu� emociones expresan con mayor frecuencia cada usuario?
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
      ylab("N�mero de veces expresado") +
      xlab("Emoci�n") +
      coord_flip() +
      facet_wrap(~author, ncol = 3, scales = "free_x") +
      ggtitle("Emociones mayormente expresadas por usuario", "Expresado por uso de emojis") + 
      theme_minimal() + theme(legend.position = "bottom")  
    
    
      
    
      
      
    