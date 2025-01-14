# Visualize

# Benötigte Pakete
library(ggplot2)
library(dplyr)
setwd("C:/Users/michels/sciebo/BDB 2025")
tracking_data = read.csv("tracking_week_1.csv")
#players = read.csv("players.csv")
plays = read.csv("plays.csv")

# Tracking DATA Week 1 Dallas vs Tampa Bay
# Tracking DATA Week 1 KC vs AZ

tracking_kc_az = tracking %>%
  filter(gameId == 2022091110) %>%
  filter(playId == 291) %>% filter(frameId >= frameId[which(event == "line_set")[1]]) #%>%
  #filter(frameType == "BEFORE_SNAP") #%>% 
  # left_join(., players %>% dplyr::select(nflId, position), by = "nflId") %>% 
  # left_join(., plays %>% dplyr::select(playId, gameId, absoluteYardlineNumber, possessionTeam, pff_manZone), 
  #           by = c("playId", "gameId"))


#20220911 031700
#data %>% filter(uniId == "2022091103170046097")
tracking_kc_az = tracking_data %>% filter(gameId == 2022091103, playId == 1700)%>% filter(frameId >= frameId[which(event == "line_set")[1]])
los = plays %>% 
  filter(gameId == 2022091103) %>% 
  filter(playId == 1700) %>% pull(absoluteYardlineNumber)

# Line of Scrimmage
los = plays %>% 
  filter(gameId == 2022091110) %>% 
  filter(playId == 291) %>% pull(absoluteYardlineNumber)

rm(tracking_data)

# Spielfeld-Abmessungen (angepasst an Football)
field_length <- 120
field_width <- 53.3

team_colors = c("blue4", "black", "red")

# probs are coming from the fitted model, must be taken from there
new_colnames = c("defender_Id", att_ids)
probs <- lapply(probs, function(df) {
  colnames(df) <- new_colnames
  return(df)
})

data1 = list()
defender_data = NULL
for (i in 1:length(probs)) {
  data1[[i]] = tracking_kc_az %>% filter(nflId == probs[[i]][1,1], 
                                          frameType == "BEFORE_SNAP") %>% 
    cbind(.,probs[[i]])
  defender_data = bind_rows(defender_data, data1[[i]])
}

# Animation: Spielerbewegungen
for (t in unique(tracking_kc_az$time)) {
  # Daten für den aktuellen Zeitpunkt filtern
  current_data <- tracking_kc_az[tracking_kc_az$time == t, ]
  
  # Spielfeld zeichnen
  plot(
    NA, 
    xlim = c(min(tracking_kc_az$x)-1, max(tracking_kc_az$x)+1),#c(0, field_length), 
    ylim = c(min(tracking_kc_az$y)-1, max(tracking_kc_az$y)+1),#c(0, field_width),
    xlab = "Spielfeld-Länge", ylab = "Spielfeld-Breite",
    main = paste("Spielerbewegungen - Zeit:", t), asp = 1
  )
  grid() # Raster für das Spielfeld
  
  # Spielerpositionen hinzufügen
  points(current_data$x, current_data$y, pch = 19, 
         col = team_colors[as.factor(current_data$club)], cex = 2)
  abline(v = los)
  #text(current_data$x, current_data$y, labels = current_data$nflId, pos = 3, col = "black")
  
  # Verbindungslinien basierend auf Wahrscheinlichkeiten hinzufügen
  defender_probs <- defender_data[defender_data$time == t, ]#[[t %% length(defender_data) + 1]] # Beispiel: Wahrscheinlichkeiten für diesen Zeitpunkt
  for (i in 1:nrow(defender_probs)) {
    defender <- defender_probs$defender_Id[i]
    # Maximale Wahrscheinlichkeit für diesen Verteidiger
    max_prob <- which.max(defender_probs[i, 20:24])
    attacker <- as.numeric(names(defender_probs)[max_prob + 19])
    
    current_data2 = current_data %>% filter(!is.na(nflId))
    # Koordinaten von Verteidiger und Angreifer finden
    defender_pos <- current_data[which(current_data2$nflId == defender), c("x", "y")]
    attacker_pos <- current_data[which(current_data2$nflId == attacker), c("x", "y")]
    
    # Linie zeichnen, falls beide gefunden wurden
    if (nrow(defender_pos) > 0 && nrow(attacker_pos) > 0) {
      lines(
        x = c(defender_pos$x, attacker_pos$x),
        y = c(defender_pos$y, attacker_pos$y),
        col = "darkgray", lwd = 2
      )
    }
  }
  # Pause für Animation
  Sys.sleep(0.1) # Wartezeit in Sekunden
}


# Feste Farbskala (von hellgrau zu schwarz)
color_palette <- colorRampPalette(c("white", "black"))

# Anzahl an Farbstufen definieren (z. B. 100)
n_colors <- 100
colors <- color_palette(n_colors)# Animation: Spielerbewegungen
for (t in unique(tracking_kc_az$time)) {
  # Daten für den aktuellen Zeitpunkt filtern
  current_data <- tracking_kc_az[tracking_kc_az$time == t, ]
  
  # Spielfeld zeichnen
  plot(
    NA, 
    xlim = c(min(tracking_kc_az$x)-1, max(tracking_kc_az$x)+1),#c(0, field_length), 
    ylim = c(min(tracking_kc_az$y)-1, max(tracking_kc_az$y)+1),#c(0, field_width),
    xlab = "Spielfeld-Länge", ylab = "Spielfeld-Breite",
    main = paste("Spielerbewegungen - Zeit:", t), asp = 1
  )
  grid() # Raster für das Spielfeld
  
  #text(current_data$x, current_data$y, labels = current_data$nflId, pos = 3, col = "black")
  
  # Verbindungslinien basierend auf Wahrscheinlichkeiten hinzufügen
  defender_probs <- defender_data[defender_data$time == t, ]#[[t %% length(defender_data) + 1]] # Beispiel: Wahrscheinlichkeiten für diesen Zeitpunkt
  for (i in 1:nrow(defender_probs)) {
    defender <- defender_probs$defender_Id[i]
    # Maximale Wahrscheinlichkeit für diesen Verteidiger
    #max_prob <- which.max(defender_probs[i, 20:24])
    for (j in 1:n_att) {
      attacker <- as.numeric(names(defender_probs)[j + 19])
      current_data2 = current_data %>% filter(!is.na(nflId))
      # Koordinaten von Verteidiger und Angreifer finden
      defender_pos <- current_data[which(current_data2$nflId == defender), c("x", "y")]
      attacker_pos <- current_data[which(current_data2$nflId == attacker), c("x", "y")]
      
      # Mapping der y-Werte auf die Farbskala
      y_scaled <- round(defender_probs[i,j+19] * (n_colors - 1)) + 1
      mapped_colors <- colors[y_scaled]
      # Linie zeichnen, falls beide gefunden wurden
      #if (nrow(defender_pos) > 0 && nrow(attacker_pos) > 0) {
        lines(
          x = c(defender_pos$x, attacker_pos$x),
          y = c(defender_pos$y, attacker_pos$y),
          lwd = 3,
          col = mapped_colors #, # Linienstärke proportional zu y[i]
        )
      #}      
    }
    
    # Spielerpositionen hinzufügen
    points(current_data$x, current_data$y, pch = 19, 
           col = team_colors[as.factor(current_data$club)], cex = 2)
    abline(v = los)

  }
  
  # Pause für Animation
  Sys.sleep(0.1) # Wartezeit in Sekunden
}


