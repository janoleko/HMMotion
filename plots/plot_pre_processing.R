par(mfrow = c(2, 2), mar = c(1, 1, 1, 1))
library(dplyr)
library(stringr)
library(magick)
setwd("C:/Users/michels/sciebo/Konferenzen/NESSIS 2025/Talk")
tracking_kc_az = read.csv("tracking_kc.csv")
defender_data = read.csv("kc_defender_data.csv")

# Spielfeld-Abmessungen (angepasst an Football)
field_length <- 120
field_width <- 53.3

pitch_colors = c("#a4e86f", "#7eb356")
team_colors = c("#97233F", "white")#"blue4")
los = 19

clubs = levels(as.factor(tracking_kc_az$club))[levels(as.factor(tracking_kc_az$club)) != "football"]

xlim = round(c(min(tracking_kc_az$x) - 8, 
               max(tracking_kc_az$x)+5), - 1)
xlim[2] = 50
# ylim = round(c(min(tracking_kc_az$y)-1, max(tracking_kc_az$y)+1), -1)
ylim = c(0, field_width)
asp_ratio =  diff(xlim) / diff(ylim)

xlabs = c("10", "20", "30", "40", "50", "40", "30", "20", "10")

imInd = 1

players = read.csv("players.csv")

t = unique(tracking_kc_az$time)[1] 
# Daten für den aktuellen Zeitpunkt filtern
current_data <- tracking_kc_az[tracking_kc_az$time == t, ] |> 
  filter(displayName != "football")

# Spielfeld zeichnen
plot(
  NA, 
  xlim = xlim, 
  ylim = c(ylim[1]-8, ylim[2]), 
  xlab = "", ylab = "",
  xaxt = "n", yaxt = "n",
  bty = "n",
  xpd = T
)

# draw the endzones if necessary
if(xlim[1] < 10){
  rect(xlim[1], ylim[1], 10, ylim[2], 
       col = colorspace::lighten(team_colors[1], 0.3),
       border = NA)
  text(5, mean(ylim), "ARIZONA", srt = 90, 
       col = team_colors[1], cex = 2.25)
}
if(xlim[2] > 110){
  rect(110, ylim[1], xlim[2], ylim[2], 
       col = colorspace::lighten(team_colors[2], 0.3),
       border = NA)
}

# draw the field and labels
i = 10
k = 1
while(i < xlim[2]){
  if(i < xlim[2]){
    right = i+5
  } else{
    right = xlim[2]
  }
  rect(i, ylim[1], right, ylim[2], 
       col = colorspace::lighten(pitch_colors[i %% 2 + 1], 0.6),
       border = NA)
  
  if(i >= 20 & i %% 10 == 0 & i < 100){
    text(i, ylim[2] - 4, xlabs[k], col = "#00000080", lwd = 2, cex = 0.9)
    k = k + 1
  }
  
  i = i + 5
}

# draw the pitch border
segments(xlim[1], 0, xlim[2], 0, lwd = 2)
segments(xlim[1], field_width, xlim[2], field_width, lwd = 2)
segments(xlim[1], 0, xlim[1], field_width, lwd = 2)

# Spielerpositionen hinzufügen
# abline(v = los, lwd = 2, col = "#00000080", lty = 2)
segments(los, ylim[1], los, ylim[2], col = "#00000080", lwd = 2)
#text(current_data$x, current_data$y, labels = current_data$nflId, pos = 3, col = "black")

# # Verbindungslinien basierend auf Wahrscheinlichkeiten hinzufügen
# defender_probs <- defender_data[defender_data$time == t, ]#[[t %% length(defender_data) + 1]] # Beispiel: Wahrscheinlichkeiten für diesen Zeitpunkt
# for (i in 1:nrow(defender_probs)) {
#   defender <- defender_probs$nflId[i]
#   # Maximale Wahrscheinlichkeit für diesen Verteidiger
#   max_prob <- which.max(defender_probs[i, 21:25])
#   attacker <- as.numeric(str_sub(names(defender_probs)[max_prob + 20], 2, 6))
#   
#   current_data2 = current_data %>% filter(!is.na(nflId))
#   # Koordinaten von Verteidiger und Angreifer finden
#   defender_pos <- current_data[which(current_data2$nflId == defender), c("x", "y")]
#   attacker_pos <- current_data[which(current_data2$nflId == attacker), c("x", "y")]
#   
#   # Linie zeichnen, falls beide gefunden wurden
#   if (nrow(defender_pos) > 0 && nrow(attacker_pos) > 0) {
#     lines(
#       x = c(defender_pos$x, attacker_pos$x),
#       y = c(defender_pos$y, attacker_pos$y),
#       col = "#00000070", lwd = 2
#     )
#   }
# }
# all players
points(current_data$x, current_data$y,
       pch = c(16, 18, 16)[as.factor(current_data$club)],
       col = team_colors[as.factor(current_data$club)], cex = 1.5)
# only KC
points(current_data$x[which(current_data$club == "KC")], 
       current_data$y[which(current_data$club == "KC")],
       pch = 1, lwd = 1.5,
       col = "#CA2430", cex = 1.5)

# football
points(current_data$x[which(current_data$club == "football")],
       current_data$y[which(current_data$club == "football")],
       pch = 18, col = "orange", cex = 1.2, lwd = 2)
points(current_data$x[which(current_data$club == "football")],
       current_data$y[which(current_data$club == "football")],
       pch = 5, col = "black", cex = 1.2, lwd = 2)

text(los + 3.5, ylim[1] + 4, "LOS", col = "#00000080", lwd = 2, cex = 0.9)

# Box mit Spiel-Infos unten drunter
br = mean(xlim)
rect(xlim[1], ylim[1] - 8, xlim[2], ylim[1], 
     col = "gray80", border = "black", lwd = 2)
# Spielstand
text(x = mean(xlim), y = ylim[1] - 2.5, cex = 1,
     labels = "0:0", col = "black", lwd = 2)
# teams
text(x = mean(c(xlim[1], br)) + 4, y = ylim[1] - 2.5, 
     labels = clubs[1], col = team_colors[1], lwd = 2, cex = 1)
text(x = mean(c(br, xlim[2])) - 4, y = ylim[1] - 2.5, 
     labels = clubs[2], col = team_colors[3], lwd = 2, cex = 1)
# time
text(x = mean(xlim), y = ylim[1] - 5.7, cex = 0.7,
     labels = paste(
       "1st", "9:43", "-", "2nd & GOAL"
     ), col = "black", lwd = 2)

# Spielfeld-Abmessungen (angepasst an Football)
field_length <- 120
field_width <- 53.3

pitch_colors = c("#a4e86f", "#7eb356")
team_colors = c("#97233F", "white")#"blue4")
los = 19

clubs = levels(as.factor(tracking_kc_az$club))[levels(as.factor(tracking_kc_az$club)) != "football"]

xlim = round(c(min(tracking_kc_az$x) - 8, 
               max(tracking_kc_az$x)+5), - 1)
xlim[2] = 50
# ylim = round(c(min(tracking_kc_az$y)-1, max(tracking_kc_az$y)+1), -1)
ylim = c(0, field_width)
asp_ratio =  diff(xlim) / diff(ylim)

xlabs = c("10", "20", "30", "40", "50", "40", "30", "20", "10")

imInd = 1


t = unique(tracking_kc_az$time)[1] 
# Daten für den aktuellen Zeitpunkt filtern
current_data <- tracking_kc_az[tracking_kc_az$time == t, ]
current_data = current_data |> 
  left_join(players %>% select(nflId, displayName, position), by = "nflId")

current_data2 = current_data |> filter(position == "QB" | position == "C" | 
                                         position == "G" | position == "T")

current_data = current_data |> filter(position != "QB", position != "C", 
                                      position != "G", position != "T")


# Spielfeld zeichnen
plot(
  NA, 
  xlim = xlim, 
  ylim = c(ylim[1]-8, ylim[2]), 
  xlab = "", ylab = "",
  xaxt = "n", yaxt = "n",
  bty = "n",
  xpd = T
)

# draw the endzones if necessary
if(xlim[1] < 10){
  rect(xlim[1], ylim[1], 10, ylim[2], 
       col = colorspace::lighten(team_colors[1], 0.3),
       border = NA)
  text(5, mean(ylim), "ARIZONA", srt = 90, 
       col = team_colors[1], cex = 2.25)
}
if(xlim[2] > 110){
  rect(110, ylim[1], xlim[2], ylim[2], 
       col = colorspace::lighten(team_colors[2], 0.3),
       border = NA)
}

# draw the field and labels
i = 10
k = 1
while(i < xlim[2]){
  if(i < xlim[2]){
    right = i+5
  } else{
    right = xlim[2]
  }
  rect(i, ylim[1], right, ylim[2], 
       col = colorspace::lighten(pitch_colors[i %% 2 + 1], 0.6),
       border = NA)
  
  if(i >= 20 & i %% 10 == 0 & i < 100){
    text(i, ylim[2] - 4, xlabs[k], col = "#00000080", lwd = 2, cex = 0.9)
    k = k + 1
  }
  
  i = i + 5
}

# draw the pitch border
segments(xlim[1], 0, xlim[2], 0, lwd = 2)
segments(xlim[1], field_width, xlim[2], field_width, lwd = 2)
segments(xlim[1], 0, xlim[1], field_width, lwd = 2)

# Spielerpositionen hinzufügen
# abline(v = los, lwd = 2, col = "#00000080", lty = 2)
segments(los, ylim[1], los, ylim[2], col = "#00000080", lwd = 2)
#text(current_data$x, current_data$y, labels = current_data$nflId, pos = 3, col = "black")

# # Verbindungslinien basierend auf Wahrscheinlichkeiten hinzufügen
# defender_probs <- defender_data[defender_data$time == t, ]#[[t %% length(defender_data) + 1]] # Beispiel: Wahrscheinlichkeiten für diesen Zeitpunkt
# for (i in 1:nrow(defender_probs)) {
#   defender <- defender_probs$nflId[i]
#   # Maximale Wahrscheinlichkeit für diesen Verteidiger
#   max_prob <- which.max(defender_probs[i, 21:25])
#   attacker <- as.numeric(str_sub(names(defender_probs)[max_prob + 20], 2, 6))
#   
#   current_data2 = current_data %>% filter(!is.na(nflId))
#   # Koordinaten von Verteidiger und Angreifer finden
#   defender_pos <- current_data[which(current_data2$nflId == defender), c("x", "y")]
#   attacker_pos <- current_data[which(current_data2$nflId == attacker), c("x", "y")]
#   
#   # Linie zeichnen, falls beide gefunden wurden
#   if (nrow(defender_pos) > 0 && nrow(attacker_pos) > 0) {
#     lines(
#       x = c(defender_pos$x, attacker_pos$x),
#       y = c(defender_pos$y, attacker_pos$y),
#       col = "#00000070", lwd = 2
#     )
#   }
# }
points(current_data2$x, current_data2$y,
       pch = 1, lwd = 0.3,
       #       pch = c(16, 18, 16)[as.factor(current_data2$club)],
       col = "gray94", cex = 1.5)

# all players
points(current_data$x, current_data$y,
       pch = c(16, 18, 16)[as.factor(current_data$club)],
       col = team_colors[as.factor(current_data$club)], cex = 1.5)

# only KC
points(current_data$x[which(current_data$club == "KC")], 
       current_data$y[which(current_data$club == "KC")],
       pch = 1, lwd = 1.5,
       col = "#CA2430", cex = 1.5)

# football
points(current_data$x[which(current_data$club == "football")],
       current_data$y[which(current_data$club == "football")],
       pch = 18, col = "orange", cex = 1.2, lwd = 2)
points(current_data$x[which(current_data$club == "football")],
       current_data$y[which(current_data$club == "football")],
       pch = 5, col = "black", cex = 1.2, lwd = 2)

text(los + 3.5, ylim[1] + 4, "LOS", col = "#00000080", lwd = 2, cex = 0.9)

# Box mit Spiel-Infos unten drunter
br = mean(xlim)
rect(xlim[1], ylim[1] - 8, xlim[2], ylim[1], 
     col = "gray80", border = "black", lwd = 2)
# Spielstand
text(x = mean(xlim), y = ylim[1] - 2.5, cex = 1,
     labels = "0:0", col = "black", lwd = 2)
# teams
text(x = mean(c(xlim[1], br)) + 4, y = ylim[1] - 2.5, 
     labels = clubs[1], col = team_colors[1], lwd = 2, cex = 1)
text(x = mean(c(br, xlim[2])) - 4, y = ylim[1] - 2.5, 
     labels = clubs[2], col = team_colors[3], lwd = 2, cex = 1)
# time
text(x = mean(xlim), y = ylim[1] - 5.7, cex = 0.7,
     labels = paste(
       "1st", "9:43", "-", "2nd & GOAL"
     ), col = "black", lwd = 2)

# Spielfeld-Abmessungen (angepasst an Football)
field_length <- 120
field_width <- 53.3

pitch_colors = c("#a4e86f", "#7eb356")
team_colors = c("#97233F", "white")#"blue4")
los = 19

clubs = levels(as.factor(tracking_kc_az$club))[levels(as.factor(tracking_kc_az$club)) != "football"]

xlim = round(c(min(tracking_kc_az$x) - 8, 
               max(tracking_kc_az$x)+5), - 1)
xlim[2] = 50
# ylim = round(c(min(tracking_kc_az$y)-1, max(tracking_kc_az$y)+1), -1)
ylim = c(0, field_width)
asp_ratio =  diff(xlim) / diff(ylim)

xlabs = c("10", "20", "30", "40", "50", "40", "30", "20", "10")

imInd = 1


t = unique(tracking_kc_az$time)[1] 
# Daten für den aktuellen Zeitpunkt filtern
current_data <- tracking_kc_az[tracking_kc_az$time == t, ]
current_data = current_data |> 
  left_join(players %>% select(nflId, displayName, position), by = "nflId")

current_data2 = current_data |> filter(position == "QB" | position == "C" | 
                                         position == "G" | position == "T" |
                                         position == "DT" | position == "DE" |
                                         position == "NT")

current_data = current_data |> filter(position != "QB", position != "C", 
                                      position != "G", position != "T",
                                      position != "DT", position != "DE",
                                      position != "NT") 

# Spielfeld zeichnen
plot(
  NA, 
  xlim = xlim, 
  ylim = c(ylim[1]-8, ylim[2]), 
  xlab = "", ylab = "",
  xaxt = "n", yaxt = "n",
  bty = "n",
  xpd = T
)

# draw the endzones if necessary
if(xlim[1] < 10){
  rect(xlim[1], ylim[1], 10, ylim[2], 
       col = colorspace::lighten(team_colors[1], 0.3),
       border = NA)
  text(5, mean(ylim), "ARIZONA", srt = 90, 
       col = team_colors[1], cex = 2.25)
}
if(xlim[2] > 110){
  rect(110, ylim[1], xlim[2], ylim[2], 
       col = colorspace::lighten(team_colors[2], 0.3),
       border = NA)
}

# draw the field and labels
i = 10
k = 1
while(i < xlim[2]){
  if(i < xlim[2]){
    right = i+5
  } else{
    right = xlim[2]
  }
  rect(i, ylim[1], right, ylim[2], 
       col = colorspace::lighten(pitch_colors[i %% 2 + 1], 0.6),
       border = NA)
  
  if(i >= 20 & i %% 10 == 0 & i < 100){
    text(i, ylim[2] - 4, xlabs[k], col = "#00000080", lwd = 2, cex = 0.9)
    k = k + 1
  }
  
  i = i + 5
}

# draw the pitch border
segments(xlim[1], 0, xlim[2], 0, lwd = 2)
segments(xlim[1], field_width, xlim[2], field_width, lwd = 2)
segments(xlim[1], 0, xlim[1], field_width, lwd = 2)

# Spielerpositionen hinzufügen
# abline(v = los, lwd = 2, col = "#00000080", lty = 2)
segments(los, ylim[1], los, ylim[2], col = "#00000080", lwd = 2)
#text(current_data$x, current_data$y, labels = current_data$nflId, pos = 3, col = "black")

# # Verbindungslinien basierend auf Wahrscheinlichkeiten hinzufügen
# defender_probs <- defender_data[defender_data$time == t, ]#[[t %% length(defender_data) + 1]] # Beispiel: Wahrscheinlichkeiten für diesen Zeitpunkt
# for (i in 1:nrow(defender_probs)) {
#   defender <- defender_probs$nflId[i]
#   # Maximale Wahrscheinlichkeit für diesen Verteidiger
#   max_prob <- which.max(defender_probs[i, 21:25])
#   attacker <- as.numeric(str_sub(names(defender_probs)[max_prob + 20], 2, 6))
#   
#   current_data2 = current_data %>% filter(!is.na(nflId))
#   # Koordinaten von Verteidiger und Angreifer finden
#   defender_pos <- current_data[which(current_data2$nflId == defender), c("x", "y")]
#   attacker_pos <- current_data[which(current_data2$nflId == attacker), c("x", "y")]
#   
#   # Linie zeichnen, falls beide gefunden wurden
#   if (nrow(defender_pos) > 0 && nrow(attacker_pos) > 0) {
#     lines(
#       x = c(defender_pos$x, attacker_pos$x),
#       y = c(defender_pos$y, attacker_pos$y),
#       col = "#00000070", lwd = 2
#     )
#   }
# }
points(current_data2$x, current_data2$y,
       pch = 1, lwd = 0.3,
       #       pch = c(16, 18, 16)[as.factor(current_data2$club)],
       col = "gray94", cex = 1.5)
# all players
points(current_data$x, current_data$y,
       pch = c(16, 18, 16)[as.factor(current_data$club)],
       col = team_colors[as.factor(current_data$club)], cex = 1.5)
# only KC
points(current_data$x[which(current_data$club == "KC")], 
       current_data$y[which(current_data$club == "KC")],
       pch = 1, lwd = 1.5,
       col = "#CA2430", cex = 1.5)

# football
points(current_data$x[which(current_data$club == "football")],
       current_data$y[which(current_data$club == "football")],
       pch = 18, col = "orange", cex = 1.2, lwd = 2)
points(current_data$x[which(current_data$club == "football")],
       current_data$y[which(current_data$club == "football")],
       pch = 5, col = "black", cex = 1.2, lwd = 2)

text(los + 3.5, ylim[1] + 4, "LOS", col = "#00000080", lwd = 2, cex = 0.9)

# Box mit Spiel-Infos unten drunter
br = mean(xlim)
rect(xlim[1], ylim[1] - 8, xlim[2], ylim[1], 
     col = "gray80", border = "black", lwd = 2)
# Spielstand
text(x = mean(xlim), y = ylim[1] - 2.5, cex = 1,
     labels = "0:0", col = "black", lwd = 2)
# teams
text(x = mean(c(xlim[1], br)) + 4, y = ylim[1] - 2.5, 
     labels = clubs[1], col = team_colors[1], lwd = 2, cex = 1)
text(x = mean(c(br, xlim[2])) - 4, y = ylim[1] - 2.5, 
     labels = clubs[2], col = team_colors[3], lwd = 2, cex = 1)
# time
text(x = mean(xlim), y = ylim[1] - 5.7, cex = 0.7,
     labels = paste(
       "1st", "9:43", "-", "2nd & GOAL"
     ), col = "black", lwd = 2)

# Spielfeld-Abmessungen (angepasst an Football)
field_length <- 120
field_width <- 53.3

pitch_colors = c("#a4e86f", "#7eb356")
team_colors = c("#97233F", "white")#"blue4")
los = 19

clubs = levels(as.factor(tracking_kc_az$club))[levels(as.factor(tracking_kc_az$club)) != "football"]

xlim = round(c(min(tracking_kc_az$x) - 8, 
               max(tracking_kc_az$x)+5), - 1)
xlim[2] = 50
# ylim = round(c(min(tracking_kc_az$y)-1, max(tracking_kc_az$y)+1), -1)
ylim = c(0, field_width)
asp_ratio =  diff(xlim) / diff(ylim)

xlabs = c("10", "20", "30", "40", "50", "40", "30", "20", "10")

imInd = 1


t = unique(tracking_kc_az$time)[1] 
# Daten für den aktuellen Zeitpunkt filtern
current_data <- tracking_kc_az[tracking_kc_az$time == t, ]
current_data = current_data |> 
  left_join(players %>% select(nflId, displayName, position), by = "nflId")

current_data2 = current_data |> filter(position == "QB" | position == "C" | 
                                         position == "G" | position == "T" |
                                         position == "DT" | position == "DE" |
                                         position == "NT" | position == "OLB" |
                                         position == "SS")

current_data = current_data |> filter(position != "QB", position != "C", 
                                      position != "G", position != "T",
                                      position != "DT", position != "DE",
                                      position != "NT", position != "OLB",
                                      position != "SS") 

# Spielfeld zeichnen
plot(
  NA, 
  xlim = xlim, 
  ylim = c(ylim[1]-8, ylim[2]), 
  xlab = "", ylab = "",
  xaxt = "n", yaxt = "n",
  bty = "n",
  xpd = T
)

# draw the endzones if necessary
if(xlim[1] < 10){
  rect(xlim[1], ylim[1], 10, ylim[2], 
       col = colorspace::lighten(team_colors[1], 0.3),
       border = NA)
  text(5, mean(ylim), "ARIZONA", srt = 90, 
       col = team_colors[1], cex = 2.25)
}
if(xlim[2] > 110){
  rect(110, ylim[1], xlim[2], ylim[2], 
       col = colorspace::lighten(team_colors[2], 0.3),
       border = NA)
}

# draw the field and labels
i = 10
k = 1
while(i < xlim[2]){
  if(i < xlim[2]){
    right = i+5
  } else{
    right = xlim[2]
  }
  rect(i, ylim[1], right, ylim[2], 
       col = colorspace::lighten(pitch_colors[i %% 2 + 1], 0.6),
       border = NA)
  
  if(i >= 20 & i %% 10 == 0 & i < 100){
    text(i, ylim[2] - 4, xlabs[k], col = "#00000080", lwd = 2, cex = 0.9)
    k = k + 1
  }
  
  i = i + 5
}

# draw the pitch border
segments(xlim[1], 0, xlim[2], 0, lwd = 2)
segments(xlim[1], field_width, xlim[2], field_width, lwd = 2)
segments(xlim[1], 0, xlim[1], field_width, lwd = 2)

# Spielerpositionen hinzufügen
# abline(v = los, lwd = 2, col = "#00000080", lty = 2)
segments(los, ylim[1], los, ylim[2], col = "#00000080", lwd = 2)
#text(current_data$x, current_data$y, labels = current_data$nflId, pos = 3, col = "black")

# # Verbindungslinien basierend auf Wahrscheinlichkeiten hinzufügen
# defender_probs <- defender_data[defender_data$time == t, ]#[[t %% length(defender_data) + 1]] # Beispiel: Wahrscheinlichkeiten für diesen Zeitpunkt
# for (i in 1:nrow(defender_probs)) {
#   defender <- defender_probs$nflId[i]
#   # Maximale Wahrscheinlichkeit für diesen Verteidiger
#   max_prob <- which.max(defender_probs[i, 21:25])
#   attacker <- as.numeric(str_sub(names(defender_probs)[max_prob + 20], 2, 6))
#   
#   current_data2 = current_data %>% filter(!is.na(nflId))
#   # Koordinaten von Verteidiger und Angreifer finden
#   defender_pos <- current_data[which(current_data2$nflId == defender), c("x", "y")]
#   attacker_pos <- current_data[which(current_data2$nflId == attacker), c("x", "y")]
#   
#   # Linie zeichnen, falls beide gefunden wurden
#   if (nrow(defender_pos) > 0 && nrow(attacker_pos) > 0) {
#     lines(
#       x = c(defender_pos$x, attacker_pos$x),
#       y = c(defender_pos$y, attacker_pos$y),
#       col = "#00000070", lwd = 2
#     )
#   }
# }
points(current_data2$x, current_data2$y,
       pch = 1, lwd = 0.3,
       #       pch = c(16, 18, 16)[as.factor(current_data2$club)],
       col = "gray94", cex = 1.5)
# all players
points(current_data$x, current_data$y,
       pch = c(16, 18, 16)[as.factor(current_data$club)],
       col = team_colors[as.factor(current_data$club)], cex = 1.5)
# only KC
points(current_data$x[which(current_data$club == "KC")], 
       current_data$y[which(current_data$club == "KC")],
       pch = 1, lwd = 1.5,
       col = "#CA2430", cex = 1.5)

# football
points(current_data$x[which(current_data$club == "football")],
       current_data$y[which(current_data$club == "football")],
       pch = 18, col = "orange", cex = 1.2, lwd = 2)
points(current_data$x[which(current_data$club == "football")],
       current_data$y[which(current_data$club == "football")],
       pch = 5, col = "black", cex = 1.2, lwd = 2)

text(los + 3.5, ylim[1] + 4, "LOS", col = "#00000080", lwd = 2, cex = 0.9)

# Box mit Spiel-Infos unten drunter
br = mean(xlim)
rect(xlim[1], ylim[1] - 8, xlim[2], ylim[1], 
     col = "gray80", border = "black", lwd = 2)
# Spielstand
text(x = mean(xlim), y = ylim[1] - 2.5, cex = 1,
     labels = "0:0", col = "black", lwd = 2)
# teams
text(x = mean(c(xlim[1], br)) + 4, y = ylim[1] - 2.5, 
     labels = clubs[1], col = team_colors[1], lwd = 2, cex = 1)
text(x = mean(c(br, xlim[2])) - 4, y = ylim[1] - 2.5, 
     labels = clubs[2], col = team_colors[3], lwd = 2, cex = 1)
# time
text(x = mean(xlim), y = ylim[1] - 5.7, cex = 0.7,
     labels = paste(
       "1st", "9:43", "-", "2nd & GOAL"
     ), col = "black", lwd = 2)

