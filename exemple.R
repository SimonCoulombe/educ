library(dplyr) # pour manipuler les données (filter les lignes, mutater et sélectionner les colonnes, etc)
library(readr) # pour importer le csv en mémoire
library(ggplot2) # pour créer des graphiques

ETP_par_CSS <- readr:::read_csv("data/ETP_par_CSS.csv")

ETP_par_CSS %>% 
  dplyr::filter(secteur != "Total") %>%   # on garde seulement les lignes où secteur n'est pas égal à "Total"
  ggplot(  # on commence un graphique
    data = .,   # on va utiliser les données qu'on vient de pousser
    aes(x= annee, y = personnel, color = secteur)) +  # 'on veut l'esthétique suivante:
   # année en x,  personnel en y, secteur en couleur
  facet_wrap(~ categorie)  +   # 
  geom_point() +   # on veut un point pour chaque observation
  geom_line() +  # on veut des lignes  qui relient les points de chaque couleur
  theme_bw() # on veut  le theme black & white, moins laid que le thème par défaut
