library(readxl)  # pour lire fichier excel
library(janitor) # pour nettoyer les noms de colonnes
library(dplyr) # pour manipuler les données
library(tidyr)  # pour fonction fill vers le bas
library(stringr) # fonction pour jouer avec les chaînes de caractères
library(ggplot2)
ETP_par_CSS_xlsx <- readxl::read_xlsx(
  path = "data-raw/ETP_par_CSS.xlsx",
  skip = 3
)

ETP_par_CSS_raw  <- ETP_par_CSS_xlsx %>%
  janitor::clean_names()

ETP_par_CSS <- ETP_par_CSS_raw %>%
  dplyr::rename(secteur = x1) %>% # renommer la colonne x1 vers type
  dplyr::mutate(categorie = dplyr::if_else(condition= is.na(x2010_2011), true = secteur, false = NA_character_)) %>% # créer la colonne categorie, qui est seulement populée quand tu n'as pas de chiffre en 2010-2011
  tidyr::fill(categorie, .direction = "down") %>%  # remplir la colonne catégorie vers le bas
  select(secteur, categorie, contains("20")) %>% # garder seulement les colonne "secteur", "categorie" ainsi que celles qui contiennent le mot "20", comme dans 2010, 2011, 2012..
  filter(!is.na("secteur"), !stringr::str_detect(secteur, "Note"), !is.na(x2010_2011)) %>% # enlever les lignes qui n'ont pas de secteur, contiennent le mot note dans le secteur ou qui n'ont pas de valeur pour 2010_2011
  # ici on fait une manip parce que la dernière ligne a la categorie "Personnel de soutien" alors que ce devrait être grand total.
  mutate(is_last = if_else(condition = row_number() == nrow(.), true = 1 , false = 0),# créer la colonne is_last, qui identifie la derniere ligne, soit celle pour laquelle le numéro de ligne est égal au nombre de lignes dans le tableau
         categorie = if_else(is_last==1, "Total", categorie)) %>% # pour la derniere ligne, la catégorie est "total", "pas personnel de soutien"
  select(-is_last)  %>% # enlever la colonne is_last
  tidyr::pivot_longer( # allonger la base de données, car on  veut une ligne pour chaque année 
    cols = contains("20"),
    names_to = "annee",
    values_to = "personnel"
  ) %>%
  mutate(
    annee = as.integer(stringr::str_sub(annee,2,5)),   # on veut les caractères de 2 à 5 de ce qui était dans la colonne année, puis on convertit en integer
    personnel = as.numeric(personnel)
    
  )

readr::write_csv(ETP_par_CSS, "data/ETP_par_CSS.csv")

