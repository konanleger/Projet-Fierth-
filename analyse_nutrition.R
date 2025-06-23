# ============================================================================
# ANALYSE NUTRITIONNELLE DES PRODUITS DE THÉ
# ============================================================================

# ----------------------------------------------------------------------------
# 1. INITIALISATION
# ----------------------------------------------------------------------------
# Nettoyage de l'environnement
rm(list = ls())

# Installation et importation des packages nécessaires
library(readxl)
library(tidyverse)
library(dplyr)
library(lubridate)
library(scales)
library(tm)
library(wordcloud)
library(tidytext)
library(stringr)

# ----------------------------------------------------------------------------
# 2. FONCTIONS UTILITAIRES
# ----------------------------------------------------------------------------
# Fonction améliorée pour extraire une valeur numérique avec regex
extract_nutrition_value <- function(text, pattern) {
  text <- tolower(text)
  matches <- str_match(text, pattern)
  # matches[,2] contient la valeur extraite selon le groupe de capture
  result <- suppressWarnings(as.numeric(matches[, 2]))
  return(result)
}

# ----------------------------------------------------------------------------
# 3. IMPORTATION ET PRÉPARATION DES DONNÉES
# ----------------------------------------------------------------------------
# Importation des données
gnpd_brut <- read_excel("GNPD_tea.xlsx")

# Nettoyage et extraction des valeurs nutritionnelles
gnpd <- gnpd_brut %>%
  mutate(
    # Nettoyage du texte Nutrition
    Nutrition = str_to_lower(Nutrition),
    Nutrition = str_replace_all(Nutrition, ",", "."),
    Nutrition = str_replace_all(Nutrition, "\\s+", " "),
    
    # Énergie (kcal)
    energy_kcal = case_when(
      str_detect(Nutrition, "energy.*kcal") ~ 
        extract_nutrition_value(Nutrition, "energy.*?(\\d+(\\.\\d+)?)\\s*kcal"),
      str_detect(Nutrition, "calories.*kcal") ~ 
        extract_nutrition_value(Nutrition, "calories.*?(\\d+(\\.\\d+)?)\\s*kcal"),
      TRUE ~ NA_real_
    ),
    
    # Énergie (kJ)
    energy_kj = case_when(
      str_detect(Nutrition, "energy.*kj") ~ 
        extract_nutrition_value(Nutrition, "energy.*?(\\d+(\\.\\d+)?)\\s*kj"),
      TRUE ~ NA_real_
    ),
    
    # Conversion kJ en kcal si kcal manquant
    energy = case_when(
      !is.na(energy_kcal) ~ energy_kcal,
      !is.na(energy_kj) ~ energy_kj / 4.184,
      TRUE ~ NA_real_
    ),
    
    # Protéines (g)
    proteins = case_when(
      str_detect(Nutrition, "protein.*?\\d+(\\.\\d+)?\\s*g") ~ 
        extract_nutrition_value(Nutrition, "protein.*?(\\d+(\\.\\d+)?)\\s*g"),
      str_detect(Nutrition, "protein.*?\\d+(\\.\\d+)?\\s*%") ~ 
        extract_nutrition_value(Nutrition, "protein.*?(\\d+(\\.\\d+)?)\\s*%") * 0.1,
      TRUE ~ NA_real_
    ),
    
    # Lipides (g)
    fats = case_when(
      str_detect(Nutrition, "fat content.*?\\d+(\\.\\d+)?\\s*g") ~ 
        extract_nutrition_value(Nutrition, "fat content.*?(\\d+(\\.\\d+)?)\\s*g"),
      str_detect(Nutrition, "fat.*?\\d+(\\.\\d+)?\\s*g") ~ 
        extract_nutrition_value(Nutrition, "fat.*?(\\d+(\\.\\d+)?)\\s*g"),
      str_detect(Nutrition, "fat.*?\\d+(\\.\\d+)?\\s*%") ~ 
        extract_nutrition_value(Nutrition, "fat.*?(\\d+(\\.\\d+)?)\\s*%") * 0.1,
      TRUE ~ NA_real_
    ),
    
    # Glucides (g)
    carbs = case_when(
      str_detect(Nutrition, "carbohydrate.*?\\d+(\\.\\d+)?\\s*g") ~ 
        extract_nutrition_value(Nutrition, "carbohydrate.*?(\\d+(\\.\\d+)?)\\s*g"),
      str_detect(Nutrition, "carbohydrate.*?\\d+(\\.\\d+)?\\s*%") ~ 
        extract_nutrition_value(Nutrition, "carbohydrate.*?(\\d+(\\.\\d+)?)\\s*%") * 0.1,
      TRUE ~ NA_real_
    ),
    
    # Sucres (g)
    sugars = case_when(
      str_detect(Nutrition, "sugars.*?\\d+(\\.\\d+)?\\s*g") ~ 
        extract_nutrition_value(Nutrition, "sugars.*?(\\d+(\\.\\d+)?)\\s*g"),
      str_detect(Nutrition, "sugar.*?\\d+(\\.\\d+)?\\s*g") ~ 
        extract_nutrition_value(Nutrition, "sugar.*?(\\d+(\\.\\d+)?)\\s*g"),
      TRUE ~ NA_real_
    ),
    
    # Sel/Sodium (g)
    salt = case_when(
      str_detect(Nutrition, "salt.*?\\d+(\\.\\d+)?\\s*g") ~ 
        extract_nutrition_value(Nutrition, "salt.*?(\\d+(\\.\\d+)?)\\s*g"),
      str_detect(Nutrition, "sodium.*?\\d+(\\.\\d+)?\\s*mg") ~ 
        extract_nutrition_value(Nutrition, "sodium.*?(\\d+(\\.\\d+)?)\\s*mg") / 1000,
      TRUE ~ NA_real_
    ),
    
    # Fibres (g)
    fiber = case_when(
      str_detect(Nutrition, "fiber.*?\\d+(\\.\\d+)?\\s*g") ~ 
        extract_nutrition_value(Nutrition, "fiber.*?(\\d+(\\.\\d+)?)\\s*g"),
      str_detect(Nutrition, "fibre.*?\\d+(\\.\\d+)?\\s*g") ~ 
        extract_nutrition_value(Nutrition, "fibre.*?(\\d+(\\.\\d+)?)\\s*g"),
      str_detect(Nutrition, "dietary fibre.*?\\d+(\\.\\d+)?\\s*g") ~ 
        extract_nutrition_value(Nutrition, "dietary fibre.*?(\\d+(\\.\\d+)?)\\s*g"),
      str_detect(Nutrition, "crude fiber.*?\\d+(\\.\\d+)?\\s*%") ~ 
        extract_nutrition_value(Nutrition, "crude fiber.*?(\\d+(\\.\\d+)?)\\s*%") * 0.1,
      TRUE ~ NA_real_
    ),
    
    # Vitamines (mg)
    vitamins = case_when(
      str_detect(Nutrition, "vitamin.*?\\d+(\\.\\d+)?\\s*mg") ~ 
        extract_nutrition_value(Nutrition, "vitamin.*?(\\d+(\\.\\d+)?)\\s*mg"),
      str_detect(Nutrition, "vitamin.*?\\d+(\\.\\d+)?\\s*mcg") ~ 
        extract_nutrition_value(Nutrition, "vitamin.*?(\\d+(\\.\\d+)?)\\s*mcg") / 1000,
      str_detect(Nutrition, "vitamin.*?\\d+(\\.\\d+)?\\s*µg") ~ 
        extract_nutrition_value(Nutrition, "vitamin.*?(\\d+(\\.\\d+)?)\\s*µg") / 1000,
      TRUE ~ NA_real_
    )
  )

# ----------------------------------------------------------------------------
# 4. VÉRIFICATION DES DONNÉES
# ----------------------------------------------------------------------------
# Vérification des valeurs extraites
print("Nombre de produits avec des valeurs nutritionnelles :")
print(paste("Énergie :", sum(!is.na(gnpd$energy))))
print(paste("Protéines :", sum(!is.na(gnpd$proteins))))
print(paste("Lipides :", sum(!is.na(gnpd$fats))))
print(paste("Glucides :", sum(!is.na(gnpd$carbs))))
print(paste("Sucres :", sum(!is.na(gnpd$sugars))))
print(paste("Sel :", sum(!is.na(gnpd$salt))))
print(paste("Fibres :", sum(!is.na(gnpd$fiber))))
print(paste("Vitamines :", sum(!is.na(gnpd$vitamins))))

# Afficher quelques exemples pour vérification
print("Exemples d'extraction :")
print(head(gnpd %>% select(Nutrition, energy, proteins, fats, carbs, sugars, salt, fiber, vitamins)))

# ----------------------------------------------------------------------------
# 5. ANALYSES STATISTIQUES
# ----------------------------------------------------------------------------
# Statistiques descriptives globales
summary_stats <- gnpd %>%
  summarise(
    across(
      c(energy, proteins, fats, carbs, sugars, salt, fiber, vitamins),
      list(
        mean = ~mean(., na.rm = TRUE),
        median = ~median(., na.rm = TRUE),
        sd = ~sd(., na.rm = TRUE),
        n = ~sum(!is.na(.))
      )
    )
  )

# Analyse par catégorie
nutrition_par_categorie <- gnpd %>%
  group_by(`Sub-Category`) %>%
  summarise(
    n = n(),
    mean_energy = mean(energy, na.rm = TRUE),
    mean_proteins = mean(proteins, na.rm = TRUE),
    mean_fats = mean(fats, na.rm = TRUE),
    mean_carbs = mean(carbs, na.rm = TRUE),
    mean_sugars = mean(sugars, na.rm = TRUE),
    mean_salt = mean(salt, na.rm = TRUE),
    mean_fiber = mean(fiber, na.rm = TRUE)
  ) %>%
  filter(n >= 30) %>%
  arrange(desc(n))

# Analyse de l'évolution temporelle
evolution_nutrition <- gnpd %>%
  group_by(year = year(`Date Published`)) %>%
  summarise(
    mean_energy = mean(energy, na.rm = TRUE),
    mean_sugar = mean(sugars, na.rm = TRUE),
    mean_salt = mean(salt, na.rm = TRUE),
    n = n()
  )

# Analyse des combinaisons nutritionnelles
combinaisons_nutrition <- gnpd %>%
  filter(!is.na(energy) & !is.na(sugars)) %>%
  mutate(
    categorie_energie = cut(energy, 
                           breaks = c(-Inf, 50, 100, 150, Inf),
                           labels = c("Très faible", "Faible", "Modéré", "Élevé")),
    categorie_sucre = cut(sugars,
                         breaks = c(-Inf, 5, 10, 15, Inf),
                         labels = c("Très faible", "Faible", "Modéré", "Élevé"))
  ) %>%
  group_by(categorie_energie, categorie_sucre) %>%
  summarise(n = n()) %>%
  arrange(desc(n))

# ----------------------------------------------------------------------------
# 6. VISUALISATIONS
# ----------------------------------------------------------------------------
# Distribution des calories par catégorie
ggplot(gnpd %>% filter(!is.na(energy)), 
       aes(x = `Sub-Category`, y = energy)) +
  geom_boxplot(fill = "lightblue", alpha = 0.7) +
  coord_flip() +
  labs(
    title = "Distribution des calories par catégorie de thé",
    x = "Catégorie",
    y = "Calories (kcal)"
  ) +
  theme_minimal()

# Évolution des valeurs nutritionnelles
ggplot(evolution_nutrition, aes(x = year)) +
  geom_line(aes(y = mean_energy, color = "Calories"), size = 1) +
  geom_line(aes(y = mean_sugar, color = "Sucre"), size = 1) +
  geom_line(aes(y = mean_salt * 100, color = "Sel"), size = 1) +
  labs(
    title = "Évolution des valeurs nutritionnelles moyennes",
    x = "Année",
    y = "Valeur moyenne",
    color = "Nutriment"
  ) +
  scale_color_manual(values = c("Calories" = "red", "Sucre" = "blue", "Sel" = "green")) +
  theme_minimal()

