# ============================================================================
# ANALYSE TEXTUELLE DES DESCRIPTIONS ET INGRÉDIENTS
# ============================================================================

# ----------------------------------------------------------------------------
# 1. INITIALISATION
# ----------------------------------------------------------------------------
# Nettoyage de l'environnement
rm(list = ls())
gc() # Nettoyage de la mémoire

# Installation et importation des packages nécessaires
library(readxl)
library(tidyverse)
library(tm)
library(wordcloud)
library(tidytext)
library(RColorBrewer)
library(ggplot2)
library(scales)

# ----------------------------------------------------------------------------
# 2. IMPORTATION DES DONNÉES
# ----------------------------------------------------------------------------
# Importation des données
gnpd_brut <- read_excel("GNPD_tea.xlsx")


gnpd_brut <- gnpd_brut %>%
  mutate(
    `Price per 100 g/ml in Euros` = as.numeric(`Price per 100 g/ml in Euros`),
    `Price in US Dollars` = as.numeric(`Price in US Dollars`),
    `Price in Euros` = as.numeric(`Price in Euros`),
    `Unit Pack Size (ml/g)` = as.numeric(`Unit Pack Size (ml/g)`),
    `Alcohol By Volume (%)` = as.numeric(`Alcohol By Volume (%)`),
    `Bar Code` = as.character(`Bar Code`),
    `Date Published` = as.Date(`Date Published`)  
  )

# ----------------------------------------------------------------------------
# 3. FONCTIONS UTILITAIRES
# ----------------------------------------------------------------------------
# Fonction pour nettoyer le texte
clean_text <- function(text) {
  # Gestion des valeurs NA
  text[is.na(text)] <- ""
  
  # Conversion en minuscules
  text <- tolower(text)
  # Suppression des caractères spéciaux
  text <- gsub("[^[:alnum:][:space:]]", " ", text)
  # Suppression des espaces multiples
  text <- gsub("\\s+", " ", text)
  # Suppression des espaces en début et fin
  text <- trimws(text)
  return(text)
}

# ----------------------------------------------------------------------------
# 4. ANALYSE DES DESCRIPTIONS DE PRODUITS
# ----------------------------------------------------------------------------
# Nettoyage des descriptions
descriptions_clean <- gnpd_brut %>%
  filter(!is.na(`Product Description`)) %>%
  mutate(description_clean = clean_text(`Product Description`)) %>%
  filter(description_clean != "") # Suppression des descriptions vides

# Analyse directe avec tidytext
word_freq_desc <- descriptions_clean %>%
  unnest_tokens(word, description_clean) %>%
  anti_join(stop_words) %>% # Suppression des mots vides
  count(word, sort = TRUE) %>%
  filter(n >= 5) # Filtrage des mots rares

# Analyse des prix par mot dans les descriptions
price_by_word_desc <- descriptions_clean %>%
  unnest_tokens(word, description_clean) %>%
  anti_join(stop_words) %>%
  filter(!is.na(`Price per 100 g/ml in Euros`)) %>%
  group_by(word) %>%
  summarise(
    n = n(),
    prix_moyen = mean(`Price per 100 g/ml in Euros`, na.rm = TRUE),
    prix_median = median(`Price per 100 g/ml in Euros`, na.rm = TRUE)
  ) %>%
  filter(n >= 5) %>%
  arrange(desc(prix_moyen))

# Visualisation des 20 mots avec les prix moyens les plus élevés
top_price_words_desc <- price_by_word_desc %>%
  head(20) %>%
  mutate(word = reorder(word, prix_moyen))

ggplot(top_price_words_desc, aes(x = word, y = prix_moyen)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  coord_flip() +
  labs(title = "Top 20 des mots associés aux prix moyens les plus élevés",
       x = "Mots",
       y = "Prix moyen") +
  theme_minimal()
#ggsave("top_prix_mots_descriptions.png", width = 10, height = 8)

# Nuage de mots pour les descriptions
#png("nuage_mots_descriptions.png", width = 800, height = 800)
wordcloud(words = word_freq_desc$word, 
          freq = word_freq_desc$n,
          min.freq = 5,
          max.words = 100,
          random.order = FALSE,
          colors = brewer.pal(8, "Dark2"))
#dev.off()

# Top 20 des mots les plus fréquents dans les descriptions
top_words_desc <- word_freq_desc %>%
  head(20) %>%
  mutate(word = reorder(word, n))

ggplot(top_words_desc, aes(x = word, y = n)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  coord_flip() +
  labs(title = "Top 20 des mots les plus fréquents dans les descriptions",
       x = "Mots",
       y = "Fréquence") +
  theme_minimal()
#ggsave("top_mots_descriptions.png", width = 10, height = 8)

# ----------------------------------------------------------------------------
# 5. ANALYSE DES INGRÉDIENTS
# ----------------------------------------------------------------------------
# Nettoyage des ingrédients
ingredients_clean <- gnpd_brut %>%
  filter(!is.na(`Ingredients (Standard form)`)) %>%
  mutate(ingredients_clean = clean_text(`Ingredients (Standard form)`)) %>%
  filter(ingredients_clean != "") # Suppression des ingrédients vides

# Analyse directe avec tidytext
word_freq_ing <- ingredients_clean %>%
  unnest_tokens(word, ingredients_clean) %>%
  anti_join(stop_words) %>% # Suppression des mots vides
  count(word, sort = TRUE) %>%
  filter(n >= 5) # Filtrage des mots rares

# Analyse des prix par mot dans les ingrédients
price_by_word_ing <- ingredients_clean %>%
  unnest_tokens(word, ingredients_clean) %>%
  anti_join(stop_words) %>%
  filter(!is.na(`Price per 100 g/ml in Euros`)) %>%
  group_by(word) %>%
  summarise(
    n = n(),
    prix_moyen = mean(`Price per 100 g/ml in Euros`, na.rm = TRUE),
    prix_median = median(`Price per 100 g/ml in Euros`, na.rm = TRUE)
  ) %>%
  filter(n >= 5) %>%
  arrange(desc(prix_moyen))

# Visualisation des 20 mots avec les prix moyens les plus élevés
top_price_words_ing <- price_by_word_ing %>%
  head(20) %>%
  mutate(word = reorder(word, prix_moyen))

ggplot(top_price_words_ing, aes(x = word, y = prix_moyen)) +
  geom_bar(stat = "identity", fill = "darkgreen") +
  coord_flip() +
  labs(title = "Top 20 des ingrédients associés aux prix moyens les plus élevés",
       x = "Ingrédients",
       y = "Prix moyen") +
  theme_minimal()
#ggsave("top_prix_mots_ingredients.png", width = 10, height = 8)

# Nuage de mots pour les ingrédients
#png("nuage_mots_ingredients.png", width = 800, height = 800)
wordcloud(words = word_freq_ing$word, 
          freq = word_freq_ing$n,
          min.freq = 5,
          max.words = 100,
          random.order = FALSE,
          colors = brewer.pal(8, "Dark2"))
#dev.off()

# Top 20 des mots les plus fréquents dans les ingrédients
top_words_ing <- word_freq_ing %>%
  head(20) %>%
  mutate(word = reorder(word, n))

ggplot(top_words_ing, aes(x = word, y = n)) +
  geom_bar(stat = "identity", fill = "darkgreen") +
  coord_flip() +
  labs(title = "Top 20 des mots les plus fréquents dans les ingrédients",
       x = "Mots",
       y = "Fréquence") +
  theme_minimal()
#ggsave("top_mots_ingredients.png", width = 10, height = 8)

# ----------------------------------------------------------------------------
# 6. EXPORTATION DES RÉSULTATS
# ----------------------------------------------------------------------------
# Affichage des statistiques
print("Nombre total de descriptions analysées :")
print(nrow(descriptions_clean))
print("Nombre total d'ingrédients analysés :")
print(nrow(ingredients_clean))
print("Nombre de mots uniques dans les descriptions :")
print(nrow(word_freq_desc))
print("Nombre de mots uniques dans les ingrédients :")
print(nrow(word_freq_ing))
print("\nTop 5 des mots avec les prix moyens les plus élevés dans les descriptions :")
print(head(price_by_word_desc, 5))
print("\nTop 5 des mots avec les prix moyens les plus élevés dans les ingrédients :")
print(head(price_by_word_ing, 5)) 