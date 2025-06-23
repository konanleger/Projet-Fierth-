# Analyse de la durabilité dans le secteur du thé
# Chargement des packages nécessaires
library(tidyverse)
library(lubridate)
library(scales)
library(tm)
library(wordcloud)
library(tidytext)

# Importation des données
gnpd <- read_excel("GNPD_tea.xlsx")

# Création d'un dictionnaire de labels et certifications durables
labels_durables <- c(
  "organic", "bio", "fair trade", "fairtrade", "rainforest alliance",
  "utz", "ethical", "sustainable", "eco-friendly", "green",
  "certified", "certification", "label", "agriculture biologique",
  "commerce équitable", "durable", "responsable"
)

# Fonction pour détecter les labels dans les claims
detect_labels <- function(text) {
  if(is.na(text)) return(NA)
  text <- tolower(text)
  any(sapply(labels_durables, function(label) grepl(label, text)))
}

# Ajout d'une colonne pour les produits labellisés
gnpd <- gnpd %>%
  mutate(
    has_label = sapply(`Positioning Claims`, detect_labels),
    year = year(`Date Published`),
    price_per_100 = as.numeric(`Price per 100 g/ml in Euros`)
  )

# 1. Évolution des labels dans le temps
evolution_labels <- gnpd %>%
  group_by(year) %>%
  summarise(
    total_products = n(),
    labeled_products = sum(has_label, na.rm = TRUE),
    percentage_labeled = labeled_products / total_products * 100
  )

# Visualisation de l'évolution
ggplot(evolution_labels, aes(x = year)) +
  geom_line(aes(y = percentage_labeled), color = "darkgreen", size = 1) +
  geom_point(aes(y = percentage_labeled), color = "darkgreen", size = 2) +
  labs(
    title = "Évolution du pourcentage de produits labellisés dans le temps",
    x = "Année",
    y = "Pourcentage de produits labellisés (%)"
  ) +
  theme_minimal() +
  scale_y_continuous(labels = scales::percent_format(scale = 1))

# 2. Analyse des prix : produits labellisés vs non labellisés
prix_labels <- gnpd %>%
  filter(!is.na(price_per_100)) %>%
  group_by(has_label) %>%
  summarise(
    median_price = median(price_per_100, na.rm = TRUE),
    mean_price = mean(price_per_100, na.rm = TRUE),
    n = n()
  )

# Visualisation de la distribution des prix
ggplot(gnpd %>% filter(!is.na(price_per_100)), 
       aes(x = price_per_100, fill = has_label)) +
  geom_density(alpha = 0.5) +
  labs(
    title = "Distribution des prix : produits labellisés vs non labellisés",
    x = "Prix par 100g/ml (€)",
    y = "Densité"
  ) +
  theme_minimal() +
  scale_fill_manual(
    values = c("TRUE" = "darkgreen", "FALSE" = "gray"),
    labels = c("TRUE" = "Labellisé", "FALSE" = "Non labellisé")
  )

# 3. Analyse des labels par marché
labels_par_marche <- gnpd %>%
  group_by(Market) %>%
  summarise(
    total_products = n(),
    labeled_products = sum(has_label, na.rm = TRUE),
    percentage_labeled = labeled_products / total_products * 100
  ) %>%
  filter(total_products >= 50) %>%  # Filtrer les marchés avec suffisamment de produits
  arrange(desc(percentage_labeled))

# Visualisation des marchés les plus engagés
ggplot(labels_par_marche %>% head(10), 
       aes(x = reorder(Market, percentage_labeled), y = percentage_labeled)) +
  geom_col(fill = "darkgreen") +
  coord_flip() +
  labs(
    title = "Top 10 des marchés par pourcentage de produits labellisés",
    x = "Marché",
    y = "Pourcentage de produits labellisés (%)"
  ) +
  theme_minimal() +
  scale_y_continuous(labels = scales::percent_format(scale = 1))

# 4. Analyse des labels par sous-catégorie
labels_par_categorie <- gnpd %>%
  group_by(`Sub-Category`) %>%
  summarise(
    total_products = n(),
    labeled_products = sum(has_label, na.rm = TRUE),
    percentage_labeled = labeled_products / total_products * 100
  ) %>%
  filter(total_products >= 30) %>%  # Filtrer les catégories avec suffisamment de produits
  arrange(desc(percentage_labeled))

# Visualisation des catégories les plus engagées
ggplot(labels_par_categorie %>% head(10), 
       aes(x = reorder(`Sub-Category`, percentage_labeled), y = percentage_labeled)) +
  geom_col(fill = "darkgreen") +
  coord_flip() +
  labs(
    title = "Top 10 des sous-catégories par pourcentage de produits labellisés",
    x = "Sous-catégorie",
    y = "Pourcentage de produits labellisés (%)"
  ) +
  theme_minimal() +
  scale_y_continuous(labels = scales::percent_format(scale = 1))

# Sauvegarde des résultats
write.csv(evolution_labels, "evolution_labels.csv", row.names = FALSE)
write.csv(labels_par_marche, "labels_par_marche.csv", row.names = FALSE)
write.csv(labels_par_categorie, "labels_par_categorie.csv", row.names = FALSE) 