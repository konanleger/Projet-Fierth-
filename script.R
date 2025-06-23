# Nettoyage de l'environnement
rm(list = ls())
# Instalation et importation des packages necessaires
library(readxl)
library(tidyverse)
library(dplyr)
library(lubridate)
library(wordcloud)
library(tidytext)
library(scales)

# Importation des donnees
gnpd_brut <- read_excel("GNPD_tea.xlsx")

# Analyse globale de la base de données
glimpse(gnpd_brut)
summary(gnpd_brut)


# convertir certaine variable en numerqiue 
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
quantile(gnpd_brut$`Price per 100 g/ml in Euros`,na.rm = T)

####################################################################
library(dplyr)
library(ggplot2)
library(gganimate)
library(scales)

# 1. Extraire l'année de Date Published
gnpd <- gnpd %>%
  mutate(annee = lubridate::year(`Date Published`))

# 2. Calculer prix moyen par marché et par année
prix_par_marche <- gnpd %>%
  filter(!is.na(`Price per 100 g/ml in Euros`), !is.na(annee), !is.na(Market)) %>%
  group_by(annee, Market) %>%
  summarise(
    prix_moyen = mean(`Price per 100 g/ml in Euros`, na.rm = TRUE),
    .groups = "drop"
  )

# 3. Sélectionner le top 10 des marchés par année
top10 <- prix_par_marche %>%
  group_by(annee) %>%
  slice_max(prix_moyen, n = 10) %>%
  ungroup()

# 4. Ajouter rang pour l'ordre visuel
top10 <- top10 %>%
  group_by(annee) %>%
  mutate(rang = rank(-prix_moyen),
         prix_label = paste0(format(round(prix_moyen, 2), nsmall = 2), " €")) %>%
  ungroup()




# 5. Graphique animé façon "bar chart race"
library(gganimate)

g <- ggplot(top10, aes(x = reorder(Market, -rang), 
                       y = prix_moyen, 
                       fill = Market)) +
  geom_col(width = 0.8) +
  geom_text(aes(label = prix_label), 
            hjust = -0.1, color = "black") +
  coord_flip(clip = "off", expand = FALSE) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.15))) +
  labs(title = 'Prix moyen des 10 premiers marchés en {closest_state}',
       subtitle = "Prix moyen en €/100g ou 100ml",
       x = NULL,
       y = "Prix moyen (€)") +
  theme_minimal(base_size = 14) +
  theme(legend.position = "none",
        plot.title = element_text(size = 20, face = "bold"),
        plot.subtitle = element_text(size = 14),
        axis.text.y = element_text(face = "bold"),
        plot.margin = margin(1, 2, 1, 4, "cm")) +
  transition_states(annee, transition_length = 4, state_length = 1) +
  enter_fly(x_loc = -1, y_loc = 0) +
  exit_fly(x_loc = 1, y_loc = 0) +
  ease_aes('cubic-in-out')

# 6. Lancer l’animation
animate(
  g,
  fps = 20,
  duration = 15,
  width = 900,
  height = 600,
  start_pause = 5,
  end_pause = 10,
  renderer = gifski_renderer()
)



##################################################################

# 1. Création d'un jeu de données avec indicateur de "valeurs bizarres"
gnpd_analyse <- gnpd_brut %>%
  mutate(valeur_bizarre = (`Unit Pack Size (ml/g)` < 10 |
                             `Unit Pack Size (ml/g)` > 2000))

# 2. Calcul du nombre total et de bizarres par région
resume_region <- gnpd_analyse %>%
  group_by(Region_Marche) %>%
  summarise(
    total = n(),
    nb_bizarres = sum(valeur_bizarre, na.rm = TRUE),
    proportion_bizarres = (nb_bizarres / total)*100
  ) %>%
  arrange(desc(proportion_bizarres))

# 3. Affichage du tableau
knitr::kable(resume_region, 
             caption = "Proportion de valeurs 'bizarres' par région du marché", 
             digits = 3)






#####################################################
# Identifier les outliers supérieurs
outliers <- gnpd_brut$`Price per 100 g/ml in Euros` > 13.98


# Nombre de valeurs extrêmes
sum(outliers, na.rm = TRUE)


df <- gnpd_brut
na_summary <- data.frame(
  Column = names(df),
  Missing = colSums(is.na(df)),
  Total = nrow(df)
)
na_summary$Percent <- round(100 * na_summary$Missing / na_summary$Total, 2)

# Trié par nombre de NA
na_summary <- na_summary[order(-na_summary$Missing), ]
print(na_summary)


gnpd <- gnpd_brut %>%
  filter(`Sub-Category` %in% c("Tea", "RTD (Iced) Tea")) %>%
  filter(!is.na(`Price per 100 g/ml in Euros`))

unique(gnpd_brut$`Format Type`)
unique(gnpd_brut$`Product Source`)

# nb de NA par colonne 
colSums(is.na(gnpd_brut))

a <- gnpd %>% 
  select(c(`Record ID`,`Company Territory`,`Location of Manufacture`,
           Market,`Price per 100 g/ml in Euros`,`Unit Pack Size (ml/g)`,
           `Packaging Units`,`Price in Euros`,`Price in US Dollars`))



###################################################

# Étape 1 : Filtrer et calculer la proportion
gnpd <- gnpd_brut
total_lignes <- nrow(gnpd)

gnpd_filtre <- gnpd %>%
  filter(`Unit Pack Size (ml/g)` >= 2,
         `Unit Pack Size (ml/g)`!= 0.00)
gnpd_filtre <- gnpd_filtre %>%
  filter(`Unit Pack Size (ml/g)` <= 5000)

proportion_conservee <- nrow(gnpd_filtre) / total_lignes

# Affichage de la proportion conservée
proportion_conservee

##########################################################################
gnpd_brut <- gnpd_filtre
gnpd_brut <- gnpd_brut %>%
  mutate(
    Region_Marche = case_when(
      Market %in% c("China", "Japan", "South Korea", "Taiwan, China", "Hong Kong,China", 
                    "Vietnam", "Thailand", "Indonesia", "Malaysia", "Philippines", 
                    "Singapore", "Myanmar", "Cambodia", "Laos","Sri Lanka","India","Bangladesh",
                    "Hong Kong, China") ~ "Asie",
      Market %in% c("UK", "France", "Germany", "Italy", "Spain", "Switzerland", 
                    "Netherlands", "Belgium", "Sweden", "Denmark", "Norway", "Finland", 
                    "Austria", "Poland", "Greece", "Portugal", "Ireland", "Luxembourg",
                    "Czech Republic", "Slovakia", "Slovenia", "Estonia", "Latvia", 
                    "Lithuania", "Hungary", "Romania", "Bulgaria", "Croatia", "Serbia",
                    "Ukraine", "Belarus", "Russia") ~ "Europe",
      Market %in% c("USA", "Canada", "Mexico", "Puerto Rico") ~ "Amérique du Nord",
      Market %in% c("Brazil", "Argentina", "Colombia", "Chile", "Peru", "Venezuela", 
                    "Ecuador", "Paraguay", "Uruguay", "Panama", "Costa Rica", 
                    "Guatemala") ~ "Amérique du Sud",
      Market %in% c("Australia", "New Zealand") ~ "Océanie",
      Market %in% c("South Africa", "Kenya", "Tanzania", "Uganda", "Nigeria", 
                    "Egypt", "Morocco", "Algeria", "Tunisia", "Ghana", "Ivory Coast",
                    "Ethiopia", "Cameroon") ~ "Afrique",
      Market %in% c("UAE", "Saudi Arabia", "Israel", "Turkey", "Iran", "Jordan", 
                    "Oman", "Qatar", "Kuwait", "Lebanon", "Bahrain","Pakistan") ~ "Moyen-Orient",
      TRUE ~ "Autres"
    )
  )
gnpd<- gnpd_brut
gnpd$Region_Marche
# Statistique descriptive

# Transformation de certaines colonnes si nécessaire
gnpd <- gnpd %>%
  mutate(
    price_per_100 = as.numeric(`Price per 100 g/ml in Euros`),
    year = year(`Date Published`),
    month = floor_date(`Date Published`, "month")
  )
unique(gnpd$year)
unique(gnpd$`Company Territory`)
unique(gnpd$`Location of Manufacture`)
unique(gnpd$`Company address`)
unique(gnpd$`Sub-Category`)
unique(gnpd$`Package Type`)
unique(gnpd$`Package Material`)
unique(gnpd$`Number of Variants`)
unique(gnpd$Nutrition)
glimpse(gnpd)

# Lancement dans le temps 
# Nombre de lancements par mois
gnpd %>%
  count(month) %>%
  ggplot(aes(x = month, y = n)) +
  geom_line(color = "steelblue", size = 1) +
  labs(title = "Lancements de produits par mois",
       x = "Mois",
       y = "Nombre de lancements")


# Lancement par marché
gnpd %>%
  filter(year>= 2010) %>% 
  count(Market, sort = TRUE) %>%
  slice_max(n, n = 10) %>%
  ggplot(aes(x = reorder(Market, n), y = n)) +
  geom_col(fill = "darkgreen") +
  coord_flip() +
  labs(title = "Top 10 des marchés de lancement",
       x = "Marché",
       y = "Nombre de lancements")


# Sous-categorie
gnpd %>%
  count(`Sub-Category`, sort = TRUE) %>%
  slice_max(n, n = 10) %>%
  ggplot(aes(x = reorder(`Sub-Category`, n), y = n)) +
  geom_col(fill = "tomato") +
  coord_flip() +
  labs(title = "Top 10 des sous-catégories",
       x = "Sous-catégorie",
       y = "Nombre de produits")

# Analyse des prix 
# Distribution générale
ggplot(gnpd, aes(x = price_per_100)) +
  geom_histogram(fill = "skyblue") +
  labs(title = "Distribution des prix par 100g/ml",
       x = "Prix en euros",
       y = "Nombre de produits") +
  scale_x_continuous(labels = comma)
summary(gnpd$`Price per 100 g/ml in Euros`)

# Boxplot par sous-catégorie
gnpd %>%
  filter(!is.na(price_per_100),
         price_per_100<100) %>%
  group_by(`Sub-Category`) %>%
  filter(n() >= 30) %>%
  ggplot(aes(x = reorder(`Sub-Category`, price_per_100, median), y = price_per_100)) +
  geom_boxplot(fill = "orange") +
  coord_flip() +
  labs(title = "Prix par sous-catégorie", x = "Sous-catégorie", y = "Prix par 100g/ml")


# Claims marketing
# Extraction des claims individuels
claims <- gnpd %>%
  filter(!is.na(`Positioning Claims`)) %>%
  unnest_tokens(claim, `Positioning Claims`, token = "regex",
                pattern = ",\\s*") %>%
  count(claim, sort = TRUE)

# Barplot des claims les plus fréquents
claims %>%
  slice_max(n, n = 15) %>%
  ggplot(aes(x = reorder(claim, n), y = n)) +
  geom_col(fill = "purple") +
  coord_flip() +
  labs(title = "Principaux arguments marketing", x = "Claim", y = "Fréquence")


library(tm)
set.seed(123)
text_corpus <- Corpus(VectorSource(gnpd$`Positioning Claims`))
toSpace <- content_transformer(function (x , pattern ) gsub(pattern, " ", x))

text_corpus <- tm_map(text_corpus, toSpace, "/")
text_corpus <- tm_map(text_corpus, toSpace, "-")

text_corpus <- tm_map(text_corpus, content_transformer(tolower))
text_corpus <- tm_map(text_corpus, removePunctuation)
text_corpus <- tm_map(text_corpus,
                      function(x)removeWords(x,stopwords(kind = "english")))

wordcloud(text_corpus, colors = brewer.pal(8, "Dark2"),
          rot.per=0, random.order = F)

dtm <- TermDocumentMatrix(text_corpus)
m <- as.matrix(dtm)
v <- sort(rowSums(m),decreasing=TRUE)
d <- data.frame(word = names(v),freq=v)
head(d, 10)



# Type d'embalage
gnpd %>%
  filter(!is.na(`Package Type`)) %>% 
  count(`Package Type`, sort = TRUE) %>%
  #slice_max(n, n = 10) %>%
  ggplot(aes(x = reorder(`Package Type`, n), y = n)) +
  geom_col(fill = "darkred") +
  coord_flip() +
  labs(title = "Top 10 des types d'emballage", x = "Type d'emballage", y = "Nombre de produits")



gnpd %>%
  filter(!is.na(`Package Material`)) %>% 
  count(`Package Material`, sort = TRUE) %>%
  #slice_max(n, n = 10) %>%
  ggplot(aes(x = reorder(`Package Material`, n), y = n)) +
  geom_col(fill = "red") +
  coord_flip() +
  labs(title = "Top 10 des materiel d'emballage", x = "Materiel d'emballage", y = "Nombre de produits")


# Sous-catégories les plus présentes
top3_cat <- gnpd %>%
  count(`Sub-Category`, sort = TRUE) %>%
  slice_max(n, n = 3) %>%
  pull(`Sub-Category`)

# Extraction des claims par sous-catégorie
claims_by_category <- gnpd %>%
  filter(`Sub-Category` %in% top3_cat, !is.na(`Positioning Claims`)) %>%
  unnest_tokens(claim, `Positioning Claims`, token = "regex", pattern = ",\\s*") %>%
  count(`Sub-Category`, claim, sort = TRUE) %>%
  group_by(`Sub-Category`) %>%
  slice_max(n, n = 5)

# Graphique clair
claims_by_category %>%
  ggplot(aes(x = reorder(claim, n), y = n, fill = `Sub-Category`)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~`Sub-Category`, scales = "free_y") +
  coord_flip() +
  labs(
    title = "Claims les plus fréquents par sous-catégorie (Top 3)",
    x = "Claim",
    y = "Fréquence"
  ) +
  theme_minimal()


# ANALYSE DU PRIX

gnpd_clean <- gnpd %>%
  filter(!is.na(price_per_100),
         price_per_100 < quantile(price_per_100, 0.99, na.rm = T))  # on retire le top 1%

# A - Analyse univariée
gnpd_clean %>% 
  summarise(
    min_price = min(price_per_100, na.rm = TRUE),
    mean_price = mean(price_per_100, na.rm = TRUE),
    median_price = median(price_per_100, na.rm = TRUE),
    max_price = max(price_per_100, na.rm = TRUE),
    sd_price = sd(price_per_100, na.rm = TRUE),
    n = n()
  )

# visualisation
ggplot(gnpd_clean, aes(x = price_per_100)) +
  geom_histogram(fill = "#2a9d8f", color = "white") +
  labs(title = "Distribution du prix (€/100g ou ml)", x = "Prix", y = "Fréquence")

ggplot(gnpd_clean, aes(y = price_per_100)) +
  geom_boxplot(fill = "#e76f51") +
  labs(title = "Boxplot du prix", y = "Prix pour 100g/ml")

# Analyse bivariée : Prix vs. Positioning Claims

gnpd_clean <- gnpd_clean %>%
  filter(!is.na(`Positioning Claims`)) %>% 
  mutate(claim_organic = ifelse(str_detect(`Positioning Claims`, "Organic"), "Organic", "Non-Organic"))

library(dplyr)
library(stringr)

gnpd_clean <- gnpd_clean %>%
  filter(!is.na(`Positioning Claims`)) %>%
  mutate(claim_category = case_when(
    str_detect(`Positioning Claims`, regex("Ethical", ignore_case = TRUE)) ~ "Ethical",
    str_detect(`Positioning Claims`, regex("Functional", ignore_case = TRUE)) ~ "Functional",
    str_detect(`Positioning Claims`, regex("Environmentally|friendly", ignore_case = TRUE)) ~ "Environmentally Friendly",
    str_detect(`Positioning Claims`, regex("Artificial", ignore_case = TRUE)) ~ "Artificial",
    str_detect(`Positioning Claims`, regex("Additives", ignore_case = TRUE)) ~ "Additives",
    str_detect(`Positioning Claims`, regex("Recycling", ignore_case = TRUE)) ~ "Recycling",
    str_detect(`Positioning Claims`, regex("Organic", ignore_case = TRUE)) ~ "Organic",
    TRUE ~ "Other"
  ))


ggplot(gnpd_clean, aes(x = claim_organic, y = price_per_100)) +
  geom_boxplot(fill = "#264653") +
  labs(title = "Prix selon présence d'un claim 'Organic'", x = "Positioning Claim", y = "Prix pour 100g/ml")

library(dplyr)
library(tidyr)
library(ggplot2)
library(stringr)
library(forcats)

# Étape 1 : Extraire tous les claims dans des lignes séparées
gnpd_long <- gnpd_clean %>%
  filter(!is.na(`Positioning Claims`)) %>%
  separate_rows(`Positioning Claims`, sep = ",\\s*")  # sépare les claims par virgule

# Étape 2 : Identifier les 10 claims les plus fréquents
top_claims <- gnpd_long %>%
  count(`Positioning Claims`, sort = TRUE) %>%
  slice_head(n = 20) %>%
  pull(`Positioning Claims`)

# Étape 3 : Filtrer uniquement les produits contenant ces claims
gnpd_top_claims <- gnpd_long %>%
  filter(`Positioning Claims` %in% top_claims)

# Étape 4 : Tracer le boxplot
ggplot(gnpd_top_claims, aes(x = fct_reorder(`Positioning Claims`,
                                            price_per_100, .fun = median), y = price_per_100)) +
  geom_boxplot(fill = "#264653") +
  labs(
    title = "Prix selon les 10 Positioning Claims les plus fréquents",
    x = "Positioning Claim",
    y = "Prix pour 100g/ml"
  ) +
  coord_flip() +  # optionnel : pivote pour une lecture plus facile
  theme_minimal()






ggplot(gnpd_clean, aes(x = fct_lump(`Package Type`, 10), y = price_per_100)) +
  geom_boxplot(fill = "#f4a261") +
  labs(title = "Prix selon le type d'emballage", x = "Type d'emballage", y = "Prix pour 100g/ml") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

ggplot(gnpd_clean, aes(x = fct_lump(`Package Material`, 10), y = price_per_100)) +
  geom_boxplot(fill = "#f4a261") +
  labs(title = "Prix selon le materiel d'emballage utilisé", x = "Materiel d'emballage", y = "Prix pour 100g/ml") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Nuage de point
ggplot(gnpd, aes(x = `Package Type`, y = as.numeric(`Price in Euros`))) +
  geom_jitter(width = 0.2, alpha = 0.3) +
  scale_y_log10() +
  coord_flip() +
  labs(title = "Prix vs Type d'emballage (échelle logarithmique)", 
       y = "Prix (€) (log10)")
################################################################################

# Filtrage des deux sous-catégories de thé
tea_data <- gnpd %>%
  filter(
    `Sub-Category` %in% c("RTD (Iced) Tea", "Tea"),
    !is.na(price_per_100),
    !is.na(month),price_per_100<400
    
  )

# Calcul du prix moyen par mois et sous-catégorie
evol_prix_tea <- tea_data %>%
  group_by(month, `Sub-Category`) %>%
  summarise(mean_price = mean(price_per_100, na.rm = TRUE), .groups = "drop")

# Visualisation
ggplot(evol_prix_tea, aes(x = month, y = mean_price, color = `Sub-Category`)) +
  geom_line(size = 1.2) +
  labs(
    title = "Évolution mensuelle du prix moyen des sous-catégories de thé",
    x = "Date (mois)",
    y = "Prix moyen (€/100g ou ml)",
    color = "Sous-catégorie"
  ) +
  theme_minimal()


