

# Nettoyage de l'environnement
rm(list = ls())

# Instalation et chargements des packages necessaires
if (!require("ggrepel")) install.packages("ggrepel")
library(readxl)
library(tidyverse)
library(dplyr)
library(lubridate)
library(wordcloud)
library(tidytext)
library(scales)
library(ggplot2)
library(gganimate)
library(gridExtra)
library(ggrepel)  

# Importation des donnees
gnpd_brut <- read_excel("GNPD_tea.xlsx")

# apperçu globale des variables
glimpse(gnpd_brut)
summary(gnpd_brut)

# convertissement de certaine variable en numerique 
gnpd_brut <- gnpd_brut %>%
  mutate(
    `Price per 100 g/ml in Euros` = as.numeric(`Price per 100 g/ml in Euros`),
    `Price in US Dollars` = as.numeric(`Price in US Dollars`),
    `Price in Euros` = as.numeric(`Price in Euros`),
    `Unit Pack Size (ml/g)` = as.numeric(`Unit Pack Size (ml/g)`),
    `Alcohol By Volume (%)` = as.numeric(`Alcohol By Volume (%)`),
    `Bar Code` = as.character(`Bar Code`),
    `Date Published` = as.Date(`Date Published`),
    year = year(`Date Published`),
    month = floor_date(`Date Published`, "month")
  )


# Creation d'une nouvelle variable 
gnpd_brut <- gnpd_brut %>%
  mutate(`Famille thé` = case_when(
    `Sub-Category` == "Tea" ~ "Thé",
    `Sub-Category` == "RTD (Iced) Tea" ~ "Thé prêt à boire",
    `Sub-Category` %in% c("Kombucha & Other Fermented Drinks", 
                          "Flavoured Water","Beverage Mixes",
                          "Beverage Concentrates","RTD (Iced) Coffee",
                          "Energy Drinks","Flavoured Water",
                          "Flavoured Alcoholic Beverages","Wine",
                          "Carbonated Soft Drinks","Vodka","Liqueur",
                          "Juice","Coffee","Beer","Malt & Other Hot Beverages" ,
                          "Fruit/Flavoured Still Drinks","Drinking Yogurt & Liquid Cultured Milk",
                          "Nectars","Plant Based Drinks (Dairy Alternatives)",
                          "Flavoured Milk",
                          "Sports Drinks","Nutritional & Meal Replacement Drinks") ~ "Autres boissons contenant du thé",
    
    `Sub-Category` %in% c("Cat Snacks & Treats",
                          "Cat Food Dry",
                          "Cat Food Wet",
                          "Dog Snacks & Treats",
                          "Dog Food Dry",
                          "Dog Food Wet") ~ "Aliments pour animaux contenant du thé",
    TRUE ~ "Autres produits contenant du thé"
  ))  


# Creation de la variable region 

  gnpd_brut <- gnpd_brut %>%
    filter(!is.na(Market)) %>% 
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


# Determinantion du taux de Na par variable

colSums(is.na(gnpd_brut)) # pour connaitre le nombre de Na par variable
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
rm(df,na_summary)


################################################################################
#-------------------------------------------------------------------------------
# Statistique descriptive
#===============================================================================

# Lancement dans le temps 
# Nombre de lancements par mois
gnpd_brut %>%
  count(month) %>%
  ggplot(aes(x = month, y = n)) +
  geom_line(color = "steelblue", size = 1) +
  labs(title = "Lancements de produits par mois de 1998 à 2025",
       x = "Année",
       y = "Nombre de produits lancés",
       caption = "Source: Base Mintel")


# Lancement par marché (Bivariée)
p1 <- gnpd_brut %>%
  filter(year>= 2010) %>% 
  count(Market, sort = TRUE) %>%
  slice_max(n, n = 10) %>%
  ggplot(aes(x = reorder(Market, n), y = n)) +
  geom_col(fill = "darkgreen") +
  coord_flip() +
  labs(title = "Top 10 des marchés de lancement en 2010",
       x = "Marché",
       y = "Nombre de lancements",
       caption = "Source : Base Mintel")
p2<- gnpd_brut %>%
  filter(year>= 2024) %>% 
  count(Market, sort = TRUE) %>%
  slice_max(n, n = 10) %>%
  ggplot(aes(x = reorder(Market, n), y = n)) +
  geom_col(fill = "green") +
  coord_flip() +
  labs(title = "Top 10 des marchés de lancement en 2020",
       x = "Marché",
       y = "Nombre de lancements",
       caption = "Source : Base Mintel")
grid.arrange(p1,p2, ncol=1)

rm(p1,p2)
#- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -  - - - - - - - - 
# Sous-categorie
# - - - - - - - - -  - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

unique(gnpd_brut$`Sub-Category`) # pour voir les sous-catégories
#===============================================================

# premier graphique
p1 <- gnpd_brut %>%
        count(`Sub-Category`, sort = TRUE) %>%
        slice_max(n, n = 10) %>%
        ggplot(aes(x = reorder(`Sub-Category`, n), y = n)) +
        geom_col(fill = "tomato") +
        coord_flip() +
        labs(title = "Top 10 des sous-catégories",
             x = "Sous-catégorie",
             y = "Nombre de produits",
             caption = "Source : Mintel")
p2 <- gnpd_brut %>%
  count(`Famille thé`, sort = TRUE) %>%
  ggplot(aes(x = reorder(`Famille thé`, n), y = n)) +
  geom_col(fill = "tomato") +
  coord_flip() +
  labs(title = "Regroupement sous categories",
       x = "Sous-catégorie",
       y = "Nombre de produits",
       caption = "Source : Mintel")
grid.arrange(p1,p2, ncol=1)
rm(p1,p2)
#####################

# 1. Préparation des données (ajout des labels pour les dernières années)
evolution_data <- evolution_data %>%
  group_by(`Famille thé`) %>%
  mutate(
    last_year = (year == max(year)),
    label = if_else(last_year, `Famille thé`, NA_character_)
  )

# 2. Graphique
ggplot(evolution_data, aes(x = year, y = n, color = `Famille thé`, group = `Famille thé`)) +
  # Lignes et points
  geom_line(linewidth = 1.2, alpha = 0.8) +
  geom_point(size = 3, alpha = 0.8) +
  
  # Labels de fin de courbe
  geom_label_repel(
    aes(label = label),
    na.rm = TRUE,
    hjust = 0,
    direction = "y",
    segment.color = NA,
    size = 3.5,
    fontface = "bold",
    show.legend = FALSE
  ) +
  
  # Échelles et couleurs
  scale_x_continuous(breaks = seq(min(evolution_data$year), max(evolution_data$year), by = 2)) +
  scale_color_viridis_d(option = "plasma", end = 0.9) +  # Palette moderne
  #scale_y_continuous(expand = expansion(mult = c(0.05, 0.1))) +  # Espace pour les labels
  
  # Titres et thème
  labs(
    title = "Évolution des lancements de produits par famille de thé",
    subtitle = "Analyse des tendances annuelles (données Mintel GNPD)",
    x = "Année",
    y = "Nombre de produits lancés",
    caption = "Source : Mintel GNPD | Projet Fierthé"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    legend.position = "none",  # Supprime la légende (remplacée par les labels)
    plot.title = element_text(face = "bold", size = 14, hjust = 0.5),
    plot.subtitle = element_text(hjust = 0.5, color = "gray40"),
    plot.caption = element_text(color = "gray50", margin = margin(t = 10)),
    panel.grid.major = element_line(color = "gray90"),
    axis.title = element_text(face = "bold")
  )
#-------------------------------------------------------------------------------
# Analyse des prix 
#-------------------------------------------------------------------------------
# Test de Anderson-Darling (plus robuste pour grands échantillons)
if (!require("nortest")) install.packages("nortest") # pour eviter de reinstaller
if (!require("moments")) install.packages("moments")
library(moments)
library(nortest)

prix <- gnpd_brut$`Price per 100 g/ml in Euros`
prix <- na.omit(prix)

ad.test(prix) # pour tester l'hypothèse de normalité du prix

# Coefficient d'asymétrie (skewness) et  d'aplatissement (Kurtosis)
skewness(prix)
kurtosis(prix)
summary(prix)

gnpd_clean <- gnpd_brut %>% 
  filter(!is.na(`Price per 100 g/ml in Euros`),
         `Price per 100 g/ml in Euros`< quantile(`Price per 100 g/ml in Euros`,
                                                 0.99, na.rm = T))  # on retire le top 1%
ggplot(gnpd_clean, aes(x = `Price per 100 g/ml in Euros`)) +
  geom_histogram(fill = "#2a9d8f", color = "white", boundary=0, binwidth = 2) +
  scale_x_continuous(breaks = seq(0, 40, 4)) +
  labs(title = "Distribution du prix (€/100g ou ml)", x = "Prix", y = "Fréquence")

# Elimination des variables "abberantes"

# option 1: Filtrer les données selon vos conditions
# gnpd_filtered <- gnpd_brut %>%
#   filter(
#     `Unit Pack Size (ml/g)` >= 2,
#     `Unit Pack Size (ml/g)` <= 5000,
#     `Unit Pack Size (ml/g)` != 0.00,
#     `Price per 100 g/ml in Euros` != 0.00
#   )

# option 2 : Imputation des valeurs aberrantes par sous categorie et marché
# gnpd_imputed <- gnpd_brut %>%
#   group_by(`Famille thé`, `Market`) %>%  # Remplacez par vos colonnes exactes
#   mutate(
#     # Calcul des quartiles et moyennes par groupe
#     Q1_price = quantile(`Price per 100 g/ml in Euros`, 0.25, na.rm = TRUE),
#     Q3_price = quantile(`Price per 100 g/ml in Euros`, 0.99, na.rm = TRUE),
#     mean_low = quantile(`Price per 100 g/ml in Euros`[`Price per 100 g/ml in Euros` < Q1_price],
#                         0.5,na.rm = TRUE),
#     mean_high = quantile(`Price per 100 g/ml in Euros`[`Price per 100 g/ml in Euros` > Q3_price],
#                          0.5,na.rm = TRUE),
#     
#     # Remplacement des valeurs extrêmes
#     `Price per 100 g/ml in Euros` = case_when(
#       `Price per 100 g/ml in Euros` < Q1_price ~ mean_low,
#       `Price per 100 g/ml in Euros` > Q3_price ~ mean_high,
#       TRUE ~ `Price per 100 g/ml in Euros`
#     )
#   ) %>%
#   ungroup()
# 
# Optionnel: Vérification des résultats
# summary(gnpd_imputed$`Price per 100 g/ml in Euros`)
# boxplot(gnpd_imputed$`Price per 100 g/ml in Euros`, main = "Prix après imputation")

# # Étape 1 : Filtrer et calculer la proportion
# gnpd <- gnpd_brut
# total_lignes <- nrow(gnpd)
# 
# gnpd_filtre <- gnpd %>%
#   filter(`Unit Pack Size (ml/g)` >= 2,
#          `Unit Pack Size (ml/g)`!= 0.00,
#          `Price per 100 g/ml in Euros` != 0.00)
# gnpd_filtre <- gnpd_filtre %>%
#   filter(`Unit Pack Size (ml/g)` <= 5000)
# 
# proportion_conservee <- nrow(gnpd_filtre) / total_lignes
# 
# # Affichage de la proportion conservée
# proportion_conservee

# gnpd <- gnpd_filtre
summary(gnpd_brut$`Price per 100 g/ml in Euros`)
gnpd_clean <- gnpd_brut %>% 
  filter(!is.na(`Price per 100 g/ml in Euros`),
         `Price per 100 g/ml in Euros`< quantile(`Price per 100 g/ml in Euros`,
                                                 0.99, na.rm = T))  # on retire le top 1%

# 4. Graphique final avec annotation dynamique
p1 <- ggplot(gnpd_clean, aes(x = `Price per 100 g/ml in Euros`)) +
        geom_histogram(binwidth = 1, boundary = 0, fill = "skyblue", alpha = 1) +
        geom_vline(xintercept = median(gnpd_brut$`Price per 100 g/ml in Euros`, na.rm = TRUE), 
                   color = "red", linetype = "dashed", size = 1) +
        geom_vline(xintercept = mean(gnpd_brut$`Price per 100 g/ml in Euros`, na.rm = TRUE), 
                   color = "green", linetype = "dashed", size = 1) +
        coord_cartesian(xlim = c(0, 30)) + # Zoom sur 0-50€
        scale_x_continuous(breaks = seq(0, 30, 5)) +
        labs(
          title = "Distribution des prix (99% des produits)", 
          subtitle = "La ligne rouge montre la médiane (1,17€) 
          et la verte la moyenne (4,35€)",
          x = "Prix pour 100g/ml (€)", 
          y = "Nombre de produits",
          caption = "Source : Base Mintel"
        ) +
        theme_minimal()
rm(hist_data,hist_plot,max_count,pct_under_5)

#############
# analyse du prix du thé sur le marché mondiale
gnpd_tea <- gnpd_brut %>% 
  filter(`Sub-Category` == "Tea")
summary(gnpd_tea$`Price per 100 g/ml in Euros`)
# 1. Filtrage des données (on garde 99% les moins chers)
gnpd_filtered <- gnpd_tea %>%
  filter(`Sub-Category` == "Tea")


# 2. Graphique final avec annotation dynamique
p2 <- ggplot(gnpd_filtered, aes(x = `Price per 100 g/ml in Euros`)) +
        geom_histogram(binwidth = 1, boundary = 0, fill = "darkblue", alpha = 1) +
        geom_vline(xintercept = median(gnpd_tea$`Price per 100 g/ml in Euros`, na.rm = TRUE), 
                   color = "red", linetype = "dashed", size = 1) +
        geom_vline(xintercept = mean(gnpd_tea$`Price per 100 g/ml in Euros`, na.rm = TRUE), 
                   color = "green", linetype = "dashed", size = 1) +
        coord_cartesian(xlim = c(0, 30)) + # Zoom sur 0-50€
        scale_x_continuous(breaks = seq(0, 30, 5)) +
        labs(
          title = "Distribution des prix du thé (99% des produits)", 
          subtitle = "La ligne rouge montre la médiane 
          et la verte la moyenne ",
          x = "Prix pour 100g/ml (€)", 
          y = "Nombre de produits",
          caption = "Source : Base Mintel"
        ) +
        theme_minimal()

grid.arrange(p1, p2, ncol = 1) # Affichage des deux graphique sur une colonne
rm(gnpd_filtered,hist_data,hist_plot,max_count,pct_under_5,p1,p2,prix)


#####################
# Evolution de densité
ggplot(gnpd_brut, aes(x = `Price per 100 g/ml in Euros`)) +
  geom_density(fill = "steelblue", alpha = 0.5) +
  coord_cartesian(xlim = c(0, 20)) +
  geom_vline(xintercept = c(1.17, 4.3), color = c("red", "green"))

# Le prix en fonction des sous categories en faisant un Zoom sur 0-20€ 
ggplot(gnpd_brut, aes(x = `Price per 100 g/ml in Euros`, fill = `Famille thé`)) +
  geom_histogram(binwidth = 0.5, alpha = 0.7,boundary=0) +
  coord_cartesian(xlim = c(0, 20))


# Distribution générale
ggplot(gnpd_brut, aes(x = `Price per 100 g/ml in Euros`)) +
  geom_boxplot(fill = "skyblue") +
  labs(title = "Distribution des prix par 100g/ml",
       x = "Prix en euros",
       y = "Nombre de produits") +
  scale_x_continuous(labels = comma)
summary(gnpd$`Price per 100 g/ml in Euros`)

# Boxplot par sous-catégorie
gnpd_brut %>%
  filter(!is.na(`Price per 100 g/ml in Euros`),
         `Price per 100 g/ml in Euros`<100) %>%
  group_by(`Famille thé`) %>%
  filter(n() >= 10) %>%
  ggplot(aes(x = reorder(`Famille thé`, `Price per 100 g/ml in Euros`, median),
             y = `Price per 100 g/ml in Euros`)) +
  geom_boxplot(fill = "orange") +
  coord_flip() +
  labs(title = "Prix par sous-catégorie", x = "Sous-catégorie", y = "Prix par 100g/ml")


gnpd <- gnpd_tea
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
  labs(title = "Principaux arguments marketing",
       x = "Claim", 
       y = "Fréquence",
       caption = "Source : Base Mintel")


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

d[,1]
################################################################################
library(dplyr)
library(stringr)

unique(gnpd_tea$`Positioning Claims`)


# Définir les mots-clés pour chaque catégorie
# 1. Santé & Bien-être 
claims_sante <- c(
  "antioxidant", "functional - digestive", "functional - energy", 
  "functional - slimming", "functional - other", "functional - stress & sleep", 
  "functional - immune system", "functional - beauty benefits", 
  "functional - brain & nervous system", "functional - cardiovascular", 
  "functional - bone health", "functional - skin", "high satiety", 
  "anti-ageing", "nails & hair", "vitamin/mineral fortified","fonctionnal"
)

# 2. Naturalité & Propreté (free-from + caractéristiques produits)
claims_naturel <- c(
  "caffeine free", "no additives/preservatives", 
  "free from added/artificial flavourings", "free from added/artificial additives", 
  "free from added/artificial preservatives", "free from added/artificial colourings", 
  "low/no/reduced allergen", "low/no/reduced calorie", "sugar free", 
  "no added sugar", "low/no/reduced fat", "low/no/reduced sodium", 
  "low/no/reduced lactose", "wholegrain", "plant based","Gluten Free", "Halal",
  "Dairy Free", "Alcohol Free","gmo free"
)

# 3. Ciblage & Besoins Spécifiques 
claims_ciblage <- c(
  # Démographie
  "maternal", "children (5-12)", "female", "male",
  # Régimes/Restrictions
  "vegetarian", "vegan/no animal ingredients", "kosher", "gmo free", "diabetic",
  "Halal"
)

# 4. Éthique & Durabilité 
claims_ethique <- c(
  "organic", "ethical - environmentally friendly package", "ethical - recycling",
  "ethical - environmentally friendly product", "ethical - human",
  "ethical - sustainable (habitat/resources)", "ethical - toxins free",
  "ethical - biodegradable", "ethical - charity", "carbon neutral", 
  "ethical - animal","biodegradable packaging", "S"
)

# 5. Packaging & Convenience 
claims_strategie <- c(
  "convenient packaging", "time/speed", "ease of use", "interesting packaging",
  "microwaveable", "refill/refillable", "portionability", "on-the-go",
  "biodegradable packaging","premium", "social media", "seasonal", "limited edition", 
  "event merchandising", "novel", "cobranded","economy"
)

# # 6. Marketing & Positionnement (stratégie produit)
# claims_marketing <- c(
#   "premium", "social media", "seasonal", "limited edition", 
#   "event merchandising", "novel", "cobranded","economy"
# )
# Fonction pour détecter la présence d’un mot-clé
detect_claim <- function(text, keywords) {
  if (is.na(text)) return("non")
  any(str_detect(text, fixed(keywords, ignore_case = TRUE))) %>% ifelse("oui", "non")
}

# Créer les colonnes
gnpd_claim <- gnpd_brut %>%
  mutate(
    claim_sante = map_chr(`Positioning Claims`, ~ detect_claim(.x, claims_sante)),
    claim_naturel = map_chr(`Positioning Claims`, ~ detect_claim(.x, claims_naturel)),
    claim_ethique = map_chr(`Positioning Claims`, ~ detect_claim(.x, claims_ethique)),
    claim_strategie = map_chr(`Positioning Claims`, ~ detect_claim(.x, claims_strategie)),
    claim_ciblage = map_chr(`Positioning Claims`, ~ detect_claim(.x, claims_ciblage))
  )

rm(claims_ciblage,claims_ethique,claims_marketing,claims_nutrition,
   claims_sante,claims_sante)




unique(claims[,1])

################################################################################

# Type d'embalage
gnpd_brut %>%
  filter(!is.na(`Package Type`)) %>% 
  count(`Package Type`, sort = TRUE) %>%
  slice_max(n, n = 20) %>%
  ggplot(aes(x = reorder(`Package Type`, n), y = n)) +
  geom_col(fill = "darkred") +
  coord_flip() +
  labs(title = "Top 20 des types d'emballage", 
       x = "Type d'emballage",
       y = "Nombre de produits",
       caption = "Source : Mintel")

unique(gnpd_brut$`Package Type`)


gnpd_brut %>%
  filter(!is.na(`Package Material`)) %>% 
  count(`Package Material`, sort = TRUE) %>%
  slice_max(n, n = 20) %>%
  ggplot(aes(x = reorder(`Package Material`, n), y = n)) +
  geom_col(fill = "red") +
  coord_flip() +
  labs(title = "Top 20 des materiels d'emballage", 
       x = "Materiel d'emballage", 
       y = "Nombre de produits",
       caption = "Source : Mintel")


# Sous-catégories les plus présentes
top3_cat <- gnpd_brut %>%
  count(`Sub-Category`, sort = TRUE) %>%
  slice_max(n, n = 3) %>%
  pull(`Sub-Category`)

# Extraction des claims par sous-catégorie
claims_by_category <- gnpd_brut %>%
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
gnpd <- gnpd_brut
gnpd_clean <- gnpd %>%
  filter(!is.na(`Price per 100 g/ml in Euros`),
         `Price per 100 g/ml in Euros`< quantile(`Price per 100 g/ml in Euros`, 0.99, na.rm = T))  # on retire le top 1%

# A - Analyse univariée
gnpd_clean %>% 
  summarise(
    min_price = min(`Price per 100 g/ml in Euros`, na.rm = TRUE),
    mean_price = mean(`Price per 100 g/ml in Euros`, na.rm = TRUE),
    median_price = median(`Price per 100 g/ml in Euros`, na.rm = TRUE),
    max_price = max(`Price per 100 g/ml in Euros`, na.rm = TRUE),
    sd_price = sd(`Price per 100 g/ml in Euros`, na.rm = TRUE),
    n = n()
  )

# visualisation
ggplot(gnpd_clean, aes(x = `Price per 100 g/ml in Euros`)) +
  geom_histogram(fill = "#2a9d8f", color = "white", boundary=0) +
  scale_x_continuous(breaks = seq(0, 30, 5)) +
  labs(title = "Distribution du prix (€/100g ou ml)", x = "Prix", y = "Fréquence")

ggplot(gnpd_clean, aes(y = `Price per 100 g/ml in Euros`)) +
  geom_boxplot(fill = "#e76f51") +
  labs(title = "Boxplot du prix", y = "Prix pour 100g/ml")

################################################################################
# Analyse bivariée : Prix vs. Positioning Claims
################################################################################
gnpd_clean <- gnpd_clean %>%
  filter(!is.na(`Positioning Claims`)) %>% 
  mutate(claim_organic = ifelse(str_detect(`Positioning Claims`, "Organic"), "Organic", "Non-Organic"))



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


ggplot(gnpd_clean, aes(x = claim_organic, y = `Price per 100 g/ml in Euros`)) +
  geom_boxplot(fill = "#264653") +
  labs(title = "Prix selon présence d'un claim 'Organic'", x = "Positioning Claim", y = "Prix pour 100g/ml")

library(dplyr)
library(tidyr)
library(ggplot2)
library(stringr)
library(forcats)

# Étape 1 : Extraire tous les claims dans des lignes séparées
gnpd_long <- gnpd_tea %>%
  filter(!is.na(`Positioning Claims`)) %>%
  separate_rows(`Positioning Claims`, sep = ",\\s*")  # sépare les claims par virgule

unique(gnpd_long$`Positioning Claims`)
# Étape 2 : Identifier les 10 claims les plus fréquents
top_claims_freq <- gnpd_long %>%
  count(`Positioning Claims`, sort = TRUE) %>%
  slice_head(n = 15) %>%
  pull(`Positioning Claims`)

# Etape 3 : 

top_claims_chers <- gnpd_long %>%
  filter(`Positioning Claims` %in% top_claims_freq) %>%
  group_by(`Positioning Claims`) %>%
  summarise(median_price = median(`Price per 100 g/ml in Euros`, na.rm = TRUE)) %>%
  arrange(desc(median_price)) %>%
  slice_head(n = 30) %>%
  pull(`Positioning Claims`)

# Étape 4 : Filtrer uniquement les produits contenant ces claims
gnpd_top_claims <- gnpd_long %>%
  filter(`Positioning Claims` %in% top_claims_chers)

# Étape 5 : Tracer le boxplot
ggplot(gnpd_top_claims %>% 
         filter(`Price per 100 g/ml in Euros`<= quantile(`Price per 100 g/ml in Euros`,0.99, na.rm = T)), 
       aes(x = fct_reorder(`Positioning Claims`,
                                            `Price per 100 g/ml in Euros`,
                                            .fun = median), y = `Price per 100 g/ml in Euros`)) +
  geom_boxplot(fill = "#6a4c93") +
  labs(
    title = "top 15 des Positioning Claims 
    les plus chers des top 30 les 
    les plus fréquents",
    x = "Positioning Claim",
    y = "Prix du thé pour 100g/ml",
    caption = "Source : Mintel"
  ) +
  coord_flip() +  # optionnel : pivote pour une lecture plus facile
  theme_minimal()

##################### Autre methode ############################################

# Étape 1 : Séparer les claims en lignes individuelles
gnpd_long <- gnpd_tea %>%
  filter(!is.na(`Positioning Claims`)) %>%
  separate_rows(`Positioning Claims`, sep = ",\\s*")

# Étape 2 : Identifier les 15 claims avec le prix médian le plus élevé
top_claims <- gnpd_long %>%
  group_by(`Positioning Claims`) %>%
  summarise(median_price = median(`Price per 100 g/ml in Euros`, na.rm = TRUE)) %>%
  arrange(desc(median_price)) %>%
  slice_head(n = 15) %>%
  pull(`Positioning Claims`)

# Étape 3 : Filtrer les données pour ces 15 claims
gnpd_top_claims <- gnpd_long %>%
  filter(`Positioning Claims` %in% top_claims)

# Étape 4 : Tracer le boxplot pour les claims les plus chers
ggplot(gnpd_top_claims %>% 
         filter(`Price per 100 g/ml in Euros` <= quantile(`Price per 100 g/ml in Euros`, 0.99, na.rm = TRUE)),
       aes(x = fct_reorder(`Positioning Claims`, `Price per 100 g/ml in Euros`, .fun = median),
           y = `Price per 100 g/ml in Euros`)) +
  geom_boxplot(fill = "#6a4c93") +
  labs(
    title = "Prix des 15 Positioning Claims les plus chers",
    x = "Positioning Claim",
    y = "Prix pour 100g/ml",
    caption = "Source : Mintel GNPD"
  ) +
  coord_flip() +
  theme_minimal()


################################################################################
#                  Analyse prix vs package
################################################################################

gnpd_tea<- gnpd_tea %>% 
  filter(`Price per 100 g/ml in Euros`<= quantile(`Price per 100 g/ml in Euros`,0.99, na.rm = T))

# Creation d'une nouvelle variable 
gnpd_tea <- gnpd_tea %>%
  mutate(material_group = case_when(
    str_detect(`Package Material`, regex("Plastic", ignore_case = TRUE)) ~ "Plastique",
    str_detect(`Package Material`, regex("Paper|Board", ignore_case = TRUE)) ~ "Papier / Carton",
    str_detect(`Package Material`, regex("Glass", ignore_case = TRUE)) ~ "Verre",
    str_detect(`Package Material`, regex("Metal|Metallised", ignore_case = TRUE)) ~ "Métal",
    str_detect(`Package Material`, regex("Wood|Pulp|Corrugated", ignore_case = TRUE)) ~ "Bois / Autres",
    str_detect(`Package Material`, regex("foil|lined|multi", ignore_case = TRUE)) ~ "Composites",
    is.na(`Package Material`) | `Package Material` == "Miscellaneous" ~ "Non spécifié",
    TRUE ~ "Autre"
  ))

gnpd_tea <- gnpd_tea %>%
  mutate(package_group_type = case_when(
    str_detect(`Package Type`, regex("Flexible|Sachet|Pouch|Stick|Sleeve|Bag", ignore_case = TRUE)) ~ "Flexible",
    str_detect(`Package Type`, regex("Rigid|Carton|Jar|Can|Bottle|Tub|Tube|Tottle|Case", ignore_case = TRUE)) ~ "Rigide",
    str_detect(`Package Type`, regex("Composite|Tray|Blister|Skinpack|Aerosol", ignore_case = TRUE)) ~ "Semi-rigide / Composite",
    is.na(`Package Type`) | `Package Type` == "Miscellaneous" ~ "Non spécifié",
    TRUE ~ "Autre"
  ))



ggplot(gnpd_tea %>% 
         filter(!is.na(`Package Material`)),
       aes(x = fct_lump(`Package Material`,10), 
                       y = `Price per 100 g/ml in Euros`)) +
  geom_boxplot(fill = "#f4a261") +
  labs(title = "Prix du thé selon le type d'emballage", 
       x = "Type d'emballage",
       y = "Prix pour 100g/ml",
       caption = "Source : Mintel") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

ggplot(gnpd_tea %>% 
         filter(!is.na(`Package Type`)), aes(x = fct_lump(`Package Type`, 10), y = `Price per 100 g/ml in Euros`)) +
  geom_boxplot(fill = "#f4a261") +
  labs(title = "Prix du thé selon le materiel d'emballage utilisé",
       x = "Materiel d'emballage",
       y = "Prix pour 100g/ml",
       caption = "Source : Mintel") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


unique(gnpd_tea$`Package Type`)


################################################################################

# Filtrage des deux sous-catégories de thé
tea_data <- gnpd %>%
  filter(
    `Sub-Category` %in% c("RTD (Iced) Tea", "Tea"),
    !is.na(price_per_100),
    !is.na(month),price_per_100<400
    
  )
tea_data <- gnpd_tea
tea_data$price_per_100 <- tea_data$`Price per 100 g/ml in Euros`
# Calcul du prix moyen par mois et sous-catégorie
evol_prix_tea <- tea_data %>%
  group_by(year, `Sub-Category`) %>%
  summarise(mean_price = mean(price_per_100, na.rm = TRUE), .groups = "drop")

# Visualisation
ggplot(evol_prix_tea, aes(x = year, y = mean_price, color = `Sub-Category`)) +
  geom_line(size = 1.2) +
  labs(
    title = "Évolution annuelle du prix du thé",
    x = "Année",
    y = "Prix moyen (€/100g ou ml)",
    color = "Sous-catégorie",
    caption = "Source : base Mintel"
  ) +
  theme_minimal()


