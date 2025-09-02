

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
gnpd_brut <- readRDS("df_final.rds")

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
#         Statistique descriptive
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



gnpd <- gnpd_brut
# Claims marketing
# Extraction des claims individuels
claims <- gnpd %>%
  filter(!is.na(`Positioning Claims`),
         `Sub-Category`=="Tea") %>%
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


######################### Focus sur la France ##################################

# Claims marketing
# Extraction des claims individuels
claims <- gnpd %>%
  filter(!is.na(`Positioning Claims`),
         `Sub-Category`=="Tea",
         Market == "France") %>%
  unnest_tokens(claim, `Positioning Claims`, token = "regex",
                pattern = ",\\s*") %>%
  count(claim, sort = TRUE)
# Barplot des claims les plus fréquents
claims %>%
  slice_max(n, n = 15) %>%
  ggplot(aes(x = reorder(claim, n), y = n)) +
  geom_col(fill = "blue3") +
  coord_flip() +
  labs(title = "Principaux arguments marketing thé brut ",
       subtitle = "Pour le marché 2000-2025",
       x = "Claim", 
       y = "Fréquence",
       caption = "Source : Base Mintel")

################### {Nuage de mots des claims}  ################################
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

# Définir les mots-clés pour chaque catégorie
# 1. Santé & Bien-être
claims_sante <- c(
  "Functional - Energy","Functional - Other","Functional - Brain & Nervous System",
  "Functional - Digestive","Functional - Immune System","Antioxidant",
  "Functional - Bone Health","Functional - Skin, Nails & Hair","Functional - Cardiovascular",
  "Probiotic","Functional - Stress & Sleep(5)
Water Resistant","Functional - Weight & Muscle Gain","Functional - Beauty Benefits",
  "Functional - Eye Health","Functional - Slimming","High Satiety","Prebiotic",
  "Anti-Bacterial","Anti-Perspirant","Breath-Freshening","Cleansing","Exfoliating",
  "Homeopathic","Long-Lasting","Odour Neutralising","Protects Against Elements",
  "UV Protection","Waterproof","No Added Sugar","Low/Reduced Sugar","Sugar Free",
  "Low/No/Reduced Calorie","Low/No/Reduced Carb","Low/No/Reduced Sodium","Diet/Light",
  "Low/No/Reduced Fat","Low/No/Reduced Cholesterol","Low/No/Reduced Glycemic",
  "Low/No/Reduced Saturated Fat","Low/No/Reduced Transfat","Low/Reduced Alcohol",
  "Not Pasteurised","High/Added Fibre","Vitamin/Mineral Fortified","High/Added Protein",
  "Added Calcium","Stanols/Sterols","Low/No/Reduced Allergen","Low/No/Reduced Lactose","Diabetic",
  ## beauté :
  "Anti-Ageing","All Skin Tones","Anti-Acne","Anti-Cellulite","Anti-Dandruff","Anti-Hairloss",
  "Brightening / Illuminating","Collagen Increasing","Damaged Hair","Firming",
  "Gradual Self-Tanning","Mattifying","Moisturising / Hydrating","Plumping",
  "Reduces Dark Circles / Puffiness","Reduces Fine Lines / Wrinkles","Reduces Redness",
  "Reduces the Appearance of Pores","Slimming","Toning","Whitening"
)

# 2. Naturalité & Propreté
claims_naturel <- c(
  "Botanical/Herbal","All Natural Product","GMO Free","No Additives/Preservatives",
  "Free from Added/Artificial Preservatives","Free from Added/Artificial Colourings",
  "Free from Added/Artificial Flavourings","Free from Added/Artificial Additives",
  "Wholegrain","Aromatherapy","Dairy Free","Alcohol Free","Hormone Free","Caffeine Free",
  "Palm Oil Free","Fragrance Free","Grain Free","Mineral","Oil/Petroleum Free","Oil Free",
  "Paraben Free","pH Balanced","Silicone Free","Sulphate/Sulfate Free"
)

# 3. Ciblage & Besoins Spécifiques 
claims_ciblage <- c(
  # Demographie
  "Children (5-12)", "Babies & Toddlers (0-4)", 
  "Female", "Male", "Maternal", "Seniors (aged 55+)","teenagers (13-17)",
  "Ethnic",
  # COnviens pour
  "Vegetarian", "Vegan/No Animal Ingredients", "Gluten Free","Low/No/Reduced Lactose",  "Kosher", "Halal", "Diabetic", "Diet/Light","Low/No/Reduced Allergen",
  "Plant Based",
  # cyblage animaux
  "Anti-Parasite","Functional Pet - Brain & Nervous System","Functional Pet - Digestion","Functional Pet - Eyesight","Functional Pet - Heart & Cardiovascular System","Functional Pet - Immune System","Functional Pet - Joints, Bones & Muscles","Functional Pet - Other","Functional Pet - Skin & Coat","Functional Pet - Slimming","Functional Pet - Teeth & Tartar Prevention","Functional Pet - Urinary Tract","Functional Pet - Weight & Muscle Gain","Pet - Adult","Pet - Junior","Pet - Senior"
)

# 4. Éthique 
claims_ethique <- c(
  "ethical - human", "ethical - charity", "ethical - animal", "ethical",
  "fair trade", "equitable", "social responsibility"
)

# 5. Bio
claims_bio <- c(
  "organic", "bio", "biologique", "non-GMO","GMO Free",
  "pesticide-free"
)

# 6. Environnement
claims_environnement <- c(
  "ethical - environmentally friendly package", "ethical - recycling",
  "ethical - sustainable (habitat/resources)", "ethical - toxins free",
  "ethical - biodegradable", "carbon neutral", "biodegradable packaging",
  "sustainable", "recyclable", "compostable", "eco-friendly", "zero waste"
)

# 7. Packaging & Convenience 
claims_strategie <- c(
  "convenient packaging", "time/speed", "ease of use", "interesting packaging",
  "microwaveable", "refill/refillable", "portionability", "on-the-go",
  "biodegradable packaging","premium", "social media", "seasonal", "limited edition",
  "Personalised Formulation","Innovative Ingredient","Doctor Brand",
  "event merchandising", "novel", "cobranded","economy","Wholegrain",
  # Product tested:
  "Dermatologically Tested","Allergy Tested","For Balanced Skin","For Combination Skin",
  "For Dry Skin","For Oily Skin","For Sensitive Skin","For Sensitive","Teeth/Gums",
  "Hypoallergenic","Non-Acnegenic","Non-Comedogenic","Ophthalmologically Tested",
  "Skin Disorders","Skin Disorders - Dermatitis","Skin Disorders - Diaper Rash",
  "Skin Disorders - Eczema","Skin Disorders - Psoriasis","Skin Disorders - Warts"
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
    claim_ciblage = map_chr(`Positioning Claims`, ~ detect_claim(.x, claims_ciblage)),
    claim_bio = map_chr(`Positioning Claims`, ~ detect_claim(.x, claims_bio)),
    claim_environnement = map_chr(`Positioning Claims`, ~ detect_claim(.x, claims_environnement))
  )

rm(claims_ciblage,claims_ethique,claims_marketing,claims_nutrition,
   claims_sante,claims_sante,claims_environnement,claims_bio)




unique(claims[,1])

################################################################################

# Type d'embalage
gnpd_brut %>%
  filter(!is.na(`Package Type`),
         `Sub-Category` == "Tea") %>% 
  count(`Package Type`, sort = TRUE) %>%
  slice_max(n, n = 20) %>%
  ggplot(aes(x = reorder(`Package Type`, n), y = n)) +
  geom_col(fill = "darkgreen") +
  coord_flip() +
  labs(title = "Top 20 des types d'emballage du thé brut", 
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




########################### les embalages du thé brut ##########################
library(dplyr)
library(ggplot2)

# Définir le nombre de catégories à afficher (top N)
top_n <- 10  #  modifiable

gnpd_brut %>%
  filter(!is.na(`Package Type`),
         `Sub-Category` == "Tea") %>% 
  # Compter et classer les types d'emballage
  count(`Package Type`, sort = TRUE) %>%
  # Créer une nouvelle colonne pour les catégories regroupées
  mutate(`Package Type Grouped` = ifelse(row_number() <= top_n, 
                                         as.character(`Package Type`), 
                                         "Autres")) %>%
  # Regrouper et sommer les "Autres"
  group_by(`Package Type Grouped`) %>%
  summarise(n = sum(n)) %>%
  # Trier par nombre décroissant (avec "Autres" en dernier)
  arrange(ifelse(`Package Type Grouped` == "Autres", Inf, -n)) %>%
  # Créer le graphique
  ggplot(aes(x = reorder(`Package Type Grouped`, n), y = n)) +
  geom_col(fill = "#26C4EC") +
  coord_flip() +
  labs(title = paste("Top", top_n, "des types d'emballage du thé"), 
       subtitle = "Les autres types sont regroupés dans 'Autres'",
       x = "Type d'emballage",
       y = "Nombre de produits",
       caption = "Source : Mintel") +
  theme_minimal()


##############       { materiau d'ambalage}  ###################################
top_n <- 15  #  modifiable

gnpd_brut %>%
  filter(!is.na(`Package Material`),
         `Sub-Category` == "Tea") %>% 
  # Compter et classer les types d'emballage
  count(`Package Material`, sort = TRUE) %>%
  # Créer une nouvelle colonne pour les catégories regroupées
  mutate(`Package Material Grouped` = ifelse(row_number() <= top_n, 
                                         as.character(`Package Material`), 
                                         "Autres")) %>%
  # Regrouper et sommer les "Autres"
  group_by(`Package Material Grouped`) %>%
  summarise(n = sum(n)) %>%
  # Trier par nombre décroissant (avec "Autres" en dernier)
  arrange(ifelse(`Package Material Grouped` == "Autres", Inf, -n)) %>%
  # Créer le graphique
  ggplot(aes(x = reorder(`Package Material Grouped`, n), y = n)) +
  geom_col(fill = "#048B9A") +
  coord_flip() +
  labs(title = paste("Top", top_n, "des materiaux d'emballage du thé"), 
       subtitle = "Les autres materiaux sont regroupés dans 'Autres'",
       x = "materiaux d'emballage",
       y = "Nombre de produits",
       caption = "Source : Mintel") +
  theme_minimal()


################################################################################

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
# gnpd_clean <- gnpd_claim %>%
#   filter(!is.na(`Positioning Claims`)) %>%
#   mutate(claim_organic = ifelse(str_detect(`Positioning Claims`, "Organic"), "Organic", "Non-Organic"))
# 
# 
# 
# gnpd_clean <- gnpd_clean %>%
#   filter(!is.na(`Positioning Claims`)) %>%
#   mutate(claim_category = case_when(
#     str_detect(`Positioning Claims`, regex("Ethical", ignore_case = TRUE)) ~ "Ethical",
#     str_detect(`Positioning Claims`, regex("Functional", ignore_case = TRUE)) ~ "Functional",
#     str_detect(`Positioning Claims`, regex("Environmentally|friendly", ignore_case = TRUE)) ~ "Environmentally Friendly",
#     str_detect(`Positioning Claims`, regex("Artificial", ignore_case = TRUE)) ~ "Artificial",
#     str_detect(`Positioning Claims`, regex("Additives", ignore_case = TRUE)) ~ "Additives",
#     str_detect(`Positioning Claims`, regex("Recycling", ignore_case = TRUE)) ~ "Recycling",
#     str_detect(`Positioning Claims`, regex("Organic", ignore_case = TRUE)) ~ "Organic",
#     TRUE ~ "Other"
#   ))
gnpd_clean <- gnpd_claim %>% 
  filter(!is.na(`Price per 100 g/ml in Euros`),
         `Price per 100 g/ml in Euros`< quantile(`Price per 100 g/ml in Euros`, 0.99, na.rm = T))  # on retire le top 1%

ggplot(gnpd_clean, aes(x = claim_bio, y = `Price per 100 g/ml in Euros`)) +
  geom_boxplot(fill = "#264653") +
  labs(title = "Prix selon présence d'un claim 'Organic'", x = "Positioning Claim", y = "Prix pour 100g/ml")

library(dplyr)
library(tidyr)
library(ggplot2)
library(stringr)
library(forcats)


gnpd_tea <- gnpd_brut %>% 
  filter(`Sub-Category` == "Tea")
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
    subtitle = "Concerne le thé brut uniquement",
    x = "Positioning Claim",
    y = "Prix du thé pour 100g/ml",
    caption = "Source : Mintel"
  ) +
  coord_flip() +  # optionnel : pivote pour une lecture plus facile
  theme_minimal()

########################### Cas de la France ###################################

# Étape 1 : Extraire tous les claims dans des lignes séparées
gnpd_long <- gnpd_tea %>%
  filter(!is.na(`Positioning Claims`),
         Market == "France") %>%
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
    subtitle = "Concerne le thé brut uniquement",
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
    str_detect(`Package Type`, regex("Can", ignore_case = TRUE)) ~ "Can",
    str_detect(`Package Type`, regex("Carton", ignore_case = TRUE)) ~ "Carton",
    str_detect(`Package Type`, regex("Composite", ignore_case = TRUE)) ~ "Composite",
    is.na(`Package Type`) | `Package Type` == "Miscellaneous" ~ "Non spécifié",
    str_detect(`Package Type`, regex("flexible", ignore_case = TRUE)) ~ "flexible",
    str_detect(`Package Type`, regex("jar", ignore_case = TRUE)) ~ "Jar",
    str_detect(`Package Type`, regex("Tub", ignore_case = TRUE)) ~ "Tub",
    TRUE ~ "Autre"
  ))

ggplot(gnpd_tea %>% 
         filter(!is.na(`Package Material`)),
       aes(x = fct_reorder(fct_lump(`Package Material`, 10), 
                           `Price per 100 g/ml in Euros`, 
                           .fun = median, 
                           .desc = TRUE), 
           y = `Price per 100 g/ml in Euros`)) +
  geom_boxplot(fill = "#f4a261") +
  labs(title = "Prix du thé brut selon le type d'emballage", 
       x = "Materiau d'emballage",
       y = "Prix pour 100g/ml (en Euros)",
       caption = "Source : Mintel") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

ggplot(gnpd_tea %>% 
         filter(!is.na(`Package Type`)), 
       aes(x =fct_reorder(fct_lump(`Package Type`, 10),
                          `Price per 100 g/ml in Euros`, 
                          .fun = median, 
                          .desc = TRUE),
           y = `Price per 100 g/ml in Euros`)) +
  geom_boxplot(fill = "#f4a261") +
  labs(title = "Prix du thé brut selon le materiel d'emballage utilisé",
       x = "Type d'emballage",
       y = "Prix pour 100g/ml",
       caption = "Source : Mintel") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


unique(gnpd_tea$`Package Type`)


################################################################################

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

#-------------------------------------------------------------------------------
#                Analyse des compagny      
#               ======================
#-------------------------------------------------------------------------------
gnpd_brut %>% 
  filter(`Company Territory`=="France") %>% 
  pull(Company) %>% 
  unique()
