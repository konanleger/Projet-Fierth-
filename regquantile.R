#-------------------------------------------------------------------------------
# """"""""" REGRESSION QUATILE """""""""""
#         =====================
#-------------------------------------------------------------------------------

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
library(ggplot2)
library(gganimate)
library(gridExtra)
library(ggrepel)
library(quantreg)

# Importation de la base de données
df <- read_excel("GNPD_tea.xlsx")

# Aperçu des varaibles
glimpse(df)

# Resumé statistique 
summary(df)

# convertissement de certaine variable en numerique 
df <- df %>%
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

# Creation de la variable region
df <- df %>% 
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

# Creation de nouvelles variables pour le positionnement

claims_sante <- c(
  "Antioxidant", "Functional - Energy", "Functional - Slimming", 
  "Functional - Immune System", "Functional - Brain & Nervous System",
  "Functional - Cardiovascular", "Functional - Digestive", 
  "Functional - Bone Health", "Functional - Stress & Sleep",
  "Functional - Beauty Benefits", "Functional - Skin", 
  "Functional - Eye Health", "Functional - Weight & Muscle Gain",
  "Anti-Ageing", "Nails & Hair", "Vitamin/Mineral Fortified",
  "High/Added Fibre", "High/Added Protein", "Added Calcium",
  "Prebiotic", "Probiotic", "Stanols/Sterols", "Whitening",
  "Breath-Freshening", "High Satiety","Low/No/Reduced Transfat", 
  "Low/No/Reduced Glycemic", "Low/No/Reduced Saturated Fat", 
  "Low/No/Reduced Carb","Low/No/Reduced Cholesterol",
  "Low/No/Reduced Lactose", "Low/No/Reduced Allergen"
)

# 2. Naturalité & Propreté (free-from + caractéristiques produits)
claims_naturel <- c(
  "All Natural Product", "No Additives/Preservatives",
  "Free from Added/Artificial Flavourings", 
  "Free from Added/Artificial Additives",
  "Free from Added/Artificial Preservatives",
  "Free from Added/Artificial Colourings",
  "GMO Free", "Gluten Free", "Dairy Free", 
  "Palm Oil Free", "Hormone Free", "Alcohol Free",
  "Low/No/Reduced Transfat", "Low/No/Reduced Glycemic",
  "Low/No/Reduced Saturated Fat", "Low/No/Reduced Carb",
  "Low/No/Reduced Cholesterol", "Innovative Ingredient","Plant Based"
)

# 3. Ciblage & Besoins Spécifiques 
claims_ciblage <- c(
  # Démographie
  "Children (5-12)", "Babies & Toddlers (0-4)", 
  "Female", "Male", "Maternal", "Seniors (aged 55+)",
  # Régimes
  "Vegetarian", "Vegan/No Animal Ingredients", 
  "Kosher", "Halal", "Diabetic", "Diet/Light",
  "Plant Based"
)

# 4. Éthique & Durabilité 
claims_ethique <- c(
  "organic", "ethical - environmentally friendly package", "ethical - recycling",
  "ethical - environmentally friendly product", "ethical - human",
  "ethical - sustainable (habitat/resources)", "ethical - toxins free",
  "ethical - biodegradable", "ethical - charity", "carbon neutral", 
  "ethical - animal","biodegradable packaging", "sustainable", "ethical"
)

# 5. Packaging & Convenience 
claims_strategie <- c(
  "convenient packaging", "time/speed", "ease of use", "interesting packaging",
  "microwaveable", "refill/refillable", "portionability", "on-the-go",
  "biodegradable packaging","premium", "social media", "seasonal", "limited edition", 
  "event merchandising", "novel", "cobranded","economy","Wholegrain"
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
df <- df %>%
  mutate(
    claim_sante = map_chr(`Positioning Claims`, ~ detect_claim(.x, claims_sante)),
    claim_naturel = map_chr(`Positioning Claims`, ~ detect_claim(.x, claims_naturel)),
    claim_ethique = map_chr(`Positioning Claims`, ~ detect_claim(.x, claims_ethique)),
    claim_strategie = map_chr(`Positioning Claims`, ~ detect_claim(.x, claims_strategie)),
    claim_ciblage = map_chr(`Positioning Claims`, ~ detect_claim(.x, claims_ciblage))
  )

rm(claims_ciblage,claims_ethique,claims_naturel,claims_sante,claims_strategie)
# convertir ces variables en facteur:

# Creation d'une nouvelle variable 
df <- df %>%
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

################################################################################
###############  MODELISATION  #################################################
###############  ============  #################################################
################################################################################


# Sélectionner les variables indépendantes pour le modèle 
X <- df %>% 
    select(Region_Marche,`Launch Type`,`Private Label`,`Famille thé`,claim_sante,
           claim_naturel,claim_ethique,claim_strategie,claim_ciblage)

# Ajout une constante
X <- cbind(1, X)
colnames(X)[1] <- "Intercept"  

# La variable dépendante est le prix
y <- df$`Price per 100 g/ml in Euros`


# Ajustement du modèle de régression quantile

# Le modèle pour le quantile médian
model_50 <- rq(`Price per 100 g/ml in Euros` ~ Region_Marche+`Launch Type`+`Private Label`+
                 `Famille thé`+ claim_sante+claim_ethique+claim_ciblage+claim_strategie+claim_naturel, 
               data = df, tau = c(0.1,0.25,0.5,0.75,0.9))

# Résumé du modèle
summary(model_50)
rm(X,y,model_50)

################################################################################
############################# THE ##############################################
################################################################################
# Considerons à present le thé uniquement
df_tea <- df %>% 
  filter(`Sub-Category` == "Tea")



df_tea <- df_tea %>%
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

df_tea <- df_tea %>%
  mutate(package_group_type = case_when(
    str_detect(`Package Type`, regex("Flexible|Sachet|Pouch|Stick|Sleeve|Bag", ignore_case = TRUE)) ~ "Flexible",
    str_detect(`Package Type`, regex("Rigid|Carton|Jar|Can|Bottle|Tub|Tube|Tottle|Case", ignore_case = TRUE)) ~ "Rigide",
    str_detect(`Package Type`, regex("Composite|Tray|Blister|Skinpack|Aerosol", ignore_case = TRUE)) ~ "Semi-rigide / Composite",
    is.na(`Package Type`) | `Package Type` == "Miscellaneous" ~ "Non spécifié",
    TRUE ~ "Autre"
  ))


# Sélectionner les variables indépendantes pour le modèle 
Z <- df_tea %>% 
  select(Region_Marche,`Launch Type`,`Private Label`,claim_sante,claim_naturel,
         claim_strategie,claim_ethique,claim_ciblage)

# Ajout une constante
Z <- cbind(1, Z)
colnames(Z)[1] <- "Intercept" 

# La variable dépendante est le prix
y <- df_tea$`Price per 100 g/ml in Euros`

# modele
model_tea <- rq(`Price per 100 g/ml in Euros` ~ Region_Marche+`Launch Type`+
                 `Private Label`+ claim_sante+claim_ethique+claim_ciblage+
                 claim_strategie+claim_naturel, 
               data = df_tea, tau = c(0.25,0.5,0.75))

summary(model_tea)
?? quantreg
####################### Remarque ###############################################
"se = boot : Bootstrap (méthode recommandée pour les petits échantillons ou 
             données non normales).

se = nid : Approximation asymptotique (moins robuste).

se = ker : Méthode kernel (pour grands échantillons)."
################################################################################