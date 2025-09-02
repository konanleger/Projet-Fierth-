#-------------------------------------------------------------------------------

#######    Nettoyage de l'environnement ###########
rm(list = ls())
################ {Instalation et importation des packages necessaires}  ########
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
library(forcats)

########## {Importation de la base de données}  ################################
df1 <- read.csv("df_pib.csv")
df2 <- read.csv("df_prod_tea.csv")
df <- read_excel("GNPD_tea.xlsx")

df$pib_par_habitant <- df1$pib_par_habitant_imputed
df$production_tea_tonne <- df2$production_tea_tonne
rm(df1,df2)

##### Aperçu des varaibles & Resumé statistique  ###############################
glimpse(df)
summary(df)

# convertissement de certaine variable en numerique  ###########################
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

# Creation de la variable region  ##############################################
df <- df %>% 
  mutate(
    Region_Marche = case_when(
      # Océanie
      Market %in% c("Australia", "New Zealand") ~ "Oceanie",
      
      # Europe de l'Ouest
      Market %in% c("UK", "France", "Germany", "Italy", "Spain", "Switzerland", 
                    "Netherlands", "Belgium", "Austria", "Portugal", "Ireland", 
                    "Luxembourg", "Monaco","Denmark","Finland","Greece",
                    "Sweden","Norway") ~ "Europe_Ouest",
      
      # Europe de l'Est
      Market %in% c("Poland", "Czech Republic", "Slovakia", "Hungary", "Romania", 
                    "Bulgaria", "Croatia", "Serbia", "Ukraine", "Belarus","Russia", 
                    "Slovenia", "Estonia", "Latvia", "Lithuania") ~ "Europe_Est",
      
      # Moyen-Orient
      Market %in% c("UAE", "Saudi Arabia", "Israel", "Turkey", "Iran", "Jordan", 
                    "Oman", "Qatar", "Kuwait", "Lebanon", "Bahrain", "Pakistan") ~ "Moyen_Orient",
      
      # Afrique
      Market %in% c("South Africa", "Kenya", "Tanzania", "Uganda", "Nigeria", 
                    "Egypt", "Morocco", "Algeria", "Tunisia", "Ghana", 
                    "Ivory Coast", "Ethiopia", "Cameroon") ~ "Afrique",
      
      # Asie de l'Est
      Market %in% c("China", "Japan", "South Korea", "Taiwan, China", "Hong Kong,China","Hong Kong, China") ~ "Asie_Orientale",
      
      # Asie du Sud
      Market %in% c("India", "Sri Lanka", "Bangladesh","Vietnam", "Thailand", "Malaysia", "Singapore", "Indonesia", "Philippines", "Myanmar", "Cambodia", "Laos") ~ "Asie_du_Sud",
      Market %in% c("USA", "Canada", "Mexico", "Puerto Rico") ~ "Amérique du Nord",
      Market %in% c("Brazil", "Argentina", "Colombia", "Chile", "Peru", "Venezuela", 
                    "Ecuador", "Paraguay", "Uruguay", "Panama", "Costa Rica", 
                    "Guatemala") ~ "Amérique du Sud",
      
      TRUE ~ "Autres"
    )
  )

# Creation de nouvelles variables pour le positionnement #######################

# 1. Santé & Bien-être
claims_sante <- c(
  "Functional - Energy","Functional - Other","Functional - Brain & Nervous System",
  "Functional - Digestive","Functional - Immune System","Antioxidant",
  "Functional - Bone Health","Functional - Skin, Nails & Hair","Functional - Cardiovascular","Probiotic",
  "Functional - Stress & Sleep(5)","Water Resistant","Functional - Weight & Muscle Gain",
  "Functional - Beauty Benefits","Functional - Eye Health","Functional - Slimming",
  "High Satiety","Prebiotic","Anti-Bacterial","Anti-Perspirant","Breath-Freshening",
  "Cleansing","Exfoliating","Homeopathic","Long-Lasting","Odour Neutralising",
  "Protects Against Elements","UV Protection","Waterproof","No Added Sugar",
  "Low/Reduced Sugar","Sugar Free","Low/No/Reduced Calorie","Low/No/Reduced Carb",
  "Low/No/Reduced Sodium","Diet/Light","Low/No/Reduced Fat","Low/No/Reduced Cholesterol",
  "Low/No/Reduced Glycemic","Low/No/Reduced Saturated Fat","Low/No/Reduced Transfat",
  "Low/Reduced Alcohol","Not Pasteurised","High/Added Fibre","Vitamin/Mineral Fortified","High/Added Protein",
  "Added Calcium","Stanols/Sterols","Low/No/Reduced Allergen","Low/No/Reduced Lactose","Diabetic",
  # Product tested:
  "Dermatologically Tested","Allergy Tested","For Balanced Skin","For Combination Skin","For Dry Skin","For Oily Skin","For Sensitive Skin","For Sensitive","Teeth/Gums","Hypoallergenic","Non-Acnegenic","Non-Comedogenic","Ophthalmologically Tested","Skin Disorders","Skin Disorders - Dermatitis","Skin Disorders - Diaper Rash","Skin Disorders - Eczema","Skin Disorders - Psoriasis","Skin Disorders - Warts",
  ## beauté :
  "Anti-Ageing","All Skin Tones","Anti-Acne","Anti-Cellulite","Anti-Dandruff","Anti-Hairloss","Brightening / Illuminating","Collagen Increasing","Damaged Hair","Firming","Gradual Self-Tanning","Mattifying","Moisturising / Hydrating","Plumping","Reduces Dark Circles / Puffiness","Reduces Fine Lines / Wrinkles","Reduces Redness","Reduces the Appearance of Pores","Slimming","Toning","Whitening"
)

# 2. Naturalité & Propreté
claims_naturel <- c(
  "Botanical/Herbal","All Natural Product","GMO Free","No Additives/Preservatives",
  "Free from Added/Artificial Preservatives","Free from Added/Artificial Colourings",
  "Free from Added/Artificial Flavourings","Free from Added/Artificial Additives",
  "Wholegrain","Aromatherapy","Dairy Free","Alcohol Free","Hormone Free","Caffeine Free",
  "Palm Oil Free","Fragrance Free","Grain Free","Mineral","Oil/Petroleum Free",
  "Oil Free","Paraben Free","pH Balanced","Silicone Free","Sulphate/Sulfate Free"
)

# 3. Ciblage & Besoins Spécifiques 
claims_ciblage <- c(
  # Demographie
  "Children (5-12)", "Babies & Toddlers (0-4)", 
  "Female", "Male", "Maternal", "Seniors (aged 55+)","teenagers (13-17)",
  # COnviens pour
  "Vegetarian", "Vegan/No Animal Ingredients", "Gluten Free","Low/No/Reduced Lactose",
  "Kosher", "Halal", "Diabetic", "Diet/Light","Low/No/Reduced Allergen",
  "Plant Based",
  # cyblage animaux
  "Anti-Parasite","Functional Pet - Brain & Nervous System","Functional Pet - Digestion",
  "Functional Pet - Eyesight","Functional Pet - Heart & Cardiovascular System",
  "Functional Pet - Immune System","Functional Pet - Joints, Bones & Muscles",
  "Functional Pet - Other","Functional Pet - Skin & Coat","Functional Pet - Slimming",
  "Functional Pet - Teeth & Tartar Prevention","Functional Pet - Urinary Tract",
  "Functional Pet - Weight & Muscle Gain","Pet - Adult","Pet - Junior","Pet - Senior"
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
  "biodegradable packaging","premium", "social media", "seasonal", "limited edition","Personalised Formulation","Innovative Ingredient","Doctor Brand",
  "event merchandising", "novel", "cobranded","economy","Wholegrain"
)


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
    claim_ciblage = map_chr(`Positioning Claims`, ~ detect_claim(.x, claims_ciblage)),
    claim_bio = map_chr(`Positioning Claims`, ~ detect_claim(.x, claims_bio)),
    claim_environnement = map_chr(`Positioning Claims`, ~ detect_claim(.x, claims_environnement))
  )

rm(claims_ciblage,claims_ethique,claims_naturel,claims_sante,claims_strategie,
   claims_environnement,claims_bio,detect_claim)


# Creation d'une nouvelle catégorie des produits  ##############################
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


# Categorisation des materiels packages ######################################## 
df <- df %>%
  mutate(material_group = case_when(
    str_detect(`Package Material`, regex("Plastic", ignore_case = TRUE)) ~ "Plastique",
    str_detect(`Package Material`, regex("Paper|Wood", ignore_case = TRUE)) ~ "Papier ou Bois",
    str_detect(`Package Material`, regex("Glass", ignore_case = TRUE)) ~ "Verre",
    str_detect(`Package Material`, regex("Metal|Metallised", ignore_case = TRUE)) ~ "Métal",
    is.na(`Package Material`) | `Package Material` == "Miscellaneous" ~ "Autre",
    TRUE ~ "Autre"
  ))

df <- df %>%
  mutate(package_group_type = case_when(
    str_detect(`Package Type`, regex("Can", ignore_case = TRUE)) ~ "Can",
    str_detect(`Package Type`, regex("Carton", ignore_case = TRUE)) ~ "Carton",
    is.na(`Package Type`) | `Package Type` == "Miscellaneous" ~ "Autre",
    str_detect(`Package Type`, regex("flexible", ignore_case = TRUE)) ~ "flexible",
    str_detect(`Package Type`, regex("jar", ignore_case = TRUE)) ~ "Jar",
    str_detect(`Package Type`, regex("Tub", ignore_case = TRUE)) ~ "Tub",
    str_detect(`Package Type`, regex("Rigid", ignore_case = TRUE)) ~ "Rigid",
    TRUE ~ "Autre"
  ))

########### pib par habitant  ##################################################

df <- df %>%
  mutate(
    pib_categorie = case_when(
      pib_par_habitant < 1041 ~ "Faible (PIB < 10.41)",
      pib_par_habitant >= 1041 & pib_par_habitant < 2537 ~ "Intermediare inf (10.41-25.37)",
      pib_par_habitant >= 2537 & pib_par_habitant < 4235 ~ "Intermediaire sup (25.37-42.35)",
      pib_par_habitant >= 4235 & pib_par_habitant <= 6822 ~ "Élevé (42.35-68.22)",
      pib_par_habitant > 6822 ~ "Très Élevé (PIB > 68.22)",
      is.na(pib_par_habitant) ~ "Non disponible"
    ),
    pib_categorie = factor(pib_categorie,
                           levels = c("Intermediaire sup (25.37-42.35)",
                                      "Faible (PIB < 10.41)", 
                                      "Intermediare inf (10.41-25.37)",
                                      "Élevé (42.35-68.22)",
                                      "Très Élevé (PIB > 68.22)", 
                                      "Non disponible"),
                           ordered = F)
  )


################    production de thé en tonne #################################

df <- df %>%
  mutate(
    categorie_prod = case_when(
      production_tea_tonne <= 495.0 ~ "Très faible (≤495 t)",
      production_tea_tonne > 495.0 & production_tea_tonne <= 8785.92 ~ "Faible (496-8,786 t)",
      production_tea_tonne > 8785.92 & production_tea_tonne <= 87497.75 ~ "Moyenne (8,787-87,498 t)",
      production_tea_tonne > 87497.75 & production_tea_tonne <= 1440118.1 ~ "Élevée (87,499-1,440,118 t)",
      production_tea_tonne > 1440118.1 ~ "Très élevée (>1,440,118 t)",
      is.na(production_tea_tonne) ~ "Pas de production"
    ),
    # Conversion en facteur ordonné
    categorie_prod = factor(
      categorie_prod,
      levels = c("Moyenne (8,787-87,498 t)",
                 "Très faible (≤495 t)", 
                 "Faible (496-8,786 t)",
                 "Élevée (87,499-1,440,118 t)", 
                 "Très élevée (>1,440,118 t)",
                 "Pas de production"),
      ordered = FALSE
    )
  )

######## {convertir ces variables en facteur} ##################################

df <- df %>%
  mutate(
    Region_Marche = fct_relevel(Region_Marche,"Amérique du Nord"),
    package_group_type = fct_relevel(package_group_type, "Carton"),
    material_group = fct_relevel(material_group,"Plastique"),
    type_lance = fct_relevel(`Launch Type`, "New Product")
  )


##############"   { le prix corriger de l'inflation } ##########################

inflation_rates <- data.frame(
  year = 1998:2025,
  Inflation_pct = c(0.6, 0.5, 1.7, 1.6, 1.9, 2.1, 2.1, 1.7, 1.7,1.5,2.8,0.1,1.5,2.1,2.0,0.9,0.5,0.0,0.2,1.0,1.9,1.1,0.5,1.6,5.2,4.9,2.0,NA)
)

# construction de l'IPC
inflation_rates$IPC <- NA
inflation_rates$IPC[1] <- 100

for (i in 2:nrow(inflation_rates)) {
  inflation_rates$IPC[i] <- inflation_rates$IPC[i - 1] * (1 + inflation_rates$Inflation_pct[i] / 100)
}



# joindre le taux d'inflation et l'IPC à la base
df <- df %>%
  left_join(inflation_rates[, c("year", "IPC")], by = "year")

# choix de l'année de reference 
ipc_ref <- inflation_rates$IPC[inflation_rates$year == 2024]

# Correction du prix
df <- df %>%
  mutate(Prix_corrige = `Price per 100 g/ml in Euros` * (ipc_ref / IPC))
rm(inflation_rates,i,ipc_ref)

################# [{ Base prete à l'emploie }] #################################
### Sauvegarde de la base propre

saveRDS(df, file = "df_final.rds")
rm(list = ls())
