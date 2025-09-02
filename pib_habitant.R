################  Nettoyage de l'environnement   ###############################

rm(list = ls())


###########  {IMPORTATION DES PACKAGES NECESSAIRES POUR LE TRAITEMENTS} ########
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
library(stringr)
library(zoo)


# Étape 1 : importer correctement les données en sautant les lignes inutiles
pib_hbt <- read_excel("pib.xls", skip = 2) %>%  # Saute les 2 premières lignes
  select(1, 43:69) %>%  # Garde colonne pays et années
  rename(pays_en = 1) %>%  # Renomme la première colonne
  filter(!is.na(pays_en),  # Enlève les lignes vides
         pays_en != "Country Name")  # Enlève la ligne d'en-tête

# Étape 2 : Nettoyage des noms de colonnes
names(pib_hbt) <- c("pays_en", 1998:2024)  # Renomme correctement les colonnes années


# str(pib_hbt) Pour voir la structure du jeu de données

# Pour eviter des problemees plutard
pib_hbt <- pib_hbt %>%
  mutate(pays_en = str_replace(pays_en, 
                                     "Égypte, République arabe d’", 
                                     "Egypte"))

# Vérifier que le changement a bien été appliqué
pib_hbt %>%
  distinct(pays_en) %>%
  filter(str_detect(pays_en, "Egypte"))

# Connaitre les pays disponibles
unique(pib_hbt$pays_en)


# Importation de la deuxieme base de donnees
gnpd_brut <- read_excel("GNPD_tea.xlsx")
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

# Pour faciliter la jointure plutard
correspondances <- c(
  "Italie" = "Italy",
  "Singapour" = "Singapore",
  "Brésil" = "Brazil",
  "Indonésie" = "Indonesia",
  "Viet Nam" = "Vietnam",
  "Malaisie" = "Malaysia",
  "Royaume-Uni" = "UK",
  "Chine" = "China",
  "Nigéria" = "Nigeria",
  "Japon" = "Japan",
  "Pays-Bas" = "Netherlands",
  "États-Unis" = "USA",
  "Philippines" = "Philippines",
  "Corée, République de" = "South Korea",
  "Pologne" = "Poland",
  "Canada" = "Canada",
  "Nouvelle-Zélande" = "New Zealand",
  "Algérie" = "Algeria",
  "Australie" = "Australia",
  "Sri Lanka" = "Sri Lanka",
  "Grèce" = "Greece",
  "Espagne" = "Spain",
  "Pakistan" = "Pakistan",
  "Région administrative spéciale de Chine (Taïwan)" = "Taiwan, China",
  "Mexique" = "Mexico",
  "Afrique du Sud" = "South Africa",
  "Norvège" = "Norway",
  "Inde" = "India",
  "Suisse" = "Switzerland",
  "Colombie" = "Colombia",
  "Portugal" = "Portugal",
  "Irlande" = "Ireland",
  "Turquie" = "Turkey",
  "Thaïlande" = "Thailand",
  "Ghana" = "Ghana",
  "Allemagne" = "Germany",
  "Finlande" = "Finland",
  "France" = "France",
  "Chine, RAS de Hong Kong" = "Hong Kong, China",
  "Egypte" = "Egypt",
  "Bangladesh" = "Bangladesh",
  "Costa Rica" = "Costa Rica",
  "Venezuela" = "Venezuela",
  "Côte d'Ivoire" = "Ivory Coast",
  "Argentine" = "Argentina",
  "Chili" = "Chile",
  "Équateur" = "Ecuador",
  "Lettonie" = "Latvia",
  "Tunisie" = "Tunisia",
  "République tchèque" = "Czech Republic",
  "Danemark" = "Denmark",
  "Tanzanie" = "Tanzania",
  "Pérou" = "Peru",
  "Arabie saoudite" = "Saudi Arabia",
  "Panama" = "Panama",
  "Belgique" = "Belgium",
  "République démocratique populaire lao" = "Laos",
  "Qatar" = "Qatar",
  "Liban" = "Lebanon",
  "Roumanie" = "Romania",
  "Autriche" = "Austria",
  "Koweït" = "Kuwait",
  "République slovaque" = "Slovakia",
  "Guatemala" = "Guatemala",
  "Éthiopie" = "Ethiopia",
  "Cameroun" = "Cameroon",
  "Lituanie" = "Lithuania",
  "Puerto Rico (US)" = "Puerto Rico",
  "Émirats arabes unis" = "UAE",
  "Slovénie" = "Slovenia",
  "Estonie" = "Estonia",
  "Suède" = "Sweden",
  "Jordanie" = "Jordan",
  "Myanmar" = "Myanmar",
  "Croatie" = "Croatia",
  "Maroc" = "Morocco",
  "Hongrie" = "Hungary",
  "Israël" = "Israel",
  "Serbie" = "Serbia",
  "Cambodge" = "Cambodia",
  "Oman" = "Oman",
  "Kenya" = "Kenya",
  "Bulgarie" = "Bulgaria",
  "Ukraine" = "Ukraine",
  "Fédération de Russie" = "Russia",
  "Bélarus" = "Belarus"
)

# je transforme les noms des pays qui étaient en français en anglais 
pib_hbt$pays_en <- correspondances[pib_hbt$pays_en]


# Étape 3 : Transformation en format long comme les années sont des colonnes dans 
#           la 1ere base et des modalité dans la seconde

pib_hbt_clean <- pib_hbt %>%
  pivot_longer(
    cols = -pays_en,
    names_to = "year",
    values_to = "pib_par_habitant",
    names_transform = list(year = as.integer)  # Convertit directement les années en integer
  ) %>%
  mutate(
    pib_par_habitant = as.numeric(pib_par_habitant),  # Conversion en numérique
    Market = pays_en  # Crée la colonne Market pour la jointure
  )

# Étape 4 : Jointure avec la base principale
final_df <- gnpd_brut %>%
  left_join(pib_hbt_clean, by = c("Market", "year"))


# Correction du pib de Taiwan qui pareille que celui de la Chine 

# Étape 1 : Créer un vecteur de référence pour la Chine
pib_chine <- final_df$pib_par_habitant[final_df$Market == "China"]
annees_chine <- final_df$year[final_df$Market == "China"]

# Étape 2 : Corriger Taiwan directement
final_df <- final_df %>%
  mutate(
    pib_par_habitant = if_else(
      Market == "Taiwan, China" & is.na(pib_par_habitant),
      pib_chine[match(year, annees_chine)],  # Indexation par année
      pib_par_habitant
    )
  )

# on nettoie les données inutile pour la suite
rm(annees_chine,correspondances,pib_chine)

####################  {estimation des pib manquants} ###########################
"Il savere qu'il y a des pays qui n'ont pas fait une mise à jour de leur pib. 
Ainsi on vera une imputation de leur pib en fonction de ce qui est disponible.
On va juste conserver la dernier valeur disponible soit avant ou apres une date 
données en fonction la donnée manquante"
final_df_imputed <- final_df %>%
  group_by(Market) %>%
  arrange(year) %>%
  mutate(
    # Forward fill d'abord
    temp = na.locf(pib_par_habitant, na.rm = FALSE),
    # Puis backward fill
    pib_par_habitant_imputed = na.locf(temp, fromLast = TRUE, na.rm = FALSE)
  ) %>%
  select(-temp) %>%  # Supprimer la colonne temporaire
  ungroup()


##############################   { Verification}  ##############################
# Lister les pays problématiques
final_df_imputed %>%
  filter(is.na(pib_par_habitant_imputed)) %>%
  distinct(Market)

# Inspecter un pays spécifique
final_df_imputed %>%
  filter(Market == "Egypt") %>%
  group_by(year) %>% 
  select(year, pib_par_habitant, pib_par_habitant_imputed)
# Vérification
glimpse(final_df_imputed)

# Version internationale (virgule, point décimal)
write.csv(final_df_imputed, "df_pib.csv",row.names = FALSE, fileEncoding = "UTF-8")

