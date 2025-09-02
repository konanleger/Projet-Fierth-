#####  Netoyage de l'environnement  ############################################

rm(list = ls())

###### Chargement des packages necessaires {###################################}
library(dplyr)
library(tidyr)
library(readxl)

######        { Chargement de la base de données }    ##########################
prodtea <- read_excel("teaprod.xls")
gnpd_brut <- read_excel("GNPD_tea.xlsx")
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

str(prodtea)

prodtea <- prodtea %>% 
  select(4,10:12)

######## 
library(dplyr)
library(stringr)




# Dictionnaire de correspondance complet
correspondance_pays <- c(
  "Argentina" = "Argentina",
  "Bolivia (Plurinational State of)" = "Bolivia",
  "Brazil" = "Brazil",
  "Burundi" = "Burundi",
  "Cameroon" = "Cameroon",
  "China" = "China",
  "China, mainland" = "China, mainland",
  "China, Taiwan Province of" = "Taiwan, China",
  "Colombia" = "Colombia",
  "Democratic Republic of the Congo" = "Congo",
  "Ecuador" = "Ecuador",
  "El Salvador" = "El Salvador",
  "Ethiopia" = "Ethiopia",
  "Georgia" = "Georgia",
  "Guatemala" = "Guatemala",
  "India" = "India",
  "Indonesia" = "Indonesia",
  "Iran (Islamic Republic of)" = "Iran",
  "Japan" = "Japan",
  "Kenya" = "Kenya",
  "Lao People's Democratic Republic" = "Laos",
  "Madagascar" = "Madagascar",
  "Malawi" = "Malawi",
  "Malaysia" = "Malaysia",
  "Mali" = "Mali",
  "Mauritius" = "Mauritius",
  "Montenegro" = "Montenegro",
  "Mozambique" = "Mozambique",
  "Myanmar" = "Myanmar",
  "Nepal" = "Nepal",
  "Panama" = "Panama",
  "Papua New Guinea" = "Papua New Guinea",
  "Peru" = "Peru",
  "Portugal" = "Portugal",
  "Republic of Korea" = "South Korea",
  "Réunion" = "Reunion",
  "Russian Federation" = "Russia",
  "Rwanda" = "Rwanda",
  "Seychelles" = "Seychelles",
  "South Africa" = "South Africa",
  "Sri Lanka" = "Sri Lanka",
  "Thailand" = "Thailand",
  "Türkiye" = "Turkey",
  "Uganda" = "Uganda",
  "United Republic of Tanzania" = "Tanzania",
  "Viet Nam" = "Vietnam",
  "Zambia" = "Zambia",
  "Zimbabwe" = "Zimbabwe"
)

# Nettoyage de la base prodtea
prodtea_clean <- prodtea %>%
  mutate(
    pays = recode(Area, !!!correspondance_pays),
    year = as.numeric(Year)
  ) %>%
  filter(Unit == "t") %>%
  select(pays, year, production_tea_tonne = Value)

# Jointure finale
df_final <- gnpd_brut %>%
  left_join(prodtea_clean, by = c("Market" = "pays", "year"))



########  {CATEGORISATION DES PRODUCTEURS}    ##################################

df_final <- df_final %>%
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


# Version internationale (virgule, point décimal)
write.csv(df_final, "df_prod_tea.csv",row.names = FALSE, fileEncoding = "UTF-8")

