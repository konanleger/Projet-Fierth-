#-------------------------------------------------------------------------------
# """"""""" REGRESSION QUANTILE """""""""""
#          ====================
#-------------------------------------------------------------------------------

#******************************************************************************#
#*              ****      MODELISATION     ****
#*                        ============                                         
#*                      *****************
#*                      

################################################################################

# Nettoyage de l'environnement 
rm(list = ls())

## Importation de la base 
df <- readRDS("df_final.rds")

#######  chargement des packages   #############################################

library(quantreg)
library(caret)
library(dplyr)
library(car)
library(tidyverse)


#******************************************************************************#
#*                       {  Le thé brut }  
#*<<==========================================================================>>
#*     ###############################################################                  
df_tea <- df %>% 
  filter(`Sub-Category` == "Tea")


df_tea_clean <- df_tea %>%
  filter(complete.cases(select(., `Price per 100 g/ml in Euros`,year,Region_Marche,
                               type_lance,`Private Label`,claim_sante,
                               claim_ethique,claim_ciblage,claim_strategie,
                               claim_naturel,package_group_type,material_group,
                               claim_bio,claim_environnement,Prix_corrige)),
         `Price per 100 g/ml in Euros` >0)# pour le log


summary(df_tea_clean$`Price per 100 g/ml in Euros`)
quantile(df_tea_clean$`Price per 100 g/ml in Euros`, probs = c(0.1,0.20,0.3,0.4,0.5,0.6,0.7,0.8,0.9,0.95,0.99))

taus <- c(0.25,0.5,0.75,0.9,0.95)
# modeles
model_tea1 <- rq(log(`Price per 100 g/ml in Euros`) ~ year+ Region_Marche +type_lance+
                 `Private Label`+ claim_sante+claim_ethique+claim_ciblage+
                 claim_strategie+claim_naturel+package_group_type+
                claim_bio+ claim_environnement,
               data = df_tea_clean,
               tau = taus,
               method = "fn")

model_tea2 <- rq(log(`Price per 100 g/ml in Euros`) ~ year+ pib_categorie +type_lance+
                  `Private Label`+ claim_sante+claim_ethique+claim_ciblage+
                  claim_strategie+claim_naturel+package_group_type+categorie_prod+
                  claim_bio+ claim_environnement,
                data = df_tea_clean,
                tau = taus,
                method = "fn")

# 1. Trouver les 10 pays les plus fréquents
top10 <- names(sort(table(df_tea_clean$Market), decreasing = TRUE))[1:10]

# 2. Recatégoriser Market : garder top10, regrouper les autres en "Other"
df_tea_clean$Market <- ifelse(df_tea_clean$Market %in% top10,
                                      as.character(df_tea_clean$Market),
                                      "Other")

# 3. Transformer en facteur
df_tea_clean$Market <- relevel(factor(df_tea_clean$Market),ref = "France")

model_tea3 <- rq(log(`Price per 100 g/ml in Euros`) ~ year+ Market +type_lance+
                   `Private Label`+ claim_sante+claim_ethique+claim_ciblage+
                   claim_strategie+claim_naturel+package_group_type+
                   claim_bio+ claim_environnement,
                 data = df_tea_clean,
                 tau = taus,
                 method = "fn")

summary_tea1 <- summary(model_tea1, se ="ker")
summary_tea2 <- summary(model_tea2, se ="ker")
summary_tea3 <- summary(model_tea3, se ="ker")

####################### Remarque ###############################################
"se = boot : Bootstrap (méthode recommandée pour les petits échantillons ou 
             données non normales).

se = nid : Approximation asymptotique (moins robuste).

se = ker : Méthode kernel (pour grands échantillons)."
################################################################################

########################## autres modeles  #####################################
df_1 <- df_tea_clean %>% 
  mutate(year2 = year - 2000)

model_tea_interact <- rq(
  log(`Price per 100 g/ml in Euros`) ~ 
    Region_Marche +year2 + year2*(claim_sante + claim_ethique + claim_ciblage + 
    claim_strategie + claim_naturel +claim_bio + claim_environnement ) + 
    type_lance + `Private Label` + package_group_type,
  data = df_1,
  tau = taus,
  method = "fn"
)


model_tea_quad <- rq(
  log(`Price per 100 g/ml in Euros`) ~ I(year^2) +
    Region_Marche + type_lance + `Private Label` +
    claim_sante + claim_ethique + claim_ciblage +
    claim_strategie + claim_naturel + package_group_type +
    claim_bio + claim_environnement,
  data = df_tea_clean,
  tau = taus,
  method = "fn"
)

summary_tea4 <- summary(model_tea_interact, se ="ker")
summary_tea5 <- summary(model_tea_quad, se ="ker")

#*                      {CHOIX DU MODELE }   

########## Fonction pour extraire l'AIC d'un modèle rq #########################
get_aic_rq <- function(model) {
  resid <- residuals(model)
  tau <- model$tau
  n <- length(resid)
  rho_tau <- resid * (tau - (resid < 0))
  sum_rho <- sum(rho_tau)
  k <- length(coef(model))
  aic <- n * log(sum_rho/n) + 2 * k
  return(aic)
}


# Calculer l'AIC pour chaque modèle
aic_tea1 <- get_aic_rq(model_tea1)
aic_tea2 <- get_aic_rq(model_tea2)
aic_tea3 <- get_aic_rq(model_tea3)
aic_tea4 <- get_aic_rq(model_tea_interact)
aic_tea5 <- get_aic_rq(model_tea_quad)

# Créer un tableau comparatif
results_aic <- data.frame(
  Modèle = c("Model_tea1", "Model_tea2","model_tea3","model_tea4","model_tea5"),
  AIC = c(aic_tea1, aic_tea2,aic_tea3,aic_tea4,aic_tea5)
)

# resultat
print(results_aic) 

# Graphique comparatif AIC
ggplot(results_aic, aes(x = Modèle, y = AIC, fill = Modèle)) +
  geom_col(width = 0.6, show.legend = FALSE) +
  geom_text(aes(label = round(AIC, 2)), vjust = -0.5, size = 4) +
  labs(title = "Comparaison des AIC des modèles quantiles",
       y = "AIC", x = "") +
  theme_minimal(base_size = 14)


rm(results_aic,aic_tea1,aic_tea2)

########################  { AFFICHAGE DES RESULTATS }  #########################

# Créer une liste de data frames avec les coefficients et p-values
results_list <- map2(
  summary_tea4,
  paste0("tau_", model_tea_interact$tau),
  function(x, tau_label) {
    df <- data.frame(
      Variable = rownames(x$coefficients),
      Coef = round(x$coefficients[, 1],2),
      Pval = round(x$coefficients[, 4],3)
    )
    # Renommer les colonnes pour indiquer le quantile
    names(df)[2:3] <- paste0(c("coef_", "pval_"), gsub("tau_", "", tau_label))
    return(df)
  }
)

# Fusionner les dataframes par Variable avec reduce + full_join
results_wide <- reduce(results_list, full_join, by = "Variable")

# Affichage
print(results_wide)



################# { PROJECTION GRAPHIQUE DES COEFFICIENTS } ####################

library(broom)
library(purrr)


# Calcul des intervalles de confiances

tidy_rq_model <- function(model, se_method = "ker", conf_level = 0.95) {
  
  if (!inherits(model, c("rq","rqs"))) {
    stop("Le modèle doit être de classe 'rq' ou 'rqs' (régression quantile)")
  }
  
  # Vérifier si le modèle a plusieurs tau
  if (length(model$tau) > 1) {
    # Cas multiple tau
    results_list <- map(seq_along(model$tau), function(i) {
      s <- summary(model, se = se_method, covariance = TRUE)
      
      # Extraire les coefficients pour le i-ème tau
      df <- as.data.frame(s[[i]]$coefficients)
      df$term <- rownames(df)
      colnames(df) <- c("estimate", "std.error", "t.value", "p.value", "term")
      
      # Calculer l'intervalle de confiance
      z_value <- qnorm(1 - (1 - conf_level)/2)
      
      df %>%
        mutate(
          tau = model$tau[i],
          conf.low = estimate - z_value * std.error,
          conf.high = estimate + z_value * std.error,
          conf.level = conf_level,
          se.method = se_method
        ) %>%
        select(term, estimate, std.error, t.value, p.value, 
               conf.low, conf.high, tau, everything())
    })
    
    # Combiner tous les résultats
    result <- bind_rows(results_list)
    
  } else {
    # Cas single tau (version précédente)
    tau <- model$tau
    s <- summary(model, se = se_method)
    
    df <- as.data.frame(s$coefficients)
    df$term <- rownames(df)
    colnames(df) <- c("estimate", "std.error", "t.value", "p.value", "term")
    
    z_value <- qnorm(1 - (1 - conf_level)/2)
    
    result <- df %>%
      mutate(
        tau = tau,
        conf.low = estimate - z_value * std.error,
        conf.high = estimate + z_value * std.error,
        conf.level = conf_level,
        se.method = se_method
      ) %>%
      select(term, estimate, std.error, t.value, p.value, 
             conf.low, conf.high, tau, everything())
  }
  
  return(result)
}

tidy_results1 <- tidy_rq_model(model_tea1) # on peut changer de model facilement
tidy_results2 <- tidy_rq_model(model_tea2)
tidy_results3 <- tidy_rq_model(model_tea3)
# Visualisation avec ggplot2
library(ggplot2)

ggplot(tidy_results1, aes(x = tau, y = estimate, color = term)) +
  geom_line() +
  geom_point() +
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high, fill = term), alpha = 0.2, color = NA) +
  facet_wrap(~ term, scales = "free_y") +
  labs(title = "Évolution des coefficients selon les quantiles",
       subtitle = "model 1",
       x = "Quantile (tau)", y = "Coefficient estimé") +
  theme_minimal()+
  theme(legend.position = "none")

ggplot(tidy_results2, aes(x = tau, y = estimate, color = term)) +
  geom_line() +
  geom_point() +
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high, fill = term), alpha = 0.2, color = NA) +
  facet_wrap(~ term, scales = "free_y") +
  labs(title = "Évolution des coefficients selon les quantiles",
       subtitle = "model 2",
       x = "Quantile (tau)", y = "Coefficient estimé") +
  theme_minimal()+
  theme(legend.position = "none")

ggplot(tidy_results3, aes(x = tau, y = estimate, color = term)) +
  geom_line() +
  geom_point() +
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high, fill = term), alpha = 0.2, color = NA) +
  facet_wrap(~ term, scales = "free_y") +
  labs(title = "Évolution des coefficients selon les quantiles",
       subtitle = "model 3",
       x = "Quantile (tau)", y = "Coefficient estimé") +
  theme_minimal()+
  theme(legend.position = "none")

rm(tidy_results)


################## fin #########################################################
################################################################################
#* ****************************************************************************#
#*                         +++++++++++++++++++
#*          ++++++++++++   LE MARCHE DE FRANCE ++++++++++++
#*                         +++++++++++++++++++
#*

####################### { FOCUS SUR LE MARCHE DE FRANCE} #######################

# #############        { LE THE BRUT } ##############

########################## ###############################
df_clean <- df %>% 
  filter(Market == 'France',
         `Sub-Category`=='Tea')

taus <- c(0.25,0.5,0.75,0.9,0.95)
model_tea_france <- rq(log(`Price per 100 g/ml in Euros`) ~ year+type_lance+
                   `Private Label`+ claim_sante+claim_ethique+claim_ciblage+
                   claim_strategie+claim_naturel+package_group_type+
                   claim_bio+ claim_environnement,
                 data = df_clean,
                 tau = taus,
                 method = "fn")
resume_france <- summary(model_tea_france,se="boot")

########################  Analyse des coefs       ##############################
# Créer une liste de data frames avec les coefficients et p-values
results_france <- map2(
  resume_france,
  paste0("tau_", model_tea_france$tau),
  function(x, tau_label) {
    df <- data.frame(
      Variable = rownames(x$coefficients),
      Coef = round(x$coefficients[, 1],2),
      Pval = round(x$coefficients[, 4],3)
    )
    # Renommer les colonnes pour indiquer le quantile
    names(df)[2:3] <- paste0(c("coef_", "pval_"), gsub("tau_", "", tau_label))
    return(df)
  }
)

# Fusionner les dataframes par Variable avec reduce + full_join
results_france_wide <- reduce(results_france, full_join, by = "Variable")

# Affichage
print(results_france_wide)

################# {graph d'evolution} ##########################################

taus <- c(0.25, 0.5, 0.75, 0.9,0.95)

# Visualisation avec ggplot2
library(ggplot2)

tidy_results <- tidy_rq_model(model_tea_france)

ggplot(tidy_results, aes(x = tau, y = estimate, color = term)) +
  geom_line() +
  geom_point() +
  facet_wrap(~ term, scales = "free_y") +
  labs(title = "Évolution des coefficients selon les quantiles",
       subtitle = "Marché français uniquement",
       x = "Quantile (tau)", y = "Coefficient estimé") +
  theme_minimal()

rm(tidy_results)


#############################################################
####################### le thé glacé   #########################################

###############################################################################
df_rtd <- df %>% 
  filter(Market == 'France',
         `Sub-Category` == 'RTD (Iced) Tea')
df_rtd_clean <- df_rtd %>%
  filter(complete.cases(select(., `Price per 100 g/ml in Euros`,year,
                               type_lance,`Private Label`,claim_sante,
                               claim_ethique,claim_ciblage,claim_strategie,
                               claim_naturel,package_group_type,material_group,
                               claim_bio,claim_environnement,Prix_corrige)),
         `Price per 100 g/ml in Euros` >0)# pour le log

summary(df_rtd_clean$`Price per 100 g/ml in Euros`)
quantile(df_rtd_clean$`Price per 100 g/ml in Euros`, probs = c(0.1,0.20,0.3,0.4,0.5,0.6,0.7,0.8,0.9,0.95,0.99))

taus <- c(0.25,0.5,0.75,0.9)
model_rtd_france <- rq(log(`Price per 100 g/ml in Euros`) ~ year+type_lance+
                         `Private Label`+ claim_bio+ claim_sante+claim_environnement,
                       data = df_rtd,
                       tau = taus,
                       method = "fn")
resume_rtd_france <- summary(model_rtd_france,se="boot")

unique(df$`Sub-Category`)


########################  Analyse des coefs       ##############################
# Créer une liste de data frames avec les coefficients et p-values
results_france <- map2(
  resume_rtd_france,
  paste0("tau_", model_rtd_france$tau),
  function(x, tau_label) {
    df <- data.frame(
      Variable = rownames(x$coefficients),
      Coef = round(x$coefficients[, 1],2),
      Pval = round(x$coefficients[, 4],3)
    )
    # Renommer les colonnes pour indiquer le quantile
    names(df)[2:3] <- paste0(c("coef_", "pval_"), gsub("tau_", "", tau_label))
    return(df)
  }
)

# Fusionner les dataframes par Variable avec reduce + full_join
results_france_wide <- reduce(results_france, full_join, by = "Variable")

# Affichage
print(results_france_wide)


################# {graph d'evolution} ##########################################

#taus <- c(0.25, 0.5, 0.75, 0.9,0.95)

library(broom)

# Convertir les résultats en dataframe long pour ggplot
tidy_results <- map_df(taus, function(tau) {
  m <- rq(log(`Price per 100 g/ml in Euros`) ~ year+type_lance+
            `Private Label`+ claim_bio+ claim_sante+claim_environnement,
          data = df_rtd,
          tau = tau,
          method = "fn")
  broom::tidy(m) %>% mutate(tau = tau)
})

# Visualisation avec ggplot2
library(ggplot2)

ggplot(tidy_results, aes(x = tau, y = estimate, color = term)) +
  geom_line() +
  geom_point() +
  facet_wrap(~ term, scales = "free_y") +
  labs(title = "Évolution des coefficients selon les quantiles",
       subtitle = "Marché français du THE GLACE uniquement ",
       x = "Quantile (tau)", y = "Coefficient estimé") +
  theme_minimal()

rm(tidy_results)


####################### MODELE MIXTE ###########################################



library(qrLMM)

# 1. Trouver les 10 pays les plus fréquents
top10 <- names(sort(table(df_tea_clean$Market), decreasing = TRUE))[1:5]

# 2. Recatégoriser Market : garder top10, regrouper les autres en "Other"
df_tea_clean$Market <- ifelse(df_tea_clean$Market %in% top10,
                                      as.character(df_tea_clean$Market),
                                      "Other")

# 3. Transformer en facteur
df_tea_clean$Market <- relevel(factor(df_tea_clean$Market),ref = "France")
df_tea_clean$price <- df_tea_clean$`Price per 100 g/ml in Euros`
df_tea_clean$label <- df_tea_clean$`Private Label`


library(lqmm)


mod <- rq(log(price)~ year+type_lance+Market+
            `Private Label`+ claim_sante+
            claim_bio,
          data = df_tea_clean,
          tau = 0.5)

summary(mod, se="ker")

# Vérifier le nombre de niveaux pour toutes les variables catégorielles
sapply(df_tea_clean[, c("year","type_lance","Market","Private Label",
                    "claim_sante","claim_ethique","claim_ciblage",
                    "claim_strategie","claim_naturel","package_group_type",
                    "claim_bio","claim_environnement")],
       function(x) if(is.factor(x)) nlevels(x) else NA)

# Filtrer les valeurs manquantes et infinies
df_tea_clean <- df_tea_clean %>%
  filter(!is.na(price),
         !is.infinite(price),
         complete.cases(across(c(year, Region_Marche, type_lance, 
                                 label, claim_sante, claim_ethique,
                                 claim_ciblage, claim_strategie, claim_naturel,
                                 package_group_type, claim_bio, claim_environnement))))

# 
# fit50 <- lqmm(
#   fixed = log(price) ~ year+type_lance+
#     label+claim_bio,
#   random = ~ 1,                  # intercept aléatoire par pays
#   group  = Market,               # le facteur de regroupement
#   tau    = 0.5,
#   data = df_tea_clean,
#   covariance = "pdDiag",         # structure parcimonieuse
#   nK = 7,                        # quadrature (baissez à 5 si mémoire serrée)
#   control = list(LP_max_iter = 10000,method = "df")  # optim + économe LP_max_iter = 1000,
# )
# 
# summary(fit50)

#ranef(fit50)

 
########################## AUTRE ANALYSE POUR LE MARCHE DE FRANCE ##############
 

 df_fr <- df %>% 
   filter(Market == 'France')
df_fr_clean <- df_fr %>%
   filter(complete.cases(select(., `Price per 100 g/ml in Euros`,year,
                                type_lance,`Private Label`,claim_sante,
                                claim_ethique,claim_ciblage,claim_strategie,
                                claim_naturel,package_group_type,material_group,
                                claim_bio,claim_environnement,Prix_corrige)),
          `Price per 100 g/ml in Euros` >0)# pour le log


df_fr_clean$`Famille thé` <- relevel(factor(df_fr_clean$`Famille thé`), ref = "Thé")


taus <- c(0.25,0.5,0.75,0.9,0.95)
model_fr <- rq(log(`Price per 100 g/ml in Euros`) ~ year+type_lance+`Famille thé`+
                 `Private Label`+ claim_bio+claim_ethique+ claim_sante,
                       data = df_fr_clean,
                       tau = taus,
                       method = "fn")
resume_fr <- summary(model_fr,se="boot")


results_fr <- map2(
  resume_fr,
  paste0("tau_", model_fr$tau),
  function(x, tau_label) {
    df <- data.frame(
      Variable = rownames(x$coefficients),
      Coef = round(x$coefficients[, 1],2),
      Pval = round(x$coefficients[, 4],3)
    )
    # Renommer les colonnes pour indiquer le quantile
    names(df)[2:3] <- paste0(c("coef_", "pval_"), gsub("tau_", "", tau_label))
    return(df)
  }
)

# Fusionner les dataframes par Variable avec reduce + full_join
results_france_wide <- reduce(results_fr, full_join, by = "Variable")

# Affichage
print(results_france_wide)

