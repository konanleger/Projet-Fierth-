#  Projet Fierthé - Analyse des produits à base de thé

## Contexte

Dans un contexte marqué par les enjeux du changement climatique et les transitions agroécologiques, les nouvelles attentes des consommateurs en faveur d’une reterritorialisation des productions agricoles stimulent l’émergence de filières innovantes.

Depuis 2016, des producteurs réunis au sein de l’association **EuT – Tea Grown in Europe** développent la production théicole en Europe. Leur modèle repose sur :
- Une exploitation respectueuse de l’environnement  
- Des produits à haute valeur ajoutée  
- Le développement de technologies innovantes à chaque étape de la production  

## Objectif du projet

Le projet **Fierthé** (Filière Innovante Et duRable de production de THÉ en France) vise à acquérir des connaissances techniques et opérationnelles en vue de structurer une filière française de production de thé durable, à haute valeur organoleptique et conforme aux exigences de l’Agriculture Biologique.

##  Rôle de l’unité SMART

L’unité de recherche **SMART** (Institut Agro Rennes-Angers / INRAE) est chargée de l’étude de la valorisation économique, sociale et environnementale des produits à base de thé.

Cette analyse repose sur l’exploitation de la base de données **MINTEL GNPD**, un recueil international mensuel contenant :  
- Environ **33 000 produits**  
- Couvre **86 pays**  
- Tous secteurs confondus  

##  Ce dépôt contient :

- Des scripts R d’analyse exploratoire et multivariée  
- Des rapports `.Rmd` et `.html` détaillant les résultats  
- Des fichiers de données utilisés à des fins exploratoires  

### 📑 Description des fichiers clés

| Fichier | Description |
|---------|-------------|
| **Analyse_du_marché_nompays.Rmd** | Analyse descriptive du marché d’un pays donné (statistiques, tendances, parts de marché). |
| **df_final.rds** | Base de données finale nettoyée et prête pour la modélisation. |
| **pib_habitant.R** | Traitement des données de PIB par habitant pour enrichir la base. |
| **production_the.R** | Préparation et traitement des données de production de thé par pays. |
| **regquantil.R** | Script de modélisation par régressions quantiles. |
| **scripte_Rbase_des_essaies.R** | Regroupe les analyses exploratoires et statistiques descriptives initiales. |
| **presentation_xxx.Rmd** | Support de présentation des résultats (codes + visualisations), utilisés lors des rencontres avec l’encadrant. |

## Auteur : KOUASSI Konan  
Stage de 2ᵉ année, Institut Agro Rennes-Angers  
Supervisé par Monsieur DAMIEN ROUSSELIERE, dans le cadre du projet Fierthé  

---

> Pour toute question ou échange : konanleger@gmail.com
