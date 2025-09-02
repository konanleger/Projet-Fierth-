

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

df <- read_excel("GNPD_tea.xlsx")
head(df$`Product Description`,10)

df <- df%>%
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

############# On demarre #######################################################
library(dplyr)
library(stringr)

df <- df %>%
  mutate(description_clean = `Product Description` %>%
           str_to_lower() %>%
           str_replace_all("[[:punct:]]", " ") %>%
           str_replace_all("[\r\n]", " ") %>%
           str_squish())

##### Etape 2
if (!require("text2vec")) install.packages("text2vec")
library(text2vec)

# Tokenisation
tokens <- word_tokenizer(df$description_clean)
it <- itoken(tokens, progressbar = FALSE)

# Vocabulaire + vectorisation
vocab <- create_vocabulary(it)
vectorizer <- vocab_vectorizer(vocab)
dtm <- create_dtm(it, vectorizer)

# TF-IDF
tfidf <- TfIdf$new()
dtm_tfidf <- fit_transform(dtm, tfidf)

############## Etape 3

df <- df %>%
  mutate(quantile_prix = ntile(`Price per 100 g/ml in Euros`, 4))  # 4 quantiles : Q1 à Q4


#############" 
if (!require(factoextra)) install.packages("factoextra")
if (!require(irlba)) install.packages("irlba")
library(factoextra)

library(irlba)  # SVD rapide

# Réduction dimensionnelle à 100 dimensions (à ajuster selon la RAM)
dtm_svd <- irlba(dtm_tfidf, nv = 100)$u


fviz_nbclust(as.matrix(dtm_tfidf), kmeans, method = "wss")

###### cluster

set.seed(123)
km <- kmeans(dtm_svd, centers = 5)  # Ou centres = 4, 6…
df$cluster_desc <- km$cluster


###########
table(df$cluster_desc, df$quantile_prix)

###############
library(tidytext)

df$doc_id <- 1:nrow(df)

tidy_dtm <- df %>%
  select(doc_id, description_clean, cluster_desc) %>%
  unnest_tokens(word, description_clean) %>%
  count(cluster_desc, word) %>%
  bind_tf_idf(word, cluster_desc, n)

# Voir les mots les plus distinctifs par cluster
tidy_dtm %>%
  group_by(cluster_desc) %>%
  slice_max(tf_idf, n = 10) %>%
  arrange(cluster_desc, -tf_idf)
################################################################################

# === Étape 0 : Chargement des packages ===
#install.packages(c("dplyr", "stringr", "text2vec", "irlba", "Rtsne", "ggplot2"))
library(dplyr)
library(stringr)
library(text2vec)
library(irlba)
library(Rtsne)
library(ggplot2)

# === Étape 1 : Nettoyage du texte ===
df <- df %>%
  mutate(description_clean = `Product Description` %>%
           str_to_lower() %>%
           str_replace_all("[[:punct:]]", " ") %>%
           str_replace_all("[\r\n]", " ") %>%
           str_squish())

# === Étape 2 : Tokenisation + TF-IDF ===
tokens <- word_tokenizer(df$description_clean)
it <- itoken(tokens, progressbar = FALSE)

vocab <- create_vocabulary(it, stopwords = stopwords::stopwords("en"))
vectorizer <- vocab_vectorizer(vocab)
dtm <- create_dtm(it, vectorizer)

tfidf <- TfIdf$new()
dtm_tfidf <- fit_transform(dtm, tfidf)

# === Étape 3 : Réduction de dimension (SVD) ===
set.seed(123)
dtm_svd <- irlba(dtm_tfidf, nv = 50)$u  # 50 dimensions latentes
# Créer un data.frame de SVD avec index
svd_df <- as.data.frame(dtm_svd)
svd_df$product_id <- row_number()

# Supprimer les doublons
svd_unique <- svd_df %>% distinct(across(-product_id), .keep_all = TRUE)

# Mettre à jour df_tea
df_tea_unique <- df_tea[svd_unique$product_id, ]


# === Étape 4 : Clustering K-means ===
k <- 5  # Choisis ton nombre de clusters ici
km <- kmeans(dtm_svd, centers = k)
df$cluster_desc <- as.factor(km$cluster)

# === Étape 5 : Visualisation avec t-SNE ===
tsne <- Rtsne(dtm_svd, dims = 2, perplexity = 30, verbose = TRUE)
tsne_df <- data.frame(
  X = tsne$Y[, 1],
  Y = tsne$Y[, 2],
  cluster = df$cluster_desc
)

# === Étape 6 : Graphique des clusters ===
ggplot(tsne_df, aes(x = X, y = Y, color = cluster)) +
  geom_point(size = 2, alpha = 0.8) +
  labs(title = "Clusters de descriptions produits (t-SNE)", color = "Cluster") +
  theme_minimal()
################################################################################

# 1. Installation des packages requis
install.packages("text")
install.packages("dplyr")
library(text)
library(dplyr)

# 2. Préparation des données
df_clean <- df %>%
  mutate(
    text_id = row_number(),  # Identifiant unique
    clean_text = gsub("\r\n- ", ". ", `Product Description`)  # Nettoyage minimal pour BERT
  )

# 3. Embedding BERT (vectorisation sémantique)
bert_embeddings <- textEmbed(
  texts = df_clean$clean_text,
  model = "bert-base-multilingual-cased",  # Modèle multilingue
  layers = -2,  # Avant-dernière couche (meilleure performance sémantique)
  keep_token_embeddings = FALSE
)

# 4. Réduction de dimension (UMAP/t-SNE)
library(uwot)
embeddings_2d <- umap(bert_embeddings$texts$embeddings, n_components = 2)

# 5. Clustering (K-means sur embeddings BERT)
set.seed(123)
kmeans_clusters <- kmeans(bert_embeddings$texts$embeddings, centers = 4)
df_clean$cluster <- as.factor(kmeans_clusters$cluster)

# 6. Croisement avec les quantiles de prix
df_clean <- df_clean %>%
  mutate(
    price_quantile = cut(Price, 
                         breaks = quantile(Price, probs = c(0, 0.25, 0.5, 0.75, 1)),
                         labels = c("Q1", "Q2", "Q3", "Q4"))
  )

# 7. Visualisation
library(ggplot2)
ggplot(df_clean, aes(x = embeddings_2d[,1], y = embeddings_2d[,2], 
                     color = cluster, shape = price_quantile)) +
  geom_point(size = 3) +
  labs(title = "Projection 2D des embeddings BERT par cluster et quantile de prix")
################################################################################

# instalations 
install.packages(c("tidyverse", "tm", "SnowballC", "wordcloud", "tidytext", "cluster", "factoextra", "udpipe"))
library(tidyverse)   # Data manipulation
library(tm)          # Text mining
library(SnowballC)   # Stemming (optional)
library(wordcloud)   # Visualization
library(tidytext)    # Modern NLP tools
library(cluster)     # Clustering (k-means)
library(factoextra)  # Cluster visualization
library(udpipe)      # Lemmatization (better than stemming for English)



df <- read_excel("GNPD_tea.xlsx")
head(df$`Product Description`,10)

df <- df%>%
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

# Sélectionner la colonne de description
descriptions <- df$`Product Description`

###### etape 2

library(tm)
# Créer un corpus (collection de documents)
corpus <- VCorpus(VectorSource(descriptions))

# Nettoyage du texte
# Text cleaning pipeline
corpus_clean <- corpus %>%
  tm_map(content_transformer(tolower)) %>%      # Lowercase
  tm_map(removePunctuation) %>%               # Remove punctuation
  tm_map(removeNumbers) %>%                   # Remove numbers
  tm_map(removeWords, stopwords("english")) %>% # English stopwords
  tm_map(stripWhitespace)                     # Extra whitespace

############ etape 2

install.packages("text")
library(text)

udpipe_model <- udpipe_load_model("english-ewt-ud-2.5-191206.udpipe")
# Création de la matrice
dtm <- DocumentTermMatrix(corpus_clean, 
                          control = list(weighting = weightTfIdf))

# Réduction de la dimension (enlever les termes trop rares/fréquents)
dtm_reduced <- removeSparseTerms(dtm, sparse = 0.99) 

# Convertir en matrice classique
matrix <- as.matrix(dtm_reduced)
