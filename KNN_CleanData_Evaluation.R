#Librarys---------------------------------------------------------------
library(NLP)
library(tm)
library(sylly)
library(koRpus)
library(koRpus.lang.de)
library(textstem)
library(dplyr)
library(rebus)
library(stringr)
library(qdapTools)
library(qdapDictionaries)
library(qdapRegex)
library(RColorBrewer)
library(qdap)
library(class)
library(SnowballC)

#Initialisation--------------------------------------------------------
tweets <- read.csv("germeval2019training.csv", header = FALSE, sep = ";", encoding = "UTF-8")
tweetsVec <- tweets[,1]
labelVec <- tweets[,2]
tweetsVec <- as.character(tweetsVec)

#Stopword-liste
stopwordlist <- c(stopwords("de"), "lbr", "ja", "dass", "usw", "\"")

#Schimpfwortliste
Schimpfwort_liste <- read.csv("Schimpfwort_list.csv", header = F, sep = ";")
Schimpfwort_liste_Vec <- as.character(Schimpfwort_liste$V1)
Schimpfwort_liste_Vec <- tolower(Schimpfwort_liste_Vec)

# Anzahl von # und @-----------------------------------------------------------------------
Hash_2019 <- str_count(tweets$V1, pattern = fixed("#"))
Ats_2019 <- str_count(tweets$V1, pattern = fixed("@"))

#Preprocessing----------------------------------------------------------------------
#remove chars
tweetsVec <- removePunctuation(tweetsVec)
#remove UTF-8 Sonderzeichen
tweetsVec <- gsub("[^\x01-\x7F]", "", tweetsVec)
#to lowercase
tweetsVec <- tolower(tweetsVec)
#remove stopwords
tweetsVec <- removeWords(tweetsVec, stopwordlist)
#remove spaces
tweetsVec <- stripWhitespace(tweetsVec)

#lemma dictionary mit treetagger erstellen
lemma_dictionary <- make_lemma_dictionary(tweetsVec, engine = "treetagger",path = NULL, lang = "de")
#lemmatisieren
tweetsVec <- lemmatize_strings(tweetsVec, dictionary = lemma_dictionary)

tweetsPreprocessed <- data.frame(tweetsVec, labelVec)

#Daten extrahieren
#werden dann als csv mit "," als Trennzeichen ausgegeben und Nummern in der ersten Spalte
#(also beim importieren dann die erste Spalte am besten entfernen)
#write.csv(tweetsPreprocessed, file = "tweetsPreprocessed.csv", fileEncoding = "UTF-8")

#tweets Preprocessed einlesen und Nummerierungsspalte entfernen
#tweetsPreprocessed <- read.csv("tweetsPreprocessed.csv", header = TRUE, sep = ",", encoding = "UTF-8")
#tweetsPreprocessed <- tweetsPreprocessed[,2-3]

# Abgleich mit Schimpfwortliste------------------------------------------------------------
k <- 1

#Matrix zum Zaehlen, bei anderen Listen Groesse Anpassen!!!
Vor_Matrix <- matrix(nrow = 12387, byrow = F, ncol =11304) 
while (k <= 11304) {
  V <- c(Schimpfwort_liste_Vec[k:k])
  abgleich <- str_count(tweetsPreprocessed$tweetsVec, pattern = fixed(V))
  Vor_Matrix[,k] <- abgleich
  k <- k + 1
}

#Anzahl Schimpfwoerter Zusammen Zaehlen
Schimpfwort_Zahl <- rowSums(Vor_Matrix)

#Document Term Matrix--------------------------------------------------------------------------
tweetsCorpus <- VectorSource(tweetsPreprocessed$tweetsVec)
tweetsSource <- VCorpus(tweetsCorpus)

#Remove Spars Terms
TweetDTM <- removeSparseTerms(DocumentTermMatrix(tweetsSource), sparse = 0.975)
TweetDTM <- as.matrix(TweetDTM)

#Featur Data Frame
tweets_Features <- data.frame(tweetsPreprocessed, Ats_2019, Hash_2019, Schimpfwort_Zahl, TweetDTM)

#knn initialisieren-----------------------------------------------------------------------------------
n <- nrow(tweets_Features)
#Teilen in Test und Training 70 - 30 
shuffled <- tweets_Features[sample(n),]
train <- shuffled[1:round(0.7 * n),]
test <- shuffled[(round(0.7 * n) + 1):n,]

#Labls ziehen
train_labels <- train$labelVec
test_labels <- test$labelVec

#Arbeits Datensaetze ohne Labels
knn_train <- train
knn_test <- test

knn_train$labelVec <- NULL
knn_test$labelVec <- NULL

#Text (V1) als Nummer
knn_test$tweetsVec <- as.numeric(knn_test$tweetsVec)
knn_train$tweetsVec <- as.numeric(knn_train$tweetsVec)

#Normalisiern--------------------------------------------------------------------------------

#knn_train$Ats_2019 <- (knn_train$Ats_2019-min(tweet_Featur$Ats_2019))/(max(tweet_Featur$Ats_2019)-min(tweet_Featur$Ats_2019))
#knn_test$Ats_2019 <- (knn_test$Ats_2019-min(tweet_Featur$Ats_2019))/(max(tweet_Featur$Ats_2019)-min(tweet_Featur$Ats_2019))

#knn_train$Hash_2019 <- (knn_train$Hash_2019-min(tweet_Featur$Hash_2019))/(max(tweet_Featur$Hash_2019)-min(tweet_Featur$Hash_2019))
#knn_test$Hash_2019 <- (knn_test$Hash_2019-min(tweet_Featur$Hash_2019))/(max(tweet_Featur$Hash_2019)-min(tweet_Featur$Hash_2019))

knn_train$Schimpfwort_Zahl <- (knn_train$Schimpfwort_Zahl-min(tweets_Features$Schimpfwort_Zahl))/(max(tweets_Features$Schimpfwort_Zahl)-min(tweets_Features$Schimpfwort_Zahl))
knn_test$Schimpfwort_Zahl <- (knn_test$Schimpfwort_Zahl-min(tweets_Features$Schimpfwort_Zahl))/(max(tweets_Features$Schimpfwort_Zahl)-min(tweets_Features$Schimpfwort_Zahl))


#Trainiren, Vorhersage mit Konfussionsmatrix -> BIS JETZT K=7 BEST--------------------------------
pred <- knn(train = knn_train, test = knn_test, cl = train_labels, k = 7)
conf <- table(test = test_labels, pred)
conf
acc <- (conf[1,1]+conf[2,2]+conf[3,3]+conf[4,4])/sum(conf)
acc

#Four-Class-Evaluation

four_class_evaluation <- function(matrix){
  
  tp_abuse <- matrix[1,1]
  tp_insult <- matrix[2,2]
  tp_other <- matrix[3,3]
  tp_profanity <- matrix[4,4]
  
  #row
  tpfn_abuse <- sum(matrix[1,])
  tpfn_insult <- sum(matrix[2,])
  tpfn_other <- sum(matrix[3,])
  tpfn_profanity <- sum(matrix[4,])
  
  #col
  tpfp_abuse <- sum(matrix[,1]) 
  tpfp_insult <- sum(matrix[,2]) 
  tpfp_other <- sum(matrix[,3]) 
  tpfp_profanity <- sum(matrix[,4]) 
  
  #precision = tp/(tp+fp)
  #recall = tp/(tp+fn)
  
  abuse_precision <- tp_abuse/tpfp_abuse
  abuse_recall <- tp_abuse/tpfn_abuse
  
  insult_precision <- tp_insult/tpfp_insult
  insult_recall <- tp_insult/tpfn_insult
  
  other_precision <- tp_other/tpfp_other
  other_recall <- tp_other/tpfn_other
  
  profanity_precision <- tp_profanity/tpfp_profanity
  profanity_recall <- tp_profanity/tpfn_profanity
  
  average_precision <- mean(abuse_precision, insult_precision, other_precision, profanity_precision)
  average_recall <- mean(abuse_recall, insult_recall, other_recall, profanity_recall)
  
  f1_score <- 2*(average_precision * average_recall)/(average_precision + average_recall)
  
  #Output:
  print(matrix)
  print("===================================")
  print(paste(paste("ABUSE[1]: Precision", abuse_precision, sep = ": ", collapse = NULL), paste("Recall", abuse_recall, sep = ": ", collapse = NULL), sep = " ", collapse = NULL))
  print(paste(paste("INSULT[2]: Precision", insult_precision, sep = ": ", collapse = NULL), paste("Recall", insult_recall, sep = ": ", collapse = NULL), sep = " ", collapse = NULL))
  print(paste(paste("OTHER[3]: Precision", other_precision, sep = ": ", collapse = NULL), paste("Recall", other_recall, sep = ": ", collapse = NULL), sep = " ", collapse = NULL))
  print(paste(paste("PROFANITY[4]: Precision", profanity_precision, sep = ": ", collapse = NULL), paste("Recall", profanity_recall, sep = ": ", collapse = NULL), sep = " ", collapse = NULL))
  print(paste("Average-Precision:", average_precision))
  print(paste("Average-Recall:", average_recall))
  print(paste("F1-Score:", f1_score))
}


four_class_evaluation(conf)

