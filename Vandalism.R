#Load the dataset
wiki = read.csv(choose.files(), stringsAsFactors = FALSE)

#Convert vandal variable to factor
wiki$Vandal = as.factor(wiki$Vandal)
table(wiki$Vandal)

#Load the libraries "tm" and "snowballC"
library(tm)
library(SnowballC)

#Create corpus for "Added" variable
corpusAdded = VCorpus(VectorSource(wiki$Added))

#Remove stopwords
corpusAdded = tm_map(corpusAdded, removeWords, stopwords("english"))

#Stem documents
corpusAdded = tm_map(corpusAdded, stemDocument)

#Create matrix
dtmAdded = DocumentTermMatrix(corpusAdded)
dtmAdded

#Remove sparse term
sparseAdded = removeSparseTerms(dtmAdded, .997)
sparseAdded

#Convert to a Data frame
wordsAdded = as.data.frame(as.matrix(sparseAdded))

#Paste "A" to wordsAdded data frame
colnames(wordsAdded) = paste("A", colnames(wordsAdded))

#Create corpus for "Removed" variable
corpusRemoved = VCorpus(VectorSource(wiki$Removed))

#Remove stopwords
corpusRemoved = tm_map(corpusRemoved, removeWords, stopwords("english"))

#Stem documents
corpusRemoved = tm_map(corpusRemoved, stemDocument)

#Create matrix
dtmRemoved = DocumentTermMatrix(corpusRemoved)
dtmRemoved

#Remove sparse term
sparseRemoved = removeSparseTerms(dtmRemoved, .997)
sparseRemoved

#Convert to a Data frame
wordsRemoved = as.data.frame(as.matrix(sparseRemoved))

#Paste "R" to wordsRemoved data frame
colnames(wordsRemoved) = paste("R", colnames(wordsRemoved))

#Combine wordsAdded and wordsRemoved
wikiWords = cbind(wordsAdded, wordsRemoved)

#Include Vandal from wiki to wikiWords
wikiWords$Vandal = wiki$Vandal

#Load caTools and split the data in ratio of .7
library(caTools)
set.seed(123)
spl = sample.split(wikiWords, SplitRatio = .7)
train = subset(wikiWords, spl == TRUE)
test = subset(wikiWords, spl == FALSE)

#Baseline accuracy
table(test$Vandal)
(622)/(622+542)

#Load libraries
library(rpart)
library(rpart.plot)

#Build Cart model
Model1 = rpart(Vandal ~ ., data = train)

#Prediction 
predictCart = predict(Model1, newdata = test, type = "class")
table(test$Vandal, predictCart)
(622+14)/(622+14+528)
prp(Model1)

#Create new data from wikiWords
wikiWords2 = wikiWords

#Create a new column in wikiWords2
wikiWords2$HTTP = ifelse(grepl("http",wiki$Added,fixed=TRUE), 1, 0)
table(wikiWords2$HTTP)

#Split the data into train and test
wikiTrain2 = subset(wikiWords2, spl==TRUE)
wikiTest2 = subset(wikiWords2, spl==FALSE)

#Build Cart model2
Model2 = rpart(Vandal ~ ., data = wikiTrain2)

#Prediction
predictCart2 = predict(Model2, newdata = wikiTest2, type = "class")
table(wikiTest2$Vandal, predictCart2)
(613+70)/(613+70+9+472)

#Add new variables 
wikiWords2$NumWordsAdded = rowSums(as.matrix(dtmAdded))
wikiWords2$NumWordsRemoved = rowSums(as.matrix(dtmRemoved))
mean(wikiWords2$NumWordsAdded)

#Split the data
wikiTrain3 = subset(wikiWords2, spl==TRUE)
wikiTest3 = subset(wikiWords2, spl==FALSE)

#Build cart model3
Model3 = rpart(Vandal ~ ., data = wikiTrain3)

#Prediction
predictCart3 = predict(Model3, newdata = wikiTrain3, type = "class")
table(wikiTest3$Vandal, predictCart3)

#Create new variables
wikiWords3 = wikiWords2
wikiWords3$Minor = wiki$Minor
wikiWords3$Loggedin = wiki$Loggedin

#Split the data
wikiTrain4 = subset(wikiWords3, spl==TRUE)
wikiTest4 = subset(wikiWords3, spl==FALSE)

#Build cart model4
wikiCART4 = rpart(Vandal ~ ., data=wikiTrain4, method="class")

#Prediction
predictTestCART4 = predict(wikiCART4, newdata=wikiTest4, type="class")
table(wikiTest4$Vandal, predictTestCART4)
(598+233)/(598+233+24+309)
prp(wikiCART4)
