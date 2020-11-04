#Import:
data <- read.csv(file.choose(), stringsAsFactors = FALSE,na.strings = c(""," ","NA"))
#READ AS STRING not factors
View(data)

#Choosing The Elder Scrolls V: Skyrim Special Edition reviews:
library(dplyr)
data<-filter(data, data$title == "The Elder Scrolls V: Skyrim Special Edition")

#Checking for missing values
library(questionr)
freq.na(data)
#a few (<1% of the reviews) were missing...

#Cleaning the data:
#Removing rows with missing reviews
data<-data[complete.cases(data), ]
freq.na(data)

#Removing users with hours_played less than 5:
short <- subset(data$hour_played, data$hour_played < 5)
View(short)
data$short <- ifelse(data$hour_played < 5, 1,0)
data<-data[!data$short == 1,]
data<-data[-9]

#Encoding funny/helpful (1) or not funny/helpful (0):
#finding the average rating for funny + helpful:
mean(data$funny) #1.5
mean(data$helpful) #7.2
#This isn't really helpfull b/c steam will boost reviews that get likes so they get more exposure.

#Encoding reviews to be funny (1) or not funny (0).
data$funny <- ifelse(data$funny > 2, 1,0)
data$funny <- factor(data$funny, labels = 0:1)
#Encoding reviews to be helpful (1) or not helpful (0).
data$helpful <- ifelse(data$helpful > 7, 1,0)
data$helpful <- factor(data$helpful, labels = 0:1)

#Encoding Recommended/Not Reccomended:
class(data$recommendation) #Checking data type of "reccomendation column
data$recommendation <- as.factor(data$recommendation)
data$recommendation <- factor(data$recommendation, labels = 0:1)

#Creating the Corpus:
#Packages:
install.packages('tm')
install.packages('SnowballC')
library('tm')
library('SnowballC')

#Bag of words:
#I am going to create a bag of words
#The next 10-12 lines are the same for ANY dataset. Feel free to copy-paste
corpus = VCorpus(VectorSource(data$review))
#use the Column Review to create a corpus (a bag of words)
#tm_map (in a corpus creates a "map" of words)
corpus = tm_map(corpus, content_transformer(tolower)) #lowercase
corpus = tm_map(corpus, removeNumbers) #No numbers
corpus = tm_map(corpus, removePunctuation) #No punctuation
corpus = tm_map(corpus, removeWords, stopwords()) #Stopwords (the, a, who, they,...)
corpus = tm_map(corpus, stemDocument)#Converting everything down to the stem
corpus = tm_map(corpus, stripWhitespace)#No whitespaces

#Creating the bag of words model
#I am now creating a matrix of numbers wherein if it sees the word "love"
#it will assign a 1 to that row for that review.
dtm = DocumentTermMatrix(corpus) #Converting the bag to a matrix
dtm = removeSparseTerms(dtm, 0.999)#All the columns with only 0s and one 1 will go away
dataset = as.data.frame(as.matrix(dtm))#Corpus to matrix to data frame
#Every column is a word that appeared in the corpus and the 1, 0 are if the word
#was present in the review
dataset$Recommend = data$recommendation #Taking the recommendation column from data and placing it in dataset

#You might have to encode the recommendation column
dataset$Recommend = factor(dataset$Recommend, labels = 0:1)

#Split the dataset
#Apply KNN and SVM

#Splitting the data into training and test sets
library(caTools)
split = sample.split(dataset$Recommend, SplitRatio = 0.8)
training_set = subset(dataset, split == TRUE)
test_set = subset(dataset, split == FALSE)

#Applying SVM
library(e1071)
classifier = svm(formula = Recommend ~., data=training_set,
                 type = 'C-classification', kernel = 'linear')

y_predict = predict(classifier, newdata = test_set[,-2113])
con_matrix = table(y_predict, test_set[,2113])
con_matrix

#Logistic
classifier = glm(formula = Recommend ~.,family = binomial,
                 data=training_set)
