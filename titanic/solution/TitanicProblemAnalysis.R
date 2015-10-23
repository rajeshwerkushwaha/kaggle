#Load raw data
train = read.csv("train.csv", header = TRUE)
test = read.csv("test.csv", header = TRUE)

#Add a "Survived" variable to the test set to allow for combining the data sets
test.survived = data.frame(Survived = rep("None", nrow(test)), test[,])

#Combine data sets
data.combined = rbind(train, test.survived)

#A bit about R data types (e.g., factors)
str(data.combined)

#Convert 'Survived' and 'Pclass' column to factor
data.combined$Survived = as.factor(data.combined$Survived)
data.combined$Pclass = as.factor(data.combined$Pclass)

#Take a look at gross survival rates
table(data.combined$Survived)

#Distribution across classes
table(data.combined$Pclass)

#Load up ggplot2 package to use for visualizations
library(ggplot2)

#Hypothesis - Rich folks survived at a higher rates
train$Pclass = as.factor(train$Pclass)
ggplot(train, aes(x = Pclass, fill = factor(Survived))) +
  geom_histogram(with = 0.5) +
  xlab("Pclass") +
  ylab("Total Count") +
  labs(fill = "Survived")

#Examine the first few names in the training data set
head(as.character(train$Name))

#How many unique names are there across both train & test?
length(unique(as.character(data.combined$Name)))

#Two duplicate names, take a closer look
#First get the duplicate names and store them as a vector
dup.names = as.character(data.combined[which(duplicated(as.character(data.combined$Name))), "Name"])

#Next, take a look at the records in the combined data set who has duplicate names
data.combined[which(data.combined$Name %in% dup.names),]

#What is up with the 'Miss.' and 'Mr.' thing?
library(stringr)

#Any correlation with other variables (e.g., sibsp)?
misses = data.combined[which(str_detect(data.combined$Name, "Miss.")),]
misses[1:5,]

#Hypothesis - Name titles correlates with age
misses = data.combined[which(str_detect(data.combined$Name, "Mrs.")),]
misses[1:5,]

#Check out males to see if pattern continues
males = data.combined[which(train$Sex == 'male'),]
males[1:5,]


#Expand upon the relationship between 'Survived' and 'Pclass' by adding the new
#'Title' variable
#data set and then explore a potential 3-dimentional relationship.

#Create a utility function to help with title extraction
extractTitle = function(name){
  name = as.character(name)
  
  if(length(grep("Miss.", name)) > 0){
    return ("Miss.")
  }else if(length(grep("Master.", name)) > 0){
    return ("Master.")
  }else if(length(grep("Mrs.", name)) > 0){
    return ("Mrs.")
  }else if(length(grep("Mr.", name)) > 0){
    return ("Mr.")
  }else{
    return ("Other")
  }
  
}

titles = NULL
for(i in 1:nrow(data.combined)){
  titles = c(titles, extractTitle(data.combined[i, "Name"]))
}
data.combined$Title = as.factor(titles)


#Since we only have survived lables for the train set, only use the 891 rows
ggplot(data.combined[1:891,], aes(x = Title, fill = Survived)) +
  geom_bar(binwith = 0.5) +
  facet_wrap(~Pclass) +
  ggtitle("Pclass") +
  xlab("Title") +
  ylab("Total Count") +
  labs(fill = "Survived")




























