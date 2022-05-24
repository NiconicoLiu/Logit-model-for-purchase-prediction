dataset<- read.csv(file.choose(), header=TRUE)
#install.packages("caTools")
#install.packages("factoextra")
library(caTools)
library(tidyverse)
library(caret)#PCA
library(factoextra)#PCA
require(dplyr)
library(corrplot)#cor
library(RColorBrewer)#cor
library(ggplot2)
#install.packages("missForest")
library(missForest)
#install.packages("mice")
library(mice)
datasetNa<- prodNA(dataset, noNA = 0.1)
datasetNa <- subset(dataset, select = c(GenderCode,Lienholder))
summary(datasetNa)
#install.packages('Rcpp') 
#library(Rcpp)
datasetNa <- mice(datasetNa, m=2, maxit = 10, method = 'pmm', seed = 500)
summary(datasetNa)
imputed_Data
datasetNa$imp
completeData <- complete(datasetNa,2)
dataset<-cbind(dataset,completeData)

dataset[,17]
dataset[,13]
dataset<-dataset[-c(13,17)]
colnames(dataset)

#fill na with average segment odometer
dataset <- dataset %>% group_by(SegmentName) %>%
  mutate(EstimatedCurrentOdometer=ifelse(is.na(EstimatedCurrentOdometer),mean(EstimatedCurrentOdometer,na.rm=TRUE),EstimatedCurrentOdometer))

#fill na with average segment age
dataset <- dataset %>% group_by(SegmentName) %>%
  mutate(Age=ifelse(is.na(Age),mean(Age,na.rm=TRUE),Age))

##replace the content within () and the symbol ()
dataset$WealthRating <- gsub("\\s*\\([^\\)]+\\)","",dataset$WealthRating)
dataset$WealthRating = as.integer(dataset$WealthRating)

##fill na with average segment wealth rating
dataset <- dataset %>% group_by(SegmentName) %>%
  mutate(WealthRating=ifelse(is.na(WealthRating),mean(WealthRating,na.rm=TRUE),WealthRating))

col_order <- c("X", "Year", "SegmentName","TireQuantity",
               "EstimatedCurrentOdometer", "OwnersNum","Last.Owner.Length.Of.Ownership.In.Days","Private.Party","Age","WealthRating")
dataset2 <- dataset[, col_order]

##cluster kmean
datasetCluster<-dataset[complete.cases(dataset),]
d_cluster = kmeans(x=datasetCluster[10:13],centers=2,nstart =1000)
d_cluster$centers
fviz_cluster(d_cluster, data = datasetCluster[10:13],
             palette = c("#2E9FDF", "#00AFBB"), 
             geom = "point",
             ellipse.type = "convex", 
             ggtheme = theme_bw()
)


##principle component analysis
datasetA<-dataset2[,4:10]

head(datasetA)

datasetA<-datasetA[complete.cases(datasetA),]

summary(datasetA)

##correlation plot##no significant correlation b/w variables 
M <-cor(datasetA)
corrplot(M, type="upper", order="hclust",
         col=brewer.pal(n=8, name="RdYlBu"))
require("corrplot")

source("http://www.sthda.com/upload/rquery_cormat.r")
rquery.cormat(datasetA)


res.pca <- prcomp(datasetA, scale = TRUE)
fviz_eig(res.pca)
fviz_pca_ind(res.pca,
             col.ind = "cos2", # Color by the quality of representation
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             repel = TRUE     # Avoid text overlapping
)
fviz_pca_var(res.pca,
             col.var = "contrib", # Color by contributions to the PC
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             repel = TRUE     # Avoid text overlapping
)

fviz_pca_biplot(res.pca, repel = TRUE,
                col.var = "#2E9FDF", # Variables color
                col.ind = "#696969"  # Individuals color
)
# Eigenvalues
eig.val <- get_eigenvalue(res.pca)
eig.val

# Results for Variables
res.var <- get_pca_var(res.pca)
res.var$coord          # Coordinates
res.var$contrib        # Contributions to the PCs
res.var$cos2           # Quality of representation 
# Results for individuals
res.ind <- get_pca_ind(res.pca)
res.ind$coord          # Coordinates
res.ind$contrib        # Contributions to the PCs
res.ind$cos2           # Quality of representation 

##logit analysis
##why delete certain variables
##private party, wholesale,retail are correlated
#tire quantity: too many variables in the model, leading to a perfect separation of cases when adding tire quantity.
colnames(dataset)

##logit model
##create the drops (at line #88) b/c too many NA; 
##drop marital status, wealthrating due to non-significant test

##fill Na in children presence with 0
dataset$ChilrenPresence[is.na(dataset$ChilrenPresence)] <-0
head(dataset$ChilrenPresence)
dataset$marital_dummy <- factor(dataset$MaritalStatus) #treating rank as a categorical variable
#drops <- c("Occupation","EducationLevel","Household.Income","HouseholdNum","HouseholdAdults","HomeOwnership","DwellingType")
#dataset <- dataset[ , !(names(dataset) %in% drops)]

##logit table for logit model
datasetLogit<-dataset[complete.cases(dataset),]
bilogit <- glm(SegmentName ~ (Lienholder+OwnersNum+ChilrenPresence+EstimatedCurrentOdometer+Age+Last.Owner.Length.Of.Ownership.In.Days+Private.Party+GenderCode)^2, data = datasetLogit, family = "binomial")
summary(bilogit)
#AIC 7166
bilogiIn<-glm(SegmentName~OwnersNum+EstimatedCurrentOdometer+Age+Last.Owner.Length.Of.Ownership.In.Days+GenderCode+Lienholder,data = datasetLogit, family = "binomial" )

summary(bilogiIn)
#AIC 7166

bilogit1<-glm(SegmentName~OwnersNum+EstimatedCurrentOdometer+Age+Last.Owner.Length.Of.Ownership.In.Days+GenderCode+Lienholder*OwnersNum+Lienholder*Private.Party+OwnersNum*EstimatedCurrentOdometer+OwnersNum*Age+OwnersNum*Last.Owner.Length.Of.Ownership.In.Days+ChilrenPresence*Private.Party+Age*Last.Owner.Length.Of.Ownership.In.Days+Last.Owner.Length.Of.Ownership.In.Days*Private.Party,data = datasetLogit, family = "binomial" )

##lowest AIC 7160
bilogit2 <- glm(SegmentName ~ (WealthRating+Lienholder+OwnersNum+ChilrenPresence+EstimatedCurrentOdometer+Age+Last.Owner.Length.Of.Ownership.In.Days+Private.Party+GenderCode)^2, data = datasetLogit, family = "binomial")
summary(bilogit2)

summary(bilogit1)

coef(bilogit)
bilogit3 <- glm(SegmentName ~ WealthRating*ChilrenPresence*Lienholder+(WealthRating+Lienholder+OwnersNum+ChilrenPresence+EstimatedCurrentOdometer+Age+Last.Owner.Length.Of.Ownership.In.Days+Private.Party+GenderCode)^2, data = datasetLogit, family = "binomial")
summary(bilogit3)
bilogit4<-glm(SegmentName~Age+GenderCode+Private.Party+OwnersNum+Last.Owner.Length.Of.Ownership.In.Days+EstimatedCurrentOdometer+WealthRating*Lienholder+WealthRating*ChilrenPresence+Lienholder*ChilrenPresence+OwnersNum*EstimatedCurrentOdometer+OwnersNum*Age+OwnersNum*Last.Owner.Length.Of.Ownership.In.Days+OwnersNum*Private.Party+EstimatedCurrentOdometer*Private.Party,data = datasetLogit, family = "binomial")
summary(bilogit4)

##Interpret individual independent variables
##If coefficient (logit) is positive, the effect of this predictor (buying) is positive and vice versa.
##ex. one unit increases in the ownersNum increase the odd of buying by 82 percent/ household with children are 1.28 times more likely to buy tires than no children

logitProb <- function(logit){
  odds <- exp(logit)
  #prob <- odds / (1 + odds)
  return(odds)
}

logitProb(coef(bilogit))

##prediction
# Split the data into training and test set
set.seed(123)
training.samples <- datasetLogit$SegmentName %>% createDataPartition(p = 0.8, list = FALSE)
train.data  <- datasetLogit[training.samples, ]
test.data <- datasetLogit[-training.samples, ]

# Make predictions
probabilities <- bilogit2 %>% predict(test.data, type = "response")
predicted.Buy <- ifelse(probabilities > 0.5, 1, 0)
head(predicted.Buy)

##accuracy rate of the model 70.2%
mean(predicted.Buy == test.data$SegmentName)

probabilities1 <- bilogit2 %>% predict(train.data, type = "response")
predicted.Buy1 <- ifelse(probabilities1 > 0.5, 1, 0)
head(predicted.Buy1)
mean(predicted.Buy1 == train.data$SegmentName)

##the amount of private party sales matter in tire sales
train.data %>%
  mutate(prob = ifelse(SegmentName == "1", 1, 0)) %>%
  ggplot(aes(Private.Party, prob)) +
  geom_point(alpha = 0.2) +
  geom_smooth(method = "glm", method.args = list(family = "binomial")) +
  labs(
    title = "Logistic Regression Model", 
    x = "PrivateParty",
    y = "Probability of being a buyer"
  )

##length of ownership matters for targeting 
train.data %>%
  mutate(prob = ifelse(SegmentName == "1", 1, 0)) %>%
  ggplot(aes(Last.Owner.Length.Of.Ownership.In.Days,prob)) +
  geom_point(alpha = 0.2) +
  geom_smooth(method = "glm", method.args = list(family = "binomial")) +
  labs(
    title = "Logistic Regression Model", 
    x = "Last.Owner.Length.Of.Ownership.In.Days",
    y = "Probability of being a buyer"
  )

##customers with high estimated odometer tend to not buy tires here 
train.data %>%
  mutate(prob = ifelse(SegmentName == "1", 1, 0)) %>%
  ggplot(aes(EstimatedCurrentOdometer,prob)) +
  geom_point(alpha = 0.2) +
  geom_smooth(method = "glm", method.args = list(family = "binomial")) +
  labs(
    title = "Logistic Regression Model", 
    x = "EstimatedCurrentOdometer",
    y = "Probability of being a buyer"
  )

train.data %>%
  mutate(prob = ifelse(SegmentName == "1", 1, 0)) %>%
  ggplot(aes(WealthRating,prob)) +
  geom_point(alpha = 0.2) +
  geom_smooth(method = "glm", method.args = list(family = "binomial")) +
  labs(
    title = "Logistic Regression Model", 
    x = "EstimatedCurrentOdometer",
    y = "Probability of being a buyer"
  )

