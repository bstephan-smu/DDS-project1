
# The following is an exploratory data analysis performed by Branum Stephan and Yang Zhang
# Doing Data Science - fall 2019

library(dplyr)
library(here)
library(ggplot2)
library(tidyr)
library(ggthemes)
library(class)
library(GGally)
library(caret)
library(e1071)



# set relative brewery file path as variable
beers_csv <- here("project_files", "Beers.csv")
breweries_csv <- here("project_files", "Breweries.csv")

# initialize dataframes
beers <- data.frame(read.csv(beers_csv))
breweries <- data.frame(read.csv(breweries_csv))

# Question 1 - How many breweries are present in each state?
per_state <- breweries %>% group_by(State) %>% tally(name="breweries_per_state") %>% arrange(desc(breweries_per_state))
per_state %>% ggplot(aes(x=reorder(State, breweries_per_state), y=breweries_per_state)) + geom_bar(stat="identity") + ggtitle("Total Breweries per State")  + coord_flip() + ylab("State") + xlab("Total Breweries")
# TODO: create base map and plot choropleth of breweries per state

# Question 2: Merge Beer data with Breweries data. Print head(6) and tail(6)
# Note: left join breweries on beers because 1-many relationship
main <- merge(beers, breweries, by.x="Brewery_id", by.y="Brew_ID")

# clean column names for good housekeeping
main_cols <- c("brewery_id", "beer_name", "beer_id", "abv", "ibu", "beer_style", "serving_ounces", "brewery_name", "city", "state")
names(main) <- main_cols

# print head and tail of resultant data set
print(rbind(head(main, 6), tail(main, 6)))

# Part 3: Address NA values in each column
# find the NA count per column to decide next steps...you will see some missing abv and A LOT of missing ibu
print(colSums(is.na(main)))

# Preserve a version of merged dataset by removing all NAs
main_clean<-na.omit(main)

# based on evidence, will use median abv and ibu per beer style to fill na
main <- main %>% group_by(beer_style) %>% mutate(ibu_corr = ifelse(is.na(ibu), median(ibu, na.rm = TRUE), ibu), abv_corr = ifelse(is.na(abv), median(abv, na.rm = TRUE), abv))

# let's see how we did...you will see all abv "corrected" and ibu had over 950 values "corrected"
print(colSums(is.na(main)))

# Export the no-NAs file to a new csv file
write.csv(main_clean,"./Brewery_and_Beer_Clean.csv", row.names = FALSE)

# Part 4: Compute Median ABV and IBU and do bar plot
library(doBy)

#Median ABV -> Alcohol content by state
ABV_by_State <- summaryBy(abv ~ state, data = main_clean, FUN = list(median))
ABV_by_State<-as.data.frame(ABV_by_State)
names(ABV_by_State) = c("State", "ABV")
print(ABV_by_State)

#Median IBU->international bitterness by state
IBU_by_State <- summaryBy(ibu ~ state, data = main_clean, FUN = list(median))
IBU_by_State<-as.data.frame(IBU_by_State)
names(IBU_by_State) = c("State", "IBU")
print(IBU_by_State)

#Merge IBU and ABV by "state"
Median_IBU_and_ABV<-merge(IBU_by_State,ABV_by_State, by = "State")

#Bar_Chart_Plotter

library(reshape)
Median_IBU_and_ABV <- melt(Median_IBU_and_ABV)
names(Median_IBU_and_ABV)[3]<-"Median"
ggplot(Median_IBU_and_ABV, aes(x =State, y= Median, fill = variable), xlab="State") +
  geom_bar(stat="identity", width=.5, position = "dodge") +facet_grid( variable~ . ,scales = "free")+
  labs(title = "Comparing Median IBU &  Median ABV by State")

#Part5: The State with max ABV and max IBU

#The item with max ABV
beer_MaxAbv <- main_clean[which.max(main_clean$abv),]
print(paste0("The state has maximum alcoholic beer is:", beer_MaxAbv$state, " with ABV of ", beer_MaxAbv$abv))

#The item with max IBU
beer_MaxIbu <- main_clean[which.max(main_clean$ibu),]
print(paste0("The state has maximum bitterness beer is:", beer_MaxIbu$state, " with IBU of ", beer_MaxIbu$ibu))

#Part6: The summary statistics and distribution of the ABV
summary(main_clean$abv)
hist(main_clean$abv)

#Part7: Relationship between IBU and ABV?
#Scatterplot
main_clean %>% ggplot(mapping = aes(ibu, abv)) + geom_point(colour = "purple", na.rm=TRUE)+geom_smooth(method=lm, se=FALSE, na.rm=TRUE) 

#Part8: KNN used for analysis IBU and ABV relationship of IPA and Ale(no IPA)

#Form Dataset with beer style IPA 
main_IPA <- main_clean[grep("IPA",main_clean$beer_style),]
#Exclude the "Ale" part within IPA subset
main_IPA <- main_IPA[!grepl("Ale", main_IPA$beer_style),]

print(head(main_IPA))

#Form Dataset with beer style Ale 
main_Ale <- main_clean[grep("Ale",main_clean$beer_style),]
#Exclude the "IPA" part within Ale subset
main_Ale <- main_Ale[!grepl("IPA", main_Ale$beer_style),]

print(head(main_Ale))

#Scatterplot of IPA and Ale datasets
main_IPA %>% ggplot(mapping = aes(ibu, abv)) + geom_point(colour = "red", na.rm=TRUE)+geom_smooth(method=lm, se=FALSE, na.rm=TRUE) 
main_Ale %>% ggplot(mapping = aes(ibu, abv)) + geom_point(colour = "green", na.rm=TRUE)+geom_smooth(method=lm, se=FALSE, na.rm=TRUE) 

#Select cols in IPA and Ale datasets and set group name
#Group numbers: Ale--1, IPA--2
test_Ale <- main_Ale %>% select(ibu,abv)
test_Ale$flag = 1
test_IPA <- main_IPA %>% select(ibu,abv)
test_IPA$flag = 2
test_4KNN <- rbind(test_Ale, test_IPA)  #Combine two subsets into one for later classification

#Plot IPA and Ale datasets together in a single scatterplot
test_4KNN %>% ggplot(mapping = aes(ibu, abv)) + geom_point(color= factor(test_4KNN$flag), na.rm=TRUE)+geom_smooth(method=lm, se=FALSE, na.rm=TRUE) 

#Divide dataset into train and test 
set.seed(123)
trainIndex = sample(seq(1:937), 650)
trainBeers = test_4KNN[trainIndex,]
testBeers = test_4KNN[-trainIndex,] 

#Train Data Visualization
trainBeers %>% ggplot(aes(x = abv,fill = flag)) + geom_histogram() + facet_grid(rows = vars(flag))
trainBeers %>% ggplot(aes(x = ibu,fill = flag)) + geom_histogram() + facet_grid(rows = vars(flag))

trainBeers$flag = as.factor(trainBeers$flag)
trainBeers %>% select(abv, ibu, flag) %>% ggpairs(aes(color = flag))

##Classification Method 1: KNN
#Train the model (k=5)
classifications = knn(trainBeers[c(1,2)],testBeers[c(1,2)],trainBeers$flag, prob = TRUE, k = 5)

#The resulting confusion matrix
table(testBeers[,'flag'],classifications)
CM_k5 = confusionMatrix(table(testBeers[,'flag'],classifications))

#Alternative K to train the model (k=10)
classifications_k10 = knn(trainBeers[c(1,2)],testBeers[c(1,2)],trainBeers$flag, prob = TRUE, k = 10)

#The resulting confusion matrix
table(testBeers[,'flag'],classifications_k10)
CM_k10 = confusionMatrix(table(testBeers[,'flag'],classifications_k10))


##################################################################
# Loop for many k and the average of many training / test partition

set.seed(1)
iterations = 10
numks = 90
splitPerc = .7

masterAcc = matrix(nrow = iterations, ncol = numks)

for(j in 1:iterations)
{
  trainIndices = sample(1:dim(test_4KNN)[1],round(splitPerc * dim(test_4KNN)[1]))
  train = test_4KNN[trainIndices,]
  test = test_4KNN[-trainIndices,]
  for(i in 1:numks)
  {
    classifications = knn(train[,c(1,2)],test[,c(1,2)],train$flag, prob = TRUE, k = i)
    table(classifications,test$flag)
    CM = confusionMatrix(table(classifications,test$flag))
    masterAcc[j,i] = CM$overall[1]
  }
  
}

MeanAcc = colMeans(masterAcc)

plot(seq(1,numks,1),MeanAcc, type = "l")

which.max(MeanAcc)
max(MeanAcc)
###############################################################

## Classification Method 2: Native Bayes
classifications_nb = naiveBayes(trainBeers[,c(1,2)],as.factor(trainBeers$flag))
table(predict(classifications_nb,testBeers[,c(1,2)]),as.factor(testBeers$flag))
CM_nb = confusionMatrix(table(predict(classifications_nb,testBeers[,c(1,2)]),as.factor(testBeers$flag)))


####################################################
# Loop for average of many training / test partition

iterations = 10
masterAcc_nb = matrix(nrow = iterations)

for(j in 1:iterations)
{
  
  trainIndices = sample(seq(1:length(test_4KNN$flag)),round(.7*length(test_4KNN$flag)))
  trainBeers = test_4KNN[trainIndices,]
  testBeers = test_4KNN[-trainIndices,] 
  
  model_nb = naiveBayes(trainBeers[,c(1,2)],as.factor(trainBeers$flag))
  table(predict(model_nb,testBeers[,c(1,2)]),as.factor(testBeers$flag))
  CM = confusionMatrix(table(predict(model_nb,testBeers[,c(1,2)]),testBeers$flag))
  masterAcc_nb[j] = CM$overall[1]
}


plot(seq(1,iterations,1),masterAcc_nb, type = "l")

which.max(masterAcc_nb)
max(masterAcc_nb)
##########################################################

## Classification Method 3: SVM
classifications_svm = svm(trainBeers[,c(1,2)],as.factor(trainBeers$flag))
table(predict(classifications_svm,testBeers[,c(1,2)]),as.factor(testBeers$flag))
CM_svm = confusionMatrix(table(predict(classifications_svm,testBeers[,c(1,2)]),testBeers$flag))

## Classification Method 4: Random Forest
library(randomForest)
classifications_rf = randomForest(trainBeers[,c(1,2)],as.factor(trainBeers$flag),ntree=500)
table(predict(classifications_rf,testBeers[,c(1,2)]),as.factor(testBeers$flag))
CM_rf = confusionMatrix(table(predict(classifications_rf,testBeers[,c(1,2)]),testBeers$flag))

## Classification Method 5: Xgboost
TrainControl <- trainControl( method = "repeatedcv", number = 10, repeats = 4)
classifications_xg = train(trainBeers[,c(1,2)],as.factor(trainBeers$flag), method = "xgbTree", trControl = TrainControl,verbose = FALSE)
table(predict(classifications_xg,testBeers[,c(1,2)]),as.factor(testBeers$flag))
CM_xg = confusionMatrix(table(predict(classifications_xg,testBeers[,c(1,2)]),testBeers$flag))

## Cluster Experiment: K-means
library(cluster)
cluster_kmeans = kmeans(test_4KNN, 2) # 2 cluster solution
table(cluster_kmeans$cluster,test_4KNN$flag)


