library(dplyr)
library(here)
library(ggplot2)
library(tidyr)
library(ggthemes)
library(doBy)
library(reshape)
library(plotly)
library(GGally)
library(caret)
library(e1071)
library(class)
library(usmap)

# set global theme
# custom theme
bg_color = "#0B0C10"
bar_color = "#66FCF1"
text_main = "#FFFFF"
text_ticks = "#CFC6C7"
axis_lines = #453a3b"
  cust_theme <- theme(plot.title = element_text(color = 'white', vjust=0.5, hjust=0.5),
                      plot.background = element_rect(fill = bg_color),
                      panel.background = element_rect(fill = bg_color),
                      axis.text.x=element_text(angle=90,hjust=1),
                      axis.text = element_text(colour = text_ticks),
                      panel.grid.major.x = element_blank(),
                      panel.grid.minor.x = element_blank(),
                      panel.grid.major.y =  element_line(colour = "#453a3b", linetype = 1, size = 0.25),
                      panel.grid.minor.y = element_blank(),
                      axis.title = element_text(colour = "white"))

# overwrite default theme with custom
theme_set(theme_foundation() + cust_theme)

# set relative brewery file path as variable
beers_csv <- here("project_files", "Beers.csv")
breweries_csv <- here("project_files", "Breweries.csv")

# initialize dataframes
beers <- data.frame(read.csv(beers_csv))
breweries <- data.frame(read.csv(breweries_csv))

# Question 1 - How many breweries are present in each state?
per_state <- breweries %>% group_by(State) %>% tally(name="breweries_per_state") %>% arrange(desc(breweries_per_state))
fig1 <- per_state %>% ggplot(aes(x=reorder(State, breweries_per_state), y=breweries_per_state)) + geom_bar(stat="identity", fill=bar_color) + ggtitle("Total Breweries per State") + ylab("Total Breweries") + xlab("State")

ggplotly(fig1)

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
medians <- main_clean %>% group_by(state) %>% summarise(median_abv = median(abv), median_ibu = median(ibu))

#Bar_Chart_Plotter

# ibu bar plot
fig2 <- medians %>% ggplot() + geom_bar(aes(x=reorder(state, -median_ibu), y=median_ibu), stat="identity", fill=bar_color) + ggtitle("Median IBU per State") +ylab("Median IBU") + xlab("State") 

# abv bar plot
fig3 <- medians %>% ggplot() + geom_bar(aes(x=reorder(state, -median_abv), y=median_abv*100), stat="identity", fill=bar_color) + ggtitle("Median ABV per State") +ylab("Median ABV") + xlab("State") + expand_limits(y=c(0, max(medians$median_abv+0.5)))

ggplotly(fig2)
ggplotly(fig3)

#Part5: The State with max ABV and max IBU
#The item with max ABV
beer_MaxAbv <- main_clean[which.max(main_clean$abv),]
print(paste0("The state has maximum alcoholic beer is:", beer_MaxAbv$state, " with ABV of ", beer_MaxAbv$abv))

#The item with max IBU
beer_MaxIbu <- main_clean[which.max(main_clean$ibu),]
print(paste0("The state has maximum bitterness beer is:", beer_MaxIbu$state, " with IBU of ", beer_MaxIbu$ibu))

#Part6: The summary statistics and distribution of the ABV
summary(main_clean$abv)
fig4 <- main_clean %>% ggplot() + geom_histogram(aes(x=abv*100), fill=bar_color) + ggtitle("Distribution of ABV") + xlab("Percent ABV")

ggplotly(fig4)

#Part7: Relationship between IBU and ABV?
#Scatterplot
fig5 <- main_clean %>% ggplot(aes(ibu, abv*100)) + geom_point(colour=bar_color, na.rm=TRUE)+geom_smooth(method=lm, se=FALSE,color="white", linetype="dashed", na.rm=TRUE) + ggtitle("ABV vs. IBU") + ylab("abv")

ggplotly(fig5)

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
fig6 <- main_IPA %>% ggplot(mapping = aes(ibu, abv)) + geom_point(colour = bar_color, na.rm=TRUE)+geom_smooth(method=lm, se=FALSE, na.rm=TRUE, colour="white") + ggtitle("IPA: ABV vs. IBU")
fig7 <- main_Ale %>% ggplot(mapping = aes(ibu, abv)) + geom_point(color = bar_color, na.rm=TRUE)+geom_smooth(method=lm, se=FALSE, na.rm=TRUE, colour="white") + ggtitle("Ale: ABV vs. IBU")

ggplotly(fig6)
ggplotly(fig7)

#Select cols in IPA and Ale datasets and set group name
#Group numbers: Ale--1, IPA--2
test_Ale <- main_Ale %>% select(ibu,abv)
test_Ale$flag = "ALE"
test_IPA <- main_IPA %>% select(ibu,abv)
test_IPA$flag = "IPA"
test_4KNN <- rbind(test_Ale, test_IPA)  #Combine two subsets into one for later classification
test_4KNN$flag <- as.factor(test_4KNN$flag)

#Plot IPA and Ale datasets together in a single scatterplot
test_4KNN %>% ggplot(mapping = aes(ibu, abv,color=test_4KNN$flag)) + geom_point(na.rm=TRUE) + geom_smooth(method=lm, se=FALSE, na.rm=TRUE, linetype="dashed") + scale_colour_manual(name="Beer Style", values=c("ALE" = "#FF652F","IPA" ="#14A76C")) + ggtitle("ABV vs. IBU by Beer Style: KNN Test")

#Divide dataset into train and test 
set.seed(123)
trainIndex = sample(seq(1:937), 650)
trainBeers = test_4KNN[trainIndex,]
testBeers = test_4KNN[-trainIndex,] 

#Train Data Visualization
trainBeers %>% ggplot(aes(x = abv,fill = flag)) + geom_histogram() + facet_grid(rows = vars(flag))
trainBeers %>% ggplot(aes(x = ibu,fill = flag)) + geom_histogram() + facet_grid(rows = vars(flag))

trainBeers$flag = as.factor(trainBeers$flag)
trainBeers %>% select(abv, ibu, flag) %>% ggpairs(aes(color = flag)) + ggtitle("Distribution of Ale and IPA IBU data")

##Classification Method 1: KNN
#Train the model (k=5)
classifications = knn(trainBeers[c(1,2)],testBeers[c(1,2)],trainBeers$flag, prob = TRUE, k = 5)

#The resulting confusion matrix
table(testBeers[,'flag'],classifications)
CM_k5 = confusionMatrix(table(testBeers[,'flag'],classifications))
CM_k5

#Alternative K to train the model (k=10)
classifications_k10 = knn(trainBeers[c(1,2)],testBeers[c(1,2)],trainBeers$flag, prob = TRUE, k = 10)

#The resulting confusion matrix
table(testBeers[,'flag'],classifications_k10)
CM_k10 = confusionMatrix(table(testBeers[,'flag'],classifications_k10))
CM_k10

##################################################################
# Loop for many k and the average of many training / test partition

set.seed(1)
iterations = 100
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

# Part 9: IPA per 100k population (additional exploration).
# Part 9: Additional Data Exploration
# Plan - to count % IPA per total craft beers in each state.

# finding the beers containing "IPA" in their style and tallying
cat_freq <- main %>% mutate(category=
                              case_when(
                                grepl("IPA", beer_style) ~ "IPA",
                                TRUE ~ "OTHER"
                              )
) %>% group_by(state, category) %>% summarise(n=n()) %>% mutate(freq = n/sum(n)) %>% filter(category=="IPA")
cat_freq$state <- trimws(cat_freq$state)

# adding full state names from merging with us_map() library function
cat_freq <- merge(distinct(us_map(), full, abbr), cat_freq, by.x="abbr", by.y="state", all.x=TRUE) %>% mutate(n=replace_na(n, 0), freq=replace_na(freq, 0))

# create percentage from frequency
cat_freq$freq = round(cat_freq$freq, digits=4)*100

# creatig dataframe from state consensus
state_pop <- data.frame(read.csv(here("project_files", "2019 consensus data.csv")))

# merging dataframe with %IPA per state dataframe
normalized_IPA <- merge(state_pop, cat_freq, by.x="State", by.y="full")

# normalizing IPA count per 100k state population
normalized_IPA <- normalized_IPA %>% mutate(IPA_per_100k = n/(Pop / 100000)) %>% arrange(desc(IPA_per_100k))

# creating a hover tag for the map
normalized_IPA$hover <- with(normalized_IPA, paste(State, '<br>', "IPA Beers", n, "<br>","IPA % of total craft beers", freq,"%", "<br>", "Population Rank", rank, "<br>", "IPA per 100k", IPA_per_100k))

Based on this evidence, you'll see the following states are the best candidates for a trial run of Budeweiser IPA, according to population and IPA interest.

# displaying a table of top 5 markets for reference
fig8 <- normalized_IPA %>% select(State, Pop, IPA_per_100k) %>% top_n(8)
fig8 %>% plot_ly(type='table',
                 header = list(
                   values = c("State", sprintf("State Population (%s Sample Size)",sum(fig8$Pop)), "IPA per 100k People")),
                 cells=list(
                   values=t(unname(as.matrix(fig8))),
                   font = list(color = c('#506784'), size = 12)
                 ))
fig8

# displaying all US IPA markets on map
fig9 <- plot_geo(normalized_IPA, locationmode = 'USA-states') %>%
add_trace(
z = ~IPA_per_100k, text=~hover, locations = ~abbr,
color = ~IPA_per_100k,
marker = list(line = list(color = toRGB(bg_color), width = 2.25)),colorscale='MAGMA'
) %>%
colorbar(title = "IPA") %>%
layout(
title = 'IPA count per 100k',
font = list(color = 'white'),
geo=list(
scope = 'usa',
projection = list(type = 'albers usa'),
showlakes = FALSE,
bgcolor=toRGB(bg_color, alpha = 1)),
paper_bgcolor=toRGB(bg_color, alpha = 1),
margin=list(l=20, r=20, t=60, b=20)
)

fig9

