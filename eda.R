
# The following is an exploratory data analysis performed by Branum Stephan and Yang Zhang
# Doing Data Science - fall 2019

library(dplyr)
library(here)
library(ggplot2)
library(tidyr)
library(ggthemes)


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
