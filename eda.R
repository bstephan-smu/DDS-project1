
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

# based on evidence, will use median abv and ibu per beer style to fill na
main <- main %>% group_by(beer_style) %>% mutate(ibu_corr = ifelse(is.na(ibu), median(ibu, na.rm = TRUE), ibu), abv_corr = ifelse(is.na(abv), median(abv, na.rm = TRUE), abv))

# let's see how we did...you will see all abv "corrected" and ibu had over 950 values "corrected"
print(colSums(is.na(main)))


