# DDS-project1
An exploratory data analysis on suggestion of new Budweiser IPA line.
--by: Branum Stephan, Yang Zhang

## Introduction
In this report, we investigated the Beers dataset contains a list of 2410 US craft beers and Breweries dataset contains 558 US breweries. We successfully addressed 9 key questions regarding the dataset, and based on the study results, we give our recommendation to a commercial beer company the top potential markets for their new beer launch.

## Files
[eda.rmd](https://github.com/bstephan-smu/DDS-project1/blob/master/Results%20and%20Presentation/eda.Rmd): The RMarkdown file which is main data analysis document. It answers the 9 Analysis Questions, demonstrates how we achieved the results and gives explanations from the outputs.

[eda.r](https://github.com/bstephan-smu/DDS-project1/blob/master/eda.R): The file that just includes the R codes for all the analysis.

[presentation.pptx](https://github.com/bstephan-smu/DDS-project1/blob/master/Results%20and%20Presentation/presentation.pptx): Presentation slide deck with results displayed and elaborated.

[\project_files](https://github.com/bstephan-smu/DDS-project1/tree/master/project_files) is the directory we stored all the data files and reference word documents
-   [Beers.csv](https://github.com/bstephan-smu/DDS-project1/blob/master/project_files/Beers.csv): Original data files Beers
-   [Breweries.csv](https://github.com/bstephan-smu/DDS-project1/blob/master/project_files/Breweries.csv): Original data file Breweries
-   [Brewery_and_Beer_Clean.csv](https://github.com/bstephan-smu/DDS-project1/blob/master/project_files/Brewery_and_Beer_Clean.csv): Merged dataset with all NAs taken out
-   [2019 consensus data.csv](https://github.com/bstephan-smu/DDS-project1/blob/master/project_files/2019%20consensus%20data.csv): Census file in use for introducing poplulation information into the analysis in Question 9

## Research Questions 

1.   How many breweries are present in each state?

2.   Merge beer data with the breweries data. Print the first 6 observations and the last six observations to check the merged file.  (RMD only, this does not need to be included in the presentation or the deck.)

3.   Address the missing values in each column.

4.   Compute the median alcohol content and international bitterness unit for each state. Plot a bar chart to compare.

5.   Which state has the maximum alcoholic (ABV) beer? Which state has the most bitter (IBU) beer?

6.   Comment on the summary statistics and distribution of the ABV variable.

7.   Is there an apparent relationship between the bitterness of the beer and its alcoholic content? Draw a scatter plot.  Make your best judgment of a relationship and EXPLAIN your answer.

8.  Budweiser would also like to investigate the difference with respect to IBU and ABV between IPAs (India Pale Ales) and other types of Ale (any beer with “Ale” in its name other than IPA).  You decide to use KNN classification to investigate this relationship.  Provide statistical evidence one way or the other. You can of course assume your audience is comfortable with percentages … KNN is very easy to understand conceptually.

    In addition, while you have decided to use KNN to investigate this relationship (KNN is required) you may also feel free to supplement your response to this question with any other methods or techniques you have learned.  Creativity and alternative solutions are always encouraged.  

9. Knock their socks off!  Find one other useful inference from the data that you feel Budweiser may be able to find value in.  You must convince them why it is important and back up your conviction with appropriate statistical evidence. 

## Conclusions

1. Some states have much more breweries than others, “CO” is the state with largest number of breweries.

2. IBU are missing in roughly half of the data collection. Because these are both critical measurements for beer classification, we provided both a traditional approach as well as a simplified approach to address missing values. 

3. The scatterplot of ABV vs. IBU shows positive correlation. Additional evidence supports the claim that they can be used to differentiate IPA vs. non-IPA beers.

4. Using the ABV and IBU as parameters for KNN clustering, model accuracy reached upward of 85%. IPA’s proved to be higher in both ABV and IBU in most cases. 

5. By investing in the IPA craze across the nation, we recognize that the potential top 5 markets: Vermont, Colorado, Alaska, Montana and Oregon, could be great test subjects for a Budweiser IPA.

6. If we would prefer to target specific regions, we recommend the Pacific Northwest as the best market to enter, and wouldn’t  recommend the Southeast Region until we are more confident in mass production of the new product.


