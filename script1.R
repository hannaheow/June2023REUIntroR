weight = 7 
height = 64 
bmi = weight/ (height^2)
bmi_round = round(bmi, digits = 3)
bmi_round = round(bmi, 3) 

# three types: character, logical, numeric 

animal_weights = c(30, 40, 45, 35) #numeric vector
animal_types = c("cat", "dog", "bird") #character vector
animal_types4 = c(animal_types, "bird") #character vector appended to new character 
can_swim = c(FALSE, TRUE, TRUE, FALSE) #logical vector 


hank = c("cat", 40, FALSE) 
#mix of character, numeric, and logical (not best practice!)

animal_types4[2]
animal_types4[animal_types4 == "dog"]

animal_weights[animal_weights>40]
animal_weights>40 

animal_weights[animal_weights > 40 | animal_weights <35]
animal_weights[animal_weights>40 & animal_weights <35]

unique(animal_types4)
animal_types4[animal_types4 %in% c("cat", "dog")]
animal_types4[animal_types4 == "cat" | animal_types4 == "dog"]

heights = c(2, 4, 6, 8, NA)
mean(heights, na.rm = TRUE)
min(heights)
min(heights, na.rm = TRUE)
is.na(heights)
heights[!is.na(heights)]
heights[is.na(heights)]
na.omit(heights)

download.file(url = "https://ndownloader.figshare.com/files/2292169",
              destfile = "data_raw/portal_data_joined.csv")
library(tidyverse)
surveys = read_csv("data_raw/portal_data_joined.csv")
str(surveys)
head(surveys)
tail(surveys)
print(surveys)
dim(surveys)
nrow(surveys)
ncol(surveys)

surveys[1,1]
surveys[1,]
surveys[,1]
surveys[c(1,3,5), c(1,2)]
surveys[1:3, 1:3]
surveys$month
surveys[-34786,]
surveys[-nrow(surveys),]
surveys[,-ncol(surveys)]
surveys_short <- surveys[,-ncol(surveys)]

colnames(surveys)
unique(surveys$sex)

surveys$sex_factor = factor(surveys$sex)
plot(surveys$sex_factor)
plot(surveys$sex)


library(dplyr)
library(tidyr)
select(surveys, plot_id, species_id, weight)
select(surveys, -record_id, -species_id)
filter(surveys, year == 1995)

surveys %>% select(plot_id, species_id, year) %>% 
  filter(year == 1995)

filter(select(surveys, plot_id, species_id, year), year == 1995)
surveys[,c("plot_id", "species_id", "year")][surveys$year == 1995,]

surveys %>% group_by(sex_factor) %>% 
  summarize(mean_weight = mean(weight, na.rm = TRUE))
surveys_weightsex = surveys %>% group_by(sex_factor) %>% 
  mutate(weight_sex = mean(weight, na.rm = TRUE))

surveys_weight = surveys %>% mutate(weight_kg = weight / 1000,
                                    weight_lb = weight_kg * 2.2)
surveys$weight_kg = surveys$weight / 1000

surveys %>% mutate(hindfoot_cm = hindfoot_length/10) %>% 
  select(species_id, hindfoot_cm) %>% filter(hindfoot_cm >4)

surveys_select = surveys %>% mutate(hindfoot_cm = hindfoot_length/10) %>% 
  filter(sex == "M") %>% select(species_id, hindfoot_cm)
surveys_select %>% arrange(desc(hindfoot_cm))

#1 Create a new dataframe from the surveys data that contains only the 
# species_id column  and a new column called hindfoot_cm which 
# contains the hindfoot_length values (currently in mm) 
# converted to cm. 

#2 Create a new dataframe from the surveys data that contains 
# only the species_id and mean weights of each species id
# Bonus: create a plot of the mean weights of each species_id 

d = surveys %>% group_by(species_id) %>% summarise(meanw = mean(weight, na.rm = TRUE))
d = d %>% filter(!is.nan(meanw))

#NaN occur when we try to do something impossible. in this case, NaN occurs because there are some species_ids which do not have any weights at all 
#use is.nan instead of is.na since R handles them differently 

boxplot(meanw~species_id, data = d)
