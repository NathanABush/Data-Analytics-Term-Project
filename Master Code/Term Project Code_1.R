###PACKAGES####
library(readxl)
library(tidyverse)
library(dplyr)
library(broom)
library(tinytex)
library(rvest)
library(tigris)
library(tmap)
library(sf)
library(estimatr)
library(fixest)
library(modelsummary)
library(ggplot2)
library(gridExtra)

### Clean Energy Data #### 
#SETYOURWDHERE
setwd("/Users/nathanbush/Documents/GitHub/Data Analytics Term Project/Master Code")

#Will need to Change Loaction of these files below to your WD


energy = read_csv("/Users/nathanbush/Documents/GitHub/Data Analytics Term Project/Raw Data/Energy Data.csv")
president = read_csv("/Users/nathanbush/Documents/GitHub/Data Analytics Term Project/Raw Data/President Data (MIT).csv")



energy_clean = energy[-c(1:9), ]  #remove rows 1-9
colnames(energy_clean) = energy_clean[1,] #set column names and remove corresponding row
energy_clean = energy_clean[-c(1), ]  #remove row corresponding to variable names

#Remove data that is unnecessary for our analysis 
energy_clean = energy_clean[-c(1:2),] 
energy_clean = energy_clean[-c(3),] 
energy_clean = energy_clean[-c(4:5),] 
energy_clean = energy_clean[-c(6),] 
energy_clean = energy_clean[-c(8:34),]
energy_clean = energy_clean[-c(8:11),] 
energy_clean = energy_clean[-c(9),] 
energy_clean = energy_clean[-c(14:27),] 
energy_clean = energy_clean[-c(15),] 
energy_clean = energy_clean[-c(16:51),] 
energy_clean = energy_clean[-c(20:21),] 
energy_clean = energy_clean[-c(21:22),] 
energy_clean = energy_clean[-c(22:25),] 

#Create a state vector including US and DC and another vector with just the fifty states
state = c("US","AL","AK","AZ", "AR" ,"CA","CO","CT","DE","DC","FL","GA","HI","ID","IL","IN","IA","KS","KY","LA","ME","MD","MA","MI","MN","MS","MO","MT","NE","NV","NH","NJ","NM.","NY","NC","ND","OH","OK","OR","PA","RI","SC","SD","TN","TX","UT","VT","VA","WA","WV","WI","WY")
fiftystates = c("AL","AK","AZ", "AR" ,"CA","CO","CT","DE","FL","GA","HI","ID","IL","IN","IA","KS","KY","LA","ME","MD","MA","MI","MN","MS","MO","MT","NE","NV","NH","NJ","NM.","NY","NC","ND","OH","OK","OR","PA","RI","SC","SD","TN","TX","UT","VT","VA","WA","WV","WI","WY")

#Transform matrix from long to wide format
transpose = t(energy_clean)
#Make the matrix a dataframe
transpose = as.data.frame(transpose)
#Set the column names
colnames(transpose) = transpose[1,]
#remove 1st and second row
transpose = transpose[-c(1:2),] 
#Add state vector as a variable
transpose_1 = cbind(transpose, state)
transpose_1 = as.data.frame(transpose_1)

#reset row names
rownames(transpose_1) = NULL
#reaarange data so state variable is in column 1
energy_clean_2 = transpose_1[,c(22,2:21)]

#Change missing values to N/A
energy_clean_3 = na_if(energy_clean_2,"--")
energy_clean_3 = na_if(energy_clean_3,"NM")

#Change * to 0 (These are values less than .5 that were rounded to 0 in dataset)
energy_clean_3[energy_clean_3 == "*"] <- "0"

#Remove US and DC data  
energy_clean_3 = energy_clean_3[-c(1), ]
energy_clean_final = energy_clean_3[-c(9), ]

#Reset row names again...not sure why I have to do this again
rownames(energy_clean_final) = NULL

#Since, I had to transform NM values, to NM. I need to rename New Mexico as "NM"
energy_clean_final[energy_clean_final == "NM."] = "NM"

#Remove columns with no observations
energy_clean_final = energy_clean_final[,-c(12)] 
energy_clean_final = energy_clean_final[,-c(18:19)] 
energy_clean_final = energy_clean_final[,-c(14)] 


###Clean Presidential Data####



#filter by year(2020)
president_1 = president |> 
  filter(year == 2020) |>
  group_by(candidate)

#Check to see if all voting methods are the same, they aren't cannot delete
unique(president_1$mode)

#Delete unnecessary columns
president_1 = president_1[,-c(2) ]
president_1 = president_1[,-c(4:5) ]
president_1 = president_1[,-c(8) ]

#Filter out parties other than REP & DEM
president_2 = president_1 |>
  filter(party == "REPUBLICAN" | party == "DEMOCRAT")

#Group data appropriately
president_3 = president_2 |>
  group_by(state_po, candidate) |>
  mutate(sum_state = sum(candidatevotes))

#Now we have a column with the total number of votes for each candidate in each state,
#and we can begin to clean the data of county information
president_4 = president_3[,-c(1) ]
president_4 = president_4[,-c(4) ]
president_4 = president_4[,-c(2) ]
president_4 = president_4[,-c(3:5) ]

#Now, we can keep only the unique rows in our dataest.
president_5 = distinct(president_4)
#Create dummy for who each state voted for
won = won = c(0,1,0,1,1,0,0,1,1,0,1,0,1,0,1,0,1,0,0,1,1,0,1,0,0,1,1,0,0,1,0,1,0,1,0,1,0,1,1,0,1,0,1,0,0,1,0,1,1,0,1,0,1,0,1,0,1,0,0,1,0,1,1,0,1,0,1,0,0,1,0,1,1,0,0,1,1,0,1,0,0,1,0,1,0,1,0,1,0,1,0,1,0,1,1,0,1,0,1,0,0,1)

#Append the datasets and remove columns that do not correspond to the winning candidate 
president_6 = cbind(president_5, won)
president_7 = filter(president_6, ...4 == "1")

#Remove unnecessary columns
president_final = president_7[,-c(3:4)]#Remove DC from the observations
president_final = president_final[-c(9),]

#Presidential data is now clean and ready to merge


###Governors Data####
#Webscrape a table showing State Governors and their party
url = "https://en.wikipedia.org/wiki/List_of_current_United_States_governors"
page = read_html(url)
page
tables = page|> html_elements("table")
tables
governors = tables[[2]] |> html_table()

#Make table a dataframe
governors = as.data.frame(governors)

#Remove unnecessary columns
governors = governors[,-c(2,4,6:10)] 
governors = governors[,-c(4)] 

#Set row 1 as variable names,
colnames(governors) = governors[1,]

#Remove row 1
governors = governors[-c(1),] 
#Remove column for name of governor
governors = governors[,-c(2)]

#Check variables for uniqueness
unique(governors$Party)

#We can see that there are two observations that don't make sense
governors[governors == "Republican[note 1]"] = "Republican"
governors[governors == "Democratic–Farmer–Labor"] = "Democratic"
unique(governors$Party)

#Create dummy variable for republican governor
governors = governors |>
  mutate(rep = ifelse(Party == "Republican", 1, 0))

#Add our previously created fiftystates vector
governors_new = cbind(governors,fiftystates)
governors_final = governors_new[,-c(1:2)] 
names(governors_final)[names(governors_final) == "fiftystates"] = "state"

#Change name of 
governors_final[governors_final == "NM."] = "NM"

###BIG DATASET WORK####

#Make the  names of the Unique ID column, states, the same in each dataset
names(president_final)[names(president_final) == "state_po"] <- "state"

#Join all three datasets together using left join
joined = left_join(energy_clean_final, president_final, by = "state")
joined_2 = left_join(joined, governors_final, by = "state" )

#Change name of rep dummy variable in dataset
names(joined_2)[names(joined_2) == "rep"] <- "republican governor"
joined_2 = joined_2 |>
  mutate(trump_vote = ifelse(candidate == "DONALD J TRUMP", 1, 0))
dataset_final = joined_2[,-c(18)]

#Rename variables in dataset to names easier to work with.
names(dataset_final)[names(dataset_final) == "Gross Domestic Product ($ billion)"] <- "GDP"
names(dataset_final)[names(dataset_final) == "Renewables (%)"] <- "Renewables"
names(dataset_final)[names(dataset_final) == "Motor Gasoline (Excludes Pipelines) (thousand barrels)"] <- "Motor Gas"
names(dataset_final)[names(dataset_final) == "Small-Scale Solar Photovoltaic Generation (thousand MWh)"] <- "Solar"

#Create a dummy for red and blue states respectively
dataset_final = dataset_final |>
  mutate(red_red = ifelse(trump_vote == 1 & `republican governor` == 1, 1, 0))

dataset_final = dataset_final |>
  mutate(blue_blue = ifelse(trump_vote == 0 & `republican governor` == 0, 1, 0))


###Graphs####
library(ggplot2)

#So, I have a left quite a view variables in the dataset, but the main relationship we are
#concerned with is renewable energy percentage and whether or not there is trend based upon
#the states political climate

#set variables to numeric
dataset_final$"Renewables" =  as.numeric(dataset_final$"Renewables")
hist(dataset_final$Renewables)
dataset_final$`Total Expenditures ($ million)` =  as.numeric(dataset_final$"Total Expenditures ($ million)")
hist(dataset_final$`Total Expenditures ($ million)`)
dataset_final$GDP =  as.numeric(dataset_final$GDP)

#Create seperate variables for blue, red, and mixed states based on pres vote and gov
dataset_final_red = dataset_final |> filter(red_red == 1)
dataset_final_blue = dataset_final |> filter(blue_blue == 1) 
dataset_final_mixed = dataset_final |> filter(blue_blue == 0 & red_red ==0)

#Take mean Renewable energy % in each of these groups
dataset_final_new = dataset_final |>
  mutate(red_renewable = as.vector(mean(dataset_final_red$Renewables)))

dataset_final_new = dataset_final_new |>
  mutate(blue_renewable = as.vector(mean(dataset_final_blue$Renewables)))

dataset_final_new = dataset_final_new |>
  mutate(mix_renewable = as.vector(mean(dataset_final_mixed$Renewables)))
#Create Group Mean Column based upon states with 2 votes red, 2 votes blue, and 1 of each
dataset_final_new = dataset_final_new |>
  mutate(group_avg = ifelse(red_red ==1, 27.78,
                            ifelse(blue_blue ==1, 30.30,
                                   ifelse(blue_blue==0 & red_red ==0,22.28,0))))


#Create categorical column for barchart
dataset_final_new = dataset_final_new |>
  mutate(state_category = ifelse(group_avg ==27.78, "Red",
                                 ifelse(group_avg ==30.30, "Blue",
                                        ifelse(group_avg==22.28, "Purple", 0))))

#Create Barplot Data
barplot_data = dataset_final_new[,-c(1:24)]
barplot_data = barplot_data[-c(1:2),]
barplot_data = barplot_data[-c(4:48),]


#Create Graph showing relationship between renewables % and states
graph_1 = ggplot(barplot_data, aes(x=state_category, y=group_avg, fill=state_category)) +
  geom_bar(stat = "identity") + labs(y= "% Renewable Energy" , x = "State Category")
color <- data.frame(state_cat=c("Purple","Red","Blue"),
                  renew=c(22.28,27.78,30.30))
graph_1_color <- ggplot(data=color, aes(x=state_cat, y=renew,fill=state_cat))+
  geom_bar(stat="identity")+
  scale_fill_manual(values=c("blue",
                             "purple",
                             "red")) + labs(y= "% Renewable Energy" , x = "State Category") + 
  theme(legend.position = "none") + geom_col(colour = "black") +  geom_label(aes(label = renew), vjust = 1.5, colour = "black", fill = "white") +
  ylim(0,33) +labs(title = "Electricity Generation by State Political Category")
graph_1_color


#Rename variables in dataset to be easier to work with
names(dataset_final_new)[names(dataset_final_new) == "Precipitation (inches)"] <- "precip"
names(dataset_final_new)[names(dataset_final_new) == "Average Temperature (degrees Fahrenheit)"] <- "avg_temp"


#Create a 2nd graph comparing rain and solar energy, I removed California in this graph because
#it was a significant outlier.

graph_2.1 = ggplot(data = dataset_final_new, aes(x = as.numeric(as.character(Solar)), y = as.numeric(as.character(precip)), col = state_category)) +
  geom_point() + scale_color_manual(values=c("blue", "purple", "red")) + 
  xlim(0,100)

graph_2.1

#Generate a 3rd graph
graph_3 = ggplot(data = dataset_final_new, aes(x = state, y = Renewables, col = state_category, label=state)) +
  geom_point() + scale_color_manual(values=c("blue", "purple", "red")) +geom_text(hjust=0, vjust=-0.5, size = 3) + theme(axis.text.x=element_blank(),
  axis.ticks.x=element_blank()) + xlab("State") + ylab("Renewable Energy (% electricity generation)")

  graph_3

#Cleveland Dot Plot for graph 3
dot_plot =  ggplot(dataset_final_new, aes(x = Renewables, y = reorder (state, Renewables), col = state_category, label = state)) +
    geom_point() + scale_color_manual(values=c("blue", "purple", "red")) +
  ylab("State") + xlab("Renewable Energy (% electricity generation)") +
  theme(legend.position = "none")
dot_plot
  
political_cat <- dataset_final_new$state[order(dataset_final_new$state_category, dataset_final_new$Renewables)]
dataset_final_new$state <- factor(dataset_final_new$state, levels = political_cat)
  
dot_plot_2 = ggplot(dataset_final_new, aes(x = Renewables, y = reorder (state, Renewables))) +
  geom_point(size = 3, aes(colour = state_category)) +
  scale_color_manual(values=c("blue", "purple", "red"), limits = c("Blue","Purple","Red")) +
  facet_grid(state_category ~ ., scales = "free_y", space = "free_y") +
  theme(legend.position = "none") +  
  ylab("State") + xlab("Renewable Energy (% Electricity Generation)")

dot_plot_2

  
#Create variables that are divided by state level GDP to control for differences in state size
dataset_final_new$Solar = as.numeric(dataset_final$Solar)
final_dataset = dataset_final_new |> mutate(new_solar = Solar/GDP)
final_dataset$new_solar = as.numeric(final_dataset$new_solar)
final_dataset$precip = as.numeric(final_dataset$precip)

#Generate a 4th graph
graph_4 = ggplot(data = final_dataset, aes(x = new_solar, y = precip, col = state_category)) +
  geom_point() + scale_color_manual(values=c("blue", "purple", "red")) + xlim(0,1)

#Compare graphs 2.1 and 4
graph_4
graph_2.1
grid.arrange(graph_4, graph_2.1, ncol=2)
#Adjust variable names to be easier to work with
names(dataset_final_new)[names(dataset_final_new) == "Average Temperature (degrees Fahrenheit)"] <- "avg_temp"

#Create a 5th graph 
final_dataset[11,6] = 75
#Above I added average temperature in Hawaii from NOAA since it was only missing value in my data. Not the best practice but important in this context. 
graph_5.0 = ggplot(data = final_dataset, aes(x = new_solar, y = avg_temp, col = state_category, label = state, group = state_category)) +
  geom_point() + scale_color_manual(values=c("blue", "purple", "red")) + 
  geom_text(hjust=1, vjust=1.5, size = 3) + 
  ylab("Average Temperature") + 
  xlab("Solar Energy") + geom_smooth(method="lm", se = FALSE)
graph_5.0

#Change variable names and set as variables as numeric to work with them
names(final_dataset)[names(final_dataset) == "Carbon Dioxide (thousand metric tons)"] <- "co2_emissions"
final_dataset$co2_emissions =  as.numeric(final_dataset$co2_emissions)
names(final_dataset)[names(final_dataset) == "Civilian Labor Force (million)"] <- "labor_force"
final_dataset$labor_force =  as.numeric(final_dataset$labor_force)

#Create a 6th graph...see that Texas is by far the biggest polluter in the US
graph_6 = ggplot(data = final_dataset, aes(x = Renewables, y = co2_emissions, col = state_category, label = state, group = state_category)) +
  geom_point() + scale_color_manual(values=c("blue", "purple", "red")) + 
  geom_text(hjust=1, vjust=1.5, size = 3) + 
  ylab("CO2 Emissions") + 
  xlab("Renewable Energy %") 
graph_6

#If we adjust new graph for CO2 divided by state population below
final_dataset = final_dataset |> mutate(co2_pop = co2_emissions/labor_force)

#Create new graph.. see that there are 3 major polluters by capita in US
graph_6.0 = ggplot(data = final_dataset, aes(x = Renewables, y = co2_pop, col = state_category, label = state, group = state_category)) +
  geom_point() + scale_color_manual(values=c("blue", "purple", "red")) + 
  geom_text(hjust=1, vjust=1.5, size = 3) + 
  ylab("CO2 Emissions") + 
  xlab("Renewable Energy %") 

graph_6.0

#Adjust view on graph to see CO2 emissions and renewable energy percentage relationship in 47 states
graph_6.0.1 = ggplot(data = final_dataset, aes(x = Renewables, y = co2_pop, col = state_category, label = state, group = state_category)) +
  geom_point() + scale_color_manual(values=c("blue", "purple", "red")) + 
  geom_text(hjust=1, vjust=1.5, size = 3) + 
  ylab("CO2 Emissions") + 
  xlab("Renewable Energy %") +ylim(0,30000) +
  geom_smooth(method = "lm", se = FALSE)

graph_6.0.1

###Maps####
library(tigris)
library(tmap)
library(sf)

#Create Map showing political affiliation
states <- states(cb = TRUE)
names(states)[names(states) == "STUSPS"] <- "state"
map_1 <- left_join(final_dataset, states, by = "state")
map_1 <- st_as_sf(map_1)
final_map = map_1[-c(2), ]
final_map = final_map[-c(10), ]
affiliation_map <- tm_shape(final_map)+tm_polygons(col ="state_category", palette = c(Blue = "blue", Red = "red", Purple = "purple"), title = "Political Category") 
affiliation_map

#Create map showing renewable energy percentage

energy_map = tm_shape(final_map)+tm_polygons(col ="Renewables", palette =c("Greens"), title = "Renewable Energy (%)")
energy_map



#Save dataset 
save(final_dataset, file = "final_dataset.Rdata")
save(final_dataset, file = "final_dataset.Rdata")
save(color, file = "color.Rdata")
save(states, file = "states.Rdata")
save(map_1, file = "map_1.Rdata")
save(final_map, file = "final_map.Rdata")
save(dataset_final_new, file = "dataset_final_new.Rdata")

###Regressions####
library(estimatr)
library(fixest)
library(modelsummary)
final_dataset$GDP =  as.numeric(final_dataset$GDP)
final_dataset$avg_temp =  as.numeric(final_dataset$avg_temp)
final_dataset$Renewables =  as.numeric(final_dataset$Renewables)
final_dataset$precip =  as.numeric(dataset_final_new$precip)
final_dataset <- rename(final_dataset, rep_gov = "republican governor")
save(final_dataset, file = "final_dataset.Rdata")
                    
library
reg_1 = feols(Renewables ~ avg_temp + GDP + precip, data = final_dataset)
summary(reg_1)

reg_2 = feols(Renewables ~ avg_temp + GDP + precip + state_category, data = final_dataset)
summary(reg_2)

reg_3 = feols(Renewables ~ avg_temp + GDP + precip + co2_pop + trump_vote * rep_gov, data = final_dataset)
summary(reg_3)
summary(reg_3) |> tidy()

reg_4 = feols(co2_pop ~ avg_temp + GDP + precip + Renewables + trump_vote * rep_gov, data = final_dataset)
summary(reg_4)
summary(reg_4) |> tidy()

etable(reg_1,reg_2)

