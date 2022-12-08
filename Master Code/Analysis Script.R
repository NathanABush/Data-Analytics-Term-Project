                            #DATA ANALYSIS SCRIPT#
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

###BARCHART####

#set variables to numeric
dataset_final$"Renewables" =  as.numeric(dataset_final$"Renewables")
hist(dataset_final$Renewables)
dataset_final$`Total Expenditures ($ million)` =  as.numeric(dataset_final$"Total Expenditures ($ million)")
hist(dataset_final$`Total Expenditures ($ million)`)
dataset_final$GDP =  as.numeric(dataset_final$GDP)

#Create separate variables for blue, red, and mixed states based on pres vote and gov
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

###Graph 2####
#Rename variables in dataset to be easier to work with
names(dataset_final_new)[names(dataset_final_new) == "Precipitation (inches)"] <- "precip"
names(dataset_final_new)[names(dataset_final_new) == "Average Temperature (degrees Fahrenheit)"] <- "avg_temp"


#Create a 2nd graph comparing rain and solar energy, I removed California in this graph because
#it was a significant outlier.

graph_2.1 = ggplot(data = dataset_final_new, aes(x = as.numeric(as.character(Solar)), y = as.numeric(as.character(precip)), col = state_category)) +
  geom_point() + scale_color_manual(values=c("blue", "purple", "red")) + 
  xlim(0,100)

graph_2.1

#GRAPH 3####
#Generate a 3rd graph
graph_3 = ggplot(data = dataset_final_new, aes(x = state, y = Renewables, col = state_category, label=state)) +
  geom_point() + scale_color_manual(values=c("blue", "purple", "red")) +geom_text(hjust=0, vjust=-0.5, size = 3) + theme(axis.text.x=element_blank(),
                                                                                                                         axis.ticks.x=element_blank()) + xlab("State") + ylab("Renewable Energy (% electricity generation)")

graph_3


###DOTPLOTS####
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

###GRAPH 4####
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

#Adjust variable names to be easier to work with
names(dataset_final_new)[names(dataset_final_new) == "Average Temperature (degrees Fahrenheit)"] <- "avg_temp"

###GRAPH 5####
#Create a 5th graph (removed outlier of Alaska here...no data available)
graph_5.0 = final_dataset |>
  filter(state != "AK") |>
  ggplot(aes(x = new_solar, y = avg_temp, col = state_category, label = state, group = state_category)) +
  geom_point() + scale_color_manual(values=c("blue", "purple", "red")) + 
  geom_text(hjust=1, vjust=1.5, size = 3) + 
  ylab("Average Temperature") + 
  xlab("Solar Energy") + geom_smooth(method="lm", se = FALSE)
graph_5.0

#Log Solar variable for analysis purposes
final_dataset = final_dataset |>
  mutate(log_solar = log(Solar))

#New graph with log of solar
graph_5.1 = final_dataset |>
  filter(state != "AK") |>
  ggplot(aes(x = log_solar, y = avg_temp, col = state_category, label = state, group = state_category)) +
  geom_point() + scale_color_manual(values=c("blue", "purple", "red")) + 
  geom_text(hjust=0, vjust=-.75, size = 3) + 
  ylab("Average Temperature") + 
  xlab("Solar Energy") + geom_smooth(method="lm", se = FALSE)
graph_5.1



###GRAPHS 6, 6.0, 6.1####
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
graph_6.1 = ggplot(data = final_dataset, aes(x = Renewables, y = co2_pop, col = state_category, label = state, group = state_category)) +
  geom_point() + scale_color_manual(values=c("blue", "purple", "red")) + 
  geom_text(hjust=1, vjust=1.5, size = 3) + 
  ylab("CO2 Emissions") + 
  xlab("Renewable Energy %") +ylim(0,30000) +
  geom_smooth(method = "lm", se = FALSE)

graph_6.1

###Maps####
library(tigris)
library(raster)
library(tmap)
library(sf)

#Create Map showing political affiliation
states <- states(cb = TRUE)
names(states)[names(states) == "STUSPS"] <- "state"
map_1 <- left_join(final_dataset, states, by = "state")
map_1 <- st_as_sf(map_1)

final_map = map_1[-c(2), ]
final_map = final_map[-c(10), ]

hawaii_AK = map_1[-c(12:50), ]
hawaii_AK = hawaii_AK[-c(3:10),]
hawaii_AK = hawaii_AK[-c(1),]

affiliation_map <- tm_shape(final_map)+tm_polygons(col ="state_category", palette = c(Blue = "blue", Red = "red", Purple = "purple"), title = "Political Category") 
affiliation_map

outlier_map <- tm_shape(hawaii_AK)+tm_polygons(col ="Renewables", palette =c("Greens"), title = "Renewable Energy (%)") 
outlier_map

#Create map showing renewable energy percentage

energy_map = tm_shape(final_map)+tm_polygons(col ="Renewables", palette =c("Greens"), title = "Renewable Energy (%)")
energy_map





###Data to Save for Markdown####
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