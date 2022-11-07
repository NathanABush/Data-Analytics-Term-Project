election_2020 = read_csv("/Users/nathanbush/Desktop/Data Analytics/Term Project/2020 election results by county.csv")
energy = read_csv("/Users/nathanbush/Desktop/Data Analytics/Term Project/Energy Data.csv")
president = read_csv("/Users/nathanbush/Desktop/Data Analytics/Term Project/President Data (MIT).csv")
setwd("/Users/nathanbush/Documents/GitHub/Data-Analytics-Term-Project")

### Clean Energy Data #### 

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

#This data is now in the format I need to be able to use it, and cleaned sufficiently. 


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

president_2 = president_1 |>
  filter(party == "REPUBLICAN" | party == "DEMOCRAT")

president_3 = president_2 |>
  group_by(state_po, candidate) |>
  mutate(sum_state = sum(candidatevotes))

#Now, we have a column with the total number of votes for each candidate in each state,
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
president_final = president_7[,-c(3:4)]

#Remove DC from the observations
president_final = president_final[-c(9),]

#Presidential data is clean and ready to merge

###Clean Election Data####
library(tinytex)
library(rvest)

election = election_2020 |> 
  filter(won ==TRUE)

sum(election$state == "Delaware")
#Wow, at first I thought there was a problem with this dataset since Delaware had just 3 counties
#listed, but it turns out they actually only have 3 counties. 


#Create a new dataset that sums the amount of votes of candidate that get above 1000 votes recieved
new_election = election_2020 %>% 
  group_by(state, candidate) %>% 
  summarise(Total = sum(total_votes, na.rm = TRUE))

new_election = new_election |> 
  filter(candidate == "Joe Biden" | candidate == "Donald Trump")


#Now, we have a dataset showing the number of votes by state for Biden and trump in 2020,
#and we can use that to determine who each state voted for 

#Election data is clean and ready to merge

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


dataset_final = dataset_final |>
  mutate(red_red = ifelse(trump_vote == 1 & `republican governor` == 1, 1, 0))

dataset_final = dataset_final |>
  mutate(blue_blue = ifelse(trump_vote == 0 & `republican governor` == 0, 1, 0))


###Graphs####

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


#Show distribution of Renewable energy % in dataset
hist(dataset_final$Renewables)
hist(log(dataset_final$Renewables))

library(ggplot2)





#Graph showing relationship between renewables % and states

ggplot(dataset_final_new, aes(x=state_category, y=group_avg)) +
  geom_bar(stat = "identity") 
  
#Based on the above graph, we can see that actually, there doesn't appear to be much of an impact
#of being a red state on the amount of energy consumption
  
  
