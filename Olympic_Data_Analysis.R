#drive link for csv file
#https://drive.google.com/open?id=1hhTQb6hlRGCsW4m3xBRb74pmOXqMEJW_

library(ggplot2)
library(dplyr)
library(DT)
library(plyr)
library(rworldmap)
library(repr)
library(readr)

raw_data = read.csv("C:/Users/Gowtham R/Documents/Sem 4/MC/athlete_events.csv")

#ANALYSIS FOR VOLLEYBALL
volley_data = filter(raw_data, Sport == "Volleyball")

#overall analysis

unique_players = volley_data[!duplicated(volley_data["ID"]),]

#all player appearance count
player_wise_overall = volley_data %>% group_by(ID,Name)
summary_player_wise = player_wise_overall %>% dplyr::summarise(Appearances = n())
datatable(summary_player_wise)#data table is very large

#number of players per team per year
team_and_year_wise = volley_data %>% group_by(Team, Year)
datatable(team_and_year_wise %>% dplyr::summarise(number_of_players = n()))

#medal count per team
medal_data = filter(volley_data, Medal == 'Gold' | Medal == 'Silver' | Medal=='Bronze')
country_medals = medal_data %>% group_by(Team, Year)
country_medal_count_data = dplyr::summarise(country_medals)
#table with what all years a country has a won a medal
datatable(country_medal_count_data)

country_medal_count = country_medal_count_data %>% group_by(Team)
medals_count_final = country_medal_count %>% dplyr::summarise(Medal_Count = n())
#table with medal count
datatable(medals_count_final)
#graph of medal count
ggplot(medals_count_final, aes(x=Team, y=Medal_Count)) + geom_col() + ggtitle("Country Medal Count ")+ theme(axis.text.x = element_text(angle = 90,hjust=1, vjust=0.2,size=10))
#world map showing medal count
options(repr.plot.width = 10,repr.plot.height = 12)
sPDF <- joinCountryData2Map(medals_count_final,joinCode = "NAME",nameJoinColumn = "Team")
mapCountryData(sPDF,nameColumnToPlot = 'Medal_Count', catMethod = "fixedWidth", colourPalette = "rainbow")

#gold medals per country
gold_data = filter(medal_data, Medal =='Gold')
unique_golds = gold_data %>% group_by(Team, Year)
unique_golds_count = dplyr::summarise(unique_golds)
#table to see which years has a team won a gold medal
datatable(unique_golds_count)

country_gold_count = unique_golds_count %>% group_by(Team)
gold_count_final = country_gold_count %>% dplyr::summarise(Gold_Count = n())
datatable(gold_count_final)
ggplot(gold_count_final, aes(x=Team, y=Gold_Count)) + geom_col() + ggtitle("Country Gold Count ")+ theme(axis.text.x = element_text(angle = 90,hjust=1, vjust=0.2,size=10))

#height distribution of volleyball players
ggplot(unique_players, aes(x=Team, y=Height))+geom_boxplot(fill="slateblue", color="Blue", alpha=0.2)+ggtitle("Distribution of Height") + theme(axis.text.x = element_text(angle = 90,hjust=1, vjust=0.2,size=10))

#comparison gender wise
Gender = factor(unique_players$Sex)
barplot(table(Gender), main='Comparison of Genders in Volleyball', col="Blue", density=100, xlab="Gender", ylab="Count")

#players per team
country_wise_data = unique_players %>% group_by(Team)
players_per_team = country_wise_data %>% dplyr::summarise(No_of_players = n())
datatable(players_per_team)

#graph showing number of players per team
#zoom to fullscreen to see all countries
barplot(players_per_team$No_of_players, 
        names.arg = players_per_team$Team ,
        las=2,
        main="Number of Players per Country",
        ylab="Number of Players",
        col="darkred",
        ylim=c(0,max(players_per_team$No_of_players)+10))


#country wise analysis
country = readline(prompt="Enter a country to analyse")
country = "Brazil"
#for example Brazil is taken, but it can be changed

#mens team analysis
country_data_male = volley_data[volley_data$Team == country & volley_data$Sex == 'M',]
year_wise_male = country_data_male[!duplicated(country_data_male['Year']),]
year_wise_male$Medal = factor(year_wise_male$Medal, levels=c(NA,"Bronze","Silver","Gold"),exclude=NULL)

#graph of number of medals (male team of a country)
plot(year_wise_male$Medal,main='Count of Medals')
ggplot(year_wise_male,aes(Year,Medal)) + geom_point()

country_data_female = volley_data[volley_data$Team == "Brazil" & volley_data$Sex == 'F',]
year_wise_female = country_data_female[!duplicated(country_data_female['Year']),]
year_wise_female$Medal = factor(year_wise_female$Medal, levels=c(NA,"Bronze","Silver","Gold"),exclude=NULL)

#graph of number of medals (female team of a country)
plot(year_wise_female$Medal,main='Count of Medals')

#graph of performance throughout the olympics
ggplot(year_wise_female,aes(Year,Medal)) + geom_point()

player_wise_data_male = country_data_male %>% group_by(ID,Name)

#graph of each male player's appearances in the olympics for a country
ggplot(country_data_male, aes(Name)) + geom_bar() + theme(axis.text.x = element_text(angle = 90, hjust = 1, size=6)) + ggtitle("Male player appeareances")
player_appearances_male = player_wise_data_male %>% dplyr::summarise(appearances= n())

#most appearing male players in olympics for a country
datatable(player_appearances_male[order(player_appearances$appearances,decreasing = TRUE),])


player_wise_data_female = country_data_female %>% group_by(ID,Name)

#graph of each female player's appearances in the olympics for a country
ggplot(country_data_female, aes(Name)) + geom_bar() + theme(axis.text.x = element_text(angle = 90, hjust = 1, size=6)) + ggtitle("Female player appeareances")
player_appearances_female = player_wise_data_female %>% dplyr::summarise(appearances= n())

#most appearing female players in olympics for a country
datatable(player_appearances_female[order(player_appearances_female$appearances,decreasing = TRUE),])

#comparison of appearances by gender in a country
barplot(c(nrow(player_appearances_male),nrow(player_appearances_female)), xlab="Gender", ylab="Apperances",main="Comparison Gender Wise", names.arg =c("Male","Female"), col="darkred")

#comparison of number of medals by gender in a country
male_medals = nrow(filter(year_wise_male, Medal=="Gold" | Medal == "Silver" | Medal=="Bronze"))
female_medals = nrow(filter(year_wise_female, Medal=="Gold" | Medal == "Silver" | Medal=="Bronze"))
barplot(c(male_medals,female_medals), main="Medals Gender Wise", xlab="Gender", ylab="Medal Count",)


#ANALYSIS FOR BADMINTON
#VARIABLE NAMES ARE REPEATED...SO THEY WILL HAVE TO BE RUN CAREFULLY

Badminton_data = filter(raw_data, Sport == 'Badminton')

unique_players = Badminton_data[!duplicated(Badminton_data["ID"]),]

#players appearance count
player_wise_overall = Badminton_data %>% group_by(ID,Name)
summary_player_wise = player_wise_overall %>% dplyr::summarise(Appearances = n())
datatable(summary_player_wise)#data table is very large

#medal count per team
medal_data = filter(Badminton_data, Medal == 'Gold' | Medal == 'Silver' | Medal=='Bronze')
country_medals = medal_data %>% group_by(Team, Year)
country_medal_count_data = dplyr::summarise(country_medals)
#table with what all years a country has a won a medal
datatable(country_medal_count_data)

country_medal_count = country_medal_count_data %>% group_by(Team)
medals_count_final = country_medal_count %>% dplyr::summarise(Medal_Count = n())
#table with medal count
datatable(medals_count_final)
#graph of medal count
ggplot(medals_count_final, aes(x=Team, y=Medal_Count)) + geom_col() + ggtitle("Country Medal Count ")+ theme(axis.text.x = element_text(angle = 90,hjust=1, vjust=0.2,size=10))
#worldmap showing medal count
options(repr.plot.width = 10,repr.plot.height = 12)
sPDF <- joinCountryData2Map(medals_count_final,joinCode = "NAME",nameJoinColumn = "Team")
mapCountryData(sPDF,nameColumnToPlot = 'Medal_Count', catMethod = "fixedWidth", colourPalette = "rainbow")

#gold medals per country
gold_data = filter(medal_data, Medal =='Gold')
unique_golds = gold_data %>% group_by(Team, Year)
unique_golds_count = dplyr::summarise(unique_golds)
#table to see which years has a team won a gold medal
datatable(unique_golds_count)

country_gold_count = unique_golds_count %>% group_by(Team)
gold_count_final = country_gold_count %>% dplyr::summarise(Gold_Count = n())
datatable(gold_count_final)
ggplot(gold_count_final, aes(x=Team, y=Gold_Count)) + geom_col() + ggtitle("Country Gold Count ")+ theme(axis.text.x = element_text(angle = 90,hjust=1, vjust=0.2,size=10))


#comparison genderwise
Gender = factor(unique_players$Sex)
barplot(table(Gender), main='Comparison of Genders in Badminton', col="Blue", density=100, xlab="Gender", ylab="Count")

#country wise analysis
country = readline(prompt="Enter a country to analyse")
#Taking China as an example
country = "China"

#mens team analysis
country_data_male = Badminton_data[Badminton_data$Team == country & Badminton_data$Sex == 'M',]
year_wise_male = country_data_male[!duplicated(country_data_male['Year']),]
year_wise_male$Medal = factor(year_wise_male$Medal, levels=c(NA,"Bronze","Silver","Gold"),exclude=NULL)

#graph of number of medals (male team of a country)
plot(year_wise_male$Medal,main='Count of Medals')
ggplot(year_wise_male,aes(Year,Medal)) + geom_point()

country_data_female = Badminton_data[Badminton_data$Team == "China" & Badminton_data$Sex == 'F',]
year_wise_female = country_data_female[!duplicated(country_data_female['Year']),]
year_wise_female$Medal = factor(year_wise_female$Medal, levels=c(NA,"Bronze","Silver","Gold"),exclude=NULL)

#graph of number of medals (female team of a country)
plot(year_wise_female$Medal,main='Count of Medals')

#graph of performance throughout the olympics
ggplot(year_wise_female,aes(Year,Medal)) + geom_point()

player_wise_data_male = country_data_male %>% group_by(ID,Name)


#graph of each male player's appearances in the olympics for a country
ggplot(country_data_male, aes(Name)) + geom_bar() + theme(axis.text.x = element_text(angle = 90, hjust = 1, size=6)) + ggtitle("Male player appeareances")
player_appearances_male = player_wise_data_male %>% dplyr::summarise(appearances= n())

#most appearing male players in olympics for a country
datatable(player_appearances_male[order(player_appearances$appearances,decreasing = TRUE),])

player_wise_data_female = country_data_female %>% dplyr::group_by(ID,Name)

#graph of each female player's appearances in the olympics for a country
ggplot(country_data_female, aes(Name)) + geom_bar() + theme(axis.text.x = element_text(angle = 90, hjust = 1, size=6)) + ggtitle("Female player appeareances")
player_appearances_female = player_wise_data_female %>% dplyr::summarise(appearances= n())

#most appearing female players in olympics for a country
datatable(player_appearances_female[order(player_appearances_female$appearances,decreasing = TRUE),])

#comparison of appearances by gender in a country
barplot(c(nrow(player_appearances_male),nrow(player_appearances_female)), xlab="Gender", ylab="Apperances",main="Comparison Gender Wise", names.arg =c("Male","Female"), col="darkred")


#comparison of number of medals by gender in a country
male_medals = nrow(filter(year_wise_male, Medal=="Gold" | Medal == "Silver" | Medal=="Bronze"))
female_medals = nrow(filter(year_wise_female, Medal=="Gold" | Medal == "Silver" | Medal=="Bronze"))
barplot(c(male_medals,female_medals), main="Medals Gender Wise", xlab="Gender", ylab="Medal Count",)

#Players height over time
Badminton_data %>% ggplot(aes(x=as.factor(Year), y=Height, fill=Sex)) +
        geom_boxplot(alpha=0.75) +
        xlab("Olympiad Year") + ylab("Height (cm)") +
        scale_fill_manual(values=c("blue","red"))

#players weight over time
Badminton_data %>% ggplot(aes(x=as.factor(Year), y=Weight, fill=Sex)) +
        geom_boxplot(alpha=0.75) +
        xlab("Olympiad Year") + ylab("Weight (kg)") +
        scale_fill_manual(values=c("blue","red"))





