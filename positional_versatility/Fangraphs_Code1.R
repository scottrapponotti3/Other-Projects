library(XML)
library(dplyr)
#scrape the HTML file of the Cubs 2016 schedule from baseball refrence
data = readHTMLTable("http://www.baseball-reference.com/teams/CHC/2016-schedule-scores.shtml")
d = data.frame(data[6])
d = d[,c(3, 5, 6, 7)] #takes the column of the date, the Cubs id, whether the game was home or away, and the opponent id
colnames(d)=c("Date", "Cubs", "HA", "Opp") #renames the corresponding columns
d$Date = as.character(d$Date)
#Transform the date string from form of Day, Month (abr) Day into a numeric form. This is applied to every row of data frame
#Ex: Monday, Apr 4 becomes 0404.
#This is done because the date is an input for the web scraper function that will be created
d$Date = mapply(function(x){strsplit(x, ", ")[[1]][2]}, d$Date)
months = mapply(function(x){strsplit(x. " ")[[1]][1]}, d$Date) %>% match(months,month.abb)
months = mapply(function(x){ ifelse(as.numeric(x) < 10, paste("0", x, sep=""), x)}, months)
days = mapply(function(x){strsplit(x, " ")[[1]][2]}, d$Date) %>% mapply(function(x){ifelse(as.numeric(x) < 10, paste("0", x, sep=""), x)}, days)
d$Date = paste(months, days, sep="")
d$Cubs = as.character(d$Cubs)
d$HA = as.character(d$HA)
d$Opp = as.character(d$Opp
d$Home = ifelse(d$HA == "", TRUE, FALSE) #This checks if the game is home or away. Games with a "@" are away, "" are home.

#Need to convert the game ids of certain away teams. This is done because they are inputs to the web scraper.
d$Opp[d$Opp == "LAA"] = "ANA"
d$Opp[d$Opp == "STL"] = "SLN"
d$Opp[d$Opp == "SFG"] = "SFN"
d$Opp[d$Opp == "WSN"] = "WAS"
d$Opp[d$Opp == "NYM"] = "NYN"
d$Opp[d$Opp == "CHW"] = "CHA"
d$Opp[d$Opp == "SDP"] = "SDN"
d$Opp[d$Opp == "LAD"] = "LAN"

#This web scraping function finds all the games that a specific Cubs player played in and returns the boxscore of the team of the player in that game
#Note that this function only applies for games during the 2016 season
#Inputs:
#	player is a string value of the full name of that player
#	id is a string value the opponent id of the team the Cubs are playing
#	date is a string of the month and day of the game in the numeric form "MMDD". Ex: "0404" is April 4th.
#	home is a logical of whether the Cubs played the game at home or on the road
#Output:
#	A list of boxscores 
Player_boxscores = function(player, position, id, date, home) {
	if (home) {
		id = "CHN"
	} 
	game = readHTMLTable(paste("http://www.baseball-reference.com/boxes/",id,"/",id,"2016",date,"0.shtml",sep=""), stringsAsFactors = FALSE)
	Bats = data.frame(game$ChicagoCubsbatting)
	if (length(grep(pattern = paste(player, "  ", position, sep = ""), x = Bats$Batting)) == 0) {
		return (Bats)
	} else if (length(grep(pattern = paste(player, "  ", position, "-", sep = ""), x = Bats$Batting)) != 0) {
		return (Bats)
	}
}

#This function takes the output of Player_boxscores and finds the contributions of all the players that either started at the player's
#main position or came in as a replacement to that player's main position
#Inputs:
#	A dataframe of a boxscore
#	A string of the player's name
#	A string of that player's primary position
#Outputs:
#	A data frame of the sub players who played the player's main position	
Player_performance = function(Bats, player, pos) {
	Batters = Bats$Batting
	index = grep(pattern = player, x = Batters)
	start_position = substring(Batters[index], 14, 15)
	if (start_position == pos & nchar(Batters[index]) > 15) {
		players_sub = subset(Bats, grepl(pattern=pos, x = Batters) & !grepl(pattern=player,x=Batting))
		players_sub$position = start_position
	} else if (start_position != pos) {
		players_sub = subset(subset(game8Cubs, grepl(pattern=pos, x = game8Cubs$Batting) & !grepl(pattern=player,x=game8Cubs$Batting)))
		players_sub$position = start_position
	} else {
		players_sub = NULL
	}
	return (players_sub)
}
