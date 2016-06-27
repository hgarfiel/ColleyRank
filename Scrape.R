#Hunter Garfield 11.06.15
#This script scrapes seasonal college football game data from Wisconsin's website and
#formats it so that it can be used to easily rank teams for any particular season using the Colley method

dftem = data.frame(matrix(ncol = 6)) #creates a temporary data frame to store unformatted data
colnames(dftem) = c("season","date","awayteam","awayscore","hometeam","homescore")
dftem = dftem[-1,] #deletes"NA" row

colwidths = c(10,29,3,29,3) #defines the column widths of the fwf


url1 = "http://homepages.cae.wisc.edu/~dwilson/rsfc/history/howell/cf"
url2 = "gms.txt"

i = 1960 #initializes a variable to loop through all seasons, starting at 1960

while(i <= 2010) {
  
  i = as.character(i)
  fwf = read.fwf(paste(url1, url2, sep = i), widths = colwidths) #reads fwf, using i (the year) as the seperator
  fwf = as.data.frame(fwf)
  colnames(fwf) = c("date","awayteam","awayscore","hometeam","homescore")
  fwf[,2] = gsub(" ","",fwf[,2]) #subs out spaces in team names to make comparing easier
  fwf[,4] = gsub(" ","", fwf[,4])
  fwf = cbind(season=NA, fwf) #add season column to the fwf
  i = as.integer(i)
  fwf$season = i #assigns a season to the newly imported year of data
  
  j=1 #loops through the data pulled from the website to check if a team played less than six games
  while(j<=nrow(fwf)) {
    if(length(c((which(fwf$awayteam[j]==fwf$awayteam)),which(fwf$awayteam[j]==fwf$hometeam)))<6) { #checks if any team in the "awayteam" column played less than six games total
       fwf = fwf[-1*(c(which(fwf$awayteam[j]==fwf$awayteam),which(fwf$awayteam[j]==fwf$hometeam))),] #deletes all instances of "awayteam"'s name, ensuring games played by others against that team are also deleted
       j=j-1 #makes sure the loop doesn't skip a row if a one gets deleted
    }
    else if(length(c((which(fwf$hometeam[j]==fwf$awayteam)),which(fwf$hometeam[j]==fwf$hometeam)))<6){ #checks if any team in the "hometeam" column played less than six games total
      fwf = fwf[-1*(c(which(fwf$hometeam[j]==fwf$awayteam),which(fwf$hometeam[j]==fwf$hometeam))),] #deletes all instances of "hometeam"'s name, ensuring games played by others against that team are also deleted
      j=j-1
    }
    j=j+1
  }
   
  j=1 #loops through the yearly data to find instances of ties
  while(j <= nrow(fwf)) {
    if(fwf$awayscore[j] == fwf$homescore[j]) {
      fwf = fwf[-j,] #deletes rows where ties occured
    }
    j=j+1
  }
  
  dftem = rbind(dftem, fwf) #bind data for the year into a larger data frame with data for all years
  
  i=i+1
}

#below creates a data frame to store data in a format that makes calculating Colley rankings easier
df = data.frame(Team = NA, Wins = NA, Losses= NA, Opponents = NA, Season = NA, SeasonID = NA)

i=1960 #this will loop through each season in the temporary data frame computed above and format the data in a way that makes it easier to use
while(i<=2010) {
  temp = dftem[dftem$season == i,] #creates a dataframe of data by season
  
  j=1 #loops through the seasonal data frame created above, and creates columns for winners and losers
  while(j<=nrow(temp)) {
    if(temp$awayscore[j]>temp$homescore[j]){
      temp$winner[j] = temp$awayteam[j]
      temp$loser[j] = temp$hometeam[j]
    }
    else if(temp$awayscore[j] < temp$homescore[j]) {
      temp$winner[j] = temp$hometeam[j]
      temp$loser[j] = temp$awayteam[j]
    }
    j=j+1
  }
  
  allteams=data.frame(temp$awayteam,temp$hometeam) 
  allteams = c(t(allteams)) #creates a vector with all team names for the season so that unique ones may be pulled out
  teams = unique(allteams) #vector of unique team names
  
  tmyr = data.frame(matrix(nrow = length(teams),ncol = 6)) #creates "teamyear" style data frame for the season
  colnames(tmyr) = c("Team", "Wins", "Losses", "Opponents", "Season", "SeasonID")
  
  tmyr$Team = teams
  tmyr$Season = i
  tmyr$SeasonID = 1:length(teams) #Team ID numbers will be given seasonally rather than having one for each team over all 50 years
  
  
  j=1 #loops through teamyear data frame to fill in wins, losses, and list of Opponents
  while(j<=nrow(tmyr)){
    tmyr$Wins[j] = length(which(temp$winner==tmyr$Team[j]))
    tmyr$Losses[j] = length(which(temp$loser==tmyr$Team[j]))
    tmyr$Opponents[j] = list(c(temp$winner[which(temp$loser==tmyr$Team[j])],temp$loser[which(temp$winner==tmyr$Team[j])])) 
      #^finds instances in temp where a team name matches the particular team in teamyear, and returns the opponent for that row (in temp),
      #then creates a list of all of these instances
    j=j+1
  }
  
  j=1 #loops through the list of opponent names to turn them into corresponding seasonal ID numbers
  while(j<=length(tmyr$Opponents)) {
    k=1
    while(k <= length(tmyr$Opponents[j][[1]])) {
      tmyr$Opponents[j][[1]][k] = which(tmyr$Team==tmyr$Opponents[j][[1]][k]) 
        #^finds correspinding team name in the "Team" column, and returns the index (index=Season ID)
      tmyr$Opponents[j][[1]][k] = as.integer(tmyr$Opponents[j][[1]][k])
        #^coerces Opponent IDs to type integer so they can be compared
      k=k+1
    }
    j=j+1
  }
  
  df = rbind(df,tmyr) #binds formatted team year data to the dataframe that stores data for all seasons
  i=i+1
}  

df = df[-1,] #delets "NA" column of data frame

save(df, file="colleyinputs.Rdata") #saves data frame as "colleyinputs" for use in the function
