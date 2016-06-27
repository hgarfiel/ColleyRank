#Hunter Garfield 11.06.2015
#This function takes a year between 1960 and 2010 as an argument, and returns
#the rnakings of college football teams for that year based on the Colley Method

Colley = function(yr) { #defines Colley as a function of season
  
  load("colleyinputs.rdata") #loads "master" input data frame stored from Part1
  
  dfc = df[(df$Season==yr),] #creates a data frame for the season specified as the function argument
  
  ColleyMatx = matrix(0,nrow=nrow(dfc),ncol=nrow(dfc)) #initializes an empty matrix to store Colley data
  
  i=1
  while(i<=nrow(ColleyMatx)) {
    j=1
    while(j<=nrow(ColleyMatx)) {
      
      if(i==j){         #identifies diagonals of the matrix
        ColleyMatx[i,j] = (2+length(dfc[[i,4]]))     #fills in diagonals with num games played (length of opponent vector)
      }
      else if(any(j==dfc$Opponents[[i]])) { #Identifies positions where a team has played another team
        ColleyMatx[i,j] = -1*(length(which(j==dfc$Opponents[[i]]))) #fills in the position with the number of times team i played team j
        
      }
      
      else{
        ColleyMatx[i,j]=0
      }
      j=j+1
    }
    i=i+1
  }
  
  b = numeric(nrow(ColleyMatx)) #initialize b vector
  i=1
  
  while(i<=length(b)) { #fills in b vector
    b[i]= 1+((dfc$Wins[i]-dfc$Losses[i])/2)
    i=i+1
  }
  
  rank = solve(ColleyMatx,b) #produces ranks (solution to Ax=b)
  
  solution = data.frame(dfc$Team,rank) #combines team name and rank
  solution = solution[order(-solution$rank),] #ranks in descending order
  print(solution)
  
}

