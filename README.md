# ColleyRank

The end use of this program is a function that takes a year between 1960 and 2010, and returns the rankings of NCAA football teams for that year according to the Colley method. The Colley method was introduced in 2002 as a way of ranking college football teams that accounts for strength of schedule without tracking biases for conference or region.

###Scrape
Part 1 of this project is a program that scrapes together all college football regular season data for the years between 1960 and 2010, and then formats it useful way for the matrix math required by the Colley ranking method. Data is scraped from the University of Wisconsin's website.

###Colleyinputs.Rdata
This is the R-style data collection that results from scraping and formatting the NCAA football data for use in the final function

###Colleyfunction
Colleyfunction takes a year between 1960 and 2010 as an input, and returns the Colley score as well as Colley rankings for each team that played more than 6 games during the regular season.
