# Code to get the information of all NFL draft picks over a certain time period

# Loading the necessary packages
library('rvest')
library('stringr')

drafts = NULL

# Change the years for whatever you want
for (year in seq(2010,2019,1)){
  year=as.character(year)
  
  # URL that we are scraping
  url <- paste('https://www.pro-football-reference.com/years/', year , '/draft.htm', sep = "")
  webpage <- read_html(url)
  
  
  # Round that player was picked in (1-7)
  round_html <- html_nodes(webpage,'th.right')
  round <- html_text(round_html)
  round <-as.numeric(round)
  
  
  # Pick number (1-256)
  pick_html <- html_nodes(webpage,'th+ .right')
  pick <- html_text(pick_html)
  pick <-as.numeric(pick)
  
  
  # Player Name
  player_html <- html_nodes(webpage,'td:nth-child(4)')
  player_name <- html_text(player_html)
  
  
  # Position
  position_html <- html_nodes(webpage,'.left~ .left+ td.left')
  position <- html_text(position_html)
  
  
  # Creating data frame and saving it as variable
  x = assign(paste("draft", year, sep = ""),data.frame(Round = round, Pick = pick, Name = player_name, Position = position))
  
  
  # Appends a dataframe with each new year
  drafts = rbind(drafts, x)
}

# Shows dataframe with draft info
drafts