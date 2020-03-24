# Loading necessary packages
library('rvest')
library('stringr')
library('xlsx')

all_ESPN = NULL

# Pick any year range
for (year in seq(2009,2012,1)) {
  # Specifying the url for desired website to be scraped. Year can be changed
  url <- paste('http://www.espn.com/college-sports/football/recruiting/playerrankings/_/view/espnu150/sort/rank/class/', year,'', sep="")
  
  # Reading the HTML code from the website
  webpage <- read_html(url)
  
  
  # player grading
  grade_html <- html_nodes(webpage,'.sortcell~ td:nth-child(8)')
  grade <- html_text(grade_html)
  grade <-as.numeric(grade)
  
  
  # Player Name
  player_html <- html_nodes(webpage,'.name')
  player_name <- html_text(player_html)
  # gets rid of extra characters
  y = unlist(strsplit(player_name, "[|]"))
  names = y[seq(1,300,2)]
  # Removes video from end of string
  names=str_remove(names, "Video")
  names=str_trim(names)
  
  
  # Position
  position_html <- html_nodes(webpage,'.sortcell~ td:nth-child(3)')
  position <- html_text(position_html)
  
  
  # High School
  location_html <- html_nodes(webpage,'.sortcell~ td:nth-child(4)')
  location <- html_text(location_html)
  # Split by comma
  y = unlist(strsplit(location, ","))
  state = y[seq(2,300,2)]
  # Only takes first 2 characters which is state abreviation
  state=str_extract(state, "^.{3}")
  state=str_trim(state)
  
  # Height
  height_html <- html_nodes(webpage,'.sortcell~ td:nth-child(5)')
  height <- html_text(height_html)
  # convert to numeric
  height=sapply(strsplit(height,"'"), function(x){12*as.numeric(x[1]) + as.numeric(x[2])})
  
  
  # Weight
  weight_html <- html_nodes(webpage,'.sortcell~ td:nth-child(6)')
  weight <- html_text(weight_html)
  # convert to numeric
  weight = as.numeric(weight)
  
  
  # College
  #school_html=html_nodes(webpage,'.school-name')
  #school=html_text(school_html)
  
  
  # Creates dataframe
  x=assign(paste("ESPN", year, sep = ""), data.frame(Grade = grade, Player = names, Position = position, State = state, Height = height, Weight = weight))
  
  #Appends a dataframe with each new year
  all_ESPN = rbind(all_ESPN, x)
  
}

# Finds any duplicate players
all_ESPN[duplicated(all_ESPN),]

# Removes duplicates if there are any
all_ESPN <- all_ESPN[!duplicated(all_ESPN),]



# Creates count and empty dataframes
count=0
draftedESPN1=NULL
undraftedESPN1=NULL

for (name in all_ESPN$Player){
  # Tests to see if each player on the top 150 is in the drafted dataframe
  if (any(name == drafts$Name)=="TRUE"){
    # Adds to count and creates dataframe of drafted players 
    count=count+1
    draftedESPN1 = rbind(draftedESPN1, subset(all_ESPN, Player == name))
  }
  else {
    # creates dataframe of undrafted players
    count=count
    undraftedESPN1 =rbind(undraftedESPN1, subset(all_ESPN, Player == name))
  }
}
all_ESPN$Player

undraftedESPN1 =undraftedESPN1[!duplicated(undraftedESPN1),]
draftedESPN1 =draftedESPN1[!duplicated(draftedESPN1),]


# the variables draftedESPN and undraftedESPN give you a dataframe of which players were and were not drafted 


draftedESPN1$Drafted = 1
undraftedESPN1$Drafted = 0

trainplayersESPN=rbind(draftedESPN1,undraftedESPN1)

