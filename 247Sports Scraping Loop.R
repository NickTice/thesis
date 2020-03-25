# Loading the necessary packages
library('rvest')
library('stringr')
library('xlsx')


all_247Sports= NULL
# Specify any year sequence
for (year in seq(2010,2013,1)) {
  
  # Specifying the url for desired website to be scraped. Year can be changed
  url <- paste('https://247sports.com/Season/', year ,'-Football/RecruitRankings/?InstitutionGroup=highschool', sep="")
  # Reading the HTML code from the website
  webpage <- read_html(url)
  
  # player grade from recruiting service
  grade_html <- html_nodes(webpage,'.rankings-page__star-and-score .score')
  grade <- html_text(grade_html)
  # Convert to numberic
  grade <-as.numeric(grade)
  
  
  # Player Name
  player_html <- html_nodes(webpage,'.rankings-page__name-link')
  player_name <- html_text(player_html)
  
  
  # Position
  position_html <- html_nodes(webpage,'.position')
  position <- html_text(position_html)
  position = str_trim(position)
  
  
  # High School and state
  location_html <- html_nodes(webpage,'.meta')
  location <- html_text(location_html)
  # Splits string by comma
  y = unlist(strsplit(location, ","))
  # Picks out second half of string
  state = y[seq(2,(length(location)*2), 2)]
  # Removes end parenthases
  state = str_remove(state, "[)]")
  # Gets rid of white space
  state = str_trim(state)
  highschool=y[seq(1,(length(location)*2), 2)]
  highschool=str_trim(highschool)
  
  # Height/Weight
  height_weight_html <- html_nodes(webpage,'.metrics')
  height_weight <- html_text(height_weight_html)
  # Splits height and weight
  x = unlist(strsplit(height_weight, "/"))
  
  # List of just height
  height = x[seq(1,(length(location)*2), 2)]
  # Converts to numberic
  height=sapply(strsplit(height,"-"), function(x){12*as.numeric(x[1]) + as.numeric(x[2])})
  
  
  # List for just weight and converts to numeric
  weight = x[seq(2,(length(location)*2),2)]
  weight = as.numeric(weight)
  
  # Creates dataframe of top 150 players from the year
  x=assign(paste("sports", year, sep = ""), data.frame(Grade = grade, Player = player_name, Position = position, State = state, HighSchool=highschool, Height = height, Weight = weight))
  #x=assign(paste("sports", year, sep = ""), data.frame(Grade = grade, Player = player_name, Position = position, State = state, Height = height, Weight = weight))[1:150,]  
  
  # Appends a dataframe with each new year
  all_247Sports = rbind(all_247Sports, x)
}
all_247Sports=subset(all_247Sports,select = -HighSchool)
# Finds any duplicate players. For some reason the 247 website lists some people twice
# all_247Sports[duplicated(all_247Sports),]

# Removes duplicates
all_247Sports <- all_247Sports[!duplicated(all_247Sports),]





# Counting the number of players drafted
count=0
# Creates two empty dataframes to be appended in the loop
drafted247=NULL
undrafted247=NULL

for (name in all_247Sports$Player){
  # Tests to see if each player on the top 150 is in the drafted dataframe
  if (any(name == drafts$Name)=="TRUE"){
    # Adds to count and creates new dataframe of drafted players
    count=count+1
    drafted247 = rbind(drafted247, subset(all_247Sports, Player == name))
  }
  else {
    # Creates dataframe of undrafted players
    count=count
    undrafted247 =rbind(undrafted247, subset(all_247Sports, Player == name))
  }
}

# 1 for a drafted player, 0 for undrafted
drafted247$Drafted = 1
undrafted247$Drafted = 0
# Bind the orginal with this new column
trainplayers247=rbind(drafted247,undrafted247)


