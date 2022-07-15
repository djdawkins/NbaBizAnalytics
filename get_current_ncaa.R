
yrmin1 <- format(seq.Date(Sys.Date(), length=2, by='-1 year')[2], '%Y')
yr <- format(Sys.Date(), '%Y')
yr <- substr(yr,3,4)
tmp = paste(yrmin1, yr, sep = '-')


teamids <- teamids[!duplicated(teamids$Team), ]
teamids <- subset(teamids, Team !="Savannah St.")
teamids <- subset(teamids, Team !="LIU Brooklyn")

library(hash)
# Create year_team
lineups_hash <- hash()
# Loop through yr team tibble
for(i in 1:nrow(teamids)){
  #   
  #   # Get year value
  year <- tmp
  
  #   # GEt team value
  team <- teamids[i,"Team"]
  team <- team %>% getElement("Team")
  #   
  #   # Create year_team value
  year_team <- paste0(year, team)
  print(year_team)
  #   
  #   # Get schedule data
  schedule <- get_team_schedule(season = year, team.name = team)
  #   
  #   # Get play by play for all games played so far in season
  play_by_play <- get_play_by_play(schedule$Game_ID)
  # 
  #   # Generate all lineups and stats from the play by play
  lineups <- get_player_stats(play_by_play_data = play_by_play, multi.games=TRUE)
  #   
  #   # Filter on Team
  lineups <- lineups %>% filter(Team == team)
  #   # Add year column
  lineups$year <- year
  #   
  #   # Set hash key and save lineups tibble
  lineups_hash[[year_team]] <- lineups
  # 
}


library(jsonlite)
# 
# # Convert lineups hash to list
lineups_list <- list(values(lineups_hash))
# # Convert list to JSON
lineups_json = toJSON(lineups_list, pretty = TRUE, auto_unbox = TRUE)

#write(lineups_json, "all_lineups.json")

college_data <- fromJSON(lineups_json)



college_names <- college_data[[1]][[1]]
college_names <- do.call(c, college_names)
college_names_tbl <- tibble(namePlayer = unlist(college_names))
college_names_tbl
# address periods and JR in front of strings and add 3 letter row


# Loop through names tbl, use year team map to find players stats
yr_college_list <- list(keys(lineups_hash))
datalist = list()
for(i in 1:length(unlist(yr_college_list))){
  
  yr_college <- yr_college_list[[1]][i]
  yr_college_tbl <- lineups_hash[[yr_college]]
  
  
  datalist[[i]] <- yr_college_tbl # add it to your list
}
yr_college_tbl = do.call(rbind, datalist)

final_tbl <- yr_college_tbl %>% rename(college_team = Team, college_year = year, namePlayer = Player)


final_tbl %>% 
  group_by(namePlayer) %>% 
  filter(n()>1)
final_tbl <- final_tbl[!duplicated(final_tbl$namePlayer), ]

write_excel_csv(final_tbl, "current_collegePlayers.csv")
