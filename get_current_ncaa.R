### GET COLLEGE PLAYER STATS ###
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

#final_tbl <- final_tbl %>% mutate(namePlayerCol = namePlayer)
final_tbl <- final_tbl %>% mutate(namePlayer = str_replace_all(namePlayer, fixed("'"), ""))
#final_tbl <- final_tbl %>% mutate(namePlayerCol = str_replace_all(namePlayerCol, fixed("-"), ""))
#final_tbl <- final_tbl %>% mutate(namePlayerCol = str_replace_all(namePlayerCol, fixed("."), ""))
final_tbl <- final_tbl %>% mutate(namePlayer = str_replace_all(namePlayer, fixed("."), " "))
final_tbl <- final_tbl %>% mutate(namePlayer = toupper(namePlayer))
# Create namePlayerCol_2 column, then remove suffixes from names
final_tbl <- final_tbl %>% mutate(namePlayer = str_replace_all(namePlayer, fixed("SR"), ""))
final_tbl <- final_tbl %>% mutate(namePlayer = str_replace_all(namePlayer, fixed("JR"), ""))
final_tbl <- final_tbl %>% mutate(namePlayer = str_replace_all(namePlayer, fixed("III"), ""))
final_tbl <- final_tbl %>% mutate(namePlayer = str_replace_all(namePlayer, fixed("II"), ""))
final_tbl <- final_tbl %>% mutate(namePlayer = str_replace_all(namePlayer, fixed("IV"), ""))






### Fetch Positions ###

library(hash)
teamids <- teamids %>% filter(Season == tmp)
# Create year_team
positions_hash <- hash()
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
  
  lineups <- get_team_roster(season = year, team.name = team)
  #   
  #   # Set hash key and save lineups tibble
  positions_hash[[year_team]] <- lineups
  # 
}


positions_list <- list(keys(positions_hash))
datalist = list()
for(i in 1:length(unlist(positions_list))){
  
  positions_college <- positions_list[[1]][i]
  positions_tbl <- positions_hash[[positions_college]]
  
  
  datalist[[i]] <- positions_tbl # add it to your list
  
}


positions_tbl = do.call(rbind, datalist)

positions_tbl <- positions_tbl %>% rename(namePlayer = Player)

positions_tbl %>% 
  group_by(namePlayer) %>% 
  filter(n()>1)
positions_tbl <- positions_tbl[!duplicated(positions_tbl$namePlayer), ]





positions_tbl <- positions_tbl %>% mutate(namePlayer = str_replace_all(namePlayer, fixed("'"), ""))
positions_tbl <- positions_tbl %>% mutate(namePlayer = str_replace_all(namePlayer, fixed("."), " "))
positions_tbl <- positions_tbl %>% mutate(namePlayer = toupper(namePlayer))


### Calculate AV and Join with position table ###


final_tbl$TRB <- final_tbl$ORB + final_tbl$DRB
final_tbl$credit <- (final_tbl$PTS + final_tbl$TRB + final_tbl$AST + final_tbl$STL + final_tbl$BLK) - (final_tbl$FGA - final_tbl$FGM) - (final_tbl$FTA - final_tbl$FTM) - final_tbl$TOV
# AV
final_tbl$Average_AV <- (final_tbl$credit**(0.75))/21
final_tbl$Average_AV[is.na(final_tbl$Average_AV)] <- 0

final_tbl <- inner_join(final_tbl, positions_tbl, by = "namePlayer")

write_excel_csv(final_tbl, "current_collegePlayers.csv")
