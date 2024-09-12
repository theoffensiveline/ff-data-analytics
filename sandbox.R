library(sleeperapi)

league_id <- 1124831356770058240 # main league

users <- get_league_users(league_id)
filtered_users <- users %>% select(team_name, avatar)

# Define a function to fetch image or use team_name as text
get_image_or_text <- function(avatar, team_name) {
  return(ifelse(is.na(avatar), team_name, avatar))
}

# Apply the function to create a new column with image URLs or team names
filtered_users$image_or_text <-
  mapply(get_image_or_text,
         filtered_users$avatar,
         filtered_users$team_name)

return(filtered_users)