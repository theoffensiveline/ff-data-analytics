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


library(jsonlite)

# Function to build tree based on the matchups
build_tree <- function(df, champion) {
  # Find the rows where the given champion is listed
  next_matchups <- df[df$Champion == champion, ]
  
  # Create a list where the champion is the key and the values are their possible opponents
  if (nrow(next_matchups) == 0) {
    return(champion)  # Leaf node, return the champion
  }
  
  # Recursively build the tree for each opponent
  tree <- list()
  for (i in 1:nrow(next_matchups)) {
    opponent <- next_matchups$Opponent[i]
    tree[[opponent]] <- build_tree(df, opponent)
  }
  
  return(tree)
}


# Step 1: Read the CSV file
df <- read.csv("currentMotW/matchup_tree.csv", stringsAsFactors = FALSE)

# Filter the dataframe for weeks less than 6
df_filtered <- df %>% filter(Week < 7)

# Build the tree starting with the first champion
motw_tree <- build_tree(df_filtered, df_filtered$Champion[1])
print(motw_tree)

# Convert to JSON
json_data <- toJSON(df_filtered, pretty = TRUE)

write_json_to_file(json_data, generate_file_path(
  current_year = current_year,
  current_week = current_week,
  file_name = "motwTree.json"
))






library(dplyr)
yahoo_df <- read.csv("~/Fantasy Football/matchups.csv")
yahoo_df %>%
  arrange(points)

