### Libraries -------------------------------------------------------------------------
library(dplyr)
library(tidyverse)
library(nflverse)
library(devtools)
library(ffscrapr)
library(sleeperapi)
library(ffpros)
library(stringi)
library(purrr)
library(stats)
library(openxlsx)
library(scales)
library(gt)
library(gtExtras)
library(lamisc)
library(htmltools)
library(webshot)
library(magick)
library(httr)
library(showtext)
library(sysfonts)


# Connection --------------------------------------------------------------------
conn1 = ff_connect(platform = "sleeper", league_id = 591530379404427264, season = 2020)
conn2 = ff_connect(platform = "sleeper", league_id = 650064757319118848, season = 2021)
conn3 = ff_connect(platform = "sleeper", league_id = 785068521058115584, season = 2022)
conn4 = ff_connect(platform = "sleeper", league_id = 917507729030352896, season = 2023)

conn_temp <-  ff_template(scoring_type = "ppr",roster_type = "1qb")

players <- sleeper_players()
position_start <- ff_starter_positions(conn_temp)

# ROSTERS BY WEEK CLEAN UP AND LOAD --------------------------------------------
starters1 <- ff_starters(conn1, week = 1:17)
starters1$season <- 2020
starters2 <- ff_starters(conn2, week = 1:18)
starters2$season <- 2021
starters3 <- ff_starters(conn3, week = 1:18)
starters3$season <- 2022

starters <- rbind(starters1,starters2,starters3)
rm(starters1,starters2,starters3)

starters$player_name <- ifelse(is.na(starters$player_name), starters$player_id, starters$player_name) 



scoring1 <- ff_scoringhistory(conn1, season = 2020)
scoring2 <- ff_scoringhistory(conn2, season = 2021)
scoring3 <- ff_scoringhistory(conn3, season = 2022)

scoring <- rbind(scoring1,scoring2,scoring3) |>
  filter(pos != "LB" & pos != "DB" &  pos != "DL") |>
  select(season, week, sleeper_id, player_name, points, team)

starters_scoring <- starters |>
  left_join(scoring, by = c("season", "week", "player_id" = "sleeper_id", "player_name"))

starters_scoring$team.x <- ifelse(is.na(starters_scoring$team.x), starters_scoring$team.y, starters_scoring$team.x) 

starters_final <- starters_scoring |>
  select(-team.y) |>
  rename(team = team.x) |>
  select(season, week, franchise_id, franchise_name, player_id, player_name, pos, team, points, starter_status)

rm(scoring1,scoring2,scoring3,starters_scoring)

starters_team <- unique(starters$franchise_name)

# Franchise Organization --------------------------------------------
franchise1 <- ff_franchises(conn1)
franchise1$season <- 2020
franchise2 <- ff_franchises(conn2)
franchise2$season <- 2021
franchise3 <- ff_franchises(conn3)
franchise3$season <- 2022
franchises <- rbind(franchise1,franchise2,franchise3)
rm(franchise1,franchise2,franchise3)

franchise_short <- franchises |>
  select(franchise_id, user_name) |>
  head(12)


# SCHEDULE CLEAN AND BIND --------------------------------------------

schedule1 <- ff_schedule(conn1)
schedule2 <- ff_schedule(conn2)
schedule3 <- ff_schedule(conn3)

schedule1_1 <- schedule1 |>
  select(week, franchise_id, franchise_score, opp_score = opponent_score, opp_id = opponent_id, -c(result)) |>
  left_join(franchise_short, by =c("franchise_id" = "franchise_id")) |>
  select(week, tm_id = franchise_id, tm = user_name, tm_score = franchise_score, opp_score, opp_id) |>
  left_join(franchise_short, by =c("opp_id" = "franchise_id")) |>
  select(week, tm_id, tm, tm_score, opp_score, opp_id, opp = user_name)

schedule1_2 <- schedule1 |>
  select(week, tm_id = opponent_id, tm_score = opponent_score, opp_score = franchise_score, franchise_id) |>
  left_join(franchise_short, by =c("tm_id" = "franchise_id")) |>
  select(week, tm_id, tm = user_name, tm_score, opp_score, franchise_id) |>
  left_join(franchise_short, by =c("franchise_id" = "franchise_id")) |>
  select(week, tm_id, tm, tm_score, opp_score, opp_id = franchise_id, opp = user_name)

schedule_2020_final <-
  merge(schedule1_1, schedule1_2) |>
  arrange(week) 

schedule_2020_final$result <- ifelse(schedule_2020_final$tm_score > schedule_2020_final$opp_score, 1, 0) 
schedule_2020_final$season <- 2020

schedule2_1 <- schedule2 |>
  select(week, franchise_id, franchise_score, opp_score = opponent_score, opp_id = opponent_id, -c(result)) |>
  left_join(franchise_short, by =c("franchise_id" = "franchise_id")) |>
  select(week, tm_id = franchise_id, tm = user_name, tm_score = franchise_score, opp_score, opp_id) |>
  left_join(franchise_short, by =c("opp_id" = "franchise_id")) |>
  select(week, tm_id, tm, tm_score, opp_score, opp_id, opp = user_name)

schedule2_2 <- schedule2 |>
  select(week, tm_id = opponent_id, tm_score = opponent_score, opp_score = franchise_score, franchise_id) |>
  left_join(franchise_short, by =c("tm_id" = "franchise_id")) |>
  select(week, tm_id, tm = user_name, tm_score, opp_score, franchise_id) |>
  left_join(franchise_short, by =c("franchise_id" = "franchise_id")) |>
  select(week, tm_id, tm, tm_score, opp_score, opp_id = franchise_id, opp = user_name)

schedule_2021_final <-
  merge(schedule2_1, schedule2_2) |>
  arrange(week)

schedule_2021_final$result <- ifelse(schedule_2021_final$tm_score > schedule_2021_final$opp_score, 1, 0) 
schedule_2021_final$season <- 2021

schedule3_1 <- schedule3 |>
  select(week, franchise_id, franchise_score, opp_score = opponent_score, opp_id = opponent_id, -c(result)) |>
  left_join(franchise_short, by =c("franchise_id" = "franchise_id")) |>
  select(week, tm_id = franchise_id, tm = user_name, tm_score = franchise_score, opp_score, opp_id) |>
  left_join(franchise_short, by =c("opp_id" = "franchise_id")) |>
  select(week, tm_id, tm, tm_score, opp_score, opp_id, opp = user_name)

schedule3_2 <- schedule3 |>
  select(week, tm_id = opponent_id, tm_score = opponent_score, opp_score = franchise_score, franchise_id) |>
  left_join(franchise_short, by =c("tm_id" = "franchise_id")) |>
  select(week, tm_id, tm = user_name, tm_score, opp_score, franchise_id) |>
  left_join(franchise_short, by =c("franchise_id" = "franchise_id")) |>
  select(week, tm_id, tm, tm_score, opp_score, opp_id = franchise_id, opp = user_name)

schedule_2022_final <-
  merge(schedule3_1, schedule3_2) |>
  arrange(week)

schedule_2022_final$result <- ifelse(schedule_2022_final$tm_score > schedule_2022_final$opp_score, 1, 0) 
schedule_2022_final$season <- 2022

listofgames <- rbind(schedule_2020_final,schedule_2021_final,schedule_2022_final)

full_schedule <- listofgames |>
  select(season, week:result) |>
  arrange(season, week, tm)

full_schedule$win_loss <- full_schedule$result
full_schedule$win_loss <- gsub("1", "W", full_schedule$win_loss)
full_schedule$win_loss <- gsub("0", "L", full_schedule$win_loss)

lookup <- c("nhosta" = "Hosta",
            "bellist" = "Tom",
            "tbellis" = "Tom",
            "pdpablo" = "Patrick",
            "zacgeoffray" = "Zac",
            "naaderbanki" = "Naad",
            "ttsao" = "Tommy",
            "elrandal" = "Randal",
            "Alanasty" = "JP",
            "JohnWickMD" = "Aviel",
            "slobonmynoblin" = "Logan",
            "asmontalvo" = "Montel",
            "cpmadden1" = "Conor")

full_schedule$tm <- sapply(full_schedule$tm, function(x) {
  if (x %in% names(lookup)) {
    return(gsub(x, lookup[x], x))
  } else {
    return(x)
  }
})

full_schedule$opp <- sapply(full_schedule$opp, function(x) {
  if (x %in% names(lookup)) {
    return(gsub(x, lookup[x], x))
  } else {
    return(x)
  }
})

rm(listofgames,schedule_2020_final,schedule_2021_final,schedule_2022_final,schedule3_1, schedule3_2,schedule2_1, schedule2_2,schedule1_1, schedule1_2,schedule3,schedule2,schedule1)

# STANDINGS ----------------

all_time_standings <- full_schedule |>
  group_by(tm) |>
  summarize(
    wins = sum(result),
    games = n(),
    pf = round(mean(tm_score),0),
    pa = round(mean(opp_score),0)) |>
  mutate(
    losses = games - wins,
    win_per = percent(wins / games, accuracy = 0.1),
    winp = wins / games) |>
  mutate(tm = ifelse(tm %in% c("Naad", "Hosta"), paste0(tm, " 🏆"), tm)) |>
  mutate(tm = ifelse(tm == "Tommy", paste0(tm, " 🏆 ** "), tm)) |>
  arrange(-winp) |>
  select(tm, wins, losses, win_per, pf, pa) |>
  gt() |>
  tab_header(title = md("**Dynasty Insanity**"),
             subtitle = md("**All-Time Standings**")) |>
  gt_theme_espn() |>
  cols_align("left", columns = tm) |>
  tab_options(data_row.padding = px(5)) |>
  tab_source_note(md("**Season: 2020-2022 | PF/PA Represent Avg/Gm**"))

gtsave(all_time_standings, 'output/history/all_time_standings.png')

standings22 <- full_schedule |>
  filter(season == 2022) |>
  group_by(tm) |>
  summarize(
    wins = sum(result),
    games = n(),
    pf = round(mean(tm_score),0),
    pa = round(mean(opp_score),0)) |>
  mutate(
    losses = games - wins,
    win_per = percent(wins / games, accuracy = 0.1),
    winp = wins / games) |>
  mutate(tm = ifelse(tm == "Tommy", paste0(tm, " 🏆 ** "), tm)) |>
  arrange(-winp) |>
  select(tm, wins, losses, win_per, pf, pa) |>
  gt() |>
  tab_header(title = md("**2022 Season Standings**"),
             subtitle = md("**Tommy's Tainted Trophy**")) |>
  gt_theme_espn() |>
  cols_align("left", columns = tm) |>
  tab_options(data_row.padding = px(5)) |>
  tab_source_note(md("**Season: 2022 | PF/PA Represent Avg/Gm**"))

gtsave(standings22, 'output/history/2022_standings.png')

standings21 <- full_schedule |>
  filter(season == 2021) |>
  group_by(tm) |>
  summarize(
    wins = sum(result),
    games = n(),
    pf = round(mean(tm_score),0),
    pa = round(mean(opp_score),0)) |>
  mutate(
    losses = games - wins,
    win_per = percent(wins / games, accuracy = 0.1),
    winp = wins / games) |>
  mutate(tm = ifelse(tm == "Naad", paste0(tm, " 🏆"), tm)) |>
  arrange(-winp) |>
  select(tm, wins, losses, win_per, pf, pa) |>
  gt() |>
  tab_header(title = md("**2021 Final Standings**"),
             subtitle = md("**Iron Banki Claims His Throne**")) |>
  gt_theme_espn() |>
  cols_align("left", columns = tm) |>
  tab_options(data_row.padding = px(5)) |>
  tab_source_note(md("**Season: 2021 | PF/PA Represent Avg/Gm**"))

gtsave(standings21, 'output/history/2021_standings.png')

standings20 <- full_schedule |>
  filter(season == 2020) |>
  group_by(tm) |>
  summarize(
    wins = sum(result),
    games = n(),
    pf = round(mean(tm_score),0),
    pa = round(mean(opp_score),0)) |>
  mutate(
    losses = games - wins,
    win_per = percent(wins / games, accuracy = 0.1),
    winp = wins / games) |>
  mutate(tm = ifelse(tm == "Hosta", paste0(tm, " 🏆"), tm)) |>
  arrange(-winp) |>
  select(tm, wins, losses, win_per, pf, pa) |>
  gt() |>
  tab_header(title = md("**2020 Final Standings**"),
             subtitle = md("**Nicks Chubb's Golden Taint**")) |>
  gt_theme_espn() |>
  cols_align("left", columns = tm) |>
  tab_options(data_row.padding = px(5)) |>
  tab_source_note(md("**Season: 2020 | PF/PA Represent Avg/Gm**"))

gtsave(standings20, 'output/history/2020_standings.png')


# DYNASTY -----------------

options(ffpros.cache = "memory") 
options(ffpros.cache = "filesystem")
options(ffpros.cache = "off")

conn4 = ff_connect(platform = "sleeper", league_id = 917507729030352896, season = 2023)
starters4 <- ff_rosters(conn4)|>
  select(-franchise_name)
starters4$season <- 2023
starters4$franchise_id <- as.integer(starters4$franchise_id)

dp <- dp_values("values-players.csv") |>
  select(fp_id,player:team,age,value_1qb) |>
  rename(dynpros_value = value_1qb)

ffpros <- ffpros::fp_rankings("dynasty-overall") |>
  rename(fp_id = fantasypros_id) |>
  select(fp_id:team, age, bye = player_bye_week, fp_rank = rank, fp_ecr = ecr, headshot = player_image_url, fp_posrank = pos_rank, fp_tier = tier) |>
  left_join(dp, by = c("fp_id" = "fp_id", "player_name" = 'player', "pos" = "pos")) |>
  relocate(headshot, .after = last_col())

ffpros$player_age <- ifelse(is.na(ffpros$age.y), ffpros$age.x, ffpros$age.y) 

ffpros <- ffpros |> 
  rename(team = team.x) |>
  select(-team.y, -age.x, -age.y) |>
  rename(age = player_age) |>
  relocate(age, .after = 2)
  

ffpros$team <- gsub("JAC", "JAX", ffpros$team)
ffpros$player_name <- gsub("Kenneth Walker Iii", "Kenneth Walker", ffpros$player_name)
ffpros$player_name <- gsub("Kenneth Walker III", "Kenneth Walker", ffpros$player_name)

id_map <- dp_playerids() |>
  select(fp_id = fantasypros_id, name, sleeper_id) |>
  mutate(name = stri_trans_totitle(gsub(",", " ", name)))

id_map$name <- gsub("Kenneth Walker Iii", "Kenneth Walker", id_map$name)
id_map$name <- gsub("Kenneth Walker III", "Kenneth Walker", id_map$name)

team_dynasty <- starters4 |>
  left_join(id_map, by = c("player_id" = "sleeper_id")) |>
  select(-player_name) |>
  filter(pos != "DEF" & pos != "K") |>
  left_join(ffpros, by = c("fp_id"))

team_dynasty$team.x <- ifelse(is.na(team_dynasty$team.x), team_dynasty$team.y, team_dynasty$team.x)

team_dynasty <- team_dynasty |>
  select(-pos.y, -team.y, -age.y) |>
  select(franchise_id,fp_id,season,name,team = team.x,age=age.x,pos=pos.x,bye,fp_tier,fp_rank,fp_ecr,dynpros_value,fp_posrank,headshot) |>
  arrange(fp_tier, -dynpros_value) |>
  left_join(franchise_short, by = c("franchise_id" = "franchise_id")) |>
  relocate(user_name, .before = 2) |>
  mutate(name = stri_trans_totitle(gsub(",", " ", name)))

playerprofiler <- read.csv('~/Documents/Learnings/code/r/sleeper_best_yet/csv/playerprofiler.csv') |>
  select(name = Full.Name, team = Team.Abbrev, pos_rank_pp = Positional.Rank,  career_value = Lifetime.Value) |>
  mutate(name = stri_trans_totitle(gsub(",", " ", name)))

playerprofiler$name <- gsub("Kenneth Walker Iii", "Kenneth Walker", playerprofiler$name)
playerprofiler$name <- gsub("Kenneth Walker III", "Kenneth Walker", playerprofiler$name)

team_dynasty_rosters <- team_dynasty |>
  left_join(playerprofiler, by = c("name" = "name")) |>
  select(franchise_id:fp_posrank,career_value,headshot) |>
  arrange(franchise_id, fp_tier, -dynpros_value)|>
  rename(team = team.x)

# TEAM ROSTER IMAGES --------------------

df <- team_dynasty_rosters |>
  select(headshot, user_name, name, team, pos, fp_tier, fp_rank, fp_ecr, playerprofiler = career_value, dynastypros = dynpros_value) |>
  mutate(user_name = stri_trans_totitle(gsub(",", " ", user_name)))

gt_theme_pff <- function(data, ...) {
  data %>%
    # add spanner for PFF Grade
    tab_spanner(
      label = "Season Rank",
      columns = contains(c("fp_tier", "fp_rank"))
    ) %>%
    # add spanner for SNAPS
    tab_spanner(
      label = "Dynasty Rank",
      columns = contains(c("playerprofiler", "dynastypros"))
    ) %>%
    # Add a "blank" spanner to add white space
    tab_spanner(
      label = "BLANK",
      columns = 1:5
    ) %>%
    # Relabel columns
    cols_label(
      fp_tier = "Tier",
      fp_rank = "Rank",
      playerprofiler = "PlyrPro",
      dynastypros = "DynPro"
    ) %>%
    # if missing, replace NA w/ ---
    fmt_missing(
      columns = everything(),
      missing_text = "---"
    ) %>%
    # add exact color from PFF table to spanners
    tab_style(
      style = list(
        cell_fill(color = "#EF6F6C"),
        cell_text(color = "white"),
        cell_borders(sides = "all", color = "#EF6F6C", weight = px(1))
      ),
      locations = list(
        cells_column_spanners(
          spanners = c("Season Rank", "Dynasty Rank")
        )
      )
    ) %>%
    # hide spanner with transparent color
    tab_style(
      style = list(
        cell_fill(color = "transparent"),
        cell_text(color = "transparent")
      ),
      locations = list(
        cells_column_spanners(
          spanners = c("BLANK")
        )
      )
    ) %>%
    # Change font color and weight for numeric col
    tab_style(
      style = list(
        cell_text(color = "#585d73", weight = "bold")
      ),
      locations = cells_body(
        columns = 1:9
      )
    ) %>%
    # Make column labels and spanners all caps
    opt_all_caps() %>%
    # add row striping
    opt_row_striping() %>%
    # change overall table styling for borders and striping
    tab_options(
      column_labels.background.color = "#000F66",
      table_body.hlines.color = "transparent",
      table.border.top.width = px(3),
      table.border.top.color = "transparent",
      table.border.bottom.color = "transparent",
      table.border.bottom.width = px(3),
      column_labels.border.top.width = px(3),
      column_labels.border.top.color = "transparent",
      column_labels.border.bottom.width = px(3),
      column_labels.border.bottom.color = "transparent",
      row.striping.background_color = "#FFFDE9",
      data_row.padding = px(10),
      ...
    ) %>% 
    # change font to Lato throughout (note no need to have Lato locally!)
    opt_table_font(
      font = c(
        google_font(name = "Roboto")
      )
    ) %>%
    # add source note
    tab_source_note(
      source_note = md("**PlyrPro:** PlayerProfiler.com  |  **DynPro:** DynastyProcess.com  |  **Bench:** Top-10 Only")
    )
}

create_user_table <- function(user_name1) {
  
  # Import font from sysfonts
  sysfonts::font_add_google("Roboto", "Roboto")
  showtext_auto()
  
  
  # Filter data for the specific user_name
  user_data <- df %>%
    filter(user_name == user_name1) %>%
    arrange(pos, -dynastypros) # arrange players by ECR
  
  # Define the number of players for each pos in the starting lineup
  starting_lineup <- bind_rows(
    user_data %>% filter(pos == "QB") %>% head(n = 1),
    user_data %>% filter(pos == "RB") %>% head(n = 2),
    user_data %>% filter(pos == "WR") %>% head(n = 2),
    user_data %>% filter(pos == "TE") %>% head(n = 1),
  )
  
  starting_lineup$roster <- "A-List"
  
  # Add the two highest ranked players from the "WR", "RB", "TE" positions to the starting lineup
  flex_players <- user_data %>%
    filter(!(name %in% starting_lineup$name)) %>%
    filter(pos %in% c("WR", "RB", "TE")) %>%
    head(n = 2)
  
  flex_players$roster <- "A-List"
  
  bench <- user_data %>%
    filter(!(name %in% starting_lineup$name)) %>%
    filter(!(name %in% flex_players$name)) %>%
    arrange(-dynastypros) %>%
    head(n = 10)
  
  bench$roster <- "Bench"
  
  lineup <- rbind(starting_lineup, flex_players, bench)
  
  # Create the gt table and apply filter
  gt_table <- lineup %>%
    filter(user_name == user_name1) %>%
    group_by(roster) %>%
    select(headshot, name, team, pos, fp_tier, fp_rank, playerprofiler, dynastypros) %>%
    gt() %>%
    tab_header(title = md("**2023 Dynasty Roster**"),
               subtitle = paste(user_name)) %>%
    gt_img_rows(headshot) %>% 
    fmt_number(
      columns = 5:9,
      decimals = 0
    ) %>% 
    gt_theme_pff()
  
  return(gt_table)
  
}

# Get unique user_names
unique_user_names <- unique(df$user_name)


# Loop through each user_name, create the table, and save it as PNG
for (user_name in unique_user_names) {
  gt_table <- create_user_table(user_name)
  
  gtsave(gt_table,
         paste0("output/2023/dynasty_roster_", user_name,".png"))
}

# 2022 ROSTER IMAGES --------------------
df2 <- starters_final |>
  filter(season == 2022) |>
  left_join(franchise_short, by = c("franchise_id" = "franchise_id")) |>
  group_by(season, user_name, player_name, pos) |>
  summarize(total_pts = sum(points, na.rm = TRUE),
            avg_pts = mean(points, na.rm = TRUE))

df2_v2 <- starters_final |>
  filter(season == 2022) |>
  group_by(player_name, pos) |>
  summarize(total_pts = sum(points, na.rm = TRUE),
            avg_pts = mean(points, na.rm = TRUE)) |>
  arrange(-total_pts) |>
  rank_in_group2(group_var = pos, arrange_var = total_pts)

dataheadshot <- load_players() |>
  select(name = display_name, pos = position, team = team_abbr, headshot)

data <- df2_v2 |>
  left_join(df2, by = c("player_name", "pos")) |>
  select(user_name, name = player_name, pos, total_pts = total_pts.y, avg_pts = avg_pts.y, rank) |>
  left_join(dataheadshot, by = c("name" = "name", "pos" = "pos")) |>
  relocate(headshot, .before = 1)

gt_theme_538 <- function(data,...) {
  data %>%
    opt_all_caps()  %>%
    opt_table_font(
      font = list(
        google_font("Roboto"),
        default_fonts()
      )
    ) %>%
    tab_spanner(
      label = "BLANK",
      columns = 1:4
    ) %>%
    tab_spanner(
      label = "Player Scoring",
      columns = c(rank, total_pts, avg_pts)
    ) %>% 
    tab_style(
      style = list(
        cell_fill(color = "#EF6F6C"),
        cell_text(color = "white"),
        cell_borders(sides = "all", color = "#EF6F6C", weight = px(1))
      ),
      locations = list(
        cells_column_spanners(
          spanners = "Player Scoring")
      )
    ) %>%
    tab_style(
      style = list(
        cell_fill(color = "transparent"),
        cell_text(color = "transparent")
      ),
      locations = list(
        cells_column_spanners(
          spanners = "BLANK"
        )
      )
    ) %>%
    tab_style(
      style = cell_borders(
        sides = "all", color = "transparent", weight = px(1)
      ),
      locations = cells_body(
        columns = everything(),
        # This is a relatively sneaky way of changing the bottom border
        # Regardless of data size
        rows = nrow(data$`_data`)
      )
    )  %>% 
    tab_style(
      style = list(
        cell_text(color = "#585d73", weight = "bold")
      ),
      locations = cells_body(
        columns = 1:8
      )
    ) %>%
    opt_row_striping() %>%
    tab_options(
      column_labels.background.color = "#000F66",
      table_body.hlines.color = "transparent",
      table.border.top.width = px(3),
      table.border.top.color = "transparent",
      table.border.bottom.color = "transparent",
      table.border.bottom.width = px(3),
      column_labels.border.top.width = px(3),
      column_labels.border.top.color = "transparent",
      column_labels.border.bottom.width = px(3),
      column_labels.border.bottom.color = "transparent",
      row.striping.background_color = "#FFFDE9",
      data_row.padding = px(10),
      ...
    ) 
}

historical_table <- function(user_name1) {

  # Filter data for the specific user_name
  user_data <- data %>%
    filter(user_name == user_name1) %>%
    select(user_name, headshot, name, team, pos, rank, total_pts, avg_pts) %>%
    arrange(pos, -total_pts) # arrange players by Total Points
  
  # Define the number of players for each pos in the starting lineup
  starting_lineup <- bind_rows(
    user_data %>% filter(pos == "QB") %>% head(1),
    user_data %>% filter(pos == "RB") %>% head(2),
    user_data %>% filter(pos == "WR") %>% head(2),
    user_data %>% filter(pos == "TE") %>% head(1),
  )
  
  starting_lineup$roster <- "A-List"
  
  # Add the two highest ranked players from the "WR", "RB", "TE" positions to the starting lineup
  flex_players <- user_data %>%
    filter(!(name %in% starting_lineup$name)) %>%
    filter(pos %in% c("WR", "RB", "TE")) %>%
    head(n = 2)
  
  flex_players$roster <- "A-List"
  
  bench <- user_data %>%
    filter(!(name %in% starting_lineup$name)) %>%
    filter(!(name %in% flex_players$name)) %>%
    filter(!pos == "K" & !pos == "DEF") %>%
    arrange(-total_pts) %>%
    head(n = 10)
  
  bench$roster <- "Bench"
  
  lineup <- rbind(starting_lineup, flex_players, bench)
  
  tab_538 <- lineup %>%
    filter(user_name == user_name1) %>%
    group_by(roster) %>%
    select(-user_name) %>%
    gt() %>%
    tab_header(title = md("**2022 Dynasty Roster**"),
               subtitle = paste("Ranked by Points Scored", user_name1)) %>%
    gt_img_rows(headshot) %>% 
    cols_label(
      rank = "PosRank"
    ) %>% 
    tab_source_note(
      source_note = md("**Bench**: Top-10 Only")
    ) %>% 
    fmt_number(
      columns = 6:7,
      decimals = 0
    ) %>% 
    fmt_number(
      columns = 8,
      decimals = 1
    ) %>% 
    gt_theme_538()
  
  return(tab_538)
  
}

# Get unique user_names
unique_user_names <- unique(data$user_name)


# Loop through each user_name, create the table, and save it as PNG
for(user_name in unique_user_names) {
  gt_table <- historical_table(user_name)
  
  gtsave(gt_table, paste0("output/2022/dynasty_roster",user_name,".png"))
}

# Head to Head -------------

head2head <- full_schedule |>
  mutate(margin = tm_score - opp_score) |>
  group_by(tm, opp) |>
  summarize(games = length(win_loss),
            wins = sum(result),
            losses = games - wins,
            winperc = percent(wins / games),
            pf = sum(tm_score),
            pa = sum(opp_score),
            margin = sum(margin),
            avg_mrg = margin / games) |>
  ungroup()

head2head$avg_mrg <- round(head2head$avg_mrg, 1)

df <- head2head |>
  select(tm, opp, wins, losses, winperc, pf, pa, avg_mrg) |>
  ungroup()

gt_theme_schedule <- function(data,...) {
  data %>%
    opt_all_caps()  %>%
    opt_table_font(
      font = list(
        google_font("Roboto"),
        default_fonts()
      )
    ) %>%
    tab_style(
      style = cell_borders(
        sides = "all", color = "#7180AC", weight = px(2)
      ),
      locations = cells_body(
        columns = everything(),
        rows = everything()
      )
    ) %>% 
    tab_options(
      column_labels.background.color = "#7180AC",
      table.border.top.width = px(2),
      table.border.top.color = "#7180AC",
      table.border.bottom.color = "#7180AC",
      table.border.bottom.width = px(2),
      column_labels.border.top.width = px(2),
      column_labels.border.top.color = "#7180AC",
      column_labels.border.bottom.width = px(2),
      column_labels.border.bottom.color = "#7180AC",
      row.striping.background_color = "#FFFDE9",
      data_row.padding = px(10),
      heading.align = "center",
      ...
    ) 
} 

create_headtohead <- function(tm1) {
  
  # Import font from sysfonts
  sysfonts::font_add_google("Roboto", "Roboto")
  showtext_auto()
  
  
  # Filter data for the specific user_name
  user_data <- df %>%
    filter(tm == tm1) %>%
    arrange(-wins, -avg_mrg) # arrange players by ECR
  
  # Create the gt table and apply filter
  gt_table <- user_data %>%
    select(-tm, opp, wins, losses, percent = winperc, pf, pa, avg_mrg) %>%
    gt() %>%
    tab_header(title = md("**All-Time Head to Head Results**"),
               subtitle = paste(tm)) %>%
    fmt_number(
      columns = 4:5,
      decimals = 0
    ) %>% 
    gt_theme_schedule()
  
  return(gt_table)
  
}

unique_tms <- unique(df$tm)

for (tm in unique_tms) {
  gt_table <- create_headtohead(tm)
  
  gtsave(gt_table,
         paste0("output/headtohead/", tm, "_head_to_head.png"))
}

# Last Year Schedule -------------------

seasonrecap <- full_schedule |>
  filter(season == 2022)

get_season_recap <- function(tm1) {
  # Import font from sysfonts
  sysfonts::font_add_google("Roboto", "Roboto")
  showtext_auto()
  
  # Filter data for the specific user_name
  user_data <- seasonrecap %>%
    filter(tm == tm1) %>%
    arrange(week) |> # arrange players by ECR
    select(week, opponent = opp, win_loss, pf = tm_score, pa = opp_score, result) |>
    mutate(margin = pf - pa)
  
  wins <- sum(user_data$result)
  losses <- length(user_data$result) - wins
  
  user_data$streak <- ave(user_data$result, cumsum(user_data$result==0), FUN = seq_along) - 1
  
  gt_table <- user_data |>
    select(week, opponent, win_loss, pf, pa, margin, streak) |>
    gt() |>
    gt_highlight_rows(rows = streak > 0,
                      fill = "#FFFDE9",
                      bold_target_only = FALSE) %>%
    tab_header(title = (paste(tm, "2022 Schedule")),
               subtitle = (paste("Record: ", wins, " - ", losses))) |>
    fmt_number(
      columns = 4:6,
      decimals = 1
    ) %>% 
    cols_align(
      align = "center",
      columns = 3:7
    ) %>%
    gt_theme_schedule()
  
  return(gt_table)
  
}

unique_tms <- unique(seasonrecap$tm)

for (tm in unique_tms) {
  gt_table <- get_season_recap(tm)
  
  gtsave(gt_table,
         paste0("output/py_schedule/season_results_",tm,".png"))
}



# Blowouts ----------
blowouts <- full_schedule |>
  mutate(margin = tm_score - opp_score) |>
  arrange(-margin) |>
  head(15)

# Create the gt table and apply filter
gt_table <- blowouts %>%
  select(season, week, victor = tm, loser = opp, pf = tm_score, pa = opp_score, margin) %>%
  gt() %>%
  tab_header(title = md("**Top-15 Blowouts**")) %>%
  fmt_number(
    columns = 5:7,
    decimals = 0
  ) %>% 
  gt_theme_schedule()

gtsave(gt_table,"output/history/blowouts.png")

# Nail Biters ----------
closecalls <- full_schedule |>
  mutate(margin = tm_score - opp_score) |>
  filter(margin > 0) |>
  arrange(margin) |>
  head(15)


# Create the gt table and apply filter
gt_table <- closecalls %>%
  select(season, week, victor = tm, loser = opp, pf = tm_score, pa = opp_score, margin) %>%
  gt() %>%
  tab_header(title = md("**Top-15 Close Calls**")) %>%
  fmt_number(
    columns = 5:7,
    decimals = 1
  ) %>% 
  gt_theme_schedule()

gtsave(gt_table,"output/history/closecalls.png")


