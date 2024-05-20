year <- 2020

franchise_short_season <- franchise_short %>%
  filter(season == 2020)

schedule1<-ff_schedule(conn1)

schedule_1 <- schedule1 %>%
  select(week, franchise_id, franchise_score, opp_score = opponent_score, opp_id = opponent_id) %>%
  left_join(franchise_short_season, by = c("franchise_id" = "franchise_id")) %>%
  select(week, tm_id = franchise_id, tm = user_name, tm_score = franchise_score, opp_score, opp_id) %>%
  left_join(franchise_short_season, by = c("opp_id" = "franchise_id")) %>%
  select(week, tm_id, tm, tm_score, opp_score, opp_id, opp = user_name)

schedule_2 <- schedule1 |>
     select(week, tm_id = opponent_id, tm_score = opponent_score, opp_score = franchise_score, franchise_id) |>
     left_join(franchise_short_season, by = c("tm_id" = "franchise_id")) %>%
     select(week, tm_id, tm = user_name, tm_score, opp_score, franchise_id) %>%
     left_join(franchise_short_season, by = c("franchise_id" = "franchise_id")) %>%
     select(week, tm_id, tm, tm_score, opp_score, opp_id = franchise_id, opp = user_name)

final_schedule <- bind_rows(schedule_1, schedule_2) %>%
  arrange(week) %>%
  mutate(result = ifelse(tm_score > opp_score, 1, 0),
         season = year)

franchise_short_season <- franchise_short %>%
  filter(season == 2021)

schedule2<-ff_schedule(conn2)

schedule_1_2 <- schedule2 %>%
  select(week, franchise_id, franchise_score, opp_score = opponent_score, opp_id = opponent_id) %>%
  left_join(franchise_short_season, by = c("franchise_id" = "franchise_id")) %>%
  select(week, tm_id = franchise_id, tm = user_name, tm_score = franchise_score, opp_score, opp_id) %>%
  left_join(franchise_short_season, by = c("opp_id" = "franchise_id")) %>%
  select(week, tm_id, tm, tm_score, opp_score, opp_id, opp = user_name)

schedule_2_2 <- schedule2 %>%
  select(week, tm_id = opponent_id, tm_score = opponent_score, opp_score = franchise_score, franchise_id) %>%
  left_join(franchise_short_season, by = c("tm_id" = "franchise_id")) %>%
  select(week, tm_id, tm = user_name, tm_score, opp_score, franchise_id) %>%
  left_join(franchise_short_season, by = c("franchise_id" = "franchise_id")) %>%
  select(week, tm_id, tm, tm_score, opp_score, opp_id = franchise_id, opp = user_name)

final_schedule <- bind_rows(schedule_1_2, schedule_2_2) %>%
  arrange(week) %>%
  mutate(result = ifelse(tm_score > opp_score, 1, 0),
         season = year)


schedule1<-ff_schedule(conn1)
# Process each year's schedule and combine
listofgames <- map2_dfr(conns, years, ~process_schedule(ff_schedule(.x), franchise_short, .y))
