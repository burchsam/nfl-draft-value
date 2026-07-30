library(cfbfastR)
library(nflreadr)
library(tidyverse)

library(ggrepel)
library(caTools)
library(car)
library(lmtest)
library(glmnet)
library(kknn)
library(randomForestSRC)
library(xgboost)
library(gt)


### SAVE NEW FILE!!!

draft_picks = load_draft_picks()

nfl_qbs_season = read_csv("coding-projects/college-football/helper-datasets/qb-performances-06-25.csv")[, -1]
df_final = read_csv("coding-projects/college-football/helper-datasets/qb-college-stats-2026")[, -1] |> 
  select(-matches("epa", ignore.case = TRUE))

df_final

# nfl_qbs_yn = read_csv("coding-projects/college-football/qbs-success-fail-thru-24.csv")[, -1]




# To-Do -------------------------------------------------------------------

# Add in 24' data
# Consider more predictors
# Build in PCs
# Find ways to increase sample size
### Add more years to NFL & College dataset
# Add in Success/Fail model
### 80th percentile (good-to-elite)
### 60th percentile (viable starter)



# Data Processing ---------------------------------------------------------

### NFL Success

## Increase to before 2006
## Consider different season & career (!) mins


# Name Data Frame

# name_df = load_player_stats(2006:2025)
# 
# 
# # name_df |> filter(player_display_name == "Tom Brady")
# #
# # colnames(draft_picks)
# #
# # draft_picks |> filter(pfr_player_name == "Tom Brady") |> select(gsis_id)
# 
# 
# draft_year = draft_picks |> select(gsis_id, season) |> unique()
# 
# qb_names = name_df |> filter(position == 'QB') |>
#   select(player_id, player_display_name) |>
#   unique() |>
#   # if undrafted, use first season as draft season
#   left_join(draft_year, by = c("player_id" = "gsis_id")) |>
#   rename(draft_year = "season")
# 
# # PBP
# 
# pbp_nfl = load_pbp(2006:2025)
# 
# 
# pbp_nfl = pbp_nfl |>
#   mutate(split_game_id = str_split(game_id, '_', simplify = TRUE),
#          year_id = as.double(split_game_id[,1]))
# 
# 
# qb_ids = qb_names |> select(player_id) |> pull()
# 
# pbp_nfl_2 = pbp_nfl |>
#   filter(!is.na(yards_gained), (qb_dropback == 1 & (passer_id %in% qb_ids)) |
#            (rush == 1 & (rusher_id %in% qb_ids))) |>
#   mutate(passer_new = case_when(!is.na(passer) ~ passer, !is.na(rusher) ~ rusher)) |>
#   mutate(passer_id_new = case_when(!is.na(passer) ~ passer_id, !is.na(rusher) ~ rusher_id)) |>
#   group_by(passer_new, passer_id_new) |>
#   summarise(mean_epa = mean(epa, na.rm = TRUE),
#             ypa = mean(yards_gained),
#             sack_rate = mean(sack),
#             cpoe = mean(cpoe, na.rm = TRUE),
#             plays = n(),
#             .groups = 'drop') |>
#   ### CHANGE???
#   filter(plays >= 400) |>
#   arrange(-mean_epa)
# pbp_nfl_2
# 
# pbp_nfl_3 = pbp_nfl |>
#   filter(!is.na(yards_gained), (qb_dropback == 1 & (passer_id %in% qb_ids)) |
#            (rush == 1 & (rusher_id %in% qb_ids))) |>
#   mutate(passer_new = case_when(!is.na(passer) ~ passer, !is.na(rusher) ~ rusher)) |>
#   mutate(passer_id_new = case_when(!is.na(passer) ~ passer_id, !is.na(rusher) ~ rusher_id)) |>
#   group_by(passer_new, passer_id_new, year_id) |>
#   summarise(mean_epa = mean(epa, na.rm = TRUE),
#             cpoe = mean(cpoe, na.rm = TRUE),
#             plays = n(),
#             .groups = 'drop') |>
#   ### CHANGE???
#   filter(plays >= 150) |>
#   arrange(-year_id, -mean_epa) |>
#   select(passer_new, passer_id_new, year_id, mean_epa, cpoe)
# pbp_nfl_3
# 
# 
# 
# 
# # PFF
# 
# ## Update Average Grade???
# 
# 
# pff_nfl_25 = read_csv('coding-projects/nfl-qbs/pff-qbs-2025.csv') |> mutate(year_id = 2025)
# pff_nfl_24 = read_csv('coding-projects/college-football/pff-data/pff_qb_nfl_24.csv') |> mutate(year_id = 2024)
# pff_nfl_23 = read_csv('coding-projects/college-football/pff-data/pff_qb_nfl_23.csv') |> mutate(year_id = 2023)
# pff_nfl_22 = read_csv('coding-projects/college-football/pff-data/pff_qb_nfl_22.csv') |> mutate(year_id = 2022)
# pff_nfl_21 = read_csv('coding-projects/college-football/pff-data/pff_qb_nfl_21.csv') |> mutate(year_id = 2021)
# pff_nfl_20 = read_csv('coding-projects/college-football/pff-data/pff_qb_nfl_20.csv') |> mutate(year_id = 2020)
# pff_nfl_19 = read_csv('coding-projects/college-football/pff-data/pff_qb_nfl_19.csv') |> mutate(year_id = 2019)
# pff_nfl_18 = read_csv('coding-projects/college-football/pff-data/pff_qb_nfl_18.csv') |> mutate(year_id = 2018)
# pff_nfl_17 = read_csv('coding-projects/college-football/pff-data/pff_qb_nfl_17.csv') |> mutate(year_id = 2017)
# pff_nfl_16 = read_csv('coding-projects/college-football/pff-data/pff_qb_nfl_16.csv') |> mutate(year_id = 2016)
# pff_nfl_15 = read_csv('coding-projects/college-football/pff-data/pff_qb_nfl_15.csv') |> mutate(year_id = 2015)
# pff_nfl_14 = read_csv('coding-projects/college-football/pff-data/pff_qb_nfl_14.csv') |> mutate(year_id = 2014)
# pff_nfl_13 = read_csv('coding-projects/college-football/pff-data/pff_qb_nfl_13.csv') |> mutate(year_id = 2013)
# pff_nfl_12 = read_csv('coding-projects/college-football/pff-data/pff_qb_nfl_12.csv') |> mutate(year_id = 2012)
# pff_nfl_11 = read_csv('coding-projects/college-football/pff-data/pff_qb_nfl_11.csv') |> mutate(year_id = 2011)
# pff_nfl_10 = read_csv('coding-projects/college-football/pff-data/pff_qb_nfl_10.csv') |> mutate(year_id = 2010)
# pff_nfl_09 = read_csv('coding-projects/college-football/pff-data/pff_qb_nfl_09.csv') |> mutate(year_id = 2009)
# pff_nfl_08 = read_csv('coding-projects/college-football/pff-data/pff_qb_nfl_08.csv') |> mutate(year_id = 2008)
# pff_nfl_07 = read_csv('coding-projects/college-football/pff-data/pff_qb_nfl_07.csv') |> mutate(year_id = 2007)
# pff_nfl_06 = read_csv('coding-projects/college-football/pff-data/pff_qb_nfl_06.csv') |> mutate(year_id = 2006)
# 
# 
# pff_nfl = pff_nfl_25 |>
#   full_join(pff_nfl_24) |>
#   full_join(pff_nfl_23) |>
#   full_join(pff_nfl_22) |>
#   full_join(pff_nfl_21) |>
#   full_join(pff_nfl_20) |>
#   full_join(pff_nfl_19) |>
#   full_join(pff_nfl_18) |>
#   full_join(pff_nfl_17) |>
#   full_join(pff_nfl_16) |>
#   full_join(pff_nfl_15) |>
#   full_join(pff_nfl_14) |>
#   full_join(pff_nfl_13) |>
#   full_join(pff_nfl_12) |>
#   full_join(pff_nfl_11) |>
#   full_join(pff_nfl_10) |>
#   full_join(pff_nfl_09) |>
#   full_join(pff_nfl_08) |>
#   full_join(pff_nfl_07) |>
#   full_join(pff_nfl_06)
# 
# 
# pff_nfl |> colnames()
# 
# # Key Stats by year
# pff_nfl_2 = pff_nfl |>
#   mutate(player_id = as.character(player_id)) |>
#   select(player_id, player, year_id, grades_offense, pressure_to_sack_rate,
#          def_gen_pressures, sacks, avg_time_to_throw, dropbacks) |>
#   mutate(player = case_when(player == "Robert Griffin III" ~ "Robert Griffin",
#                             player == "Michael Vick" ~ "Mike Vick",
#                             player == "E.J. Manuel" ~ "EJ Manuel",
#                             player == "Michael Penix Jr." ~ "Michael Penix",
#                             .default = player))
# 
# total_dropbacks = pff_nfl_2 |>
#   group_by(player_id, player) |>
#   summarise(career_dropbacks = sum(dropbacks),
#             .groups = "drop") |>
#   arrange(career_dropbacks)
# 
# 
# # Career Numbers
# pff_nfl_3 = pff_nfl_2 |>
#   # Removed because of weighted average
#   # filter(dropbacks >= 150) |>
#   left_join(total_dropbacks) |>
#   mutate(grade_weight = grades_offense * (dropbacks / career_dropbacks)) |>
#   group_by(player_id, player) |>
#   summarise(grade_avg = sum(grade_weight),
#             ptsr_tot = sum(sacks) / sum(def_gen_pressures),
#             .groups = 'drop') |>
#   arrange(-grade_avg)
# pff_nfl_3
# 
# 
# # Season numbers
# pff_nfl_4 = pff_nfl_2 |>
#   filter(dropbacks >= 150) |>
#   arrange(-year_id, -grades_offense) |>
#   select(player, player_id, year_id, grades_offense, pressure_to_sack_rate, dropbacks)
# pff_nfl_4
# 
# 
# # Full NFL DF
# 
# nfl_qbs = pbp_nfl_2 |>
#   left_join(qb_names, by = c('passer_id_new' = 'player_id')) |>
#   select(-passer_new, player_display_name, passer_id_new:plays) |>
#   left_join(pff_nfl_3, by = c('player_display_name' = 'player')) |>
#   select(player_display_name, everything(), -c(passer_id_new, player_id)) |>
#   # Combined EPA and Grade to make new QBR metric
#   mutate(qbr = .5*scale(mean_epa) + .5*scale(grade_avg)) |>
#   mutate(qbr_pct = pnorm(qbr)) |>
#   select(player_display_name, qbr, qbr_pct, mean_epa, grade_avg, everything()) |>
#   arrange(-qbr)
# 
# 
# 
# nfl_qbs_2 = pbp_nfl_3 |>
#   left_join(qb_names, by = c('passer_id_new' = 'player_id')) |>
#   select(player_display_name, year_id, mean_epa, cpoe, draft_year) |>
#   left_join(pff_nfl_4, by = c('player_display_name' = 'player', "year_id")) |>
#   select(-player_id) |>
#   rename(year = year_id,
#          player = player_display_name) |>
#   mutate(qbr = .5*scale(mean_epa) + .5*scale(grades_offense)) |>
#   mutate(qbr_pct = pnorm(qbr)) |>
#   select(player, year, draft_year, qbr_pct, mean_epa:dropbacks) |>
#   arrange(-qbr_pct)
# 
# 
# # Comparison of EPA and Grade
# ggplot(nfl_qbs, aes(y = mean_epa, x = grade_avg)) +
#   labs(
#     title = 'NFL QB EPA against PFF Grade',
#     subtitle = 'Data from 2006-2025; Min 400 NFL plays',
#     caption = 'By: Sam Burch  |  Data @nflfastR & @pff',
#     y = 'EPA per play',
#     x = 'PFF Grade'
#   ) +
#   geom_point() +
#   theme_minimal()
# cor(nfl_qbs$mean_epa, nfl_qbs$grade_avg)
# 
# nfl_qbs_tot = nfl_qbs |>
#   select(-c(qbr, ypa, sack_rate)) |>
#   rename(player = player_display_name,
#          qbr_pct_tot = qbr_pct,
#          mean_epa_tot = mean_epa,
#          cpoe_tot = cpoe,
#          plays_tot = plays) |>
#   select(player:cpoe_tot, ptsr_tot, plays_tot)
# 
# 
# nfl_qbs_season = nfl_qbs_2 |>
#   select(player:mean_epa, grades_offense, cpoe:dropbacks) |>
#   left_join(nfl_qbs_tot, by = "player")
# 
# 
# write.csv(nfl_qbs_season, file = "qb-performances-06-25.csv")







### Success / Fail

# qb_names
#
# latest_year = nfl_qbs_season |>
#   group_by(player) |>
#   summarise(recent_year = max(year),
#             .groups = "drop")
#
# first_year = nfl_qbs_season |>
#   group_by(player) |>
#   summarise(rookie_season = min(year),
#             .groups = "drop")
#
# qb_names
#
# nfl_qb_successes1 = nfl_qbs_tot |>
#   left_join(qb_names |> select(-draft_year), by = c("player" = "player_display_name")) |>
#   left_join(first_year, by = "player")
#
# # nfl_qb_successes1 =
# # nfl_qbs_season |>
# #   filter(is.na(qbr_pct_tot)) |>
# #   left_join(qb_names |> select(-draft_year), by = c("player" = "player_display_name")) |>
# #   left_join(first_year, by = "player") |>
# #   rename()
#   # mutate(years_in_league = recent_year - draft_year) |>
#   # mutate(draft_year = if_else(is.na(draft_year), rookie_season, draft_year)) |>
#   # select(player, year, qbr_pct, qbr_pct_tot)
#   # Add in qbs who never played!!!
#   # Adjust for undrafted players that played some!!!
#   # Use 80th percentile for great qbs
#   # mutate(success = case_when((qbr_pct_tot >= .8) ~ 1,
#   #                             (is.na(qbr_pct_tot) & (years_in_league > 2)) ~ 0,
#   #                              .default = NA))
#
#
# nfl_qbs_success_fail = qb_names |>
#   left_join(nfl_qbs_tot, by = c("player_display_name" = "player")) |>
#   left_join(first_year, by = c("player_display_name" = "player")) |>
#   mutate(current_year = 2025) |>
#   # In case were undrafted, but played in the NFL
#   mutate(draft_year = if_else(is.na(draft_year), rookie_season, draft_year)) |>
#   # Gets rid of every other undrafted player, since we don't know if they were good or not
#   filter(!is.na(draft_year)) |>
#   mutate(years_since_drafted = current_year - draft_year) |>
#   mutate(great = case_when((qbr_pct_tot >= .85) ~ 1,
#                              (qbr_pct_tot < .85) ~ 0,
#                             (is.na(qbr_pct_tot) & (years_since_drafted >= 2)) ~ 0,
#                              .default = NA),
#          okay = case_when((qbr_pct_tot >= .7) ~ 1,
#                           (qbr_pct_tot < .7) ~ 0,
#                           (is.na(qbr_pct_tot) & (years_since_drafted >= 2)) ~ 0,
#                           .default = NA),) |>
#   filter(!is.na(great)) |>
#   select(player_id, player_display_name, great, okay)


# write.csv(nfl_qbs_success_fail, file = "qbs-success-fail-thru-24.csv")
  





### College Success


# PBP

## No 2025 data!!!
# pbp_cfb = load_cfb_pbp(2014:2025)
# 
# 
# qbs_college = pbp_cfb |>
#   mutate(passer_player_name = if_else(passer_player_name == "Cameron Ward", "Cam Ward", passer_player_name)) |>
#   group_by(passer_player_name) |>
#   summarise(plays = n()) |> 
#   filter(plays >= 100, !is.na(passer_player_name)) |>
#   pull(passer_player_name) |>
#   unique()
# 
# 
# # Edit Potential Predictors!!!
# 
# cfb_epa = pbp_cfb |>
#   mutate(passer_player_name = if_else(passer_player_name == "Cameron Ward", "Cam Ward", passer_player_name)) |>
#   filter(!is.na(yards_gained), pass == 1 | (rush == 1 & (rusher_player_name %in% qbs_college)),
#          # !is.na(passer_player_name),
#          # passer_player_name != "incomplete"
#   ) |>
#   mutate(passer_player_name = if_else(rush == 1, rusher_player_name, passer_player_name)) |>
#   group_by(passer_player_name, year) |>
#   summarise(mean_epa = mean(EPA, na.rm = TRUE),
#             total_epa = sum(EPA, na.rm = TRUE),
#             avg_pass_epa = mean(EPA[pass == 1], na.rm = TRUE),
#             total_pass_epa = sum(EPA[pass == 1], na.rm = TRUE),
#             avg_rush_epa = mean(EPA[rush == 1], na.rm = TRUE),
#             total_rush_epa = sum(EPA[pass == 1], na.rm = TRUE),
#             avg_sack_epa = mean(EPA[sack == 1], na.rm = TRUE),
#             total_sack_epa = sum(EPA[sack == 1], na.rm = TRUE),
#             avg_ground_epa = mean(EPA[rush == 1 | sack == 1], na.rm = TRUE),
#             total_ground_epa = sum(EPA[rush == 1 | sack == 1], na.rm = TRUE),
# 
#             passes = sum(pass == 1),
#             rushes = sum(rush == 1),
#             sacks = sum(sack == 1),
#             plays = n(),
# 
#             .groups = "drop") |>
#   # Change to 150?
#   filter(plays >= 100) |>
#   group_by(passer_player_name) |>
#   summarise(# Career
#     mean_epa_tot = sum(total_epa) / sum(plays),
#     total_epa_tot = sum(total_epa),
#     avg_pass_epa_tot = sum(total_pass_epa) / sum(passes),
#     total_pass_epa_tot = sum(total_pass_epa),
#     avg_rush_epa_tot = sum(total_rush_epa) / sum(passes),
#     total_rush_epa_tot = sum(total_rush_epa),
#     avg_sack_epa_tot = sum(total_sack_epa) / sum(sacks),
#     total_sack_epa_tot = sum(total_sack_epa),
#     avg_ground_epa_tot = sum(total_ground_epa) / (sum(sacks) + sum(rushes)),
#     total_ground_epa_tot = sum(total_ground_epa),
# 
#     # Totals
#     passes_tot = sum(passes),
#     rushes_tot = sum(rushes),
#     sacks_tot = sum(sacks),
#     plays_tot = sum(plays),
# 
#     # Lasts
#     mean_epa_last = mean_epa[year == max(year)],
#     total_epa_last = total_epa[year == max(year)],
#     avg_pass_epa_last = avg_pass_epa[year == max(year)],
#     total_pass_epa_last = total_pass_epa[year == max(year)],
#     avg_rush_epa_last = avg_rush_epa[year == max(year)],
#     total_rush_epa_last = total_rush_epa[year == max(year)],
#     avg_sack_epa_last = avg_sack_epa[year == max(year)],
#     total_sack_epa_last = total_sack_epa[year == max(year)],
#     avg_ground_epa_last = avg_ground_epa[year == max(year)],
#     total_ground_epa_last = total_ground_epa[year == max(year)],
# 
#     # Firsts
#     mean_epa_first = mean_epa[year == min(year)],
#     total_epa_first = total_epa[year == min(year)],
#     avg_pass_epa_first = avg_pass_epa[year == min(year)],
#     total_pass_epa_first = total_pass_epa[year == min(year)],
#     avg_rush_epa_first = avg_rush_epa[year == min(year)],
#     total_rush_epa_first = total_rush_epa[year == min(year)],
#     avg_sack_epa_first = avg_sack_epa[year == min(year)],
#     total_sack_epa_first = total_sack_epa[year == min(year)],
#     avg_ground_epa_first = avg_ground_epa[year == min(year)],
#     total_ground_epa_first = total_ground_epa[year == min(year)],
# 
#     # Maxes
#     mean_epa_max = max(mean_epa),
#     total_epa_max = max(total_epa),
#     avg_pass_epa_max = max(avg_pass_epa),
#     total_pass_epa_max = max(total_pass_epa),
#     avg_rush_epa_max = max(avg_rush_epa),
#     total_rush_epa_max = max(total_rush_epa),
#     avg_sack_epa_max = max(avg_sack_epa),
#     total_sack_epa_max = max(total_sack_epa),
#     avg_ground_epa_max = max(avg_ground_epa),
#     total_ground_epa_max = max(total_ground_epa),
# 
#     # Mins
#     mean_epa_min = min(mean_epa),
#     total_epa_min = min(total_epa),
#     avg_pass_epa_min = min(avg_pass_epa),
#     total_pass_epa_min = min(total_pass_epa),
#     avg_rush_epa_min = min(avg_rush_epa),
#     total_rush_epa_min = min(total_rush_epa),
#     avg_sack_epa_min = min(avg_sack_epa),
#     total_sack_epa_min = min(total_sack_epa),
#     avg_ground_epa_min = min(avg_ground_epa),
#     total_ground_epa_min = min(total_ground_epa),
# 
# 
#     .groups = "drop") |>
#   filter(passer_player_name != "incomplete", !is.na(passer_player_name)) |>
#   # Ugh, Mitch Trubisky
#   mutate(passer_player_name = if_else(passer_player_name == "Mitch Trubisky", "Mitchell Trubisky", passer_player_name)) |>
#   arrange(-mean_epa_tot)
# 
# cfb_epa
# 
# 
# # PFF
# 
# 
# 
# pff_cfb_25 = read_csv('coding-projects/college-football/pff-data/pff_qb_cfb_25.csv') |> mutate(year_id = 2025)
# pff_cfb_24 = read_csv('coding-projects/college-football/pff-data/pff_qb_cfb_24.csv') |> mutate(year_id = 2024)
# pff_cfb_23 = read_csv('coding-projects/college-football/pff-data/pff_qb_cfb_23.csv') |> mutate(year_id = 2023)
# pff_cfb_22 = read_csv('coding-projects/college-football/pff-data/pff_qb_cfb_22.csv') |> mutate(year_id = 2022)
# pff_cfb_21 = read_csv('coding-projects/college-football/pff-data/pff_qb_cfb_21.csv') |> mutate(year_id = 2021)
# pff_cfb_20 = read_csv('coding-projects/college-football/pff-data/pff_qb_cfb_20.csv') |> mutate(year_id = 2020)
# pff_cfb_19 = read_csv('coding-projects/college-football/pff-data/pff_qb_cfb_19.csv') |> mutate(year_id = 2019)
# pff_cfb_18 = read_csv('coding-projects/college-football/pff-data/pff_qb_cfb_18.csv') |> mutate(year_id = 2018)
# pff_cfb_17 = read_csv('coding-projects/college-football/pff-data/pff_qb_cfb_17.csv') |> mutate(year_id = 2017)
# pff_cfb_16 = read_csv('coding-projects/college-football/pff-data/pff_qb_cfb_16.csv') |> mutate(year_id = 2016)
# pff_cfb_15 = read_csv('coding-projects/college-football/pff-data/pff_qb_cfb_15.csv') |> mutate(year_id = 2015)
# pff_cfb_14 = read_csv('coding-projects/college-football/pff-data/pff_qb_cfb_14.csv') |> mutate(year_id = 2014)
# 
# 
# pff_cfb = pff_cfb_25 |>
#   full_join(pff_cfb_24) |>
#   full_join(pff_cfb_23) |>
#   full_join(pff_cfb_22) |>
#   full_join(pff_cfb_21) |>
#   full_join(pff_cfb_20) |>
#   full_join(pff_cfb_19) |>
#   full_join(pff_cfb_18) |>
#   full_join(pff_cfb_17) |>
#   full_join(pff_cfb_16) |>
#   full_join(pff_cfb_15) |>
#   full_join(pff_cfb_14) |>
#   mutate(player = if_else(player == "Cameron Ward", "Cam Ward", player))
# 
# pff_cfb |> select(player) |> arrange(player) |> unique() |> View()
# 
# nfl_qbs2 = nfl_qbs |>
#   rename(player = player_display_name,
#          nfl_pct = qbr_pct) |>
#   select(player, nfl_pct)
# 
# 
# # nfl_qbs2 = nfl_qbs_season |>
# #   select(player, qbr_pct_tot) |>
# #   unique() |>
# #   rename(nfl_pct = qbr_pct_tot)
# 
# 
# 
# # pff_cfb |> filter(player == "Cameron Ward")
# 
# df_final = pff_cfb |>
#   # Changed from 200
#   filter(dropbacks >= 150) |>
#   mutate(tot_ttt = avg_time_to_throw * dropbacks,
#          pressure_to_scramble_rate = scrambles / def_gen_pressures,
#          scramble_rate = scrambles / dropbacks,
#          td_rate = touchdowns / dropbacks,
#          int_rate = interceptions / dropbacks,
#          bat_rate = bats / attempts,
#          hat_rate = hit_as_threw / attempts,
#          ta_rate = thrown_aways / dropbacks,
#          fd_rate = first_downs / dropbacks,
#          pen_rate = penalties / dropbacks) |>
#   group_by(player) |>
#   summarise(#Grades
#     off_grade_avg = mean(grades_offense),
#     pass_grade_avg = mean(grades_pass),
#     run_grade_avg = mean(grades_run),
#     fum_grade_avg = mean(grades_hands_fumble),
# 
# 
#     # Totals
#     aimed_passes = sum(aimed_passes),
#     attempts = sum(attempts),
#     dropbacks = sum(dropbacks),
#     completions = sum(completions),
#     yards = sum(yards),
#     tds = sum(touchdowns),
#     ints = sum(interceptions),
#     sacks = sum(sacks),
#     pressures = sum(def_gen_pressures),
#     scrambles = sum(scrambles),
#     btts = sum(big_time_throws),
#     twps = sum(turnover_worthy_plays),
#     first_downs = sum(first_downs),
#     penalties = sum(penalties),
#     bats = sum(bats),
#     hats = sum(hit_as_threw),
#     tas = sum(thrown_aways),
# 
# 
#     # Efficiecy & Rates
#     cp = completions / attempts,
#     adj_cp = (completions + sum(drops)) / aimed_passes,
#     ypa_tot = yards / attempts,
#     sack_rate = sacks / dropbacks,
#     pressure_to_sack_rate_tot = sacks / pressures,
#     pressure_to_scramble_rate_tot = scrambles / pressures,
#     scramble_rate_tot = scrambles / dropbacks,
#     ttt = sum(tot_ttt) / dropbacks,
#     btt_rate_tot = btts / attempts,
#     twp_rate_tot = twps / attempts,
#     adot_avg = mean(avg_depth_of_target),
#     nfl_pr = 100*(((completions/attempts - .3)*5 + (yards/attempts - 3)*.25 + (tds/attempts)*20 + 2.375 - (ints/attempts)*25)/6),
#     td_rate_tot = tds / dropbacks,
#     int_rate_tot = ints / dropbacks,
#     bat_rate_tot = bats / attempts,
#     hat_rate_tot = hats / attempts,
#     ta_rate_tot = tas / dropbacks,
#     fd_rate_tot = first_downs / dropbacks,
#     pen_rate_tot = penalties / dropbacks,
# 
#     # Maxs (Raw)
#     tds_max = max(touchdowns),
#     off_grade_max = max(grades_offense),
#     pass_grade_max = max(grades_pass),
#     run_grade_max = max(grades_run),
#     fum_grade_max = max(grades_hands_fumble),
#     cp_max = max(completion_percent),
#     ## Correct?
#     adj_cp_max = max(accuracy_percent),
#     ypa_max = max(ypa),
#     sack_rate_max = max(sack_percent),
#     pressure_to_sack_rate_max = max(pressure_to_sack_rate),
#     pressure_to_scramble_rate_max = max(pressure_to_scramble_rate),
#     scramble_rate_max = max(scramble_rate),
#     ttt_max = max(avg_time_to_throw),
#     btt_rate_max = max(btt_rate),
#     twp_rate_max = max(twp_rate),
#     adot_max = max(avg_depth_of_target),
#     nfl_pr_max = max(qb_rating),
#     td_rate_max = max(td_rate),
#     int_rate_max = max(int_rate),
#     bat_rate_max = max(bat_rate),
#     hat_rate_max = max(hat_rate),
#     ta_rate_max = max(ta_rate),
#     fd_rate_max = max(fd_rate),
#     pen_rate_max = max(pen_rate),
# 
#     # Mins (Raw)
#     tds_min = min(touchdowns),
#     off_grade_min = min(grades_offense),
#     pass_grade_min = min(grades_pass),
#     run_grade_min = min(grades_run),
#     fum_grade_min = min(grades_hands_fumble),
#     cp_min = min(completion_percent),
#     ## Correct?
#     adj_cp_min = min(accuracy_percent),
#     ypa_min = min(ypa),
#     sack_rate_min = min(sack_percent),
#     pressure_to_sack_rate_min = min(pressure_to_sack_rate),
#     pressure_to_scramble_rate_min = min(pressure_to_scramble_rate),
#     scramble_rate_min = min(scramble_rate),
#     ttt_min = min(avg_time_to_throw),
#     btt_rate_min = min(btt_rate),
#     twp_rate_min = min(twp_rate),
#     adot_min = min(avg_depth_of_target),
#     nfl_pr_min = min(qb_rating),
#     td_rate_min = min(td_rate),
#     int_rate_min = min(int_rate),
#     bat_rate_min = min(bat_rate),
#     hat_rate_min = min(hat_rate),
#     ta_rate_min = min(ta_rate),
#     fd_rate_min = min(fd_rate),
#     pen_rate_min = min(pen_rate),
# 
#     # Lasts
#     tds_last = touchdowns[year_id == max(year_id)],
#     off_grade_last = grades_offense[year_id == max(year_id)],
#     pass_grade_last = grades_pass[year_id == max(year_id)],
#     run_grade_last = grades_run[year_id == max(year_id)],
#     fum_grade_last = grades_hands_fumble[year_id == max(year_id)],
#     cp_last = completion_percent[year_id == max(year_id)],
#     ## Correct?
#     adj_cp_last = accuracy_percent[year_id == max(year_id)],
#     ypa_last = ypa[year_id == max(year_id)],
#     sack_rate_last = sack_percent[year_id == max(year_id)],
#     pressure_to_sack_rate_last = pressure_to_sack_rate[year_id == max(year_id)],
#     pressure_to_scramble_rate_last = pressure_to_scramble_rate[year_id == max(year_id)],
#     scramble_rate_last = scramble_rate[year_id == max(year_id)],
#     ttt_last = avg_time_to_throw[year_id == max(year_id)],
#     btt_rate_last = btt_rate[year_id == max(year_id)],
#     twp_rate_last = twp_rate[year_id == max(year_id)],
#     adot_last = avg_depth_of_target[year_id == max(year_id)],
#     nfl_pr_last = qb_rating[year_id == max(year_id)],
#     td_rate_last = td_rate[year_id == max(year_id)],
#     int_rate_last = int_rate[year_id == max(year_id)],
#     bat_rate_last = bat_rate[year_id == max(year_id)],
#     hat_rate_last = hat_rate[year_id == max(year_id)],
#     ta_rate_last = ta_rate[year_id == max(year_id)],
#     fd_rate_last = fd_rate[year_id == max(year_id)],
#     pen_rate_last = pen_rate[year_id == max(year_id)],
# 
#     # Firsts
#     tds_first = touchdowns[year_id == min(year_id)],
#     off_grade_first = grades_offense[year_id == min(year_id)],
#     pass_grade_first = grades_pass[year_id == min(year_id)],
#     run_grade_first = grades_run[year_id == min(year_id)],
#     fum_grade_first = grades_hands_fumble[year_id == min(year_id)],
#     cp_first = completion_percent[year_id == min(year_id)],
#     ## Correct?
#     adj_cp_first = accuracy_percent[year_id == min(year_id)],
#     ypa_first = ypa[year_id == min(year_id)],
#     sack_rate_first = sack_percent[year_id == min(year_id)],
#     pressure_to_sack_rate_first = pressure_to_sack_rate[year_id == min(year_id)],
#     pressure_to_scramble_rate_first = pressure_to_scramble_rate[year_id == min(year_id)],
#     scramble_rate_first = scramble_rate[year_id == min(year_id)],
#     ttt_first = avg_time_to_throw[year_id == min(year_id)],
#     btt_rate_first = btt_rate[year_id == min(year_id)],
#     twp_rate_first = twp_rate[year_id == min(year_id)],
#     adot_first = avg_depth_of_target[year_id == min(year_id)],
#     nfl_pr_first = qb_rating[year_id == min(year_id)],
#     td_rate_first = td_rate[year_id == min(year_id)],
#     int_rate_first = int_rate[year_id == min(year_id)],
#     bat_rate_first = bat_rate[year_id == min(year_id)],
#     hat_rate_first = hat_rate[year_id == min(year_id)],
#     ta_rate_first = ta_rate[year_id == min(year_id)],
#     fd_rate_first = fd_rate[year_id == min(year_id)],
#     pen_rate_first = pen_rate[year_id == min(year_id)],
# 
# 
#     .groups = 'drop') |>
# 
#   left_join(nfl_qbs2, by = c('player')) |>
#   left_join(cfb_epa, by = c("player" = "passer_player_name")) |>
#   filter(player != "Taysom Hill") |>
#   select(-c(aimed_passes:attempts, completions:tas, passes_tot:plays_tot,
#             starts_with("total"), starts_with("tds")))
# 
# df_final
# 
# 
# # df_final |> filter(player == "Trinidad Chambliss")
# 
# 
# write.csv(df_final, file = "qb-college-stats-2026")




### Draft Picks

# draft_picks |> colnames()
#
# draft_picks_red = draft_picks |>
#   filter(position == "QB") |>
#   select(pfr_player_name, round, pick, age) |>
#   mutate(pfr_player_name =
#            if_else(pfr_player_name == "Gardner Minshew II",
#                    "Gardner Minshew", pfr_player_name))
#
#
#
# df_final_2 = df_final |>
#   left_join(draft_picks_red, by = c("player" = "pfr_player_name")) |>
#   # All Undrafted QBs
#   mutate(round = if_else((player == "Kyle Allen") |
#                            (player == "Nick Mullens") |
#                            (player == "Taylor Heinicke") |
#                            (player == "Cooper Rush") |
#                            (player == "Tyler Huntley"), 8, round)) |>
#   mutate(pick = if_else((player == "Kyle Allen") |
#                           (player == "Nick Mullens") |
#                           (player == "Taylor Heinicke") |
#                           (player == "Cooper Rush") |
#                           (player == "Tyler Huntley"), 300, pick)) |>
#   mutate(age = case_when((player == "Kyle Allen") ~ 22,
#                          (player == "Nick Mullens") ~ 22,
#                          (player == "Taylor Heinicke") ~ 22,
#                          (player == "Cooper Rush") ~ 23,
#                          (player == "Tyler Huntley") ~ 22,
#                          .default = age))

# write.csv(df_final_2, file = "qb-college-stats-2025")


# Data Mining -------------------------------------------------------------

### Average Starter
# nfl_qbs |> filter(plays >= 1500) |> select(nfl_pct) |> pull() |> mean()

nfl_qbs = nfl_qbs_season |> 
  filter(!is.na(qbr_pct_tot)) |> 
  select(player, qbr_pct_tot:plays_tot) |> 
  unique() |> 
  rename(nfl_pct = qbr_pct_tot,
         mean_epa = mean_epa_tot,
         mean_grade = grade_avg,
         plays = plays_tot) |> 
  arrange(-nfl_pct)


# nfl_qbs_yn = nfl_qbs_yn |> 
#   rename(player = player_display_name) |> 
#   select(player:okay)

nfl_qbs_season |> 
  filter(year == 2025, !is.na(qbr_pct_tot)) |> 
  select(player, qbr_pct_tot:plays_tot) |> 
  arrange(-qbr_pct_tot)


# Correlations


cor_df = cor(
  df_final |> 
    filter(!is.na(nfl_pct)
           # , player != "Brock Purdy"
           ) |>
    select(-player)
)

# cor_df |> View()

cor_tbl = as_tibble(cor_df) |> 
  mutate(names = colnames(cor_df)) |> 
  dplyr::select(names, everything()) |> 
  filter(names != "nfl_pct")
# filter(names != "nfl_pct_rookie")

cor_tbl |> select(names, nfl_pct) |> arrange(-abs(nfl_pct))


ggplot(cor_tbl
       |> filter(abs(nfl_pct) >= .3)
       , aes(x = abs(nfl_pct), y = reorder(names, abs(nfl_pct)))) +
  geom_col(aes(fill = nfl_pct > 0), alpha = .8) +
  labs(
    title = "Absolute Correlation with NFL Percentile",
    subtitle = "min. 0.30 abs. correlation  |  blue = positive / red = negative  |  pct = 50-50 composite of epa/play & pff grade",
    y = "Predictors",
    x = "Correlation",
    caption = "By: Sam Burch  |  Data: nflfastR, cfbfastR, & pff (2014-2025)",
    fill = element_blank()
  ) +
  scale_x_continuous(breaks = seq(0, 1, .1)) +
  scale_fill_brewer(palette = "Set1") + 
  theme(plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5, size = 6),
        axis.line = element_line(color = "black", size = 0.5),
        panel.grid.major.x = element_line(color = "lightgray", size = 0.5, linetype = 2),  # Customize vertical major grid lines
        panel.grid = element_blank(),
        panel.background = element_blank(),
        legend.position = "none")

ggsave("nfl-draft-qbs-correlations.png", width = 16, height = 12, units = "cm")


# S/F Correlations






# Best College Season

df_final |> filter(
    player == "Shedeur Sanders" |
    player == "Cam Ward"  |
    player == "Quinn Ewers" |
    player == "Jalen Milroe" |
    player == "Dillon Gabriel" |
    player == "Jaxson Dart"  |
    player == "Will Howard" |
    # player == "Kyle McCord" |
    player == "Riley Leonard" |
    player == "Kurtis Rourke" |
    player == "Tyler Shough" |
    # player == "Max Brosmer" |
    
    # player == "Joe Milton III" |
    # player == "Michael Penix Jr." |
    # player == "J.J. McCarthy" |
    !is.na(nfl_pct),
  off_grade_avg >= 70,
  mean_epa_tot >= 0
) |> 
  ggplot(aes(y = mean_epa_max, x = off_grade_max)) +
  labs(
    title = "QB College Production (Best Season)",
    subtitle = "outliers (Kyle Allen & Trevor Siemian) removed  |  blue = above average starter  | red = below average  |  gray = prospect",
    x = "PFF Grade",
    y = "EPA / Play",
    caption = "By: Sam Burch  |  Data: nflfastR, pff, and cfbfastR"
  ) +
  geom_point(aes(color = nfl_pct >= .67)) +
  geom_text_repel(size = 1.5, aes(label = player)) +
  stat_smooth(formula = y ~ x, method = 'lm', geom = 'line', se=FALSE, color='grey30', linetype = 2, alpha = .3) +
  nflplotR::geom_mean_lines(aes(y0 = mean_epa_max, x0 = off_grade_max), alpha = .3) +
  theme(
    plot.title = element_text(hjust = 0.5),
    plot.subtitle = element_text(hjust = 0.5, size = 7),
    axis.line = element_line(color = "black", size = 0.5),
    panel.grid = element_blank(),
    panel.background = element_blank(),
    legend.position = "none"
  )

# ggsave("nfl-draft-qbs-scatter-1.png", width = 16, height = 12, units = "cm")



# Overall College Production

df_final |> filter(
  player == "Shedeur Sanders" |
    player == "Cam Ward"  |
    player == "Quinn Ewers" |
    player == "Jalen Milroe" |
    player == "Dillon Gabriel" |
    player == "Jaxson Dart"  |
    player == "Will Howard" |
    # player == "Kyle McCord" |
    player == "Riley Leonard" |
    player == "Kurtis Rourke" |
    player == "Tyler Shough" |
    # player == "Max Brosmer" |
    
    # player == "Michael Penix Jr." |
    # player == "J.J. McCarthy" |
    !is.na(nfl_pct),
  off_grade_avg >= 70,
  mean_epa_tot >= 0
) |> 
  ggplot(aes(y = mean_epa_tot, x = off_grade_avg)) +
  labs(
    title = "QB College Production (Career)",
    subtitle = "outliers (Kyle Allen & Trevor Siemian) removed  |  blue = above average starter  | red = below average  |  gray = prospect",
    x = "Avg. PFF Grade",
    y = "EPA / Play",
    caption = "By: Sam Burch  |  Data: nflfastR, pff, and cfbfastR"
  ) +
  geom_point(aes(color = nfl_pct >= .67)) +
  geom_text_repel(size = 2, aes(label = player)) +
  stat_smooth(formula = y ~ x, method = 'lm', geom = 'line', se=FALSE, color='grey30', linetype = 2, alpha = .3) +
  nflplotR::geom_mean_lines(aes(y0 = mean_epa_tot, x0 = off_grade_avg), alpha = .3) +
  theme(
    plot.title = element_text(hjust = 0.5),
    plot.subtitle = element_text(hjust = 0.5, size = 7),
    axis.line = element_line(color = "black", size = 0.5),
    panel.grid = element_blank(),
    panel.background = element_blank(),
    legend.position = "none"
  )

# ggsave("nfl-draft-qbs-scatter-2.png", width = 16, height = 12, units = "cm")





# How QBs Deal with Pressure

df_final |> filter(
    player == "Shedeur Sanders" |
    player == "Cam Ward"  |
    player == "Quinn Ewers" |
    player == "Jalen Milroe" |
    player == "Dillon Gabriel" |
    player == "Jaxson Dart"  |
    player == "Will Howard" |
    # player == "Kyle McCord" |
    player == "Riley Leonard" |
    player == "Kurtis Rourke" |
    player == "Tyler Shough" |
    # player == "Max Brosmer" |
    
    # player == "Michael Penix Jr." |
    # player == "J.J. McCarthy" |
    !is.na(nfl_pct)
) |> 
  ggplot(aes(avg_ground_epa_tot, pressure_to_sack_rate_tot)) +
  labs(
    title = "How QBs Deal with Pressure in College",
    subtitle = "blue = above average starter  | red = below average  |  gray = prospect",
    y = "PTSR",
    x = "EPA / (Rush + Sack)",
    caption = "By: Sam Burch  |  Data: nflfastR, pff, and cfbfastR"
  ) +
  scale_y_reverse() +
  geom_point(aes(color = nfl_pct >= .67)) +
  geom_text_repel(size = 2, aes(label = player)) +
  stat_smooth(formula = y ~ x, method = 'lm', geom = 'line', se=FALSE, color='grey30', linetype = 2, alpha = .3) +
  nflplotR::geom_mean_lines(aes(x0 = avg_ground_epa_tot, y0 = pressure_to_sack_rate_tot), alpha = .3) +
  theme(
    plot.title = element_text(hjust = 0.5),
    plot.subtitle = element_text(hjust = 0.5, size = 8),
    axis.line = element_line(color = "black", size = 0.5),
    panel.grid = element_blank(),
    panel.background = element_blank(),
    legend.position = "none"
  )

# ggsave("nfl-draft-qbs-scatter-3.png", width = 16, height = 12, units = "cm")




# Top Correlations


df_final |> filter(
    player == "Shedeur Sanders" |
    player == "Cam Ward"  |
    player == "Quinn Ewers" |
    player == "Jalen Milroe" |
    player == "Dillon Gabriel" |
    player == "Jaxson Dart"  |
    player == "Will Howard" |
    # player == "Kyle McCord" |
    player == "Riley Leonard" |
    player == "Kurtis Rourke" |
    player == "Tyler Shough" |
    # player == "Max Brosmer" |
    
    # player == "Michael Penix Jr." |
    # player == "J.J. McCarthy" |
    !is.na(nfl_pct),
    player != "Kyle Allen", player != "Trevor Siemian"
) |> 
  ggplot(aes(mean_epa_max, avg_ground_epa_last)) +
  labs(
    title = "Top Correlators to NFL Success",
    subtitle = "outliers (Kyle Allen & Trevor Siemian) removed  |  blue = above average starter  | red = below average  |  gray = prospect",
    y = "Final Season's EPA / (Rush + Sack)",
    x = "Best EPA / Play",
    caption = "By: Sam Burch  |  Data: nflfastR, pff, and cfbfastR"
  ) +
  # scale_y_reverse() +
  geom_point(aes(color = nfl_pct >= .67)) +
  geom_text_repel(size = 2, aes(label = player)) +
  stat_smooth(formula = y ~ x, method = 'lm', geom = 'line', se=FALSE, color='grey30', linetype = 2, alpha = .3) +
  nflplotR::geom_mean_lines(aes(x0 = mean_epa_max, y0 = avg_ground_epa_tot), alpha = .3) +
  theme(
    plot.title = element_text(hjust = 0.5),
    plot.subtitle = element_text(hjust = 0.5, size = 7),
    axis.line = element_line(color = "black", size = 0.5),
    panel.grid = element_blank(),
    panel.background = element_blank(),
    legend.position = "none"
  )

# ggsave("nfl-draft-qbs-scatter-4.png", width = 16, height = 12, units = "cm")





# Modeling ----------------------------------------------------------------

df_final_no_name = df_final |> filter(!is.na(nfl_pct)
                                      # , player != "Brock Purdy"
                                      ) |>  select(-player)

df_final_no_name

n = nrow(df_final_no_name)


set.seed(123)

# Need Training & Testing to be decent size
## Changed from 80% to get large enough testing sample
train_ind = sample(1:n, .5*n)


train_data = df_final_no_name[train_ind, ]
test_data = df_final_no_name[-train_ind, ]


# Average

(avg_nfl_pct = train_data |> select(nfl_pct) |> pull() |> mean())
sqrt(mean((rep(avg_nfl_pct, ceiling(.5*n)) - test_data |> select(nfl_pct) |> pull())^2))
# .37 rmse


# Linear Regression

top_predictors = cor_tbl |>
  arrange(-nfl_pct) |> 
  filter(abs(nfl_pct) >= .25) |> 
  select(names) |> 
  pull()


m1a = lm(nfl_pct ~ 1, data = train_data)
summary(m1a)

m1b = lm(nfl_pct ~ ., data = train_data[, c(top_predictors, "nfl_pct")])
summary(m1b)


m1 = step(m1a, direction = "both", scope = formula(m1b), trace = 0)
summary(m1)
## r^2 = 0.46

pred = predict(m1, newdata = test_data)

sqrt(mean((pred - test_data |> select(nfl_pct) |> pull())^2))
## 0.32

sqrt(vif(m1))
## Fine


## All
par(mfrow = c(2, 2))
plot(m1)

## HIPs
sort(cooks.distance(m1), decreasing = TRUE)[1:10]
### All good, since less than 1.

## Constant Variance
bptest(m1)
### With the p-value = .13 > .05 = $\alpha$, we fail to reject the null and conclude the constant variance assumption is satisfied.

## Normaility, n < 50
shapiro.test(m1$residuals)
### With the p-value = .38 > .05 = $\alpha$, we fail to reject the null and conclude the normality assumption is satisfied.


# Fine



# LR 2


top_predictors = cor_tbl |>
  arrange(-nfl_pct) |> 
  filter(abs(nfl_pct) >= .3) |> 
  select(names) |> 
  pull()

m2a = lm(nfl_pct ~ 1, data = train_data)
summary(m2a)

m2b = lm(nfl_pct ~ ., data = train_data[, c(top_predictors, "nfl_pct")])
summary(m2b)


m2 = step(m2a, direction = "both", scope = formula(m2b), trace = 0)
summary(m2)
## r^2 = 0.29

pred = predict(m2, newdata = test_data)

sqrt(mean((pred - test_data |> select(nfl_pct) |> pull())^2))
# 0.35

sqrt(vif(m2))
## Okay


## All
par(mfrow = c(2, 2))
plot(m2)

## HIPs
sort(cooks.distance(m2), decreasing = TRUE)[1:10]
### All good, since less than 1.

## Constant Variance
bptest(m2)
### With the p-value = .15 > .05 = $\alpha$, we fail to reject the null and conclude the constant variance assumption is satisfied.

## Normaility, n < 50
shapiro.test(m2$residuals)
### With the p-value = .85 > .05 = $\alpha$, we fail to reject the null and conclude the normality assumption is satisfied.

# Good!





### Lasso



set.seed(123)

x_train = as.matrix(train_data |> select(-nfl_pct))
y_train = as.matrix(train_data$nfl_pct)

x_test = as.matrix(test_data |> select(-nfl_pct))
y_test = as.matrix(test_data$nfl_pct)



lasso_model = glmnet(x_train, y_train, alpha = 1)


# Perform cross-validation to select lambda
cv_lasso = cv.glmnet(x_train, y_train, alpha = 1)  # alpha = 1 for Lasso regression
par(mfrow = c(1, 1))
plot(cv_lasso)

# Print optimal lambda value
print(cv_lasso$lambda.min)

coefficients = coef(lasso_model, s = cv_lasso$lambda.min)
coefficients

# Example predictions

pred = predict(lasso_model, newx = x_test, s = cv_lasso$lambda.min)

sqrt(mean((pred - y_test)^2))
## 0.36



### KNN
# Error!!!


# Changed from 35
error = numeric(35)

set.seed(123)

for (i in 1:35) {
  
  knn.fit = kknn(nfl_pct ~ ., train = train_data, 
                 test = test_data |> dplyr::select(-nfl_pct),
                 k = i, kernel = "rectangular")
  
  test.pred = knn.fit$fitted.values
  
  error[i] = sqrt(mean((test.pred - (test_data |> dplyr::select(nfl_pct) |> pull()))^2))
  
  
}

min(error)


which.min(error)


### Random Forrest


set.seed(123)
# Random Forests


# train_data$qbr_nfl
m1 = rfsrc(nfl_pct ~ ., data = as.data.frame(train_data))
# OOB Error Rate
tail(m1$err.rate, 1)


tuning_grid = expand.grid(mtry = c(1, 5, 10, 15, 20), nodesize = c(1, 5, 10, 15, 20))
tuned_models = vector(mode = "list", length = 25)
oob_error_rates = numeric(25)
set.seed(123)
for (i in 1:nrow(tuning_grid)) {
  rf_model = rfsrc(nfl_pct ~ ., data = as.data.frame(train_data),
                   mtry = tuning_grid[i, 1], nodesize = tuning_grid[i, 2])
  tuned_models[[i]] = rf_model
  oob_error_rates[i] = tail(rf_model$err.rate, 1)
}
# OOB ER for each model
oob_error_rates


# Find the index of the minimum OOB error rate
best_index = which.min(oob_error_rates)
# Best tuning parameters
best_tuning = tuning_grid[best_index, ]
best_tuning

# Extract the random forest model with the best tuning parameters
best_rf_model = tuned_models[[best_index]]
best_rf_model
## r^2 = 0.03


# Calculate the variable importance for the best model
variable_importance = vimp(best_rf_model)
sort(abs(variable_importance$importance), decreasing = TRUE)



pred_rf = predict(best_rf_model, newdata = as.data.frame(test_data))

sqrt(mean((pred_rf$predicted - as.vector(y_test))^2))
## 0.36


### XGBoost

x_train = as.matrix(train_data |> select(-nfl_pct))
y_train = as.matrix(train_data$nfl_pct)
x_test = as.matrix(test_data |> select(-nfl_pct))
y_test = as.matrix(test_data$nfl_pct)



set.seed(123)

train_data_xgb = xgb.DMatrix(data = data.matrix(train_data |> select(-nfl_pct)), label = train_data$nfl_pct)
test_data_xgb = xgb.DMatrix(data = data.matrix(test_data |> select(-nfl_pct)), label = test_data$nfl_pct)

params = list(
  objective = "reg:squarederror",
  # num_class = 10, # Number of classes
  eta = 0.5, # Learning rate
  max_depth = 2 # Maximum depth of trees
)

num_round = 50


xgb.fit = xgb.train(params, train_data_xgb, num_round)


pred_xgb = predict(xgb.fit, newdata = test_data_xgb)

sqrt(mean((pred_xgb - as.vector(y_test))^2))
## Bad




tuning_grid2 = expand.grid(eta = c(0.1, 0.5, 1.0), max_depth = c(2, 5, 10))


best_eta = c(numeric(9))
best_max_depth = c(numeric(9))
best_ntrees = c(numeric(9))
best_error = c(rep(1, 9))

set.seed(123)

# Loop over the search space
for (i in 1:nrow(tuning_grid2)) {
  # Set the xgboost parameters
  params = list(
    objective = "reg:squarederror",
    # num_class = 10,
    eta = tuning_grid2[i, 1],
    max_depth = tuning_grid2[i, 2]
  )
  
  train_data_xgb = xgb.DMatrix(data = data.matrix(train_data |> select(-nfl_pct)), label = train_data$nfl_pct)
  test_data_xgb = xgb.DMatrix(data = data.matrix(test_data |> select(-nfl_pct)), label = test_data$nfl_pct)
  
  num_round = 50
  
  bst = xgb.train(params, train_data_xgb, num_round)
  
  for (j in 1:50) {
    pred_xgb = predict(bst, test_data_xgb, iterationrange = c(1, j))
    
    # sqrt(mean((pred_xgb - as.vector(y_test))^2))
    # Calculate the testing error
    error = sqrt(mean((pred_xgb - as.vector(y_test))^2))
    
    # Update the best parameters and the corresponding testing error
    if (error < best_error[i]) {
      best_eta[i] = tuning_grid2[i, 1]
      best_max_depth[i] = tuning_grid2[i, 2]
      best_ntrees[i] = j
      best_error[i] = error
    }
    
  }
}

(results = data.frame(best_eta, best_max_depth, best_ntrees, best_error))

# Best Error

results[which.min(best_error), ]
## 0.33

# set.seed(123)
# 
# train_data_xgb = xgb.DMatrix(data = data.matrix(train_data |> select(-nfl_pct)), label = train_data$nfl_pct)
# test_data_xgb = xgb.DMatrix(data = data.matrix(test_data |> select(-nfl_pct)), label = test_data$nfl_pct)
# 
# params = list(
#   objective = "reg:squarederror",
#   # num_class = 10, # Number of classes
#   eta = results[which.min(best_error), 1], # Learning rate
#   max_depth = results[which.min(best_error), 2] # Maximum depth of trees
# )
# 
# 
# xgb.fit = xgb.train(params, train_data_xgb, results[which.min(best_error), 3])
# 
# 
# pred_xgb = predict(xgb.fit, newdata = test_data_xgb)
# 
# sqrt(mean((pred_xgb - as.vector(y_test))^2))
# 
# 
# (xgb_var_importance = xgb.importance(colnames(train_data_xgb), model = xgb.fit))
# 
# 
# par(mfrow = c(1, 1))
# 
# xgb.plot.importance(xgb_var_importance)




# Model Selection...




# Predictions -------------------------------------------------------------

df_final |> View()

df_final_no_nfl = df_final |> 
  filter(is.na(nfl_pct))

projections = predict(m1, newdata = df_final_no_nfl |> select(-player, -nfl_pct))

summary(m1)

teams = draft_picks |> 
  select(pfr_player_name, team) |> 
  mutate(team = clean_team_abbrs(team),
         pfr_player_name = if_else(pfr_player_name == "Michael Penix", 
                                   "Michael Penix Jr.", pfr_player_name))


# Fix NAs in college dataset!!!
top_upcoming_qbs = df_final_no_nfl |> 
  mutate(nfl_pct_proj = projections) |>
  filter(player == "Fernando Mendoza"  |
         player == "Ty Simpson"  |
         player == "Garrett Nussmeier"  |
         player == "Cade Klubnik"  |
         player == "Drew Allar"  |
         player == "Carson Beck"  |
         player == "Sawyer Robertson"  |
         player == "Luke Altmyer"  |
         # player == "Cole Payton"  |
         player == "Taylen Green") |> 
  # left_join(teams, by = c("player" = "pfr_player_name")) |> 
  # select(player, nfl_pct_proj, pressure_to_sack_rate_tot, adot_first) |>
  select(player, nfl_pct_proj, ypa_tot, off_grade_avg, td_rate_max,  
         pressure_to_sack_rate_first, scramble_rate_max, dropbacks) |>
  arrange(-nfl_pct_proj)
top_upcoming_qbs

summary(m1)

ggplot(top_upcoming_qbs, aes(y = reorder(player, nfl_pct_proj), x = nfl_pct_proj)) +
  labs(
    y = element_blank(),
    x = 'NFL Percentile Projection',
    title = '2026 New QB Projections',
    subtitle = "rmse = .32  |  gray line is average starter  |  linear regression based on efficiency, experience, sack avoidance, and rushing  |  pct = 50-50 compsite of EPA/play and PFF grade",
    caption = "By: Sam Burch  |  Data: nflfastR & pff"
  ) +
  scale_x_continuous(breaks = seq(0, 1, .1)) +
  geom_col(aes(color = player, fill = player), alpha = .8, width = 1) +
  # nflplotR::scale_fill_nfl(type = "primary") +
  # nflplotR::scale_color_nfl(type = "secondary") +
  # nflplotR::geom_nfl_logos(aes(team_abbr = team), width = .05, alpha = .8) +
  geom_vline(xintercept =  .64, color = 'grey30', linetype = 2) +
  theme(
    plot.title = element_text(hjust = 0.5),
    plot.subtitle = element_text(hjust = 0.5, size = 5),
    plot.caption = element_text(size = 6),
    axis.line = element_line(color = "black", size = 0.5),
    axis.text.x = element_text(size = 7),
    axis.text.y = element_text(size = 7),
    panel.grid = element_blank(),
    panel.background = element_blank(),
    legend.position = "none"
  )

ggsave("qb_proj_26.png", width = 16, height = 9, units = "cm")



qb_proj_table = top_upcoming_qbs |>
  # gt() |>
  mutate(nfl_pct_proj = nfl_pct_proj*100,
         td_rate_max = td_rate_max*100,
         scramble_rate_max = scramble_rate_max*100) |> 
  mutate(round(across(nfl_pct_proj:scramble_rate_max), 1)) |> 
  mutate(dropbacks = round(dropbacks, -1)) |>
  # mutate(total_epa_tot = round(total_epa_tot)) 

# |> 
  # select(player:pressure_to_sack_rate_tot) |>
  # mutate(plays_tot = round(plays_tot, -1)) |>
  gt() |>
  tab_header(title = "2026 Draft QB Projections") |>
  tab_footnote(
    footnote = "Ranking is ordered by projected NFL percentile  |  50-50 composite of epa/play and grade.",
    locations = cells_column_labels(columns = nfl_pct_proj),
  ) |>
  tab_footnote(
    footnote = "Model predictor",
    locations = cells_column_labels(columns = td_rate_max:dropbacks),
  ) |>
  # tab_footnote(
  #   footnote = "PTSR = pressure-to-sack-rate, lower is better",
  #   locations = cells_column_labels(columns = pressure_to_sack_rate_tot),
  # ) |>
  tab_footnote("By: Sam Burch  |  Data: nflfastR & pff") |>
  cols_label(
    player = "Player",
    nfl_pct_proj = "Percentile",
    ypa_tot = "YPA",
    off_grade_avg = "Average PFF Grade",
    td_rate_max = "TD Rate (Best Season)",
    pressure_to_sack_rate_first = "PTSR (First Season)",
    scramble_rate_max = "Scramble Rate (Highest Season)",
    dropbacks = "Career Dropbacks"
    # avg_ground_epa_tot = "EPA / (Rush + Sack)",
    # total_epa_tot = "Total EPA",
    # pressure_to_sack_rate_tot = "PTSR"
  ) |>
  cols_align("center"
             , columns = nfl_pct_proj:dropbacks
  )

  # gt_theme_espn()

gtsave(qb_proj_table, filename = "qb-proj-table.png"
       # , expand = 50
)





# Logistic Modeling -------------------------------------------------------


nfl_qbs_yn



df_final2 = df_final |> left_join(nfl_qbs_yn, by = c("player" = "player_display_name"))

df_final_no_name2 = df_final2 |> 
  filter(!is.na(okay)) |> 
  select(-player_id, -player, -nfl_pct)

df_final_no_name2 |> colnames()

n = nrow(df_final_no_name2)

set.seed(123)

# Need Training & Testing to be decent size
## 60% to get more reliable large training set & bigger testing set
train_ind = sample(1:n, .6*n)

train_data_great = df_final_no_name2[train_ind, ] |> select(-okay)
test_data_great = df_final_no_name2[-train_ind, ] |> select(-okay)

train_data_ok = df_final_no_name2[train_ind, ] |> select(-great)
test_data_ok = df_final_no_name2[-train_ind, ] |> select(-great)

train_data_great |> nrow()

top_predictors = cor_tbl |>
  arrange(-nfl_pct) |> 
  filter(abs(nfl_pct) >= .35) |> 
  select(names) |> 
  pull()

top_predictors




m1a = glm(as.formula(paste("great ~", paste(top_predictors, collapse = " + "))), 
          family = "binomial", data = train_data_great, na.action = na.omit)
summary(m1a)


m1 = step(m1a, direction = "both", trace = 0)
summary(m1)

predict.glmnet(m1, newx = test_data_great |> select(-great))

# Convert to classes (0/1 or 1/2 depending on your coding)
pred_class <- ifelse(pred_probs > 0.5, 1, 0)

# Compare to actual:
table(Predicted = pred_class, Actual = y)


