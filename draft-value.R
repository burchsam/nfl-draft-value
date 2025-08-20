library(tidyverse)
library(nflreadr)



draft_df = read_csv("coding-projects/nfl-fast-r/draft_value_99")[, -1]

draft_picks = load_draft_picks()



# Creating Draft Value Dataset --------------------------------------------



draft_picks = load_draft_picks()

draft_value = read_csv("coding-projects/college-football/fs-draft-value-chart.csv")

draft_df = draft_picks |>
  filter(season >= 2000) |>
  left_join(draft_value, by = c("pick" = "Pick")) |>
  rename(value = Value) |>
  mutate(team = clean_team_abbrs(team),
         value = if_else(pick >= 257, 100, value)) |>
  mutate(qb_value = if_else(category == "QB", value, 0),
         tier1_value = if_else(category == "DL" | category == "OL", value, 0),
         tier2_value = if_else(category == "WR" | category == "DB", value, 0),
         tier3_value = if_else(category == "RB" | category == "TE" |
                                 category == "LB" | category == "OG" |
                                 category == "FS", value, 0),
         tier4_value = if_else(category == "P" | category == "K" |
                                 category == "KR" | category == "LS", value, 0)
  ) |>
  group_by(team, season) |>
  arrange(season) |>
  summarise(draft_value_added = sum(value),
            dv_qb = sum(qb_value),
            dv_t1 = sum(tier1_value),
            dv_t2 = sum(tier2_value),
            dv_t3 = sum(tier3_value),
            dv_t4 = sum(tier4_value),
            .groups = "drop") |>
  arrange(season, -draft_value_added) |>
#  Helps with joining with other datasets
  mutate(season = season - 1)

# write.csv(draft_df, file = "draft_value_99.csv")



# Draft Analysis -----------------------------------------------------


draft_df |> filter(season == 2024) |> arrange(-draft_value_added)


ggplot(draft_df |> filter(season == 2024), 
       aes(x = draft_value_added, y = reorder(team, draft_value_added))) +
  labs(
    title = "NFL Draft Value Added (2025)",
    subtitle = "based on Fitzgerald-Spielberger chart",
    caption = "By: Sam Burch  |  Data @nflfastR",
    x = "Draft Value Added"
  ) +
  geom_col(aes(color = team, fill = team), alpha = .8, width = 1) +
  nflplotR::scale_fill_nfl(type = "primary") +
  nflplotR::scale_color_nfl(type = "secondary") +
  theme(
    plot.subtitle = element_text(size = 8, hjust = .5),
    plot.title = element_text(hjust = 0.5),
    panel.grid.major.y = element_blank(),  # Remove horizontal major grid lines
    panel.grid.minor.y = element_blank(),  # Remove horizontal minor grid lines
    panel.grid.major.x = element_line(color = "lightgray", size = 0.5, linetype = 2),  # Customize vertical major grid lines
    panel.grid.minor.x = element_blank(),
    panel.background = element_blank(),
    axis.title.y = element_blank(),
    axis.text.y = element_blank(),
    axis.ticks.y = element_blank()
  ) +
  nflplotR::geom_nfl_logos(aes(team_abbr = team), width = .03, alpha = .8)

# ggsave("nfl-draft-value-added-25.png", width = 16, height = 12, units = "cm")


# More charts...
## Breakdown by Position Tier
## Breakdown by Position
## Other???


draft_df |> filter(season == 2024) |> arrange(-dv_t4)



draft_df |> filter(season == 2024)


ggplot(draft_df |> filter(season == 2024), aes(x = dv_qb + dv_t1 + dv_t2, y = dv_t3)) +
  labs(x = "Premium Positions",
       y = "Non-Premium Positions",
       title = "How NFL Teams Spent their Draft Capital",
       ### CHANGE!!!
       subtitle = "premium positions = QB, DL, OT, WR, or CB  |  uses Fitzgerald-Spielberger chart",
       caption = "By: Sam Burch  |  Data @nflfastR") +
  theme(
    plot.title = element_text(hjust = 0.5),
    plot.subtitle = element_text(hjust = 0.5, size = 6),
    axis.line = element_line(color = "black", size = 0.5),
    panel.grid = element_blank(),
    panel.background = element_blank()
  ) +
  scale_y_reverse() +
  nflplotR::geom_nfl_logos(aes(team_abbr = team), width = .07, alpha = .8) +
  stat_smooth(formula = y ~ x, method = 'lm', geom = 'line', se=FALSE, color='gray') +
  nflplotR::geom_mean_lines(aes(x0 = (dv_qb + dv_t1 + dv_t2), y0 = dv_t3))

# ggsave("nfl-draft-25.png", width = 16, height = 12, units = "cm")


