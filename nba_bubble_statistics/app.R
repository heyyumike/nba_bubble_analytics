library(shiny)
library(tidyverse)
library(DT)
library(gridExtra)
library(rlang)
library(scales)

# stats courtesy of stats.nba.com
# 5 different categories of stats: traditional, advanced, four factors, miscellaneous, and scoring
# initially imported as a .xlsx under different sheets then combined into csv
bubble_stats <- read_csv("nba_bubble_stats.csv", 
                             col_types = cols(GAME_DATE = col_date(format = "%m/%d/%y")))

# NBA team logos courtesy of sportslogos.net
bubble_stats <- bubble_stats %>% mutate(
  team_logo = case_when(
    TEAM == "OKC" ~ normalizePath(file.path("./nba_logos/Oklahoma City Thunder.png")),
    TEAM == "TOR" ~ normalizePath(file.path("./nba_logos/Toronto Raptors.png")),
    TEAM == "IND" ~ normalizePath(file.path("./nba_logos/Indiana Pacers.png")),
    TEAM == "LAC" ~ normalizePath(file.path("./nba_logos/Los Angeles Clippers.png")),
    TEAM == "MIA" ~ normalizePath(file.path("./nba_logos/Miami Heat.png")),
    TEAM == "DEN" ~ normalizePath(file.path("./nba_logos/Denver Nuggets.png")),
    TEAM == "PHI" ~ normalizePath(file.path("./nba_logos/Philadelphia 76ers.png")),
    TEAM == "HOU" ~ normalizePath(file.path("./nba_logos/Houston Rockets.png")),
    TEAM == "LAL" ~ normalizePath(file.path("./nba_logos/Los Angeles Lakers.png")),
    TEAM == "MEM" ~ normalizePath(file.path("./nba_logos/Memphis Grizzlies.png")),
    TEAM == "WAS" ~ normalizePath(file.path("./nba_logos/Washington Wizards.png")), 
    TEAM == "SAC" ~ normalizePath(file.path("./nba_logos/Sacramento Kings.png")),
    TEAM == "SAS" ~ normalizePath(file.path("./nba_logos/San Antonio Spurs.png")),
    TEAM == "DAL" ~ normalizePath(file.path("./nba_logos/Dallas Mavericks.png")),
    TEAM == "BOS" ~ normalizePath(file.path("./nba_logos/Boston Celtics.png")),
    TEAM == "NOP" ~ normalizePath(file.path("./nba_logos/New Orleans Pelicans.png")),
    TEAM == "BKN" ~ normalizePath(file.path("./nba_logos/Brooklyn Nets.png")),
    TEAM == "UTA" ~ normalizePath(file.path("./nba_logos/Utah Jazz.png")),
    TEAM == "PHX" ~ normalizePath(file.path("./nba_logos/Phoenix Suns.png")),
    TEAM == "ORL" ~ normalizePath(file.path("./nba_logos/Orlando Magic.png")),
    TEAM == "POR" ~ normalizePath(file.path("./nba_logos/Portland Trail Blazers.png")),
    TEAM == "MIL" ~ normalizePath(file.path("./nba_logos/Milwaukee Bucks.png"))
  ),
  home_away = ifelse(grepl("@", MATCH_UP), "AWAY", "HOME"),
  conference = ifelse(TEAM %in% c("OKC", "LAC", "DEN", "HOU", "LAL", "MEM", "SAC", "SAS", "DAL", "NOP", "UTA", "PHX", "POR"), "West", "East"),
  full_team_name = case_when(
    TEAM == "OKC" ~ "Oklahoma City Thunder",
    TEAM == "TOR" ~ "Toronto Raptors",
    TEAM == "IND" ~ "Indiana Pacers",
    TEAM == "LAC" ~ "Los Angeles Clippers",
    TEAM == "MIA" ~ "Miami Heat",
    TEAM == "DEN" ~ "Denver Nuggets",
    TEAM == "PHI" ~ "Philadelphia 76ers",
    TEAM == "HOU" ~ "Houston Rockets",
    TEAM == "LAL" ~ "Los Angeles Lakers",
    TEAM == "MEM" ~ "Memphis Grizzlies",
    TEAM == "WAS" ~ "Washington Wizards", 
    TEAM == "SAC" ~ "Sacramento Kings",
    TEAM == "SAS" ~ "San Antonio Spurs",
    TEAM == "DAL" ~ "Dallas Mavericks",
    TEAM == "BOS" ~ "Boston Celtics",
    TEAM == "NOP" ~ "New Orleans Pelicans",
    TEAM == "BKN" ~ "Brooklyn Nets",
    TEAM == "UTA" ~ "Utah Jazz",
    TEAM == "PHX" ~ "Phoenix Suns",
    TEAM == "ORL" ~ "Orlando Magic",
    TEAM == "POR" ~ "Portland Trail Blazers",
    TEAM == "MIL" ~ "Milwaukee Bucks"
  ),
  `2PM` = (PTS - FTM - (`3PM` * 3)) / 2,
  `2PTM%` = `2PM` / FGM,
  `3PTM%` = `3PM` / FGM,
  `FG%` = `FG%` / 100, `TS%` = `TS%` / 100, `FT%` = `FT%` / 100, `3P%` = `3P%` / 100
)

# variables that are measured as a percentage will need a different axis when graphed
percent_variables <- c("TS%", "FT%", "FG%", "EFG%", "3PTM%", "3P%", "2PTM%")
# list of variables available 
nba_variables <- c("PTS", "PTS OFF TO", "PITP", "FBPS", "2ND PTS", "TOV", "STL", "REB", "OREB", "DREB", "BLK", "AST/TO", "AST",
                   "FG%", "TS%", "FT%", "EFG%", "3PTM%", "3P%", "2PTM%")

ui <- fluidPage(
    titlePanel(title = "2019-2020 NBA Bubble Statistics"),
    
    sidebarLayout(
      sidebarPanel(width = 2,
        imageOutput(outputId = "team_photo",
                    height = 175),
        
        selectInput(inputId = "team_select",
                    label = "NBA Team",
                    choices = unique(bubble_stats$full_team_name),
                    selected = "Boston Celtics"),
        
        dateRangeInput(inputId = "date_range",
                       label = "Date range",
                       start = min(bubble_stats$GAME_DATE),
                       end = max(bubble_stats$GAME_DATE),
                       min = min(bubble_stats$GAME_DATE),
                       max = max(bubble_stats$GAME_DATE)),
        
        selectInput(inputId = "statistic",
                    label = "Metric",
                    choices = all_of(nba_variables)),
        
        helpText(
          p("Visit",
            a("NBA Stat Glossary",
              href = "https://stats.nba.com/help/glossary/"), "for detailed explanation of metrics provided")
        ),
        
        radioButtons(inputId = "other_teams",
                     label = "Conference/Other Teams",
                     choices = c("Conference", "Other Teams"))),
      
      mainPanel(
        plotOutput("metric_rank_stats"),
        plotOutput("daily_team_stats"),
        plotOutput("points_distribution_ratings"),
        dataTableOutput("team_stats")
      )
    )
)

server <- function(input, output) {
  selected_team_conference <- reactive(bubble_stats %>% filter(full_team_name == input$team_select) %>% distinct(conference, .keep_all = FALSE) %>% pull())
  daily_team_stats <- reactive(bubble_stats %>% filter(full_team_name == input$team_select, GAME_DATE >= input$date_range[1], GAME_DATE <= input$date_range[2]))
  
  # shooting stats and selected team's respective ranks 
  shooting_team_stats <- reactive(bubble_stats %>% filter(GAME_DATE >= input$date_range[1], GAME_DATE <= input$date_range[2]) %>% group_by(full_team_name) %>% 
    summarise(fgm = sum(FGM), fga = sum(FGA), fg_percent = fgm / fga, `3pm` = sum(`3PM`), `3pa` = sum(`3PA`), `3p%` = `3pm` / `3pa`,
              ftm = sum(FTM), fta = sum(FTA), ft_percent = ftm / fta, efg_percent = (fgm + 0.5 * `3pm`) / fga, pts = sum(PTS),
              true_shooting_percent = (pts * 100) / (2 * (fga + (0.44 * fta))), `2pm` = sum(`2PM`),
              `2pm%` = `2pm` / fgm, `3pm%` = `3pm` / fgm) %>%
    mutate(`FG%` = rank(-fg_percent), `3P%` = rank(-`3p%`), `FT%` = rank(-ft_percent), `EFG%` = rank(-efg_percent), `TS%` = rank(-true_shooting_percent),
           `2PTM%` = rank(-`2pm%`), `3PTM%` = rank(-`3pm%`)) %>%
    filter(full_team_name == input$team_select) %>% select(full_team_name, 17:23) %>% gather(key = "Metric", value = "Rank", -full_team_name))
  
  # scoring stats rank
  scoring_team_stats <- reactive(bubble_stats %>% filter(GAME_DATE >= input$date_range[1], GAME_DATE <= input$date_range[2]) %>% group_by(full_team_name) %>%
    summarise(ppg = sum(PTS) / 8, fbps_g = sum(FBPS) / 8, pot_g = sum(`PTS OFF TO`) / 8, pitp_g = sum(PITP) / 8, `2nd_pts_g` = sum(`2ND PTS`) / 8) %>%
    mutate(PPG = rank(-ppg), FBPS = rank(-fbps_g), PoT = rank(-pot_g), PITP = rank(-pitp_g), `2ND PTS` = rank(-`2nd_pts_g`)) %>%
    filter(full_team_name == input$team_select) %>% select(full_team_name, 7:11) %>% gather(key = "Metric", value = "Rank", -full_team_name))
  
  # miscellaneous stats rank
  other_team_stats <- reactive(bubble_stats %>% filter(GAME_DATE >= input$date_range[1], GAME_DATE <= input$date_range[2]) %>% group_by(full_team_name) %>%
    summarise(oreb_g = sum(OREB) / 8, dreb_g = sum(DREB) / 8, reb_g = sum(REB) / 8, ast_g = sum(AST) / 8, tov_g = sum(TOV) / 8, stl_g = sum(STL) / 8, 
              blk_g = sum(BLK) / 8, ast_tov_g = ast_g / tov_g) %>%
    mutate(OREB = rank(-oreb_g), DREB = rank(-dreb_g), REB = rank(-reb_g), AST = rank(-ast_g), TOV = rank(-tov_g), STL = rank(-stl_g), BLK = rank(-blk_g), `AST/TOV` = rank(-ast_tov_g)) %>%
    filter(full_team_name == input$team_select) %>% select(full_team_name, 10:17) %>% gather(key = "Metric", value = "Rank", -full_team_name))
  
  # using NBA hex colors
  pie_chart_colors <- c("#17408B", "#FFFFFF", "#C9082A")
  
  # percentage of points that come from various shots (free throw, 2 pointer, and 3 pointer)
  team_shot_percentages <- reactive(bubble_stats %>% filter(GAME_DATE >= input$date_range[1], GAME_DATE <= input$date_range[2], full_team_name == input$team_select) %>% 
    mutate(`2PM` = `2PM` * 2, `3PM` = `3PM` * 3) %>% select(`3PM`, `2PM`, FTM) %>% gather(key = "shot_type", value = "value") %>% group_by(shot_type) %>% summarise(number_of_points = sum(value)) %>%
    mutate(point_percent = number_of_points / sum(number_of_points)))
  
  # offensive and defensive ratings for selected team on selected game dates
  off_vs_def_ratings <- reactive(bubble_stats %>% filter(GAME_DATE >= input$date_range[1], GAME_DATE <= input$date_range[2], full_team_name == input$team_select) %>% select(GAME_DATE, OFFRTG, DEFRTG) %>%
    gather(key = "metric", value = "rating", -GAME_DATE))
  
  output$daily_team_stats <- renderPlot(
    {
      validate(
        need(dim(shooting_team_stats())[1] != 0, paste0("No data for the ", input$team_select, " for selected date range.")),
        need(input$date_range[1] <= input$date_range[2], "Please select a valid date range.")
      )
      
      # separate based on whether compared to conference (west or east) of selected team or vs. rest of NBA teams in bubble 
      # changing axis to percent based on whether selected variable is within percent_variables 
      if (input$other_teams == "Conference") {
        team_vs_conference <- reactive(bubble_stats %>% filter(conference == selected_team_conference(), GAME_DATE >= input$date_range[1], GAME_DATE <= input$date_range[2]) %>% 
          mutate(vs_conference = ifelse(full_team_name == input$team_select, input$team_select, paste0("Rest of ", selected_team_conference(), "ern Conference"))) %>%
          group_by(vs_conference) %>% summarise(avg_stat = mean(!! sym(input$statistic))))
        if (input$statistic %in% percent_variables) {
          p1 <- ggplot(data = team_vs_conference(), aes(x = vs_conference, y = avg_stat, label = percent(avg_stat, accuracy = 0.01), fill = vs_conference)) + geom_bar(stat = "identity") + ylab(paste0("Avg ", input$statistic, " per game")) +
            scale_y_continuous(limits = c(0,1), labels = percent) + scale_fill_manual(values = c("#17408B", "#C9082A")) + guides(fill=FALSE) + xlab("") + theme_minimal() + geom_text(vjust = -0.5) + ggtitle(paste0("Avg ", input$statistic, " per game Comparison"))
          p2 <- ggplot(data = daily_team_stats(), aes(x = GAME_DATE, y = !! sym(input$statistic), color = `W/L`, label = percent(!! sym(input$statistic), accuracy = 0.01))) + geom_point(size = 4) + ylab(input$statistic) +
            scale_y_continuous(limits = c(0,1), labels = percent) + scale_color_manual(values = c("W" = "#17408B", "L" = "#C9082A")) + scale_x_date(name = "Game Date", date_breaks = "1 day", labels = date_format(format = "%m/%d")) + geom_text(vjust = -1, color = "black") + theme_minimal() + ggtitle(paste0(input$statistic, " by Game"))
          grid.arrange(p1, p2, ncol = 2)
        } else {
          p1 <- ggplot(data = team_vs_conference(), aes(x = vs_conference, y = avg_stat, label = comma(avg_stat, accuracy = 0.01), fill = vs_conference)) + geom_bar(stat = "identity") + ylab(paste0("Avg ", input$statistic, " per game")) +
            expand_limits(y = 0) + geom_text(vjust = -0.5) + scale_fill_manual(values = c("#17408B", "#C9082A")) + guides(fill=FALSE) + xlab("") + theme_minimal() + ggtitle(paste0("Avg ", input$statistic, " per game Comparison"))
          p2 <- ggplot(data = daily_team_stats(), aes(x = GAME_DATE, y = !! sym(input$statistic), color = `W/L`, label = !! sym(input$statistic))) + geom_point(size = 4) + ylab(input$statistic) +
            expand_limits(y = 0) + geom_text(vjust = -1, color = "black") + scale_color_manual(values = c("W" = "#17408B", "L" = "#C9082A")) + scale_x_date(name = "Game Date", date_breaks = "1 day", labels = date_format(format = "%m/%d")) + theme_minimal() + ggtitle(paste0(input$statistic, " by Game"))
          grid.arrange(p1, p2, ncol = 2)
        }
      } else {
        team_vs_others <- reactive(bubble_stats %>% filter(GAME_DATE >= input$date_range[1], GAME_DATE <= input$date_range[2]) %>% mutate(vs_others = ifelse(full_team_name == input$team_select, input$team_select, "Rest of NBA Bubble Teams")) %>% 
          group_by(vs_others) %>% summarise(avg_stat = mean(!! sym(input$statistic))))
        if (input$statistic %in% percent_variables) {
          p1 <- ggplot(data = team_vs_others(), aes(x = vs_others, y = avg_stat, label = percent(avg_stat, accuracy = 0.01), fill = vs_others)) + geom_bar(stat = "identity") + scale_y_continuous(limits = c(0,1), labels = percent) + ylab(paste0("Avg ", input$statistic, " per game")) +
            scale_fill_manual(values = c("#17408B", "#C9082A")) + guides(fill=FALSE) + theme_minimal() + geom_text(vjust = -0.5) + xlab("") + ggtitle(paste0("Avg ", input$statistic, " per game Comparison"))
          p2 <- ggplot(data = daily_team_stats(), aes(x = GAME_DATE, y = !! sym(input$statistic), color = `W/L`, label = percent(!! sym(input$statistic), accuracy = 0.01))) + geom_point(size = 4) + 
            scale_y_continuous(limits = c(0,1), labels = percent) + scale_color_manual(values = c("W" = "#17408B", "L" = "#C9082A")) + scale_x_date(name = "Game Date", date_breaks = "1 day", labels = date_format(format = "%m/%d")) + theme_minimal() + geom_text(vjust = -1, color = "black") + ggtitle(paste0(input$statistic, " by Game"))
          grid.arrange(p1, p2, ncol = 2)
        } else {
          p1 <- ggplot(data = team_vs_others(), aes(x = vs_others, y = avg_stat, label = comma(avg_stat, accuracy = 0.01), fill = vs_others)) + geom_bar(stat = "identity") + expand_limits(y = 0) + xlab("") + ylab(paste0("Avg ", input$statistic, " per game")) +
            scale_fill_manual(values = c("#17408B", "#C9082A")) + guides(fill=FALSE) + theme_minimal() + geom_text(vjust = -0.5) + ggtitle(paste0("Avg ", input$statistic, " per game Comparison"))
          p2 <- ggplot(data = daily_team_stats(), aes(x = GAME_DATE, y = !! sym(input$statistic), color = `W/L`, label = !! sym(input$statistic))) + geom_point(size = 4) + expand_limits(y = 0) + scale_x_date(name = "Game Date", date_breaks = "1 day", labels = date_format(format = "%m/%d")) +
            scale_color_manual(values = c("W" = "#17408B", "L" = "#C9082A")) + theme_minimal() + geom_text(vjust = -1, color = "black") + ggtitle(paste0(input$statistic, " by Game"))
          grid.arrange(p1, p2, ncol = 2)
        }
      }
    }
  )
  
  output$metric_rank_stats <- renderPlot({
    validate(
      need(dim(shooting_team_stats())[1] != 0, paste0("No data for the ", input$team_select, " for selected date range.")),
      need(input$date_range[1] <= input$date_range[2], "Please select a valid date range.")
    )
    
    # lollipop charts for metric rank stats graphs
    p1 <- ggplot(data = shooting_team_stats(), aes(x = Metric, y = Rank)) + geom_segment(aes(x = Metric, xend = Metric, y = 0, yend = Rank), color = "#403a3a") + geom_point(size = 4, color = "#17408B") + 
      theme_minimal() + ggtitle("Shooting Stats (in %) by Rank") + scale_y_continuous(limits = c(0,25)) + coord_flip()
    p2 <- ggplot(data = scoring_team_stats(), aes(x = Metric, y = Rank)) + geom_segment(aes(x = Metric, xend = Metric, y = 0, yend = Rank), color = "#403a3a") + geom_point(size = 4, color = "#17408B") + 
      theme_minimal() + ggtitle("Scoring Stats (per game) by Rank") + scale_y_continuous(limits = c(0,25)) + coord_flip()
    p3 <- ggplot(data = other_team_stats(), aes(x = Metric, y = Rank)) + geom_segment(aes(x = Metric, xend = Metric, y = 0, yend = Rank), color = "#403a3a") + geom_point(size = 4, color = "#17408B") + 
      theme_minimal() + ggtitle("Miscellaneous Stats (per game) by Rank") + scale_y_continuous(limits = c(0,25)) + coord_flip()
    grid.arrange(p1, p2, p3, ncol = 3)
  })
  
  output$points_distribution_ratings <- renderPlot({
    validate(
      need(dim(shooting_team_stats())[1] != 0, paste0("No data for the ", input$team_select, " for selected date range.")),
      need(input$date_range[1] <= input$date_range[2], "Please select a valid date range.")
    )
    
    # defensive vs. offensive ratings; percentage of points coming from various shots 
    p1 <- ggplot(data = off_vs_def_ratings(), aes(x = GAME_DATE, y = rating, color = metric, label = rating)) + geom_point(size = 3) + geom_line(linetype = "dashed") + expand_limits(y = 0) + 
      geom_text(color = "black", vjust = -1, check_overlap = TRUE) + scale_x_date(name = "Game Date", date_breaks = "1 day", labels = date_format(format = "%m/%d")) + scale_y_continuous(name = "Rating", limits = c(0, 150)) +
      scale_color_manual(values = c("DEFRTG" = "#17408B", "OFFRTG" = "#C9082A")) + labs(color = "Metric") + theme_minimal() + ggtitle("OFFRTG vs. DEFRTG by Game")
    p2 <- ggplot(data = team_shot_percentages(), aes(x = 0, y = number_of_points, fill = shot_type)) + geom_bar(stat = "identity", color = "black") + coord_polar(theta = "y", start = 0) +
      geom_text(aes(label = percent(point_percent, digits = 0)), position = position_stack(vjust = 0.5)) + theme_minimal() +
      theme(axis.text = element_blank(), axis.ticks = element_blank(), panel.grid = element_blank(), axis.title = element_blank(), plot.title = element_text(hjust = 0.5)) + scale_fill_manual(name = "Shot Type",values = pie_chart_colors) +
      ggtitle("Distribution of Points Based on Shot Type")
    grid.arrange(p1, p2, ncol = 2)
  })
  
  # team logo based on selected team
  output$team_photo <- renderImage({
    list(src = paste0("nba_logos/", input$team_select, ".png"),
         height = "70%", width = "100%")
  }, deleteFile = FALSE)
  
  # data table for team stats
  output$team_stats <- renderDataTable({
    datatable(daily_team_stats() %>% select(TEAM, MATCH_UP, GAME_DATE, `W/L`, MIN, nba_variables), options = list(scrollX = TRUE),
              colnames = c("MATCH UP" = "MATCH_UP", "GAME DATE" = "GAME_DATE")) %>% 
      formatPercentage(columns = percent_variables, digits = 2)
  })
}

shinyApp(ui = ui, server = server)