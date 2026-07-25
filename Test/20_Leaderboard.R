library(dplyr)
library(tidyverse)
library(gt)

leaderboard_function <- function(player_code_runner_input = "PHD-8619",
                                 leaderboard_df = leaderboard) {
  
  
  # Add rank column based on leaderboard order
  leaderboard_df <- leaderboard_df %>%
    mutate(Rank = row_number())
  
  
  # Find selected player's rank
  player_rank <- leaderboard_df %>%
    filter(player_code_runner == player_code_runner_input) %>%
    pull(Rank)

  if(player_rank <= 5) {
    above_number <- player_rank - 1
    below_number <- 10 - above_number
  } else if(player_rank >= (max(leaderboard_df$Rank)-4)) {
    below_number <- max(leaderboard_df$Rank) - player_rank
    above_number <- 10 - below_number
  } else {
    above_number <- 5
    below_number <- 5
  }
  
  
  # Pull players within specified range above and below selected player
  temp <- leaderboard_df %>%
    filter(Rank >= player_rank - below_number,
           Rank <= player_rank + above_number)%>%
    
    # Format decision score to exactly two decimal places
    mutate(Decision_Score = sprintf("%.2f", avg_percent * 100)) %>%
    
    # Keep only scoreboard columns
    select(Rank, player_code_runner, Decision_Score, count, grade)
  
  
  # Create formatted GT scoreboard
  temp %>%
    gt() %>%
    
    
    # Rename columns for clean display
    cols_label(Rank = "RANK", player_code_runner = "RUNNER", Decision_Score = "DECISION SCORE", count = "PLAYS",grade = "GRADE") %>%
    
    
    # Add scoreboard title and subtitle
    tab_header(title = md("♠️ **THE ADVANTAGE TABLE** ♣️"),
               subtitle = md("Baserunning leaderboard powered by expected value modeling")) %>%
    
    
    # Title font
    tab_style(style = cell_text(font = google_font("Limelight"), size = px(30), weight = "bold", color = "white"),
              locations = cells_title(groups = "title")) %>%
    
    
    # Subtitle font
    tab_style(style = cell_text(font = google_font("Limelight"),size = px(16), color = "white"), 
              locations = cells_title(groups = "subtitle")) %>%
    
    
    # Black table background + white text
    tab_style(style = list(cell_fill(color = "black"), cell_text(color = "white")),
              locations = cells_body()) %>%
    
    
    # Highlight selected player
    tab_style(style = list(cell_fill(color = "#1E6432"),cell_text(weight = "bold", size = px(18),color = "white")),
              locations = cells_body(rows = player_code_runner == player_code_runner_input)) %>%
    
    
    # Column header styling
    tab_style(style = list(cell_fill(color = "black"),
                           cell_text(color = "white",weight = "bold",
                                     font = google_font("Limelight"),
                                     size = px(18))),locations = cells_column_labels()) %>%
    
    
    # Bold runner names
    tab_style(style = cell_text(weight = "bold", color = "white"), locations = cells_body(columns = player_code_runner)) %>%
    
    
    # Center all columns
    cols_align(align = "center", columns = everything()) %>%
    
    
    # Adjust column widths
    cols_width(Rank ~ px(80),
               player_code_runner ~ px(160),
               Decision_Score ~ px(160),
               count ~ px(100),
               grade ~ px(100)) %>%
    
    
    # Table formatting
    tab_options(
      table.font.size = 18,
      data_row.padding = px(12),
      heading.align = "center",
      heading.title.font.size = px(30),
      heading.subtitle.font.size = px(16),
      table.background.color = "black")}
leaderboard_function()
