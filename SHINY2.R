library(shiny)
library(ggplot2)

# Change file paths if needed
# Read the "actions" CSV file
actions <- read.csv("data/actions.csv")

# Read the "teams" CSV file
teams <- read.csv("data/teams.csv")

actions$team_name <- teams$officialName[match(actions$team_id, teams$wyId)]

filter_data_for_team <- function(actions, target_team_id) {
  game_ids_with_team <- unique(actions$game_id[actions$team_id == target_team_id])
  
  filtered_data <- actions[actions$game_id %in% game_ids_with_team, ]
  
  return(filtered_data)
}

extract_all_shots <- function(data, target_type = "shot", target_team_id) {
  sequences <- list()
  
  count <- 0
  
  target_in_sequence <- FALSE
  
  for (i in seq_len(nrow(data))) {
    if (i == 1 || data$team_id[i] != data$team_id[i - 1]) {
      count <- count + 1
      
      current_sequence <- data[i, , drop = FALSE]
      
      if (data$type_name[i] == target_type && data$team_id[i] == target_team_id) {
        target_in_sequence <- TRUE
      }
    } else {
      current_sequence <- rbind(current_sequence, data[i, , drop = FALSE])
      
      if (data$type_name[i] == target_type && data$team_id[i] == target_team_id) {
        target_in_sequence <- TRUE
      }
    }
    
    if (i == nrow(data) || data$team_id[i + 1] != data$team_id[i]) {
      if (target_in_sequence || (nrow(current_sequence) == 1 && current_sequence$type_name == target_type && current_sequence$team_id == target_team_id)) {
        if (!any(grepl("^corner_crossed|^corner_short|^freekick_crossed|^freekick_short|^throw_in", current_sequence$type_name))) {
          start_x <- current_sequence$start_x[1]
          start_y <- current_sequence$start_y[1]
          if (start_x >= 52.5 && start_x <= 105 && start_y >= 0 && start_y <= 68) {
            sequences[[count]] <- current_sequence
          }
        }
      }
      
      current_sequence <- NULL  
      target_in_sequence <- FALSE 
    }
  }
  
  return(sequences)
}

# Define UI
ui <- fluidPage(
  titlePanel("Sequence Length Analysis for Two Teams"),
  sidebarLayout(
    sidebarPanel(
      selectInput("team_name1", "Select Team Name 1:", choices = unique(actions$team_name)),
      selectInput("team_name2", "Select Team Name 2:", choices = unique(actions$team_name)),
      actionButton("submit", "Submit")
    ),
    mainPanel(
      fluidRow(
        column(width = 12,
               textOutput("median_sequence_length1")),
        column(width = 12,
               textOutput("mean_sequence_length1")),
        column(width = 12,
               plotOutput("histogram1", height = "350px", width = "1000px"))
      ),
      fluidRow(
        column(width = 12,
               textOutput("median_sequence_length2")),
        column(width = 12,
               textOutput("mean_sequence_length2")),
        column(width = 12,
               plotOutput("histogram2", height = "350px", width = "1000px"))
      )
    )
  )
)




# Define server logic
server <- function(input, output) {
  observeEvent(input$submit, {
    target_team_id1 <- teams$wyId[match(input$team_name1, teams$officialName)]
    target_team_id2 <- teams$wyId[match(input$team_name2, teams$officialName)]
    
    filtered_data1 <- filter_data_for_team(actions, target_team_id1)
    
    filtered_data2 <- filter_data_for_team(actions, target_team_id2)
    
    target_types <- c("shot")
    result_all_shots1 <- list()
    
    for (target_type in target_types) {
      result_all_shots1 <- c(result_all_shots1, extract_all_shots(filtered_data1, target_type = target_type, target_team_id = target_team_id1))
    }
    
    result_all_shots1 <- result_all_shots1[!sapply(result_all_shots1, is.null)]
    
    sequence_lengths1 <- sapply(result_all_shots1, function(seq) nrow(seq))
    
    sequence_lengths1 <- sequence_lengths1[sequence_lengths1 > 1]
    
    median_sequence_length1 <- median(sequence_lengths1)
    
    mean_sequence_length1 <- mean(sequence_lengths1)
    
    sequence_lengths_table1 <- table(sequence_lengths1)
    
    sequence_lengths_df1 <- as.data.frame(sequence_lengths_table1)
    
    colnames(sequence_lengths_df1) <- c("Length", "Count")
    
    sequence_lengths_df1 <- sequence_lengths_df1[order(sequence_lengths_df1$Length), ]
    
    output$median_sequence_length1 <- renderText({
      paste("Median length of sequences for", input$team_name1,":" , round(median_sequence_length1, 2))
    })
    
    output$mean_sequence_length1 <- renderText({
      paste("Mean length of sequences for", input$team_name1,":" , round(mean_sequence_length1, 2))
    })
    
    output$histogram1 <- renderPlot({
      ggplot(sequence_lengths_df1, aes(x = Length, y = Count)) +
        geom_bar(stat = "identity", fill = "skyblue", color = "black") +
        labs(title = paste("Histogram of Sequence Lengths for", input$team_name1),
             x = "Sequence Length",
             y = "Count") +
        theme_minimal()
    })
    
    result_all_shots2 <- list()
    
    for (target_type in target_types) {
      result_all_shots2 <- c(result_all_shots2, extract_all_shots(filtered_data2, target_type = target_type, target_team_id = target_team_id2))
    }
    
    result_all_shots2 <- result_all_shots2[!sapply(result_all_shots2, is.null)]
    
    sequence_lengths2 <- sapply(result_all_shots2, function(seq) nrow(seq))
    
    sequence_lengths2 <- sequence_lengths2[sequence_lengths2 > 1]
    
    median_sequence_length2 <- median(sequence_lengths2)
    
    mean_sequence_length2 <- mean(sequence_lengths2)
    
    sequence_lengths_table2 <- table(sequence_lengths2)
    
    sequence_lengths_df2 <- as.data.frame(sequence_lengths_table2)
    
    colnames(sequence_lengths_df2) <- c("Length", "Count")
    
    sequence_lengths_df2 <- sequence_lengths_df2[order(sequence_lengths_df2$Length), ]
    
    output$median_sequence_length2 <- renderText({
      paste("Median length of sequences for", input$team_name2,":" , round(median_sequence_length2, 2))
    })
    
    output$mean_sequence_length2 <- renderText({
      paste("Mean length of sequences for", input$team_name2,":" , round(mean_sequence_length2, 2))
    })
    
    output$histogram2 <- renderPlot({
      ggplot(sequence_lengths_df2, aes(x = Length, y = Count)) +
        geom_bar(stat = "identity", fill = "skyblue", color = "black") +
        labs(title = paste("Histogram of Sequence Lengths for", input$team_name2),
             x = "Sequence Length",
             y = "Count") +
        theme_minimal()
    })
  })
}

# Run the application
shinyApp(ui = ui, server = server)
