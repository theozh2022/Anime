
library(shiny)
library(shinydashboard)
library(tidyverse)
library(tidytext)
library(lubridate)
library(DT)
library(plotly)

# Load data
anime1 <- read.csv('https://opal.ils.unc.edu/~theozh/Anime-2.csv') 
bing_lexicon <- get_sentiments("bing")
shoujo_data <- anime1 %>% 
  filter(Demographics == "Shoujo") %>% 
  select(-3, -4, -9, -c(11:16)) %>% 
  rename(Minutes = Duration_Minutes) 

shoujo_data$Start_Aired <- as.Date(shoujo_data$Start_Aired, format = "%d-%b-%y")
future_dates <- !is.na(shoujo_data$Start_Aired) & shoujo_data$Start_Aired > Sys.Date()
shoujo_data$Start_Aired[future_dates] <- shoujo_data$Start_Aired[future_dates] - years(100)
shoujo_data$Year <- format(shoujo_data$Start_Aired, "%Y")

shounen_data <- anime1 %>% 
  filter(Demographics == "Shounen") %>% 
  select(-3, -4, -9, -c(11:16)) %>% 
  rename(Minutes = Duration_Minutes) 

shounen_data$Start_Aired <- as.Date(shounen_data$Start_Aired, format = "%d-%b-%y")
future_dates <- !is.na(shounen_data$Start_Aired) & shounen_data$Start_Aired > Sys.Date()
shounen_data$Start_Aired[future_dates] <- shounen_data$Start_Aired[future_dates] - years(100)
shounen_data$Year <- format(shounen_data$Start_Aired, "%Y")



# Function to process data and calculate sentiments
process_data <- function(data, lexicon) {
  tokens <- data %>%
    mutate(Synopsis = tolower(Synopsis)) %>%
    unnest_tokens(word, Synopsis)
  
  sentiments <- tokens %>%
    inner_join(lexicon) 
  
  return(sentiments)
}

# Function to create plots
plot_sentiments <- function(sentiments, title) {
  positive_terms <- sentiments %>%
    filter(sentiment == "positive") %>%
    count(word, sort = TRUE) %>%
    top_n(10)
  
  negative_terms <- sentiments %>%
    filter(sentiment == "negative") %>%
    count(word, sort = TRUE) %>%
    filter(word != "unknown") %>% 
    top_n(10)
  
  combined_terms <- bind_rows(
    mutate(positive_terms, sentiment = "positive"),
    mutate(negative_terms, sentiment = "negative")
  )
  
  combined_terms <- combined_terms %>%
    group_by(sentiment, word) %>%
    summarise(n = n, lower = n - 1.96*sqrt(n), upper = n + 1.96*sqrt(n))
  
  ggplot(combined_terms, aes(x = reorder(word, n), y = n, fill = sentiment)) +
    geom_bar(stat = "identity", position = 'dodge') +
    geom_errorbar(aes(ymin = lower, ymax = upper), width = 0.2, position = position_dodge(0.9)) +
    labs(title = title,
         x = NULL,
         y = "Frequency") +
    facet_wrap(~ sentiment, scales = "free_y") +
    coord_flip() +
    theme_bw()
}

aggregate_sentiments <- function(data) {
  aggregated_data <- data %>%
    group_by(ID) %>%
    summarise(
      English = first(English),
      Type = first(Type),
      Episodes = first(Episodes),
      Start_Aired = first(Start_Aired),
      Year = first(Year),
      Genres = first(Genres),
      Themes = first(Themes),
      Minutes = first(Minutes),
      Rating = first(Rating),
      Score = first(Score),
      Scored_Users = first(Scored_Users),
      Ranked = first(Ranked),
      Popularity = first(Popularity),
      Members = first(Members),
      Favorites = first(Favorites),
      sentiment = sum(ifelse(sentiment == "positive", 1, -1))
    ) 
  
  rating_groups <- split(aggregated_data, aggregated_data$Rating)
  
  sentiment_stats <- lapply(rating_groups, function(group) {
    n = nrow(group)
    mean_sentiment = mean(group$sentiment)
    sd_sentiment = sd(group$sentiment)
    
    ci_lower = mean_sentiment - 1.96 * sd_sentiment / sqrt(n)
    ci_upper = mean_sentiment + 1.96 * sd_sentiment / sqrt(n)
    
    return(data.frame(Mean_Sentiment = mean_sentiment, 
                      CI_lower = ci_lower,
                      CI_upper = ci_upper))
  })
  
  sentiment_stats_df <- do.call(rbind, sentiment_stats)
  rownames(sentiment_stats_df) = names(sentiment_stats)
  
  return(list(aggregated_data = aggregated_data, sentiment_stats_df = sentiment_stats_df))
}

anime2 = shoujo_data %>%
  separate_rows(Themes, sep = ", ") %>%
  separate_rows(Genres, sep = ", ")

anime3 = shounen_data %>%
  separate_rows(Themes, sep = ", ") %>%
  separate_rows(Genres, sep = ", ")









#----------------------------------------------------------------------------------------------

ui <- fluidPage(
  
  titlePanel("Anime Analysis"),
  
  sidebarLayout(
    sidebarPanel(
      selectInput("animeType", "Select Anime Type", choices = c("Shoujo", "Shounen"), selected = "Shoujo"),
      selectInput("modelType", "Select Model", choices = c("XGBoost", "Linear Regression", "Random Forest"), 
                  selected = "XGBoost")
    ),
    mainPanel(
      tabsetPanel(
        tabPanel("Sentiment Trends", plotOutput("sentimentPlot")),
        tabPanel("Top 10 Sentiment-Rich Terms", plotOutput("freqPlot")),
        tabPanel("Sentiment by Ratings", plotOutput("ratingPlot")),
        tabPanel("Top 30 Popular Themes and Genres Combinations", plotOutput("heatmap")),
        tabPanel("Top 10 genres & Mean Sentiment by Genre", plotOutput("genrePlot")),
        tabPanel("Regression Model Prediction", uiOutput("modelPlot"))
        
      )
    )
  )
)








#----------------------------------------------------------------------------------------------

server <- function(input, output, session) {
  # Render sentiment trend plot
  process_sentiment_data <- function(dataset) {
    dataset_tokens <- dataset %>%
      mutate(Synopsis = tolower(Synopsis)) %>%
      unnest_tokens(word, Synopsis)
    
    dataset_sentiment_by_anime <- dataset_tokens %>%
      inner_join(bing_lexicon) %>%
      count(Start_Aired, sentiment) %>%
      spread(sentiment, n, fill = 0) %>%
      mutate(Sentiment = positive - negative)
    
    return(dataset_sentiment_by_anime)
  }
  
  # Apply the function to shoujo_data and shonen_data
  shoujo_sentiment_by_anime <- process_sentiment_data(shoujo_data)
  shounen_sentiment_by_anime <- process_sentiment_data(shounen_data)
  
  # Render sentiment trend plot
  output$sentimentPlot <- renderPlot({
    # Plotting both shoujo and shounen sentiment on the same plot
    ggplot() +
      geom_point(data = shoujo_sentiment_by_anime, aes(Start_Aired, Sentiment, group = 1, color = "Shoujo")) +
      geom_point(data = shounen_sentiment_by_anime, aes(Start_Aired, Sentiment, group = 1, color = "Shounen"), alpha = 0.4) +
      geom_smooth(data = shoujo_sentiment_by_anime, method = 'lm', se = FALSE, aes(Start_Aired, Sentiment, group = 1), color = "red") +
      geom_smooth(data = shounen_sentiment_by_anime, method = 'lm', se = FALSE, aes(Start_Aired, Sentiment, group = 1), color = "darkblue") +
      labs(title = "Sentiment Trends Over Years",
           x = NULL,
           y = "Sentiment Score",
           color = "Anime Demographics") +
      theme_bw() +
      scale_x_date(breaks = scales::breaks_pretty(n = 12))
  })
  
#-------------------------------------  
  # Render term freqency plot
  selected_data <- reactive({
    if (input$animeType == "Shoujo") {
      return(shoujo_data)
    } else {
      return(shounen_data)
    }
  })
  
  # Reactive function for sentiment analysis and plotting
  output$freqPlot <- renderPlot({
    selected_sentiments <- process_data(selected_data(), bing_lexicon)
    selected_plot <- plot_sentiments(selected_sentiments, paste("Top 10 Sentiment-Rich Terms in", input$animeType, "Anime"))
    print(selected_plot)
  })
  
  


#------------------------------------- 
 # Render rating plot
# Reactive function for sentiment analysis
selected_sentiments <- reactive({
  process_data(selected_data(), bing_lexicon)
})

# Reactive function for aggregated sentiments and sentiment statistics by rating
aggregate_sentiments_by_rating <- reactive({
  results <- aggregate_sentiments(selected_sentiments())
  return(results)
})

# Render rating plot
output$ratingPlot <- renderPlot({
  results <- aggregate_sentiments_by_rating()
  sentiment_stats_df <- results$sentiment_stats_df %>% 
    filter(row.names(.) != 'None')
  
  rating_plot = ggplot(sentiment_stats_df, aes(x = rownames(sentiment_stats_df), y = Mean_Sentiment, fill = ifelse(Mean_Sentiment > 0, "positive", "negative"))) +
    geom_bar(stat = "identity", width = 0.5) +
    geom_errorbar(aes(ymin = CI_lower, ymax = CI_upper), width = 0.2) +
    labs(x='', y = "Mean Sentiment", title = paste('Sentiment by Ratings in', input$animeType, 'Anime')) +
    coord_flip() +
    scale_fill_manual(values = c("positive" = "cornflowerblue", "negative" = "lightsalmon")) +
    theme_bw() +
    theme(legend.position = "none")
  
  print(rating_plot)
})

#------------------------------------- 
# Render heatmap 
# Reactive function to switch between Shoujo and Shounen data
selected_data <- reactive({
  if (input$animeType == "Shoujo") {
    return(anime2)  # Assuming anime2 is the Shoujo dataset
  } else {
    return(anime3)  # Assuming anime3 is the Shounen dataset
  }
})

# Function to create heatmap plot
generate_heatmap <- function(data, title, low_color, mid_color, high_color) {
  combinations = expand.grid(unique(data$Themes), unique(data$Genres))
  colnames(combinations) = c("Themes", "Genres")
  
  combinations = combinations %>%
    mutate(Avg_Popularity = sapply(1:nrow(combinations), function(i) {
      theme = combinations$Themes[i]
      genre = combinations$Genres[i]
      avg_popularity = mean(data$Favorites[data$Themes == theme & data$Genres == genre], na.rm = TRUE)
      return(avg_popularity)
    })) 
  
  combinations = combinations %>%
    arrange(desc(Avg_Popularity))
  
  # Assuming you have the 'combinations' data frame
  # Filter for the top 20 combinations
  top_combinations <- combinations %>%
    top_n(30, Avg_Popularity)
  
  heatmap_plot <- ggplot(top_combinations, aes(x = Themes, y = Genres, fill = Avg_Popularity)) +
    geom_tile(color = "white") +
    geom_text(aes(label = round(Avg_Popularity)), color = "black", size = 3) +
    scale_fill_gradient2(low = low_color, mid = mid_color, high = high_color, midpoint = median(top_combinations$Avg_Popularity), guide = "colorbar") +
    labs(title = title, x = "Themes", y = "Genres", fill = "Average Popularity") +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 25, hjust = 1))
  
  return(heatmap_plot)
}

# Render heatmap
output$heatmap <- renderPlot({
  selected_heatmap <- generate_heatmap(selected_data(), paste("Top 30 Popular Themes and Genres Combinations in", input$animeType, "Anime"), "white", "lightpink", "red")
  return(selected_heatmap)
})


#-----------------------------------------------------------------------------------------

# Render the genre plots

  output$genrePlot <- renderPlot({
    # Check the selected anime type
    if (input$animeType == "Shoujo") {
      # Load and render the Shoujo genre plot
      shoujo_plot <- png::readPNG("combined_plots1.png")
      
      # Set the dimensions of the plot
      width <- 800  
      height <- 400 
      
      # Create the plot
      plot(1, type = 'n', xlim = c(0, 1), ylim = c(0, 1), xaxt = 'n', yaxt = 'n', ann = FALSE)
      rasterImage(shoujo_plot, 0, 0, 1, 1)
    } else {
      # Load and render the Shounen genre plot
      shounen_plot <- png::readPNG("combined_plots2.png")
      
      # Set the dimensions of the plot
      width <- 800  
      height <- 400  
      
      # Create the plot
      plot(1, type = 'n', xlim = c(0, 1), ylim = c(0, 1), xaxt = 'n', yaxt = 'n', ann = FALSE)
      rasterImage(shounen_plot, 0, 0, 1, 1)
    }
  }, width = 800, height = 500)  

#-----------------------------------------------------------------------------------------

linear_regression_plot <- "figure2.html"
random_forest_plot <- "figure3.html"
xgboost_plot <- "figure1.html"

# Define a reactive expression to switch between plots based on the selected model type
selected_model_plot <- reactive({
  switch(input$modelType,
         "Linear Regression" = linear_regression_plot,
         "Random Forest" = random_forest_plot,
         "XGBoost" = xgboost_plot)
})

# Render the selected model plot
output$modelPlot <- renderUI({
  selected_model <- selected_model_plot()
  
  # Check if the selected_model is not NULL
  if (!is.null(selected_model)) {
    htmlOutput("model_plot")
  }
})

# Render the HTML content based on the selected model
output$model_plot <- renderUI({
  selected_model <- selected_model_plot()
  
  # Check if the selected_model is not NULL
  if (!is.null(selected_model)) {
    includeHTML(selected_model)
  }
})




#--------
}








#----------------------------------------------------------------------------------------------
shinyApp(ui, server)


