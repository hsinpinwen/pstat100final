library(shiny)
library(tidyverse)
library(bslib)
library(janitor)
library(DT)

# Reading and cleaning data should be done outside of the server function
# if the data is not supposed to be reloaded every time a user interacts with the app.
happiness_clean <- read.csv("data/whr-2023.csv") %>%
  clean_names() %>%
  rename(
    gdp_per_capita = log_gdp_per_capita,
    healthy_life_expectancy = healthy_life_expectancy_at_birth,
    freedom_of_choice = freedom_to_make_life_choices
  ) %>%
  group_by(country_name) %>%
  summarize(
    life_ladder = mean(life_ladder, na.rm = TRUE),
    gdp_per_capita = mean(gdp_per_capita, na.rm = TRUE),
    social_support = mean(social_support, na.rm = TRUE),
    healthy_life_expectancy = mean(healthy_life_expectancy, na.rm = TRUE),
    freedom_of_choice = mean(freedom_of_choice, na.rm = TRUE),
    generosity = mean(generosity, na.rm = TRUE),
    perceptions_of_corruption = mean(perceptions_of_corruption, na.rm = TRUE),
    positive_affect = mean(positive_affect, na.rm = TRUE),
    negative_affect = mean(negative_affect, na.rm = TRUE)
  )

# LIFE LADDER
avg_gdp_avg_life_ladder <- happiness_clean %>%
  group_by(country_name) %>%
  summarize(avg_gdp_per_capita = round(mean(gdp_per_capita, na.rm = TRUE), 4),
            avg_life_ladder = round(mean(life_ladder, na.rm = TRUE), 4)) %>%
  filter(!is.na(avg_gdp_per_capita), !is.na(avg_life_ladder)) %>%
  arrange(desc(avg_gdp_per_capita)) %>%
  rename(Country = country_name, 
         GDP = avg_gdp_per_capita, 
         "Life Ladder" = avg_life_ladder)


# SOCIAL SUPPORT
avg_gdp_avg_social_support <- happiness_clean %>%
  group_by(country_name) %>%
  summarize(avg_gdp_per_capita = round(mean(gdp_per_capita, na.rm = TRUE), 4),
            avg_social_support = round(mean(social_support, na.rm = TRUE), 4)) %>%
  filter(!is.na(avg_gdp_per_capita), !is.na(avg_social_support)) %>%
  arrange(desc(avg_gdp_per_capita)) %>%
  rename(Country = country_name, 
         GDP = avg_gdp_per_capita, 
         "Social Support" = avg_social_support)


# HEALTHY_LIFE_EXPECTANCY
avg_gdp_avg_healthy_life_expectancy <- happiness_clean %>%
  group_by(country_name) %>%
  summarize(avg_gdp_per_capita = round(mean(gdp_per_capita, na.rm = TRUE), 4),
            avg_healthy_life_expectancy = round(mean(healthy_life_expectancy, na.rm = TRUE), 4)) %>%
  filter(!is.na(avg_gdp_per_capita), !is.na(avg_healthy_life_expectancy)) %>%
  arrange(desc(avg_gdp_per_capita)) %>%
  rename(Country = country_name, 
         GDP = avg_gdp_per_capita, 
         "Life Expectancy" = avg_healthy_life_expectancy)


# FREEDOM_OF_CHOICE
avg_gdp_avg_freedom_of_choice <- happiness_clean %>%
  group_by(country_name) %>%
  summarize(avg_gdp_per_capita = round(mean(gdp_per_capita, na.rm = TRUE), 4),
            avg_freedom_of_choice = round(mean(freedom_of_choice, na.rm = TRUE), 4)) %>%
  filter(!is.na(avg_gdp_per_capita), !is.na(avg_freedom_of_choice)) %>%
  arrange(desc(avg_gdp_per_capita)) %>%
  rename(Country = country_name, 
         GDP = avg_gdp_per_capita, 
         "Freedom of Choice" = avg_freedom_of_choice)


# GENEROSITY
avg_gdp_avg_generosity <- happiness_clean %>%
  group_by(country_name) %>%
  summarize(avg_gdp_per_capita = round(mean(gdp_per_capita, na.rm = TRUE), 4),
            avg_generosity = round(mean(generosity, na.rm = TRUE), 4)) %>%
  filter(!is.na(avg_gdp_per_capita), !is.na(avg_generosity)) %>%
  arrange(desc(avg_gdp_per_capita)) %>%
  rename(Country = country_name, 
         GDP = avg_gdp_per_capita, 
         "Generosity" = avg_generosity)


# PERCEPTIONS OF CORRUPTION
avg_gdp_avg_perceptions_of_corruption <- happiness_clean %>%
  group_by(country_name) %>%
  summarize(avg_gdp_per_capita = round(mean(gdp_per_capita, na.rm = TRUE), 4),
            avg_perceptions_of_corruption = round(mean(perceptions_of_corruption, na.rm = TRUE), 4)) %>%
  filter(!is.na(avg_gdp_per_capita), !is.na(avg_perceptions_of_corruption)) %>%
  arrange(desc(avg_gdp_per_capita)) %>%
  rename(Country = country_name, 
         GDP = avg_gdp_per_capita, 
         "Corruption Perceptions" = avg_perceptions_of_corruption)


# NEGATIVE AFFECT
avg_gdp_avg_negative_affect <- happiness_clean %>%
  group_by(country_name) %>%
  summarize(avg_gdp_per_capita = round(mean(gdp_per_capita, na.rm = TRUE), 4),
            avg_negative_affect = round(mean(negative_affect, na.rm = TRUE), 4)) %>%
  filter(!is.na(avg_gdp_per_capita), !is.na(avg_negative_affect)) %>%
  arrange(desc(avg_gdp_per_capita)) %>%
  rename(Country = country_name, 
         GDP = avg_gdp_per_capita, 
         "Negative Affect" = avg_negative_affect)


# POSITIVE AFFECT
avg_gdp_avg_positive_affect <- happiness_clean %>%
  group_by(country_name) %>%
  summarize(avg_gdp_per_capita = round(mean(gdp_per_capita, na.rm = TRUE), 4),
            avg_positive_affect = round(mean(positive_affect, na.rm = TRUE), 4)) %>%
  filter(!is.na(avg_gdp_per_capita), !is.na(avg_positive_affect)) %>%
  arrange(desc(avg_gdp_per_capita)) %>%
  rename(Country = country_name, 
         GDP = avg_gdp_per_capita, 
         "Positive Affect" = avg_positive_affect)


ui <- page_sidebar(
  theme = bs_theme(version = 5, bootswatch = "lux"),
  title = "Evaluating Happiness Using GDP, PSTAT 100 Final Project",
  sidebar = sidebar(h6("add abtstract of project here"),
                    selectInput(inputId = "var",
                                label = "Choose a happiness factor to display",
                                choices = 
                                  list(
                                    "Life Ladder" = "life_ladder", 
                                    "Social Support" = "social_support", 
                                    "Healthy Life Expectancy" = "healthy_life_expectancy", 
                                    "Freedom of Choice" = "freedom_of_choice",
                                    "Generosity" = "generosity", 
                                    "Perceptions of Corruption" = "perceptions_of_corruption", 
                                    "Positive Affect" = "positive_affect", 
                                    "Negative Affect" = "negative_affect"
                                  )
                    ),
  ),
  layout_columns(
    card(card_header("Graphs"), 
         plotOutput("distPlot")
    ),
    layout_columns(
      card(card_header("GDP"), 
           DTOutput("table")),
      card(card_header("Description"), 
           textOutput("description")), 
      col_widths = c(12, 12)
    )
  )
)

server <- function(input, output) {
  output$distPlot <- renderPlot({
    req(input$var)
    
    ggplot(happiness_clean, aes(x = gdp_per_capita, y = .data[[input$var]])) +
      geom_point(alpha = 0.8) +
      theme_minimal() +
      labs(title = paste("GDP per Capita vs", input$var),
           x = "GDP per Capita",
           y = input$var)
  })
  
  output$table <- renderDT({
    generate_table <- function(data) {
      datatable(data, options = list(
        pageLength = 3,  # Number of rows per page
        autoWidth = TRUE,  # Automatically adjust column width
        searching = TRUE,  # Enable search box
        ordering = FALSE,  # Enable column sorting
        paging = TRUE,  # Enable pagination
        lengthMenu = c(3, 5, 10),  # Rows per page options
        columnDefs = list(
          list(className = 'dt-center', 
               targets = '_all') # Center align all columns
        )
      ))
    }
    
    # Check which variable is selected and generate the corresponding table
    if (input$var == "life_ladder") {
      generate_table(avg_gdp_avg_life_ladder)
    } else if (input$var == "social_support") {
      generate_table(avg_gdp_avg_social_support)
    } else if (input$var == "healthy_life_expectancy") {
      generate_table(avg_gdp_avg_healthy_life_expectancy)
    } else if (input$var == "freedom_of_choice") {
      generate_table(avg_gdp_avg_freedom_of_choice)
    } else if (input$var == "generosity") {
      generate_table(avg_gdp_avg_generosity)
    } else if (input$var == "perceptions_of_corruption") {
      generate_table(avg_gdp_avg_perceptions_of_corruption)
    } else if (input$var == "negative_affect") {
      generate_table(avg_gdp_avg_negative_affect)
    } else if (input$var == "positive_affect") {
      generate_table(avg_gdp_avg_positive_affect)
    }
  })
  
  output$description <- renderText({
    req(input$var)
    
    descriptions <- list(
      life_ladder = "The Life Ladder is a subjective measure of well-being. 
      It represents the overall life satisfaction of individuals.",
      social_support = "Social Support measures the perceived quality and 
      availability of social support systems.",
      healthy_life_expectancy = "Healthy Life Expectancy at birth is the average 
      number of years a newborn is expected to live in good health.",
      freedom_of_choice = "Freedom of Choice refers to the extent to which 
      individuals feel they have control over their lives and decisions.",
      generosity = "Generosity measures the willingness of individuals to help 
      others, often assessed through charitable donations and volunteer work.",
      perceptions_of_corruption = "Perceptions of Corruption evaluates the 
      level of corruption as perceived by citizens, particularly in government 
      and business.",
      positive_affect = "Positive Affect reflects the frequency of positive 
      emotions such as joy, laughter, and happiness.",
      negative_affect = "Negative Affect reflects the frequency of negative 
      emotions such as sadness, anger, and fear."
    )
    
    descriptions[[input$var]]
  })

}

# Run the application 
shinyApp(ui = ui, server = server)
