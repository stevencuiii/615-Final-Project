#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    https://shiny.posit.co/
#


library(tidyverse)
library(shiny)
library(leaflet)

# Load data
data <- read.csv("1.csv")

# Clean column names to extract only the year
colnames(data) <- gsub("^X(\\d{4})..YR\\1\\.$", "\\1", colnames(data))

# Verify column names
print(colnames(data))

# Define the indicators of interest
selected_indicators <- c('GDP (current US$)', 'GDP growth (annual %)', 
                         'GDP per capita (current US$)', 'Population, total', 
                         'Population growth (annual %)', 'Life expectancy at birth, total (years)')

# Filter and reshape the data
filtered_data <- data %>%
  filter(Series.Name %in% selected_indicators) %>%
  select(Series.Name, any_of(as.character(2000:2023))) %>%
  pivot_longer(cols = any_of(as.character(2000:2023)), names_to = "Year", values_to = "Value") %>%
  mutate(
    Year = as.numeric(Year)  # Ensure Year is numeric
  ) %>%
  drop_na(Value)

# View cleaned data
head(filtered_data)

# Add resource path for images
#addResourcePath("images", "C:/Users/cuih1/OneDrive/Desktop/615/Aruba data/aruba_analysis/www")

# UI
ui <- navbarPage(
  "Aruba Analysis",
  tabPanel("Introduction",
           fluidPage(
             titlePanel("About Aruba"),
             fluidRow(
               column(6, 
                      p("Aruba is a Caribbean island located off the coast of Venezuela, known for its white-sand beaches, turquoise waters, and year-round sunny climate. Aruba is part of the Kingdom of the Netherlands, with its own government and currency. Its economy heavily relies on tourism, aloe production, and oil refining."),
                      p("Capital City: Oranjestad"),
                      p("Currency: Aruban Florin (AWG)"),
                      p("Land area: 180 square kilometers")
               ),
               column(6, img(src = "Aruba_1.png", width = "100%"))
             )
           )
  ),
  tabPanel("Biodiversity",
           fluidPage(
             titlePanel("Aruba's Biodiversity"),
             p("Aruba's natural ecosystems include desert landscapes, coral reefs, and unique flora like the Divi-Divi tree. Native species include sea turtles, lizards, and exotic birds."),
             fluidRow(
               column(6, img(src = "Aruba_2.jpg", width = "100%")),
               column(6, img(src = "Aruba_3.jpg", width = "100%"))
             )
           )
  ),
  tabPanel("Map",
           fluidPage(
             titlePanel("Interactive Map of Aruba"),
             leafletOutput("arubaMap", height = 500)
           )
  ),
  tabPanel("Economic Indicators",
           fluidPage(
             titlePanel("Economic Indicators"),
             selectInput("econVar", "Choose a variable to display:", 
                         choices = c("GDP (current US$)", "GDP growth (annual %)")),
             plotOutput("econPlot")
           )
  ),
  tabPanel("Population Statistics",
           fluidPage(
             titlePanel("Population Trends"),
             selectInput("popVar", "Choose a variable to display:", 
                         choices = c("Population, total", "Population growth (annual %)", "Life expectancy at birth, total (years)")),
             plotOutput("popPlot")
           )
  ),
  tabPanel("Comparison",
           fluidPage(
             titlePanel("Comparison with Dominica and Jamaica"),
             h4("Aruba vs Dominica"),
             p("- Aruba has a significantly higher GDP compared to Dominica due to its developed tourism infrastructure and stable economy. Dominica, on the other hand, relies more on agriculture and eco-tourism."),
             p("- In terms of population, Dominica is much smaller than Aruba with approximately 70,000 people, while Aruba has around 120,000. Both islands share vulnerabilities to natural disasters such as hurricanes."),
             img(src = "Dominica.png", width = "100%"),
             h4("Aruba vs Jamaica"),
             p("- Jamaica, with a population of nearly 3 million, far exceeds Aruba in terms of size and workforce. However, Aruba's GDP per capita is significantly higher due to its higher-income economy."),
             p("- Jamaica's economy is more diversified, including agriculture, mining, and manufacturing, while Aruba remains heavily dependent on tourism."),
             p("- Both Aruba and Jamaica face challenges from climate change, but Jamaica has more resources to invest in long-term solutions given its larger economy."),
             img(src = "Jamaica.png", width = "100%")
           )
  ),
  tabPanel("SWOT Analysis",
           fluidPage(
             titlePanel("SWOT Analysis of Aruba"),
             h4("Strengths"),
             p("- GDP: Aruba's current GDP indicates a relatively strong economy supported by tourism and industry."),
             p("- GDP Growth: A positive annual growth rate of 3.5% shows economic resilience."),
             p("- Life Expectancy: At 76.5 years, Aruba has a higher-than-average life expectancy compared to other SIDS (Small Island Developing States)."),
             h4("Weaknesses"),
             p("- Population Growth: A modest growth rate of 1.2% may indicate limited workforce expansion."),
             p("- Population Size: With a small population of 120,000, Aruba faces challenges scaling its economy and workforce for sustainable growth."),
             p("- Dependence on Tourism: The GDP structure heavily relies on external factors like global tourism, making it vulnerable to global economic shocks (e.g., pandemics)."),
             h4("Opportunities"),
             p("- Diversification: Economic diversification into renewable energy and eco-tourism can reduce reliance on traditional tourism."),
             p("- Sustainability: With a stable GDP growth rate, Aruba can invest in green initiatives to align with global environmental trends."),
             p("- Life Expectancy: A higher life expectancy can attract retirees, boosting sectors like healthcare and real estate."),
             h4("Threats"),
             p("- Climate Change: Rising sea levels and hurricanes pose severe threats to the island's economy and infrastructure."),
             p("- Economic Shocks: Tourism dependency makes Aruba susceptible to external shocks like pandemics or global recessions."),
             p("- Resource Scarcity: Aruba’s small land size limits natural resources for self-sustained growth.")
           )
  )
)

# Server
server <- function(input, output, session) {
  # Interactive Map
  output$arubaMap <- renderLeaflet({
    leaflet() %>%
      addTiles() %>%
      setView(lng = -69.9683, lat = 12.5211, zoom = 10) %>%
      addMarkers(lng = -69.9683, lat = 12.5211, popup = "Aruba: Oranjestad")
  })
  
  # Economic Indicators Plot
  output$econPlot <- renderPlot({
    req(input$econVar)  # Ensure a variable is selected
    econ_data <- filtered_data %>% filter(`Series.Name` == input$econVar)
    ggplot(econ_data, aes(x = Year, y = Value, group = 1)) +  # Add group = 1
      geom_line(color = "blue", size = 1) +
      theme_minimal() +
      labs(
        title = paste("Trends in", input$econVar),
        y = input$econVar,
        x = "Year"
      )
  })
  
  # Population Statistics Plot
  output$popPlot <- renderPlot({
    req(input$popVar)  # Ensure a variable is selected
    pop_data <- filtered_data %>% filter(`Series.Name` == input$popVar)
    ggplot(pop_data, aes(x = Year, y = Value, group = 1)) +  # Add group = 1
      geom_line(color = "red", size = 1) +
      geom_point(color = "red", size = 2) +
      theme_minimal() +
      labs(
        title = paste("Trends in", input$popVar),
        y = input$popVar,
        x = "Year"
      )
  })
}

# Run App
shinyApp(ui, server)
