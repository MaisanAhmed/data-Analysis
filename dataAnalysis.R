library(shiny)
library(dplyr)
library(arules) 
# Define UI
ui <- fluidPage(
  titlePanel("Data Analysis"),
  
  sidebarLayout(
    sidebarPanel(
      fileInput("file", "Choose Excel File (.csv)",
                accept = c(".csv")),
      numericInput("num_clusters", "Number of Clusters:",
                   min = 2, max = 4, value = 2),
      numericInput("support", "Support :", min = 0.001, max = 1, value = 0.1),
      numericInput("confidence", "Confidence :", min = 0.001, max = 1, value = 0.1),
      actionButton("submit", "Submit")
    ),
    
    mainPanel(
      tabsetPanel(
        tabPanel("Data", tableOutput("data_table")),
        tabPanel("Plots", plotOutput("plots")),
        tabPanel("Cluster Table", tableOutput("cluster_table")),
        tabPanel("Association Rules", verbatimTextOutput("rules_output")),
        fluidRow(
          column(12, tableOutput("association_results"), title = "Association")
        )
      )
    )
  )
)

# Define server logic
server <- function(input, output) {
  data <- reactive({
    req(input$file)
    read.csv(input$file$datapath)
  })
  cleaned_data <- reactive({
    req(data())
    # Remove duplicate rows
    cleaned_data <- unique(data())
  })
  output$data_table <- renderTable({
    req(cleaned_data())
    cleaned_data()
  })
  
  output$plots <- renderPlot({
    req(data())
    par(mfrow = c(2, 2))
    x <- table(data()$paymentType)
    percentage <- round(100 * x / sum(x))
    pie(x, labels = paste0(percentage, "%"), main = "compare between cash and credit", col = c("skyblue", "blue"))
    legend("topright", legend = c("Credit", "Cash"), fill = c("skyblue", "blue"))
    barplot(table(data()$age), main = "each age and sum of total spending", xlab = "Age", ylab = "Count")
    barplot(table(data()$city), main = " total spending of each City", xlab = "City", ylab = "Count")
    boxplot(data()$total, main = "Total Spending Distribution")
  })
  
  output$cluster_table <- renderTable({
    req(input$submit)
    groupedData <- data() %>%
      group_by(customer) %>%
      summarise(age = first(age), total_spending = sum(total))
    
    # Perform k-means clustering
    kmean_clustering <- kmeans(groupedData[, c("age", "total_spending")], centers = input$num_clusters)
    
    # Add cluster assignment to grouped data
    groupedData$cluster <- kmean_clustering$cluster
    
    # Combine customer data with cluster assignments
    cluster_table <- data.frame(
      Name = groupedData$customer,
      Age = groupedData$age,
      Total = groupedData$total_spending,
      No_of_clusters = groupedData$cluster
    )
    return(cluster_table)
  })
  
  output$rules_output <- renderPrint({
    req(cleaned_data())
    items <- cleaned_data()$items
    items_list <- strsplit(items, ",")
    transactions <- as(items_list, "transactions")
    
    # Apply association rule mining
    rules <- apriori(transactions, parameter = list(support = as.numeric(input$support), confidence = as.numeric(input$confidence)))
    
    # Inspect and display the rules
    inspected_rules <- inspect(rules)
  })
}

# Run the application
shinyApp(ui = ui, server = server)

