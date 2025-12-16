library(shiny)
library(DT)

ui <- fluidPage(
  titlePanel("Stratified Sampling Sample Size Estimation"),
  
  sidebarLayout(
    sidebarPanel(
      numericInput("B", "Allowable Error (B)", value = 2),
      selectInput("conf", "Confidence Level",
                  choices = c("90%" = 1.645, "95%" = 1.96, "99%" = 2.576),
                  selected = 1.96),
      actionButton("calc", "Calculate Sample Size")
    ),
    
    mainPanel(
      DTOutput("result")
    )
  )
)

server <- function(input, output) {
  
  strata_data <- reactive({
    data.frame(
      Stratum = c("A", "B", "C"),
      Nh = c(500, 300, 200),
      Sh = c(10, 15, 20),
      Cost = c(1, 2, 3)
    )
  })
  
  output$result <- renderDT({
    req(input$calc)
    
    df <- strata_data()
    N <- sum(df$Nh)
    W <- df$Nh / N
    Z <- as.numeric(input$conf)
    B <- input$B
    
    # Total sample sizes
    n_prop <- (Z^2 * sum(W * df$Sh^2)) / B^2
    n_neyman <- (Z^2 * (sum(W * df$Sh))^2) / B^2
    n_opt <- (Z^2 * (sum(W * df$Sh * sqrt(df$Cost)))^2) / B^2
    
    # Allocation
    df$Proportional <- round(n_prop * W)
    df$Neyman <- round(n_neyman * (df$Nh * df$Sh) / sum(df$Nh * df$Sh))
    df$Optimised <- round(n_opt * (df$Nh * df$Sh / sqrt(df$Cost)) /
                            sum(df$Nh * df$Sh / sqrt(df$Cost)))
    
    df
  })
}

shinyApp(ui, server)

