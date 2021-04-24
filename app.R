#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/

cereal <- read.csv("cereal.csv")

names(cereal)[names(cereal) == "mfr"] <- "manufacturer"
names(cereal)[names(cereal) == "potass"] <- "potassium"
names(cereal)[names(cereal) == "carbo"] <- "carbohydrate"

# Define UI for dataset viewer app ----
ui <- fluidPage(
    
    # App title ----
    titlePanel("Cereal's Nutritional Value Based On Manufacturer"),
    
    # Sidebar layout with input and output definitions ----
    sidebarLayout(
        
        # Sidebar panel for inputs ----
        sidebarPanel(
            
            # Input: Text for providing a caption ----
            # Note: Changes made to the caption in the textInput control
            # are updated in the output area immediately as you type
            textInput(inputId = "caption",
                      label = "Caption:",
                      value = "Data Summary"),
            
            # Input: Selector for choosing dataset ----
            selectInput(inputId = "manufacturer",
                        label = "Choose a manufacturer:",
                        choices = c(
                        "American Home Food Products",
                        "General Mills",
                        "Kelloggs",
                        "Nabisco",
                        "Post",
                        "Quaker Oats",
                        "Ralston Purina")),
            
            # Input: Numeric entry for number of obs to view ----
            numericInput(inputId = "obs",
                         label = "Number of observations to view:",
                         value = 10)
            
        ),
        
        # Main panel for displaying outputs ----
        mainPanel(
            
            # Output: Formatted text for caption ----
            h3(textOutput("caption", container = span)),
            
            # Output: Verbatim text for data summary ----
            verbatimTextOutput("summary"),
            
            # Output: HTML table with requested number of observations ----
            tableOutput("view")
            
        )
    )
)

# Define server logic to summarize and view selected dataset ----
server <- function(input, output) {
    
    # Return the requested dataset ----
    # By declaring datasetInput as a reactive expression we ensure
    # that:
    #
    # 1. It is only called when the inputs it depends on changes
    # 2. The computation and result are shared by all the callers,
    #    i.e. it only executes a single time
    datasetInput <- reactive({
        switch(input$manufacturer,
               "American Home Food Products" = cereal[cereal$manufacturer == "A",],
               "General Mills" =cereal[cereal$manufacturer == "G",] ,
               "Kelloggs" = cereal[cereal$manufacturer == "K",],
               "Nabisco" =cereal[cereal$manufacturerr == "N",] ,
               "Post" =cereal[cereal$manufacturer == "P",] ,
               "Quaker Oats" = cereal[cereal$manufacturerr == "Q",],
               "Ralston Purina" = cereal[cereal$manufacturer == "R",])
    })
    
    # Create caption ----
    # The output$caption is computed based on a reactive expression
    # that returns input$caption. When the user changes the
    # "caption" field:
    #
    # 1. This function is automatically called to recompute the output
    # 2. New caption is pushed back to the browser for re-display
    #
    # Note that because the data-oriented reactive expressions
    # below don't depend on input$caption, those expressions are
    # NOT called when input$caption changes
    output$caption <- renderText({
        input$caption
    })
    
    # Generate a summary of the dataset ----
    # The output$summary depends on the datasetInput reactive
    # expression, so will be re-executed whenever datasetInput is
    # invalidated, i.e. whenever the input$dataset changes
    output$summary <- renderPrint({
        dataset <- datasetInput()
        summary(dataset)
    })
    
    # Show the first "n" observations ----
    # The output$view depends on both the databaseInput reactive
    # expression and input$obs, so it will be re-executed whenever
    # input$dataset or input$obs is changed
    output$view <- renderTable({
        head(datasetInput(), n = input$obs)
    })
    
}
# Run the application 
shinyApp(ui = ui, server = server)
