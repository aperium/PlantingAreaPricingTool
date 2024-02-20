#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    https://shiny.posit.co/
#

# Load packages
pacs <- c("shiny", "fs", "measurements", "readxl")
sapply(pacs, require, character = TRUE)

# Retrieve data
data_path <- "Greenstreet Growers/TeamSite - Documents/Shared/Production Greenstreet/Production Finished/Spring 2024/pricing/4and6inchPricesSp2024.xlsx" |> fs::path_home()


# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Planting Area Pricing Tool"),

    # Sidebar with a slider input for number of bins
    sidebarLayout(
        sidebarPanel(
            numericInput("area",
                        "Enter your bed area",
                        min = 0,
                        value = 0),
            selectInput("units",
                        "Choose area units",
                        measurements::conv_unit_options$area,
                        selected = "ft2"),
            selectInput("products",
                        "Select products to compare",
                        c("a","b","c"),
                        multiple = TRUE)

        ),

        # Show a plot of the generated distribution
        mainPanel(
           textOutput("outText")
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {

    output$outText <- renderText({
        area <- input$area

        # draw the histogram with the specified number of bins
        area
    })
}

# Run the application
shinyApp(ui = ui, server = server)
