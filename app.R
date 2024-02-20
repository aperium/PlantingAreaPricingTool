#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    https://shiny.posit.co/
#

# Load packages
pacs <- c("shiny", "dplyr")
sapply(pacs, require, character = TRUE)

# Retrieve data
price_level <- 6
freight <- 0.07
# data_path <- "Greenstreet Growers/TeamSite - Documents/Shared/Production Greenstreet/Production Finished/Spring 2024/pricing/4and6inchPricesSp2024.xlsx" |> fs::path_home()
data_path <- "4and6inchPricesSp2024.xlsx"
data <- data_path |>
  readxl::read_xlsx() |>
  dplyr::select(Annuals, `Each per Tray`, matches("Planting Density"), matches(paste0("Price", if_else(price_level %in% 1:6, paste0(".",price_level), "$")))) |>
  dplyr::rename(Price = matches("Price"))



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
                        value = ""),
            # selectInput("units",
            #             "Choose area units",
            #             measurements::conv_unit_options$area,
            #             selected = "ft2"),
            selectInput("products",
                        "Select products to compare",
                        data$Annuals,
                        multiple = TRUE),
            tableOutput("refData")

        ),

        # Show a plot of the generated distribution
        mainPanel(
          tableOutput("estTable"),
          textOutput("disclaimer")
        )
    )
)



# Define server logic required to draw a histogram
server <- function(input, output) {

  output$refData <- renderTable({data})

  output$estTable <- renderTable({
    data |>
      dplyr::filter(Annuals %in% input$products) |>
      dplyr::mutate("Units (ea) Required" = input$area * `Planting Density (ea. per ft2)`,
             "Units Rounded up to Full Tray" =  (plyr::round_any(plyr::round_any(`Units (ea) Required`,1) / `Each per Tray`,1,ceiling) * `Each per Tray`) |> as.integer(),
             "Price Estimate per Full Tray" = `Units Rounded up to Full Tray` * Price,
             "Estimated Freight (7%)" = `Price Estimate per Full Tray` * freight,
             "Estimated Total" = `Price Estimate per Full Tray` + `Estimated Freight (7%)`) |>
      dplyr::mutate(across(all_of(c("Units (ea) Required","Units Rounded up to Full Tray")), function(x) {format(x) |> stringr::str_remove("(?<=[:punct:])0*$")}),
                    across(all_of(c("Price Estimate per Full Tray","Estimated Freight (7%)","Estimated Total")),
                      function(x) {x |>
                          cleaner::as.currency(currency_symbol = "$", as_symbol = TRUE) |>
                        format(currency_symbol = "$", as_symbol = TRUE)})) |>
      dplyr::select(!c(`Each per Tray`, matches("Planting Density"), Price)) |>
      tidyr::pivot_longer(!Annuals) |>
      tidyr::pivot_wider(names_from = Annuals) |>
      tibble::column_to_rownames("name")
  },spacing = "l", rownames = TRUE)

    output$disclaimer <- renderText({"This tool is provided to help choose between product options and is for estimation purposes only."})
}

# Run the application
shinyApp(ui = ui, server = server)

