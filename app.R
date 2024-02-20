#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    https://shiny.posit.co/
#

# Load packages
pacs <- c("shiny", "shinyjs", "fs", "measurements", "readxl", "dplyr", "tidyr", "plyr", "cleaner", "data.table", "tibble")
sapply(pacs, require, character = TRUE)

# Retrieve data
price_level <- 0
freight <- 0.07
data_path <- "Greenstreet Growers/TeamSite - Documents/Shared/Production Greenstreet/Production Finished/Spring 2024/pricing/4and6inchPricesSp2024.xlsx" |> fs::path_home()
data <- data_path |>
  read_xlsx() |>
  select(Annuals, `Each per Tray`, matches("Planting Density"), matches(paste0("Price[:space:]?", if_else(price_level %in% 1:6, price_level|> as.character(), ""),"$"))) |>
  dplyr::rename(Price = matches("Price")) ## |>
  # dplyr::mutate(Price = Price |> cleaner::as.currency(currency_symbol = "$", as_symbol = TRUE))


# Define UI for application that draws a histogram
ui <- fluidPage(
  # ShinyJS is suppose to improve user experience
  useShinyjs(),

    # Application title
    titlePanel("Planting Area Pricing Tool"),

    # Sidebar with a slider input for number of bins
    sidebarLayout(
        sidebarPanel(
            numericInput("area",
                        "Enter your bed area",
                        min = 0,
                        value = ""),
            selectInput("units",
                        "Choose area units",
                        measurements::conv_unit_options$area,
                        selected = "ft2"),
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

# input_products <- data$Annuals[1:2]
# input_area <- 100
# tmp <- data |>
#   filter(Annuals %in% input_products) |>
#   mutate("Units (ea) Required" = input_area * `Planting Density (ea. per ft2)`,
#          "Units Rounded up to Full Tray" =  round_any(round_any(`Units (ea) Required`,1) / `Each per Tray`,1,ceiling) * `Each per Tray`,
#          "Price Estimate per Full Tray" = `Units Rounded up to Full Tray` * Price,
#          "Estimated Freight (7%)" = `Price Estimate per Full Tray` * freight,
#          "Estimated Total" = `Price Estimate per Full Tray` + `Estimated Freight (7%)`) |>
#   data.table::transpose(make.names = "Annuals") |>
#   remove_rownames() |>
#   column_to_rownames("description")
# data |>
#   filter(Annuals %in% input_products) |>
#   mutate("Units (ea) Required" = input_area * `Planting Density (ea. per ft2)`,
#          "Units Rounded up to Full Tray" =  (round_any(round_any(`Units (ea) Required`,1) / `Each per Tray`,1,ceiling) * `Each per Tray`) |> as.integer(),
#          "Price Estimate per Full Tray" = `Units Rounded up to Full Tray` * Price,
#          "Estimated Freight (7%)" = `Price Estimate per Full Tray` * freight,
#          "Estimated Total" = `Price Estimate per Full Tray` + `Estimated Freight (7%)`) |>
#   select(!c(`Each per Tray`, matches("Planting Density"), Price)) |>
#   remove_rownames() |>
#   # dplyr::mutate(across(everything(),as.character)) |>
#   pivot_longer(!Annuals) |>
#   pivot_wider(names_from = Annuals) |>
#   column_to_rownames("name")


# pivot_wider()# Define server logic required to draw a histogram
server <- function(input, output) {

  output$refData <- renderTable({data})

  output$estTable <- renderTable({
    data |>
      filter(Annuals %in% input$products) |>
      mutate("Units (ea) Required" = input$area * `Planting Density (ea. per ft2)`,
             "Units Rounded up to Full Tray" =  (round_any(round_any(`Units (ea) Required`,1) / `Each per Tray`,1,ceiling) * `Each per Tray`) |> as.integer(),
             "Price Estimate per Full Tray" = `Units Rounded up to Full Tray` * Price,
             "Estimated Freight (7%)" = `Price Estimate per Full Tray` * freight,
             "Estimated Total" = `Price Estimate per Full Tray` + `Estimated Freight (7%)`) |>
      mutate(across(any_of(`Units (ea) Required`),
                    function(x) prettyNum(x, drop0trailing = TRUE, zero.print = FALSE)),
             across(any_of(`Units Rounded up to Full Tray`),
                    function(x) prettyNum(x, drop0trailing = TRUE, zero.print = FALSE)),
             across(any_of(`Price Estimate per Full Tray`,`Estimated Freight (7%)`,`Estimated Total`),
                    function(x) {x |> as.currency(currency_symbol = "$", as_symbol = TRUE) |> as.character()})) |>
      select(!c(`Each per Tray`, matches("Planting Density"), Price)) |>
      pivot_longer(!Annuals) |>
      pivot_wider(names_from = Annuals) |>
      column_to_rownames("name")
  },spacing = "l", rownames = TRUE)

    output$disclaimer <- renderText({"This tool is provided to help choose between product options for estimating purposes only."})
}

# Run the application
shinyApp(ui = ui, server = server)

