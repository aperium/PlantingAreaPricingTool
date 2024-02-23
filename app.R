#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    https://shiny.posit.co/
#

# Load packages
pacs <- c("shiny", "dplyr", "rlang", "stringr")
sapply(pacs, require, character = TRUE)

shinyOptions(shiny.sanitize.errors = FALSE,
             shiny.suppressMissingContextError = TRUE)

# Retrieve users
users_path <- "data/customers.xlsx"
users <- users_path |>
  readxl::read_xlsx()


# Retrieve data
# price_level <- 0
# freight <- 0.07
data_path <- "data/4and6inchPricesSp2024.xlsx"
# data <- data_path |>
#   readxl::read_xlsx()
#   dplyr::select(Annuals, `Each per Tray`, matches("Planting Density"), matches(paste0("Price", if_else(price_level %in% 1:6, paste0(".",price_level), "$")))) |>
#   dplyr::rename(Price = matches("Price"))

# Retrieve Logo Image
logo_height <- 60
logo_path <- "images/2022_Greenstreet_Logo_HorizontalAlign_Semi-Bold_BrownText.png" |>
  fs::path_wd() |>
  normalizePath()

# A user entry parsing support function
str_strip_sqft <- function(s) {
  s |>
    str_squish() |>
    str_to_lower() |>
    str_remove("^[:space:]*=") |>
    str_remove_all("(?<=[:digit:])[:space:]?(sq)?f((ee)|(oo))?t(\\^?2)?|(sq)?")
}

# A user entry parsing support function
str_strip_ft <- function(s) {
  s |>
    str_squish() |>
    str_to_lower() |>
    str_remove("^[:space:]*=") |>
    str_remove_all("(?<=[:digit:][:space:]?)((f((ee)|(oo))?t)|('))")
}

# A user entry parsing support function
str_correct_multiply <- function(s) {
  s |>
    str_squish() |>
    str_to_lower() |>
    str_replace_all("x|(by)|(times)|×","*")
}

# A user entry parsing support function
str_correct_pi <- function(s) {
  s <- s |>
    str_squish() |>
    str_to_lower()
  p <- "((π)|(?<![:digit:])3.14[:digit:]*)"
  m <- s |> str_extract(p)
  l <- str_length(m)
  if (!isTruthy(m)) s
  else if(m |> str_equal("π")) str_replace_all(s, "π"," pi ")
  else if(m |> str_equal(str_trunc(pi, l, ellipsis = ""))) str_replace_all(s, p," pi ")
  else if(m |> str_equal(format(pi, TRUE, l-1, scientific = FALSE))) str_replace_all(s, p," pi ")
  else s
}

# A user entry parsing function
parse_area <- function(s) {
  if (is.numeric(s)) {s}
  else {
    s |>
      str_strip_sqft() |>
      str_strip_ft() |>
      str_correct_multiply() |>
      str_correct_pi() |>
      rlang::parse_expr() |>
      eval()
  }
}


# Define UI for application
ui <- fluidPage( theme = bslib::bs_theme(bootswatch = "lumen") |> bslib::bs_add_rules(".well { background-color: #ECECEC }"),

  # Branding
  imageOutput("gglogo", height = paste0(logo_height,"px")),

    # Application title
    titlePanel("Bed Area Planting & Pricing Tool"),

    # input
    sidebarLayout(
        sidebarPanel(
          fluidRow(
            textInput("uid", "Enter your business account number"),
            actionButton("uidSubmit","Submit", width = 'fit-content')
            ),
          textOutput("uidText"),
          tags$hr(),
            textInput("dimentions",
                      "Enter your bed area (sqft) or dimentions (ft)"),
          helpText("area of a rectangle = length × width",tags$br(),
                   "area of a triangle = 1/2 base × height",tags$br(),
                   "area of a circle = π × radius^2"),
            # selectInput("units",
            #             "Choose area units",
            #             measurements::conv_unit_options$area,
            #             selected = "ft2"),
            selectInput("products",
                        "Select products to compare",
                        data$Annuals,
                        multiple = TRUE)
        ),

        # Show output
        mainPanel(
          uiOutput("estTitle", container = tags$h3),
          tableOutput("estTable"),
        )
    ),
  tags$hr(),
  uiOutput("refTitle", container = tags$h3, style="text-align:center"),
  tableOutput("refData"),
  tags$p("© 2024 Greenstreet Growers, Inc. All Rights Reserved.", style="text-align:center")
)



# Define server logic
server <- function(input, output) {

  output$gglogo <- renderImage({
    list(src = logo_path,
         contentType = 'image/png',
         alt="Greenstreet Growers Logo",
         height = logo_height)
    }, deleteFile = FALSE)

  usr <- reactive({
    # input$uid |> req()
    users |>
      slice(purrr::detect_index(CUST_NO,\(x) str_equal(x,input$uid |> str_squish(), ignore_case = TRUE)))
  })  |>
    bindEvent(input$uidSubmit)

  price_level <- reactive({
    usr()$PROF_COD_3 |> req()
    usr()$PROF_COD_3 |>
      switch(WHSLPRICE6 = 6)
  })

  freight <- reactive({
    usr()$FREIGHT |> req()
    usr()$FREIGHT |> as.numeric()
  })

  data <- reactive({
    price_level() |> req()
    data_path |>
      readxl::read_xlsx() |>
      dplyr::select(Annuals, `Each per Tray`, matches("Planting Density"), matches(paste0("Price", if_else(price_level() %in% 1:6, paste0(".",price_level()), "$")))) |>
      dplyr::rename(Price = matches("Price"))

  })

  output$uidText <- renderText({
    if(usr()$NAM_UPR |> isTruthy()) paste0("Welcome, ", usr()$NAM_UPR |> str_to_title(), "!")
    else paste("Account number",input$uid,"not found.")
  }) |> bindEvent(input$uidSubmit)

    output$refData <- renderTable({
    data() |> req()
    data() |>
      dplyr::mutate(
        Price = Price |> cleaner::as.currency(currency_symbol = "$", as_symbol = TRUE) |> format(currency_symbol = "$", as_symbol = TRUE),
        across(all_of(c("Planting Density (ea. per ft2)", "Each per Tray")), function(x) {format(x) |> stringr::str_remove("[:punct:]0*$")}))
    },spacing = "xs", align = 'c', html.table.attributes = "style=\"max-width:700px;margin-left:auto;margin-right:auto;table-layout:auto;\"")

    output$refTitle <- renderUI({
      data() |> req()
      "Unit Prices and Recommended Planting Densities"
    })

  output$estTable <- renderTable({

    # calculate area if given dimentions
    input$dimentions |> str_squish() |> req()
    input$products |> req()
    area <- input$dimentions |> parse_area()

    data() |> req()
    data() |>
      dplyr::filter(Annuals %in% input$products) |>
      dplyr::mutate("Units (ea) Required" = area * `Planting Density (ea. per ft2)`,
             "Units Rounded up to Full Tray" =  (plyr::round_any(plyr::round_any(`Units (ea) Required`,1) / `Each per Tray`,1,ceiling) * `Each per Tray`) |> as.integer(),
             "Price Estimate per Full Tray" = `Units Rounded up to Full Tray` * Price,
             "Estimated Freight" = `Price Estimate per Full Tray` * freight(),
             "Estimated Total" = `Price Estimate per Full Tray` + `Estimated Freight`) |>
      dplyr::mutate(across(all_of(c("Units (ea) Required","Units Rounded up to Full Tray")), function(x) {format(x) |> stringr::str_remove("[:punct:]0*$")}),
                    across(all_of(c("Price Estimate per Full Tray","Estimated Freight","Estimated Total")),
                      function(x) {x |>
                          cleaner::as.currency(currency_symbol = "$", as_symbol = TRUE) |>
                          format(currency_symbol = "$", as_symbol = TRUE)})) |>
      dplyr::select(!c(`Each per Tray`, matches("Planting Density"), Price)) |>
      tidyr::pivot_longer(!Annuals) |>
      tidyr::pivot_wider(names_from = Annuals) |>
      mutate(name = if_else(str_equal(name,"Estimated Freight"),paste0(name," (",freight() |> scales::percent(),")"),name)) |>
      tibble::column_to_rownames("name")
  },spacing = "l", rownames = TRUE, hover = TRUE, align = 'c', caption = "This tool is provided to help choose between product options and is for estimation purposes only.")

  output$estTitle <- renderUI({
    input$dimentions |> str_squish() |> req()
    input$products |> req()
    data() |> req()
    "Calculated Estimates"
    })

}

# Run the application
shinyApp(ui = ui, server = server)

