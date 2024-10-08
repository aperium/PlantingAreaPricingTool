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

# Retrieve Freight Rates
freight_path <- "data/freight.xlsx"
freight_data <- freight_path |>
  readxl::read_xlsx()

# Retrieve users
users_path <- "data/Client.xlsx"
users <- users_path |>
  readxl::read_xlsx()

# Retrieve data
data_path <- "data/4and6inchPricesFa2024.xlsx"
# data <- data_path |>
#   readxl::read_xlsx()

# Retrieve special pricing data
special_pricing_data <- "data/special_prices.xlsx" |>
  readxl::read_xlsx()

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
  p <- "(([π])|(?<![:digit:])3.14[:digit:]*)"
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

# function for calculating density to spacing using packing models
# based on http://datagenetics.com/blog/june32014/index.html
# can expand using optimal cirlce packing solutions as a table or perhaps fitted function http://hydra.nat.uni-magdeburg.de/packing/csq/csq.html
# currently n does nothing.
distance <- function(density, n=1, method = c("avg","hex","sqr")) {
  d_hex = pi/6*3^(1/2)
  d_sqr = pi/4
  d_calc = switch ("avg",
    "avg" = (d_hex*d_sqr)^(1/2),
    "hex" = d_hex,
    "sqr" = d_sqr)
  (d_calc/pi/density)^(1/2)*2
}

# UI Element for user to enter the area
input_area <- textInput("dimentions",
                        "Or enter your bed area (sqft) or dimentions (ft)")
input_choose_shape <- radioButtons("shape",
                                   "What shape is the bed?",
                                   choices = c("rectangle", "triangle", "circle"),
                                   inline = TRUE)



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
          input_choose_shape,
          uiOutput("dimentionEntry"),
          input_area,
          helpText("Tip: Modify your calculations using this box ↑",tags$br(),
                   "For example, if there are 2 identical beds type \"×2\" at the end.",tags$br(),tags$br()),
            # selectInput("units",
            #             "Choose area units",
            #             measurements::conv_unit_options$area,
            #             selected = "ft2"),
            # selectInput(inputId = "products",
            #             label = "Select products to compare",
            #             choices = data()$Annuals,
            #             multiple = TRUE)
          uiOutput("productOptions")
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
    usr()$PROF_COD_3 |> str_extract("(?<=^WHSLPRICE)[123456]$")
      # switch(WHSLPRICE6 = 6,
      #        WHSLPRICE5 = 5,
      #        WHSLPRICE4 = 4,
      #        WHSLPRICE3 = 3,
      #        WHSLPRICE2 = 2,
      #        WHSLPRICE1 = 1)
  })

  freight <- reactive({
    usr()$CUST_NO |> req()
    i <- freight_data$account_number |> purrr::detect_index(\(x) {str_equal(x,usr()$CUST_NO, ignore_case = TRUE) |> isTruthy()})
    if (i <= 0) i <- freight_data$account_pattern |> purrr::detect_index(\(x) {str_detect(usr()$NAM |> str_to_title(), x |> str_to_title()) |> isTruthy()})
    if (i <= 0) i <- freight_data$freight_rate |> purrr::detect_index(\(x) {x == max(freight_data$freight_rate)})
    slice(freight_data, i)$freight_rate
  })

  data <- reactive({
    price_level() |> req()
    usr()$CUST_NO |> req()
    data_path |>
      readxl::read_xlsx() |>
      dplyr::select(Annuals, `Each per Tray`, matches("Planting Density"), matches(paste0("((Price).*(",format(price_level()),"))|(\2.*\1)"))) |>
      dplyr::rename(Price = matches("Price")) |>
      # full_join(special_pricing_data |> filter(str_detect(usr()$NAM |> str_to_title(), `Customer Name` |> str_to_title())) |> select(-"Customer Name")) |>
      dplyr::rows_upsert(special_pricing_data |> filter(str_detect(usr()$NAM |> str_to_title(), `Customer Name` |> str_to_title())) |> select(-"Customer Name"), by = "Annuals") |>
      # summarise(Price = min(Price), .by = !c("Price")) |>
      arrange(Price) |>
      dplyr::mutate("Plant Spacing (in)" = `Planting Density (ea. per ft2)` |> distance() |> measurements::conv_unit("ft","in"), .after = `Planting Density (ea. per ft2)`)
  })

  output$dimentionEntry <- renderUI({
    input$shape |> req()
    switch (input$shape,
      "rectangle" = {fluidRow(numericInput("length","Rectangle length (ft):",0,min=0,max =.Machine$double.xmax),numericInput("width","Rectangle width (ft):",0,min=0,max =.Machine$double.xmax))},
      "triangle" = {fluidRow(numericInput("base","Triangle Base length (ft):",0,min=0,max =.Machine$double.xmax),numericInput("height","Triangle height (ft):",0,min=0,max =.Machine$double.xmax))},
      "circle" = {numericInput("diameter","Circle width (ft):",0,min=0,max =.Machine$double.xmax)}
    )
  })

  dimentionCalcdArea <- reactive({
    input$shape |> req()
    switch (input$shape,
            "rectangle" = req(input$length>0, input$width>0),
            "triangle" = req(input$base>0, input$height>0),
            "circle" = req(input$diameter>0)
    )
    switch (input$shape,
            "rectangle" = input$length * input$width,
            "triangle" = (input$base * input$height)/2,
            "circle" = pi*(input$diameter/2)^2
    )
  })

  observe({
    updateTextInput(inputId = "dimentions",
                    value = dimentionCalcdArea() |> paste("sqft"),
                    label = "Your bed area in sqft (or enter your own area formula):")
  })

  output$productOptions <- renderUI({
    usr()$CUST_NO |> req()
    selectInput(inputId = "products",
                label = "Select products to compare",
                choices = data()$Annuals,
                multiple = TRUE)
  })

  output$uidText <- renderText({
    if(usr()$NAM |> isTruthy()) paste0("Welcome, ", usr()$NAM, "!")
    else paste("Account number",input$uid,"not found.")
  }) |> bindEvent(input$uidSubmit)

    output$refData <- renderTable({
    data() |> req()
    data() |>
      dplyr::mutate(
        Price = Price |> cleaner::as.currency(currency_symbol = "$", as_symbol = TRUE) |> format(currency_symbol = "$", as_symbol = TRUE),
        across(all_of(c("Planting Density (ea. per ft2)", "Each per Tray")), function(x) {format(x) |> stringr::str_remove("[:punct:]0*$")}))
    },spacing = "xs", align = 'c', html.table.attributes = "style=\"max-width:800px;margin-left:auto;margin-right:auto;table-layout:auto;\"")

    output$refTitle <- renderUI({
      data() |> req()
      "Unit Prices and Recommended Planting Densities"
    })

  output$estTable <- renderTable({

    # calculate area if given dimentions
    input$dimentions |> str_squish() |> req()
    input$products |> req()
    area <- input$dimentions |> parse_area()

    freight() |> req()
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
      dplyr::select(!c(`Each per Tray`, matches("Planting Density"), matches("Plant Spacing"), Price)) |>
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

