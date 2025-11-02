#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    https://shiny.posit.co/
#

xfun::pkg_attach("dplyr", "magrittr", "shiny", "gt")

ui <- fluidPage(
  titlePanel("Kandidátka"),
  sidebarLayout(
    sidebarPanel(
      selectInput("region", "Volební kraj", choices = NULL),
      selectInput("party", "Volební strana", choices = NULL),
      width = 3
    ),
    mainPanel(
      gt_output("table")
    )
  )
)

server <- function(input, output, session) {
  candidates <- readRDS("psp/data/candidates.rds")
  
  # Populate region choices
  observe({
    updateSelectInput(session, "region",
                      choices = sort(unique(candidates$VOLKRAJ))
    )
  })
  
  # Populate party choices dynamically
  observeEvent(input$region, {
    parties <- candidates |>
      filter(VOLKRAJ == input$region) |>
      distinct(NAZ_STR) |>
      pull()
    updateSelectInput(session, "party", choices = sort(parties))
  })
  
  # Reactive filtered table
  filtered_data <- reactive({
    req(input$region, input$party)
    candidates %>%
      filter(VOLKRAJ == input$region, NAZ_STR == input$party)
  })
  
  # Render with gt
  output$table <- render_gt({
    filtered_data() %>%
      arrange(PORADIMAND, PORADINAHR) %>%
      select(JMENO, PRIJMENI, VEK, PORADIMAND) %>%
      gt() %>%
      tab_header(
        title = paste0(input$party, ", ", input$region),
      ) %>%
      cols_label(
        JMENO = "Jméno",
        PRIJMENI = "Příjmení",
        VEK = "Věk",
        PORADIMAND = "Poř.",
      ) %>%
      tab_options(
        table.font.size = 14,
        heading.title.font.size = 18,
        heading.subtitle.font.size = 14
      )
  })
}

# Run the application 
shinyApp(ui, server)
