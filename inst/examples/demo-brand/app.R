library(shiny)
library(bslib)

# A small app whose only job is to make branding visible. shinyelectron themes
# the Electron splash and window chrome from _brand.yml; bslib themes the app
# itself from the same file via brand = TRUE. If the brand is applied, the
# accent colour, background, and font all come from _brand.yml.
ui <- page_fluid(
  theme = bs_theme(),
  card(
    card_header("shinyelectron Brand Demo"),
    p("The histogram bars use the brand primary colour. The splash screen",
      "and window chrome should match."),
    sliderInput("n", "Observations", min = 50, max = 500, value = 200),
    plotOutput("plot")
  )
)

server <- function(input, output) {
  output$plot <- renderPlot({
    hist(rnorm(input$n), col = "#6d28d9", border = "white",
         main = "Brand Demo", xlab = "value")
  })
}

shinyApp(ui, server)
