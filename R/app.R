#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#


# Define UI for application that draws a histogram
ui <- shiny::fluidPage(

    # Application title
  shiny::titlePanel("Password encoder"),

    # Sidebar with a slider input for number of bins
  shiny::sidebarLayout(
    shiny::sidebarPanel(
      shiny::textInput("name",
                      "Input (alphanumeric)",
                      value = "",
                      width = NULL,
                      placeholder = "Text"),
      shiny::sliderInput("n",
                      "Password length",
                      min = 8,
                      max = 32,
                      value = 12),
      shiny::numericInput("seed",
                      "Forced seed value",
                      value = NA,
                      width = NULL)
        ),

        # Show a plot of the generated distribution
    shiny::mainPanel(
          shiny::textOutput("password")
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  get_pass <- function(x, n=15, force_seed=NULL){
    if(!is.na(force_seed)){
      set.seed(force_seed)
    } else{
      set.seed(floor(log(as.numeric(paste(match(strsplit(tolower(x), "")[[1]], letters), collapse="")))))
    }
    paste(sample(c(letters, LETTERS, 0:9, "_", "!"), n, prob = c(rep(1/n, 62), 1, 1/n)), collapse="")
  }

  output$password <- shiny::renderText({
        get_pass(input$name, n=input$n, force_seed=input$seed)
    })
}

# Run the application
run_app <- function(options = list()){
  shiny::shinyApp(ui = ui, server = server, options = options)
}
