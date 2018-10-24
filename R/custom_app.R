#' custom app for generating subgraphs
#' @param df_network    dataframe with atleast two columns (from, to, weight)
#' @param directed      Logical (default FALSE)
#' @param coloring      node colouring based on edge-betweeness-algorithm
#' @import shiny igraph visNetwork shinyjs
#' @export
#'

custom_app <- function(df_network = "random", directed = FALSE, coloring = TRUE){
  ui <- pageWithSidebar(
    headerPanel(substitute(df)),
    sidebarPanel(useShinyjs(),
                 selectInput("layout", "Layout",
                             c("layout_nicely","layout_in_circle",
                               "layout_as_star" ,"layout_with_kk" ,
                               "layout_with_fr"),  selected = "layout_with_fr"),
                 sliderInput("degree", "k cut-off", min = 0, max = 10, value = 0)),
    mainPanel(visNetworkOutput("network", width = 600, height = 600), style = "background: white")
  )
  server <- function(input, output, session) {
    data <- reactive({
      if(df_network == "random"){
        set.seed(0)
        df_network <- get.data.frame(erdos.renyi.game(40, 50, "gnm"))
      }
      g <- graph.data.frame(df_network, directed = directed)
      updateSliderInput(session, "degree", value = input$degree, min = 0, max = (max(degree(g))-1))
      g <- induced_subgraph(g, degree(g) >= input$degree)
      if(isTRUE(coloring) & directed == FALSE){
        V(g)$color <- membership(fastgreedy.community(g))
      }
      g <- visIgraph(g, layout = input$layout)
    })
    output$network <- renderVisNetwork(data())
  }
  shinyApp(ui, server, options = list(height = 800, width = 800))
}
