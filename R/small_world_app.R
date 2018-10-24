#' small world simulation
#'
#' @import igraph shiny shinyjs visNetwork
#' @export
#'
#'
#'
#'
small_world_app <- function(){
  ui <- pageWithSidebar(
    headerPanel("Small-World Simulation"),
    sidebarPanel(useShinyjs(),
                 sliderInput("n", "nodes", min = 10, max = 5000, value = 20),
                 selectInput("nei", "neighbours", choices = 1:10),
                 selectInput("p", "p", choices = seq(0,1, by = .1), selected = 0),
                 selectInput("seed", "seed", choices = c(FALSE, 0:10), selected = 5),
                 actionButton("reset", "reset")
    ),
    mainPanel(fixedRow(column(12, h3(textOutput("text2")),align = "center")),
              visNetworkOutput("network", width = 800, height = 800)

    )
  )
  server <- function(input, output, session) {
    observeEvent(input$reset, {
      reset("p")
      reset("nei")
      reset("n")
    })
    g <- reactive({
      if(input$seed != 0){
        set.seed(input$seed)
      }
      network <- watts.strogatz.game(1, input$n, input$nei, input$p)
      special <- sample(1:vcount(network), 2)
      sp <- shortest.paths(network, special[1], special[2], mode = "all")
      md <- mean_distance(network)
      V(network)$color <- "blue"
      V(network)$size  <- 5
      V(network)$size[special] <- (input$n * .03) + 15
      V(network)$color[special] <- "red"
      e <- shortest_paths(network, special[1], special[2], output = "both")[[2]]
      E(network)$color <- "blue"
      E(network)$color[unlist(e)] <- "orange"
      E(network)$width <- 1.5
      E(network)$width[unlist(e)] <-  (input$n * .03) + 15
      net <- visIgraph(network, layout = "layout.circle")
      l <- list(net, sp, md)
    })
    output$text2 <- renderText(paste("steps: ", g()[[2]], " |  mean: ", round(g()[[3]], 2)))

    output$network <- renderVisNetwork(g()[[1]])
  }
  shinyApp(ui, server, options = list(height = 900, width = 1000))
}
