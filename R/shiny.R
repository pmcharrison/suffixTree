# The beginnings of a Shiny demo app

sanitise_input <- function(x) {
  x <- gsub("[^A-Za-z0-9 ]", "", x)
  strsplit(x, " ")[[1]]
}


#' Run Shiny demo
#'
#' Runs a Shiny demo for the tst package.
#' @param max_seq_length Maximum length of sequence that can be added each time.
#' @param order_bound Order bound of the suffix tree.
#' @export
demo_suffix_tree <- function(max_seq_length = 20, order_bound = 10) {
  ui <- shiny::fluidPage(
    # title = "tst demo",
    shiny::titlePanel("tst demo"),
    shiny::sidebarLayout(
      shiny::sidebarPanel(
        shiny::textAreaInput(
          "input_string", label = "Input string",
          width = 300,
          height = 100,
          placeholder = paste0("Write your input string here. ",
                               "Non-alphanumeric characters will be removed. ",
                               "Separate symbols with spaces. ")),
        shiny::checkboxInput("terminal_sym", "Add terminal symbol?", value = TRUE),
        shiny::checkboxInput("continue_seq", "Continue previous sequence?"),
        shiny::p(
          shiny::actionButton("add_to_tree", "Add to tree"),
          shiny::actionButton("reset_tree", "Reset tree")
        ),
        shiny::p(paste0("Tree order bound: ", order_bound, ".")),
        shiny::p(paste0("Maximum sequence length: ", max_seq_length, "."))),
      shiny::mainPanel(
                       # DiagrammeR::grVizOutput("tree_plot")) #, width = "100%", height = "760px"))
                       shiny::imageOutput("tree_plot", width = "100%"),
                       width = "400px")
                       # DiagrammeR::DiagrammeROutput("tree_plot"))
  ))
  server <- function(input, output) {
    rv <- shiny::reactiveValues(tree = new_tree(order_bound = order_bound),
                                plot_up_to_date = stats::runif(1))

    # output$tree_plot <- DiagrammeR::renderDiagrammeR(plot(rv$tree, shiny = TRUE))
    # output$tree_plot <- DiagrammeR::renderGrViz({
    #   plot(rv$tree, shiny = TRUE)
    # })

    shiny::observeEvent(input$add_to_tree, {
      add_seq(tree = rv$tree,
              seq = sanitise_input(input$input_string),
              reset_active_nodes = !shiny::isolate(input$continue_seq),
              terminate = shiny::isolate(input$terminal_sym),
              save = TRUE)
      rv$plot_up_to_date <- stats::runif(1)
    })

    shiny::observeEvent(input$reset_tree, {
      rv$tree <- new_tree(order_bound = order_bound)
      rv$plot_up_to_date <- stats::runif(1)
    })

    output$tree_plot <- shiny::renderImage({
      rv$plot_up_to_date
      path <- tempfile(fileext = ".svg")
      DiagrammeR::export_graph(plot(shiny::isolate(rv$tree), shiny = TRUE),
                               file_name = path,
                               width = 400,
                               file_type = "svg")
      list(src = path, alt = "Suffix tree plot")
    }, deleteFile = FALSE)


  }
  shiny::shinyApp(ui = ui, server = server)
}
