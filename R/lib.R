#' Create or run a simple shiny app given a module name
#' 
#' The purpose of this function is to be able to examine and test any Shiny module on this
#' packages without having to develop an actual shiny app.
#'
#' @param module Character. The module name in snake case.
#' @param id Character. Module's id, "m" by default as there is no need to change it.
#' @param ui_args Named list. Arguments to be passed to the UI module's function.
#' @param server_args Named list. Arguments to be passed to the Server module's function.
#'
#' @returns The app shiny object.
#'
#' @export
#' 
#' @rdname create_run_module_app
#' 
#' @examples
#' create_module_app("plotDistOneline",server_args =  list(x = shiny::reactive(rnorm(100))))
#' 
#' 
create_module_app <- function(module,id = "m",ui_args = list(),server_args = list()) {
   
  ui <- shiny::fluidPage(
    shiny::titlePanel(paste0("Module: ",module)),
    shiny::fluidRow(
        shiny::tags$h3("UI Arguments (ui_args)"),
        shiny::tags$code(ui_args |> substitute() |> deparse()),
        shiny::tags$h3("Server Arguments (server_args)"),
        shiny::tags$code(server_args |> substitute() |> deparse())
    ),
    shiny::tags$div(
        style = "border: 2px solid #333333; padding: 15px; border-radius: 5px; margin: 10px 0;",
        shiny::fluidRow(
          do.call(paste0(module,"UI"),list("id" = id) |> c(ui_args))
        )
    )
  )

  server <- function(input, output, session) {
    do.call(paste0(module,"Server"),list("id" = id) |> c(server_args))
  }
  shiny::shinyApp(ui, server)  
}

#' @rdname create_run_module_app
#' 
#' @returns NULL This function is called for its side effects.
#'
#' @export
run_module_app <- function(module,id = "m",ui_args = list(),server_args = list()){
  shiny::runApp(create_module_app(module,id,ui_args,server_args))
  invisible(NULL)
}