htmltools::tagList(
  bslib::bs_theme_dependencies(csdta_bs_theme()),
  ui_csdta_header(languages = c("ca","es","en"),title = "This is a reasonable long title for a shiny app."),
  htmltools::div(class = "container",
    selectInput("dataset", label = "Dataset", choices = ls("package:datasets")),
    verbatimTextOutput("summary"),
    tableOutput("table")
  )
)

