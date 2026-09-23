#' Get the header for a csdta shiny app
#'
#' @param title Character of length 1. Title of the app.
#' @param theory Boolean. Indicates whether or not to show the theory button.
#' @param credits Boolean. Indicates whether or not to show the credits button.
#' @param languages Character. Languages to be displayed, the sorting will be respcted.
#' @param csdta_link Character of length 1. href of the csdta logo anchor.
#' @param github_link Character of length 1. href of the github logo anchor.
#'
#' @returns The header shyni.tag object
#'
#' @export
#' @examples
#' 
#' ui_csdta_header(languages = "ca",title = "This is an app title")
ui_csdta_header <- function(title,theory=TRUE,credits=TRUE,languages,csdta_link="/",github_link="/"){
  # arguments are saved into a list to avoid using existing html tags within htmltools::withTags
  f_args <- as.list(environment())

  # dynamically construct the languages ul content
  final_languages <- intersect(languages,c("ca","es","en")) 
  if(length(final_languages) > 1){
    languages_li_tags <- final_languages |> 
        purrr::map(~ htmltools::withTags(li(a(class = "nav-link px-1 text-white", ref = "#", .x))))
  }else{
    languages_li_tags <- ""
  }

  htmltools::withTags(
    header(
      class = "csdta-header p-0 text-bg-dark",
      div(
        class = "container",
        div(
          class = "d-flex flex-wrap align-items-center justify-content-center",
          div(
            class = "d-flex align-items-center justify-content-center me-auto",
            a(
              class = "me-3",
              href = f_args[["csdta_link"]],
              img(
                src = "logo_header.svg",
                alt = "csdta logo",
                width = "64px",
                height = "64px"
              )
            ),
            h3(
              class = "mb-0",
              f_args[["title"]]
            )
          ),
          div(
            class = "d-flex my-2",
            div(
              class = "btn-group mx-3",
              if (f_args[["theory"]]) {
                button(
                  id = "button-theory",
                  class = "btn btn-primary",
                  "Theory"
                )
              },
              if (f_args[["credits"]]) {
                button(
                  id = "button-credits",
                  class = "btn btn-primary",
                  "Credits"
                )
              }
            ),
            ul(
              class = "nav mx-3",
              languages_li_tags
            ),
            a(
              class = "mx-3",
              href = f_args[["github_link"]],
              img(
                src = "GitHub_Invertocat_White.svg",
                alt = "GitHub logo",
                width = "40px",
                height = "40px"
              )
            )
          )
        )
      )
    )
  )

}