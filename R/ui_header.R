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

  modal_body_taglist <- htmltools::tagList(
    p("Lorem ipsum dolor sit amet, consectetur adipiscing elit. 
            Pellentesque vitae ipsum et ipsum imperdiet placerat. 
            Mauris sodales ac nibh vitae eleifend. Suspendisse fermentum 
            dolor eget ante lobortis, eu dictum felis venenatis. 
            Aenean fringilla vehicula dui vel sollicitudin. Praesent 
            facilisis, nisi et ullamcorper facilisis, ex ex blandit velit, 
            a ullamcorper est lacus finibus elit. Nunc a urna nec sem 
            placerat congue non et diam. In rutrum suscipit convallis. 
            Nam sit amet neque non lorem feugiat mollis sed id lorem. 
            Maecenas tristique rhoncus lectus, eget aliquet massa fermentum 
            nec."),
            p("Vivamus gravida elementum diam. Maecenas a luctus neque. 
            Integer molestie nibh tellus, id tempus diam convallis et. 
            Curabitur tempus sem mauris, non interdum dui blandit sit amet. 
            Nunc condimentum diam eget venenatis fringilla. Ut quis enim 
            venenatis lacus feugiat volutpat id at purus. Nullam iaculis 
            diam id viverra dignissim."),
            p("Etiam nec aliquam neque. Morbi metus ipsum, molestie tincidunt 
            cursus at, consequat ut elit. Etiam egestas consectetur tempus. 
            In tincidunt, ligula ut bibendum lacinia, purus ligula gravida 
            urna, vitae pretium ex augue vitae elit. Sed faucibus a tellus 
            a volutpat. Ut consectetur, nisl in elementum iaculis, tellus 
            arcu imperdiet turpis, vel fringilla tortor felis ut dui. 
            Integer sit amet semper felis, non venenatis urna."),
            p("Quisque porta mauris sit amet tortor cursus faucibus. Suspendisse 
            velit tortor, dignissim ac odio ac, dapibus fermentum quam. 
            Maecenas nulla est, sollicitudin in vehicula sit amet, pellentesque 
            sed ex. Nam felis urna, mattis et faucibus nec, malesuada tempus 
            libero. Vivamus nec massa nibh. Donec eget ligula sapien. 
            Pellentesque ornare lectus at urna consectetur venenatis."),
            p("Vivamus sed rutrum metus. Curabitur sit amet est tincidunt 
            elit pellentesque placerat. Proin non erat mi. Maecenas porttitor 
            pretium purus, quis dictum justo aliquam vitae. Nunc eros tortor, 
            sollicitudin quis nisi in, mollis commodo purus. Integer a est elit. 
            Vivamus a pellentesque turpis. Maecenas id metus id felis mattis 
            commodo posuere vel lacus. Aliquam eleifend bibendum magna, 
            aliquet tincidunt diam imperdiet eu. In congue nibh nisl, 
            sollicitudin congue ex molestie quis. Morbi non nibh mollis 
            turpis posuere accumsan ac sit amet sem. Nunc vestibulum urna 
            ac magna porta porttitor.")
  )

  modal_body_taglist <- modal_body_taglist |> c(modal_body_taglist)

  htmltools::withTags(
    tagList(
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
                  `data-bs-toggle`="modal",
                  `data-bs-target`="#theory-modal",
                  "Theory"
                )
              },
              if (f_args[["credits"]]) {
                button(
                  id = "button-credits",
                  class = "btn btn-primary",
                  `data-bs-toggle`="modal",
                  `data-bs-target`="#credits-modal",
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
    ),
    
    # theory modal
    div(
      class = "modal fade",
      id = "theory-modal",
      tabindex = "-1",
      `aria-labelledby` = "theoryModal",
      `aria-hidden` = "true",
      div(
        class = "modal-dialog modal-fullscreen",
        div(
          class = "modal-content",
          div(
            class = "modal-header",
            h1(
              class = "modal-title fs-5",
              id = "exampleModalLabel",
              "Theory"
            ),
            button(
              type = "button",
              class = "btn-close",
              `data-bs-dismiss` = "modal",
              `aria-label` = "Close"
            )
          ),
          div(
            class = "modal-body",
            modal_body_taglist
          ),
          div(
            class = "modal-footer justify-content-between",
            button(
              type = "button",
              class = "btn btn-secondary",
              span(class = "glyphicon glyphicon-print me-2"),
              "Export"
            ),
            button(
              type = "button",
              class = "btn btn-secondary",
              `data-bs-dismiss` = "modal",
              "Close"
            )
          )
        )
      )
    )

  # tagsList END 
  )

  # withTags END
  )

}