#' Get the csdta corporative colours
#' 
#' @param show logical; if `TRUE`, the colours are displayed instead of returned.
#'
#' @returns Named string vector with csdta colours.
#'
#' @export
get_csdta_colours <- function(show = FALSE){
  colours <- c(
    "red" = "#da0932ff",
    "dark" = "#0e2626ff",
    "bright_green" = "#2fed64ff",
    "bright_blue" = "#2f97edff"
  )
  if(show){
    colours_matrix <- matrix(1:length(colours), ncol = 1)
    image(1:length(colours), 1, colours_matrix, col = colours,
      asp = 1,xaxt = "n", yaxt = "n", xlab = "", ylab = "")
    print_labels <- purrr::imap_chr(colours, ~paste0(.y,"\n\n",.x))
    text(x = 1:length(colours), y = 1, labels = lab, cex = 1.5, col = "white")
  }else{
    colours
  }
}


