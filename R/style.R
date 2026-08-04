#' Get or show the csdta colours
#' 
#' The csdta brnad has a series of corporative colors that are used in the plots and other elements of
#' the shiny apps.
#' 
#' @param colours a subseting vector to filter colours. If `NULL` provided, all colours are selected.
#' @param show logical; if `TRUE`, the colours are displayed instead of returned.
#'
#' @returns Named string vector with csdta colours in hex format.
#'
#' @examples
#' get_csdta_colours()
#' get_csdta_colours(1)
#' get_csdta_colours(c("red","bright_green"))
#' get_csdta_colours(show = TRUE)
#' 
#' @export
get_csdta_colours <- function(colours = NULL,show = FALSE){
  # defined corporative csdta colors
  co <- c(
    "red" = "#da0932ff",
    "dark" = "#0e2626ff",
    "bright_green" = "#2fed64ff",
    "bright_blue" = "#2f97edff"
  )
  # sanity checks on colours 
  if(!is.null(colours)){
    if(!is.character(colours) && !(is.numeric(colours) && all(colours == floor(colours)))){
      stop("The colours vector must be either a character vector or a numeric vector.")
    }
    if(length(colours) > length(co)){
      stop(sprintf("Two many positions provided, the length must be between 1 and %s",length(co)))
    }
    if(is.character(colours) && (length(setdiff(colours,names(co))) > 0)){
      warning("Some provided colours do not exist. See the full list with names(get_csdta_colours()).")
    }     
    co <- co[colours]
  }
  # logic for either printing or returning
  if(show){
    print_labels <- purrr::imap_chr(co, ~paste0(.y,"\n\n",.x)) |> c(rep("",8 - length(co)))
    co <- unname(co) |> c(rep("white",8 - length(co)))
    # invert the colors so the first drawn is on the upper row
    co <- c(co[5:8],co[1:4])
    colours_matrix <- matrix(1:length(co), ncol = 2)
    image(1:4, 1:2, colours_matrix, col = co,
      asp = 1,axes = FALSE, xlab = "", ylab = "")
    # note that labels are not inverted and hence y = 2:1 in the following line
    grid_coords <- expand.grid(x = 1:4, y = 2:1)
    text(x = grid_coords$x, y = grid_coords$y, labels = print_labels, cex = 1.5, col = "white")
  }else {
    # do not retunr NA values
    co[!is.na(co)]
  }
}

#' Show csdta colors
#' 
#' Helper function that calls `get_csdta_colours()` with `show = TRUE` so it doesn return anything.
#'
#' @param colours a subseting vector to filter colours. If `NULL` provided, all colours are selected.
#'
#' @export
#' @examples
#' show_csdta_colours()
#' show_csdta_colours(c(1:3))
#' show_csdta_colours(c("red","bright_green"))
#' 
show_csdta_colours <- function(colours = NULL){
  get_csdta_colours(colours,show = TRUE)
}



#' Toy function to show colours
#'
toy_colours_show <- function(){
  cols8 <- c("#1F77B4", "#FF7F0E", "#2CA02C", "#D62728", "#9467BD", "#8C564B", "#E377C2", "#7F7F7F")
  colours_matrix <- matrix(1:length(cols8), ncol = 2)
  image(1:4, 1:2, colours_matrix, col = cols8,
      asp = 1,axes = FALSE, xlab = "", ylab = "")
  grid_coords <- expand.grid(x = 1:4, y = 1:2)
  text(x = grid_coords$x, y = grid_coords$y, labels = 1:8, cex = 1.5, col = "black")
}


