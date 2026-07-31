#' Get the csdta corporative colours
#' 
#' @param colours character; a character vector of colour names to get. If not provided, all colours are returned.
#' @param show logical; if `TRUE`, the colours are displayed instead of returned.
#'
#' @returns Named string vector with csdta colours.
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
  # logic for printing or returning
  if(show){
    colours_matrix <- matrix(1:length(co), ncol = 1)
    image(1:length(co), 1, colours_matrix, col = co,
      asp = 1,xaxt = "n", yaxt = "n", xlab = "", ylab = "")
    print_labels <- purrr::imap_chr(co, ~paste0(.y,"\n\n",.x))
    text(x = 1:length(co), y = 1, labels = print_labels, cex = 1.5, col = "white")
  }else {
    # do not retunr NA values
    co[!is.na(co)]
  }
}


