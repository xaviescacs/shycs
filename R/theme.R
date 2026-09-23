#' Get the bslib csdta theme
#'
#' @returns The csdta bs_theme object.
#'
#' @export
csdta_bs_theme <- function(){
  bslib::bs_theme(version = 5,preset = "bootstrap")
}

# PENDING: add css rules that apply to the class csdta-header 
# to be able to differenciate the header from the rest if
# necessary.