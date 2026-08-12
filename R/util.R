deparse_list <- function(l){
  l |> substitute() |> deparse()
}