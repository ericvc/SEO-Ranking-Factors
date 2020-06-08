#' 
#' @description Create an input control field (see 'shiny' package) that can be placed alongside
#' existing textInput field. In other words, this function allows multiple text inputs using a horizontal
#' layout.
#' @details #See ?shiny::textInput for additional details.
#' @param size string. Indicates display size of text input field. Options are 'small' or 'large'.
#' @import shiny
#' @return An HTML Tag Object definition (see ?shiny::tag).
#'
textInputRow <- function(inputId, label, value = "", size="small",...){
  
  stopifnot(size %in% c("small", "large"))
  size_class <- paste0("input-",size)
  
  div(style="display:inline-block",
      tags$label(label, `for` = inputId), 
      tags$input(id = inputId, type = "text", value = value, class=size_class))

}