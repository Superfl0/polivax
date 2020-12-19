#uiElements.R

#' @export
#' @rdname uiElements

library(shiny)
library(ggplot2)
library(tidyverse)
library(shiny.semantic)

main_menu <- function(){
  menu(menu_item("Polivax"), 
       dropdown_menu("Menu", 
                   menu(menu_header(icon("file"), "About",is_item = FALSE), 
                        menu_item(icon("wrench"), "Methods"), 
                        menu_item(icon("download"), "Get graph"), 
                        menu_divider(), 
                        menu_header(icon("user"), "Credits", is_item = FALSE), 
                        menu_item(icon("wordpress"), "Author"), 
                        menu_item(icon("github"), "Fork me", link="https://github.com/Superfl0/polivax")), 
                   class = "", name = "main_dropdown", is_menu_item = TRUE)
       )
}


box_ui <- function(code){
  div(class = "ui raised segment",
      code,
      div(style = "width: 100%; height: 10px"))
}

