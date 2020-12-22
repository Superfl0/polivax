#uiElements.R

#' @name ui_header
#' @title ui_header
#' @description generates the UI header
#' @rdname ui_header
#' @param i18n translation object from i18n package
#' @import shiny shiny.semantic
#' @export


ui_header <- function(i18n){
div(class = "container-fluid", style = "font-size: 30px; padding: 20px 20px 30px 20px; background-color: #008084; color:white; line-height: 1;", "Polivax",
    div(style = "float:right; padding: 0px 0px 0px 0px;",
        dropdown_menu(class = "ui right pointing dropdown", icon("large hamburger"), 
                      menu(menu_header(icon("file"), i18n$t("About"),is_item = FALSE), 
                           menu_item(icon("wrench"), i18n$t("Methods"), href = "#methods"), 
                           menu_item(icon("download"), i18n$t("Get graphs")), 
                           menu_divider(), 
                           menu_header(icon("user"), i18n$t("Credits"), is_item = FALSE), 
                           menu_item(icon("wordpress"), i18n$t("Author"), href = "http://sciflow.eu"), 
                           menu_item(icon("github"), i18n$t("Fork me"), href = "https://github.com/upfl0/polivax"),
                           menu_divider(), 
                           menu_header(icon("comment"), i18n$t("Change language"), is_item = FALSE),
                           selectInput('selected_language',
                                       "",
                                       choices = i18n$get_languages(),
                                       selected = "de")
                      ), 
                      name = "main_dropdown", is_menu_item = TRUE))
)
}

#' @name box_ui
#' @title box_ui
#' @description generates boxed content for the UI
#' @rdname box_ui
#' @param code elements to put box around
#' @export

box_ui <- function(code){
  div(class = "ui raised segment",
      code,
      div(style = "width: 100%; height: 10px"))
}

