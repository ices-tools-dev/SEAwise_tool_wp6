#' The application User-Interface
#'
#' @param request Internal parameter for `{shiny}`.
#'     DO NOT REMOVE.
#' @import shiny
#' @importFrom bslib card card_body card_header
#' @noRd
app_ui <- function(request) {
  tagList(
    golem_add_external_resources(),

    navbarPage(
      theme = seawise_theme,
      position = "static-top",
      collapsible = TRUE,
      windowTitle = "SEAwise",
      id = "tabset",
      fluid = TRUE,
      title = span(tags$img(src ="www/PRIMARY_SeaWiseLOGO_Full Colour.png",
                            style = "padding-right:10px;padding-bottom:10px; padding-top:0px; margin-top: -10px",
                            height = "50px"), "Management Strategy Evaluation"),
      shiny::sidebarLayout(
        sidebarPanel = shiny::sidebarPanel(width = 2,
          shiny::selectInput(inputId = "mse_ecoregion",
                             label = "Select ecoregion", 
                             choices = c("All Ecoregions" = "all_ecoregions", 
                                         "Greater North Sea" = "North Sea",
                                         "Celtic Sea",
                                         "Bay of Biscay", 
                                         "Baltic Sea",
                                         "Central Mediterranean", "Eastern Mediterranean"), 
                             selected = "all_ecoregions"),
          shiny::selectInput(inputId = "mse_plot_id",
                             label = "Select plot for display", 
                             choices = c("Change in SSB" = "SSB",
                                         "F/Fmsy" = "F_ratio",
                                         "Change in\n average age" = "mean_age",
                                         "p(SSB < Blim)" = "ssb_blim",
                                         "Change in Fleet Landings" = "fleet_landings",
                                         "Change in value of Fleet Landings" = "fleet_landings_value",
                                         "Change in Stock Landings" = "stock_landings",
                                         "Change in value of Stock Landings" = "stock_landings_value"))),
        shiny::mainPanel(
          card(full_screen = T, 
            card_body(plotOutput("plot", height = "900px"), max_height_full_screen =  "100%", fill = T, )) 
        )
      )
    )
  )
}

#' Add external Resources to the Application
#'
#' This function is internally used to add external
#' resources inside the Shiny application.
#'
#' @import shiny
#' @importFrom golem add_resource_path activate_js favicon bundle_resources
#' @noRd
golem_add_external_resources <- function() {
  add_resource_path(
    "www",
    app_sys("app/www")
  ) 
  add_resource_path(
    "img",
    app_sys("app/img")
  )

  tags$head(
    favicon(ext = "png"),
    bundle_resources(
      path = app_sys("app/www"),
      app_title = "SEAwise"
    )
    # Add here other external resources
    # for example, you can add shinyalert::useShinyalert()
  )
}
