#' The application server-side
#'
#' @param input,output,session Internal parameters for {shiny}.
#'     DO NOT REMOVE.
#' @import shiny
#' @importFrom magrittr %>% 
#' @noRd
app_server <- function(input, output, session) {
  
  output$plot <- shiny::renderPlot({
    
    if (input$mse_plot_id %in% c("SSB", "F_ratio", "mean_age", "ssb_blim")) {
      
      filtered_stocks <- stock %>% dplyr::filter(!stock_name %in% constant_cpue)
      plot_mse_generic(df = filtered_stocks, input = input$mse_plot_id, list_params = mse_plot_params, input$mse_ecoregion)
      
    } else if (input$mse_plot_id %in% c("fleet_landings", "fleet_landings_value")) {
      
      plot_mse_generic(df = total_landings_fleet, input = input$mse_plot_id, list_params = mse_plot_params, input$mse_ecoregion)
      
    } else if (input$mse_plot_id %in% c("stock_landings", "stock_landings_value")) {
      
      plot_mse_generic(df = total_landings_stock, input = input$mse_plot_id, list_params = mse_plot_params, input$mse_ecoregion)
      
    } else if (input$mse_plot_id == "regional_change") {
      
      plot_mse_indicators_regional(df = tab_stock)
      
    }
    
  })
  
  
}
