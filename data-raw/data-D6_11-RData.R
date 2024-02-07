## code to prepare `data/` datasets goes here

load("data-raw/D6_11_catch.RData")
load("data-raw/D6_11_stock.RData")


total_landings_stock <- catch %>% group_by(region, case_study, fleet_dynamics, HCR, period, stock) %>%  
  summarize_at(c('landings', 'value_of_landings'), sum)  %>% group_by(region, case_study, fleet_dynamics, HCR, stock) %>% 
  mutate(ratio_landings       = landings/landings[period == '2021'],
         ratio_value          = value_of_landings/value_of_landings[period == '2021'],
         ratio_landings_trunc = ifelse(abs(ratio_landings) > 100, 100*sign(ratio_landings), ratio_landings),
         ratio_value_trunc    = ifelse(abs(ratio_value) > 100, 100*sign(ratio_value), ratio_value)) %>% 
  arrange(region, fleet_dynamics, HCR, stock, period) %>% 
  mutate(position = length(stock), stock = reorder(as.factor(stock), position))


total_landings_fleet <- catch %>% group_by(region, case_study, fleet_dynamics, HCR, period, fleet) %>%  
  summarize_at(c('landings', 'value_of_landings'), sum) %>%  
  group_by(region, case_study, fleet_dynamics, HCR, fleet) %>%  
  mutate(ratio_landings       = landings/landings[period == '2021'],
         ratio_value          = value_of_landings/value_of_landings[period == '2021'],
         ratio_landings_trunc = ifelse(abs(ratio_landings) > 100, 100*sign(ratio_landings), ratio_landings),
         ratio_value_trunc     = ifelse(abs(ratio_value) > 100, 100*sign(ratio_value), ratio_value)) %>% 
  arrange(region, fleet_dynamics, HCR, fleet, period) %>% 
  mutate(position = length(fleet), fleet = reorder(as.factor(fleet), position))


tab_stock <- stock %>% group_by(region, period, fleet_dynamics, HCR) %>% filter(period != '2021') %>% 
  summarize_at(c('ratio_f_fmsy', 'ratio_average_age', 'ratio_ssb'), mean, na.rm=T) %>% 
  arrange(region, period, fleet_dynamics, HCR)


tab_stock_land <- total_landings_stock %>% filter(!(region == 'Bay of Biscay' & ratio_landings == Inf))%>% group_by(region, period, fleet_dynamics, HCR) %>% filter(period != '2021') %>% 
  summarize_at(c('ratio_landings'), mean, na.rm=T) %>% 
  arrange(region, period, fleet_dynamics, HCR) 


tab_stock <- tab_stock %>% bind_cols(tab_stock_land[,-(1:4)]) %>% tidyr::pivot_longer(cols = 5:8, names_to = 'indicator') %>% 
  mutate(value_trunc = ifelse(value > 100,100, value))



mse_plot_params <- list(
        SSB = list(y = "stock",
                        var = "ratio_ssb_trunc",
                        col = list(low = "darkblue",
                                   mid = "white",
                                   high = "darkred"),
                        midpoint = 0,
                        title = "Change\n in SSB",
                        lims = c(-100,100),
                        breaks = seq(-100, 100, 50),
                        y_lab = "Stock"),
        F_ratio = list(y = "stock",
                        var = "ratio_f_fmsy_trunc",
                        col = list(low = "darkblue",
                                   mid = "white",
                                   high = "darkred"),
                        midpoint = 0,
                        title = "F/Fmsy",
                        lims = c(-100,100),
                        breaks = seq(-100, 100, 50),
                        y_lab = "Stock"),
        mean_age = list(y = "stock",
                        var = "ratio_average_age_trunc",
                        col = list(low = "darkblue",
                                   mid = "white",
                                   high = "darkred"),
                        midpoint = 0,
                        title = "Change in\n average age",
                        lims = c(-100,100),
                        breaks = seq(-100, 100, 50),
                        y_lab = "Stock"),
        ssb_blim = list(y = "stock",
                        var = "pBlim",
                        col = list(low = "darkblue",
                                   mid = "white",
                                   high = "darkred"),
                        midpoint = 5,
                        title = "p(SSB < Blim)",
                        lims = c(0,100),
                        breaks = seq(0, 100, 25),
                        y_lab = "Stock"),
  fleet_landings = list(y = "fleet",
                        var = "ratio_landings_trunc",
                        col = list(low = "white",
                                   mid = "darkblue",
                                   high = "darkgreen"),
                        midpoint = 50,
                        title = "Change\n in Landings",
                        lims = c(0,100),
                        breaks = seq(0, 100, 25),
                        y_lab = "Fleet"),
  fleet_landings_value = list(y = "fleet",
                        var = "ratio_value_trunc",
                        col = list(low = "white",
                                   mid = "darkblue",
                                   high = "darkgreen"),
                        midpoint = 50,
                        title = "Change\n in Landings",
                        lims = c(0,100),
                        breaks = seq(0, 100, 25),
                        y_lab = "Fleet"),
  stock_landings = list(y = "stock",
                        var = "ratio_landings_trunc",
                        col = list(low = "white",
                                   mid = "darkblue",
                                   high = "darkgreen"),
                        midpoint = 50,
                        title = "Change\n in Landings",
                        lims = c(0,100),
                        breaks = seq(0, 100, 25),
                        y_lab = "Stock"),
  stock_landings_value = list(y = "stock",
                        var = "ratio_value_trunc",
                        col = list(low = "white",
                                   mid = "darkblue",
                                   high = "darkgreen"),
                        midpoint = 50,
                        title = "Change\n in Landings",
                        lims = c(0,100),
                        breaks = seq(0, 100, 25),
                        y_lab = "Stock")
)



usethis::use_data(stock, overwrite = T)
usethis::use_data(catch, overwrite = T)
usethis::use_data(tab_stock, overwrite = T)
usethis::use_data(tab_stock_land, overwrite = T)
usethis::use_data(total_landings_fleet, overwrite = T)
usethis::use_data(total_landings_stock, overwrite = T)
usethis::use_data(mse_plot_params, overwrite = T)
