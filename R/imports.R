# Global Vars *****************************************************####
utils::globalVariables(c(":=", "mess", "models","warning", "warns"))

#' @importFrom parallel makeCluster mclapply detectCores stopCluster
#' @importFrom future.apply future_lapply
#' @importFrom future plan
#' @importFrom landscapemetrics calculate_lsm
#' @importFrom stats reshape AIC BIC median na.omit as.formula terms terms.formula
#' @importFrom dplyr filter %>% group_by all_of bind_cols bind_rows full_join left_join mutate rename pull select
#' @importFrom tidyr pivot_wider pivot_longer nest unnest drop_na as_tibble
#' @importFrom purrr map
#' @importFrom utils installed.packages globalVariables
#' @importFrom sf as_Spatial st_as_sf
#' @importFrom landscapemetrics calculate_lsm list_lsm
#' @importFrom raster mask raster crop stack brick aggregate buffer extent nlayers levels ratify merge
#' @importFrom methods as
#' @importFrom grDevices hcl.colors recordPlot n2mfrow
#' @importFrom graphics par abline axis title
#' @importFrom rasterVis levelplot
#' @importFrom rlang .data
#' @importFrom ggplot2 ggplot aes geom_density facet_wrap facet_grid coord_flip theme_minimal theme scale_fill_manual scale_color_manual scale_fill_manual labs geom_line element_blank geom_point aes_string as_labeller scale_color_brewer stat_summary element_line element_text scale_fill_stepsn
#' @importFrom broomExtra glance tidy augment
NULL
