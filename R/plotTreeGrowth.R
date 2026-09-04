#' @include joinLocEvent.R
#' @include joinTreeData.R
#'
#' @title plotTreeGrowth: plot individual tree growth and mortality
#'
#' @importFrom dplyr arrange case_when lag lead left_join mutate select
#' @importFrom tidyr fill
#' @import ggplot2
#'
#' @description This function plots individual tree growth over time for all trees that were alive on their
#' first record in the plot. Trees that started out as dead are not plotted. Each line on a graph represents
#' the DBH of a live tree up until the first time it is classified as dead. Lines are color coded based
#' on growth between visits to indicate shrinkage, slowing of growth, and/or fast growth. Each tick or X on the
#' figure is a sample event for a given tree.
#'
#' @param park Combine data from all parks or one or more parks at a time. Valid inputs:
#' \describe{
#' \item{"all"}{Includes all parks in the network}
#' \item{"APCO"}{Appomattox Court House NHP only}
#' \item{"ASIS"}{Assateague Island National Seashore}
#' \item{"BOWA"}{Booker T. Washington NM only}
#' \item{"COLO"}{Colonial NHP only}
#' \item{"FRSP"}{Fredericksburg & Spotsylvania NMP only}
#' \item{"GETT"}{Gettysburg NMP only}
#' \item{"GEWA"}{George Washington Birthplace NM only}
#' \item{"HOFU"}{Hopewell Furnace NHS only}
#' \item{"PETE"}{Petersburg NBP only}
#' \item{"RICH"}{Richmond NB only}
#' \item{"SAHI"}{Sagamore Hill NHS only}
#' \item{"THST"}{Thomas Stone NHS only}
#' \item{"VAFO"}{Valley Forge NHP only}}
#'
#' @param subunit Filter park data by subunit. Must match units within a park. Valid inputs:
#' COLO: "JAMES", "YORK"
#' FRSP: "CHWILD", "FRED", "SPOT"
#' PETE: "EAST", "FIVE"
#'
#' @param from Year to start analysis, ranging from 2006 to current year
#' @param to Year to stop analysis, ranging from 2006 to current year
#'
#' @param locType Allows you to only include plots that are part of the GRTS sample design or include all plots,
#' such as deer exclosures
#' \describe{
#' \item{"VS"}{Only include plots that are part of the Vital Signs GRTS sample design}
#' \item{"all"}{Include all plots, such as plots in deer exclosures or test plots.}}
#'
#' @param panels Allows you to filter individual panels from 1 to 4. Default is all 4 panels (1:4).
#' If more than one panel is selected, specify by c(1, 3), for example.
#'
#' @param species Allows you to filter on species
#'
#' @param plotName Allows you to select a specific plot to run function for. Value inputs are "PARK-###", like "FRSP-001".
#' If no plot name is specified, or multiple plots are specified, the function will facet on plotName.
#'
#' @param elev_mort Logical. If TRUE, only includes plots that have experienced at least 5 percent mortality between a cycle and over
#' the time interval specified. If FALSE (default), includes all plots with at least one live tree of the species
#' specified.
#'
#' @param exc_wind Logical. If TRUE, excludes trees where windthrow was the likely cause of mortality, due to
#' a dead broken or dead fallen status the first time a tree was observed to be dead. If FALSE (default), includes
#' trees that died from windthrow or other causes.
#'
#' @param title Logical. If TRUE (default) will include a title for the graph, which either shows park, species, or subunit,
#' depending on the arguments specified.
#'
#' @return Returns a plot of tree growth rates over time, color coded by rate and whether a tree died.
#'
#' @examples
#' \dontrun{
#' importData()
#'
#' # Make plot of loblolly pine in PETE subunit Five Forks.
#' plotTreeGrowth(park = "PETE", subunit = "FIVE", species = "Pinus taeda")
#' 
#' # Make plot of loblolly pine in PETE subunit Eastern and Western Front.
#' plotTreeGrowth(park = "PETE", subunit = "EAST", species = "Pinus taeda")
#'
#' # Plot ash in GETT
#' plotTreeGrowth(park = "GETT", species = c("Fraxinus americana", "Fraxinus pennsylvanica", "Fraxinus nigra", "Fraxinus"))
#'
#' # Specific plots
#' plotTreeGrowth(plotName = "PETE-269")
#'
#' # Plot beech for GETT with vertical line where EAB was detected by crew
#' frax <- c("Fraxinus americana", "Fraxinus pennsylvanica", "Fraxinus nigra", "Fraxinus")
#' plotTreeGrowth(park = "GETT", species = frax) +
#'   geom_vline(xintercept = 2017, lty = 'dashed')
#'
#' # Plot all species of oaks in RICH
#' oaks <- joinTreeData(park = "RICH") |> filter(grepl("Quercus", ScientificName)) |> 
#'   select(ScientificName) |> distinct() 
#' plotTreeGrowth(park = "RICH", species = oaks$ScientificName)
#'
#' # Plot COLO plots in Jamestown unit
#' plotTreeGrowth(park = "COLO", subunit = "JAMES")
#'
#' # View RICH plots with elevated mortality
#' plotTreeGrowth(park = "RICH", elev_mort = T)
#'
#' # View RICH plots with elevated mortality not from windthrow
#' plotTreeGrowth(park = "RICH", elev_mort = T, exc_wind = T)
#'
#' }
#'
#' @export
#'
plotTreeGrowth <- function(park = 'all', from = 2007, to = as.numeric(format(Sys.Date(), "%Y")),
                           locType = c('VS', 'all'), panels = 1:4, subunit = NA,
                           plotName = NA_character_, species = NA_character_,
                           elev_mort = FALSE, exc_wind = FALSE, title = TRUE){
  
  options(warn = -1)
  
  # Match args and class
  park <- match.arg(park, several.ok = TRUE,
                    c("all", "APCO", "ASIS", "BOWA", "COLO", "FRSP", "GETT", "GEWA", "HOFU", "PETE",
                      "RICH", "SAHI", "THST", "VAFO"))
  stopifnot(class(from) == "numeric", from >= 2007)
  stopifnot(class(to) == "numeric", to >= 2007)
  stopifnot(panels %in% c(1, 2, 3, 4))
  locType <- match.arg(locType)
  stopifnot(nchar(plotName) == 8 | is.na(plotName))
  stopifnot(class(elev_mort) == "logical")
  stopifnot(class(exc_wind) == "logical")
  stopifnot(class(title) == "logical")
  
  # compile data
  trees_ind <- joinTreeData(park = park, from = from, to = to, QAQC = FALSE, panels = panels,
                            locType = locType, status = "active")
  
  live <- c("1", "AB", "AF", "AL", "AM", "AS", "RB", "RF", "RL", "RS")
  dead <- c("2", "DB", "DF", "DL", "DM", "DS")
  
  # filter on fxn args
  trees_ind1a <- if(any(!is.na(plotName))){filter(trees_ind, Plot_Name %in% plotName)
  } else {trees_ind}
  
  if(nrow(trees_ind1a) == 0){stop("Specified plotName resulted in a data frame with no records.
                                   Check plot spelling or that park, year range, etc. will return records.")}
  
  trees_ind1b <- if(any(!is.na(species))){filter(trees_ind1a, ScientificName %in% species)
  } else {trees_ind1a}
  
  
  if(nrow(trees_ind1b) == 0){stop("Specified species resulted in a data frame with no records.
                                  Check species spelling or that park, year range, etc. will return records.")}
  
  trees_ind1b$SubUnit <- substr(trees_ind1b$ParkSubUnit, 6, nchar(trees_ind1b$ParkSubUnit))
  
  trees_ind1c <- if(any(!is.na(subunit))){filter(trees_ind1b, SubUnit %in% subunit)
  } else {trees_ind1b}
  
  if(nrow(trees_ind1c) == 0){stop("Specified park subunit resulted in a data frame with no records.
                                  Check subunit spelling or that park, year range, etc. will return records.")}
  
  plot_evs <- joinLocEvent(park = park, from = from, to = to, QAQC = FALSE, panels = panels,
                           locType = locType) |>
    select(Plot_Name, SampleYear, cycle, IsStuntedWoodland)
  
  trees_ind2 <- left_join(trees_ind1c, plot_evs, by = c("Plot_Name", "cycle", "SampleYear")) |>
    #filter(IsStuntedWoodland == FALSE) |> # might turn off
    mutate(tag = sprintf("%02d", TagCode),
           tree_id = paste0(Plot_Name, "-", tag),
           status = ifelse(TreeStatusCode %in% live, "live",
                           ifelse(TreeStatusCode %in% dead, "dead",
                                  "unk")),
           subunit = sub("ACAD_", "", ParkSubUnit)) |>
    select(Plot_Name, tree_id, subunit, PanelCode, status, TreeStatusCode,
           SampleYear, cycle, ScientificName, DBHcm) |>
    arrange(Plot_Name, tree_id, SampleYear)  |>
    mutate(dbh_fill = DBHcm) |>
    tidyr::fill(dbh_fill, .direction = 'down', .by = c(Plot_Name, tree_id)) |> # fills for dead trees missing DBH
    mutate(dbh_prev = dplyr::lag(dbh_fill, 1),
           year_prev = dplyr::lag(SampleYear, 1),
           year_length1 = SampleYear - year_prev,
           year_length = ifelse(is.na(year_length1), 4, year_length1), # including a filler
           dbh_growth = (dbh_fill - dbh_prev)/year_length,
           .by = c(Plot_Name, tree_id))
  
  # Remove trees that started dead and after first dead record
  trees_ind3 <- trees_ind2 |>
    mutate(prev_stat = lag(status, 1),
           .by = c(Plot_Name, tree_id, subunit, PanelCode, ScientificName)) |>
    mutate(first_stat = first(status), .by = c(Plot_Name, tree_id)) |>
    filter(!prev_stat %in% 'dead') |> filter(!first_stat %in% "dead") |>
    mutate(windthrow1 = ifelse(grepl("B|L|F", TreeStatusCode), 1, 0)) |>
    mutate(windthrow = ifelse(sum(windthrow1 > 0), 1, 0),
           .by = c(Plot_Name, tree_id))
  
  trees_ind4 <- if(exc_wind == TRUE){
    filter(trees_ind3, windthrow == FALSE)
  } else {trees_ind3}
  
  # Select plots that have more than 1 tree die since 2006
  trees_ind5 <- trees_ind4 |>
    mutate(num_live = sum(status == "live"),
           num_dead = sum(status == 'dead'),
           elev_mort1 = ifelse(num_dead/num_live > 0.05, 1, 0),
           .by = c(Plot_Name, SampleYear, cycle)) |>
    mutate(elev_mort = ifelse(sum(elev_mort1 > 0), 1, 0),
           .by = Plot_Name) |>
    mutate(dbh_growth_prev = dplyr::lead(dbh_growth, 1), .by = c(Plot_Name, tree_id)) |>
    mutate(growth_color = factor(case_when(dbh_growth_prev < 0 ~ "shrink",
                                           dbh_growth_prev == 0 ~ "no growth",
                                           dbh_growth_prev > 0 & dbh_growth_prev < 0.25 ~ "slow",
                                           dbh_growth_prev >= 0.25 & dbh_growth_prev < 1 ~  "moderate",
                                           dbh_growth_prev >= 1 ~ "fast"),
                                 levels = c("shrink", "no growth", "slow", "moderate", "fast")))
  
  trees_ind6 <- if(elev_mort == TRUE){
    filter(trees_ind5, elev_mort == 1)
  } else {trees_ind5}
  
  facet_plot <- if(length(unique(trees_ind4$Plot_Name)) > 0) {TRUE} else {FALSE}
  
  xlimits = c(from - 0.1, to + 0.1)
  xbreaks = if(to - from > 8){seq(from, to, 4)} else {seq(from, to, 2)}
  
  plot_title <-
    if(title == TRUE){
      paste0("Tree growth for",
             if(any(!is.na(subunit))){paste0(" ", park, " ", subunit, collapse = ", ")} else NULL,
             if(length(panels) < 4){paste0(" panels ", paste0(panels, collapse = ", "))} else NULL,
             if(all(is.na(species))){paste0(" all species ")} else NULL,
             if(all(!is.na(c(species, subunit)))){paste0(" and")} else NULL,
             if(any(!is.na(species))){paste0(" ", species, collapse = ", ")} else NULL,
             if(all(is.na(c(species, subunit))) & !park %in% "all"){paste0("in ", park, collapse = ", ")} else NULL,
             if(elev_mort == TRUE & (all(is.na(plotName)))){paste0(" in plots with elevated mortality")} else NULL,
             if(exc_wind == TRUE){paste0(" excluding windthrow")} else NULL)
    } else {NULL}
  
  p <-
    suppressWarnings(
      ggplot(trees_ind6, aes(x = SampleYear, y = dbh_fill, group = tree_id)) +
        theme_FVM() +
        geom_line(aes(color = growth_color), linewidth = 1) +
        geom_point(aes(shape = status, size = status), stroke = 1) + #geom_smooth(span = 0.8, se = F) +
        scale_shape_manual(values = c("dead" = "X", "live" = "|"), name = 'Status at measurement') +
        scale_size_manual(values = c("dead" = 3, "live" = 2), name = 'Status at measurement') +
        scale_color_manual(values = c("#d7191c", "#fdae61", "#e6f598","#78c679", "#238443"),
                           breaks = c("shrink", "no growth", "slow", "moderate", "fast"),
                           labels = c("shrink: < 0 cm", "no growth: 0 cm", "slow: 0.1 - 0.25 cm",
                                      "moderate: 0.25 - 1 cm", "fast: > 1 cm"),
                           name = 'Annual Growth Rate') +
        {if(facet_plot == TRUE){facet_wrap(~Plot_Name)}} +
        scale_x_continuous(breaks = xbreaks,
                           limits = xlimits) +
        theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1)) +
        labs(y = "Tree DBH (cm)", x = NULL, title = plot_title)
    )
  
  return(suppressWarnings(p))
  
}
