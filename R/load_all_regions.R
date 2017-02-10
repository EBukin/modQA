#' Function for loading all variables for ordering items and elements form the XLS file
load_all_regions <-
  function(file = "data/AllVarsPlots.xlsx") {
    require(dplyr)
    require(readxl)
    
    readxl::read_excel(file, sheet = "areas_regions_order")
    
  }
