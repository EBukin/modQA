#' Function for loading all variables for ordering items and elements form the XLS file
load_all_vars <-
  function(file = "data/AllVarsPlots.xlsx") {
    require(dplyr)
    require(readxl)
    
    # Order of variables for plotting used in data
    readxl::read_excel(file, sheet = "to_plot") %>%
      mutate(ItemOrder = rownames(.)) %>%
      select(ItemOrder, ItemCode, everything()) %>%
      gather(Element, ElementCode, 3:length(.)) %>%
      separate(Element, c("ElementSubOrder", "ElementOrder"), sep = "_") %>%
      mutate(
        ItemOrder = as.integer(ItemOrder),
        ElementSubOrder = as.integer(ElementSubOrder),
        ElementOrder = as.integer(ElementOrder)
      ) %>%
      filter(!is.na(ElementCode))
  }

