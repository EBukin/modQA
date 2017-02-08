# Author: Eduard Bukin
# Version: 0.1.0
# Script for validating Commodity specific data per country
#     ____ ___  __  __ __  __  ___  ____ ___ _______   __
#    / ___/ _ \|  \/  |  \/  |/ _ \|  _ \_ _|_   _\ \ / /
#   | |  | | | | |\/| | |\/| | | | | | | | |  | |  \ V / 
#   | |__| |_| | |  | | |  | | |_| | |_| | |  | |   | |  
#    \____\___/|_|  |_|_|  |_|\___/|____/___| |_|   |_|  
#   
#                 ,---. 
#              ,.'-.   \ 
#            ( ( ,'"""""-. 
#            `,X          `. 
#            /` `           `._ 
#           (            ,   ,_\ 
#           |          ,---.,'o `. 
#           |         / o   \     ) 
#            \ ,.    (      .____, 
#             \| \    \____,'     \ 
#           '`'\  \        _,____,' 
#           \  ,--      ,-'     \ 
#             ( C     ,'         \ 
#              `--'  .'           | 
#                |   |         .O | 
#              __|    \        ,-'_ 
#             / `L     `._  _,'  ' `. 
#            /    `--.._  `',.   _\  ` 
#            `-.       /\  | `. ( ,\  \ 
#           _/  `-._  /  \ |--'  (     \ 
#          '  `-.   `'    \/\`.   `.    ) 
#                \  -hrr-    \ `.  |    | 
### Commodities lists 
commodity <- c("MK", "BT", "CH", "SMP", "WMP", "FDP", "BV", "PK",
               "PT", "SH", "CN", "FDN", "OOS", "PL", "PM", "SB", "VL")
# commodity <- c( "SH")

# Setups ------------------------------------------------------------------

# Installing packages
packs <- c("plyr", "dplyr", "tidyr","readxl", "stringr", 
           "gridExtra", "grid", "ggplot2", "ggthemes", "scales", "devtools")
lapply(packs[!packs %in% installed.packages()[,1]], 
       install.packages,
       dependencies = TRUE)
lapply(packs, require, character.only = TRUE)

# Loading additional functions previously developed
# devtools::install_github("EBukin/plotTS")
# library("plotTS")

# Loading locally developed functions
l_ply(str_c("R/", list.files("R/", pattern="*.R")), source)

# Loading data  -----------------------------------------------------------

# The year where projection starts
proj_year <- 2016

# Plottin grange
plotting_range <- c(2000:2026)

# Loading data --------------------------------------------------

# Loading data used for ordering items and elements during plotting
all_vars <- load_all_vars(file = "data/AllVarsPlots.xlsx")
all_regions <- load_all_regions(file = "data/AllVarsPlots.xlsx")

# Loading new model data
new_mod_c <- 
  load_troll_csv(file = "data/cosimobase.csv", d.source = "new_mod_2017" )%>% 
  filter(Year > proj_year)

# Loading historical data
mod_hist_c <-
  load_troll_csv(file = "data/cosimobase.csv", d.source = "hist") %>% 
  filter(Year <= proj_year)

# Loading old model data
old_mod_c <- 
  load_troll_csv(file = "data/lastBaseLine.csv", d.source = "old_mod_2016") %>% 
  filter(Year > proj_year - 1) %>% 
  filter(AreaCode %in% unique(new_mod_c$AreaCode))  

# Combining data and calculating missing variables -----------------------

# Combining cleaning
full_data <-
  bind_rows(new_mod_c, old_mod_c, mod_hist_c) %>% 
  select(AreaCode, ItemCode, ElementCode, Year, Value, d.source) %>% 
  
  # Removing areas which codes starts from "R."
  filter(!str_detect(AreaCode, "R\\."))

# Calculating
all_data_c <-
  full_data %>% 
  spread(ElementCode, Value) %>% 
  filter(ItemCode == "ME") %>% 
  select(AreaCode, Year, d.source, POP, CPI) %>% 
  rename(POP_ME = POP,
         CPI_ME = CPI) %>% 
  right_join(
    spread(full_data, ElementCode, Value), 
    by = c("AreaCode", "Year", "d.source")
    ) %>% 
  
  # Calculating consumption per capita and Real prices
  mutate(PR = PP / CPI_ME,
         PC = FO / POP_ME * 1000,
         PR = ifelse(ItemCode == "ME", NA, PR),
         PC = ifelse(ItemCode == "ME", NA, PC)) %>% 
  filter(ItemCode != "ME") %>% 
  select(-POP_ME, - CPI_ME) %>% 
  gather(ElementCode, Value, 5:length(.)) %>% 
  filter(!is.na(Value))

# Preparing data for plotting and ordering variables  ---------------------

p_data <-
  all_data_c  %>% 
  right_join(all_vars , by = c("ItemCode", "ElementCode")) %>% 
  
  # After we join data to the list of items and elements which we want to plot,
  #   we filter out all records where AreaCode is NA.
  filter(!is.na(AreaCode)) %>% 
  
  # Adding regions order
  left_join(all_regions, "AreaCode") %>%
  
  # Adding lables
  join_names()  %>% 
  arrange(RegionOrder, AreaCode, ItemOrder, ElementSubOrder, ElementOrder, Year) %>% 
  
  # Final filtering data
  filter(Year %in% plotting_range, ItemCode %in% commodity)


# Plotting everything -----------------------------------------------------


plot_pages_into_pdf(
  p_data,
  n_page = 9,
  output_path = "./output/",
  files_var = "ItemCode",
  groups_var = c("RegionOrder", "AreaCode", "ElementSubOrder") ,
  plots_var = c("ElementOrder")
)

# # Test plot
# p_data %>%
#   filter(AreaCode == "GHA") %>%
#   plot_group(.,
#              n_page = 9,
#              groups_var = c("RegionOrder", "AreaCode", "ElementSubOrder") ,
#              plots_var = c("ElementOrder"))

