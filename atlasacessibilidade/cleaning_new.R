library(sf)
library(dplyr)
library(data.table)


# access and land use data for each city ------------------------------------------------------


# download data - get walk only for testing
acess_2017 <- aopdata::read_access(city = c("for", "spo", "rio", "mac"), 
                                   mode = c("walk", "bicycle", "public_transport"), 
                                   year = 2017) %>%
  filter(year == 2017)
acess_2018 <- aopdata::read_access(city = c("for", "spo", "rio", "mac"), 
                                   mode = c("walk", "bicycle", "public_transport"), 
                                   year = 2018) %>%
  filter(year == 2018)
acess_2019 <- aopdata::read_access(city = c("for", "spo", "rio", "mac"), 
                                   mode = c("walk", "bicycle", "public_transport"), 
                                   year = 2019) %>%
  filter(year == 2019)

# juntar
acess <- rbind(acess_2017, acess_2018, acess_2019)

# por enquanto, so pico
acess <- acess %>% filter(peak == 1)
# filtrar hexagonos vazios
acess <- acess %>% filter(!is.na(mode))

# filter columns
acess <- acess %>% 
  dplyr::select(id_hex, abbrev_muni, year, mode, 
                starts_with("P0"),
                starts_with("CMA"),
                starts_with("CMP"),
                starts_with("TMI"))

# fix wide format
acess <- acess %>%
  # ajeitar infinitos
  mutate_at(vars(starts_with("TMI")), function(x) ifelse(is.infinite(x), 120, x)) %>%
  # truncar valores acima de 30 minutos
  mutate_at(vars(starts_with("TMI")), function(x) ifelse(x > 30, 30, x))

# save for each city
# list every city
cities <- unique(acess$abbrev_muni)

purrr::walk(cities, function(x) readr::write_rds(setDT(acess)[abbrev_muni == x], 
                                                 sprintf("atlasacessibilidade/data/new/access_%s.rds", x)))




# hex -----------------------------------------------------------------------------------------

hex <- aopdata::read_grid(city = "all")

hex <- hex %>% dplyr::select(abbrev_muni, id_hex)
cities <- unique(hex$abbrev_muni)
purrr::walk(cities, function(x) readr::write_rds(setDT(hex)[abbrev_muni == x], 
                                                 sprintf("atlasacessibilidade/data/new/hex/hex_%s.rds", x)))
