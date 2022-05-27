library(sf)
library(dplyr)
library(data.table)


# access and land use data for each city ------------------------------------------------------


# download data - get walk only for testing
acess_20171 <- aopdata::read_access(city = c("for", "spo", "rio", "mac"), 
                                   mode = c("walk"), 
                                   year = 2017) %>%
  filter(year == 2017)
acess_20172 <- aopdata::read_access(city = c("for", "spo", "rio", "mac"), 
                                   mode = c("car"), 
                                   year = 2017) %>%
  filter(year == 2017)
acess_20173 <- aopdata::read_access(city = c("for", "spo"), 
                                   mode = c("public_transport"), 
                                   year = 2017) %>%
  filter(year == 2017)

acess_20181 <- aopdata::read_access(city = c("for", "spo", "rio", "mac"), 
                                   mode = c("walk"), 
                                   year = 2018) %>%
  filter(year == 2018)
acess_20182 <- aopdata::read_access(city = c("for", "spo", "rio", "mac"), 
                                   mode = c("car"), 
                                   year = 2018) %>%
  filter(year == 2018)
acess_20183 <- aopdata::read_access(city = c("for", "spo", "rio"), 
                                   mode = c("public_transport"), 
                                   year = 2018) %>%
  filter(year == 2018)

acess_20191 <- aopdata::read_access(city = c("for", "spo", "rio", "mac"), 
                                   mode = c("walk"), 
                                   year = 2019) %>%
  filter(year == 2019)
acess_20192 <- aopdata::read_access(city = c("for", "spo", "rio", "mac"), 
                                   mode = c("car"), 
                                   year = 2019) %>%
  filter(year == 2019)
acess_20193 <- aopdata::read_access(city = c("for", "spo", "rio"), 
                                   mode = c("public_transport"), 
                                   year = 2019) %>%
  filter(year == 2019)

# juntar
acess <- rbind(acess_20171, acess_20172, acess_20173,
               acess_20181,acess_20182, acess_20183, 
               acess_20191, acess_20192, acess_20193, 
               fill = TRUE)

# por enquanto, so pico
acess <- acess %>% filter(peak == 1)
# filtrar hexagonos vazios
acess <- acess %>% filter(!is.na(mode))

# filter columns
acess <- acess %>% 
  dplyr::select(id_hex, abbrev_muni, year, mode, 
                starts_with("P001"),
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
                                                 sprintf("atlasacessibilidade/data/new/access/access_%s.rds", x)))




# landuse -----------------------------------------------------------------
landuse_2017 <- aopdata::read_landuse(city = c("for", "spo", "rio", "mac"), 
                                    year = 2017,
                                    geometry = FALSE) %>%
  dplyr::filter(year == 2017)
landuse_2018 <- aopdata::read_landuse(city = c("for", "spo", "rio", "mac"), 
                                    year = 2018,
                                    geometry = FALSE) %>%
  dplyr::filter(year == 2018)
landuse_2019 <- aopdata::read_landuse(city = c("for", "spo", "rio", "mac"), 
                                     geometry = FALSE,
                                    year = 2019) %>%
  dplyr::filter(year == 2019)

# juntar
landuse <- rbind(landuse_2017, landuse_2018,landuse_2019, fill = TRUE)
landuse <- landuse %>% dplyr::filter(P001 > 0)

# filter columns
landuse <- landuse %>% 
  dplyr::select(id_hex, abbrev_muni, year, 
                # rename vars
                PT = P001,
                # PH, PM,
                PB = P002, PN = P003, PI = P004, PA = P005,
                P0005I = P010, P0614I = P011, P1518I = P012, P1924I = P013, P2539I = P014, P4069I = P015, P70I = P016,
                starts_with("R0"),
                TT = T001, TB = T002, TM = T003, TA = T004,
                ET = E001, EI = E002, EF = E003, EM = E004,
                MT = M001, MI = M002, MF = M003, MM = M004,
                ST = S001, SB = S002, SM = S003, SA = S004,
                CT = C001)
                

# save for each city
# list every city
cities <- unique(landuse$abbrev_muni)

purrr::walk(cities, function(x) readr::write_rds(setDT(landuse)[abbrev_muni == x], 
                                                 sprintf("atlasacessibilidade/data/new/landuse/landuse_%s.rds", x)))


# hex -----------------------------------------------------------------------------------------

hex <- aopdata::read_grid(city = "all")

hex <- hex %>% dplyr::select(abbrev_muni, id_hex)
cities <- unique(hex$abbrev_muni)
purrr::walk(cities, function(x) readr::write_rds(setDT(hex)[abbrev_muni == x], 
                                                 sprintf("atlasacessibilidade/data/new/hex/hex_%s.rds", x)))
