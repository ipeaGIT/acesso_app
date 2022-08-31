library(sf)
library(dplyr)
library(data.table)


# access and land use data for each city ------------------------------------------------------

# my_read_access <- function(modo, ...) {
#   
#   aopdata::read_access(mode = modo, ...)
#   
# }
# 
# 
# # download data - get walk only for testing
# acess_20171 <- lapply(c("walk", "bicycle", "car"), my_read_access,
#                       city = "all", 
#                       year = 2017) %>%
#   rbindlist(fill = TRUE) %>%
#   dplyr::filter(year == 2017)
# 
# acess_20172 <- aopdata::read_access(city = c("for", "spo", "bho", "poa", "cam", "cur"), 
#                                     mode = c("public_transport"), 
#                                     year = 2017) %>%
#   dplyr::filter(year == 2017)
# 
# acess_20181 <- lapply(c("walk", "bicycle", "car"), my_read_access,
#                       city = "all", 
#                       year = 2018) %>%
#   rbindlist(fill = TRUE) %>%
#   dplyr::filter(year == 2018)
# 
# acess_20182 <- aopdata::read_access(city = c("for", "spo", "bho", "poa", "cam", "cur", "rio"), 
#                                     mode = c("public_transport"), 
#                                     year = 2018) %>%
#   dplyr::filter(year == 2018)
# 
# 
# acess_20191 <- lapply(c("walk", "bicycle", "car"), my_read_access,
#                       city = "all", 
#                       year = 2019) %>%
#   rbindlist(fill = TRUE) %>%
#   dplyr::filter(year == 2019)
# 
# acess_20192 <- aopdata::read_access(city = c("for", "spo", "bho", "poa", "cam", "cur", "rio", "rec", "goi"), 
#                                     mode = c("public_transport"), 
#                                     year = 2019) %>%
#   dplyr::filter(year == 2019)
# 
# # juntar
# acess <- rbind(acess_20171, acess_20172, 
#                acess_20181,acess_20182,  
#                acess_20191, acess_20192,  
#                fill = TRUE)

acess_2017 <- rbind(readRDS("../../data/acesso_oport/output_base_final/2017/dados2017_AcessOport_access_active_v1.0.rds") %>% st_set_geometry(NULL),
                    readRDS("../../data/acesso_oport/output_base_final/2017/dados2017_AcessOport_access_tpcar_v1.0.rds") %>% st_set_geometry(NULL),
                    fill = TRUE)
acess_2018 <- rbind(readRDS("../../data/acesso_oport/output_base_final/2018/dados2018_AcessOport_access_active_v1.0.rds") %>% st_set_geometry(NULL),
                    readRDS("../../data/acesso_oport/output_base_final/2018/dados2018_AcessOport_access_tpcar_v1.0.rds") %>% st_set_geometry(NULL),
                    fill = TRUE)
acess_2019 <- rbind(readRDS("../../data/acesso_oport/output_base_final/2019/dados2019_AcessOport_access_active_v1.0.rds") %>% st_set_geometry(NULL),
                    readRDS("../../data/acesso_oport/output_base_final/2019/dados2019_AcessOport_access_tpcar_v1.0.rds") %>% st_set_geometry(NULL),
                    fill = TRUE)

acess <- rbind(acess_2017, 
               acess_2018,
               acess_2019
               )

# rename
acess <- acess %>%
  rename(peak = pico, mode = modo, year = ano, abbrev_muni = sigla_muni, name_muni = nome_muni) %>%
  mutate(mode = case_when(mode == "tp" ~ "public_transport",
                          mode == "carro" ~ "car",
                          mode == "bicicleta" ~ "bicycle",
                          mode == "caminhada" ~ "walk"))


# por enquanto, so pico
acess <- acess %>% filter(peak == 1)
# filtrar hexagonos vazios
acess <- acess %>% filter(!is.na(mode))

# bring pop
landuse <- readRDS(sprintf("../../data/acesso_oport/output_base_final/%s/dados%s_AcessOport_landuse_v1.0.rds", "2017", "2017")) %>%
    setDT() %>%
  distinct(id_hex, P001)

# filter columns
acess <- acess %>% 
  left_join(landuse, by = "id_hex") %>%
  dplyr::select(id_hex, name_muni, abbrev_muni, year, mode, 
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
# landuse_2017 <- aopdata::read_landuse(city = "all", 
#                                       year = 2017,
#                                       geometry = FALSE) %>%
#   dplyr::filter(year == 2017)
# landuse_2018 <- aopdata::read_landuse(city = "all", 
#                                       year = 2018,
#                                       geometry = FALSE) %>%
#   dplyr::filter(year == 2018)
# landuse_2019 <- aopdata::read_landuse(city = "all", 
#                                       geometry = FALSE,
#                                       year = 2019) %>%
#   dplyr::filter(year == 2019)
# 
# # juntar
# landuse <- rbind(landuse_2017, landuse_2018,landuse_2019, fill = TRUE)
landuse <- rbind(
  # oepn all access
  readRDS(sprintf("../../data/acesso_oport/output_base_final/%s/dados%s_AcessOport_landuse_v1.0.rds", "2017", "2017")) %>%
    setDT(),
  readRDS(sprintf("../../data/acesso_oport/output_base_final/%s/dados%s_AcessOport_landuse_v1.0.rds", "2018", "2018")) %>%
    setDT(),
  readRDS(sprintf("../../data/acesso_oport/output_base_final/%s/dados%s_AcessOport_landuse_v1.0.rds", "2019", "2019")) %>%
    setDT())
landuse <- landuse %>% dplyr::filter(P001 > 0 | T001 > 0 | E001 > 0 | S001 > 0)

# truncar variaveis de trabalho
landuse <- landuse %>%
  group_by(abbrev_muni) %>%
  mutate(p951 = quantile(T001, 0.99)) %>%
  mutate(p952 = quantile(T002, 0.99)) %>%
  mutate(p953 = quantile(T003, 0.99)) %>%
  mutate(p954 = quantile(T004, 0.99)) %>%
  # sunstitute
  mutate(T001 = ifelse(T001 > p951, p951, T001)) %>%
  mutate(T002 = ifelse(T002 > p952, p952, T002)) %>%
  mutate(T003 = ifelse(T003 > p953, p953, T003)) %>%
  mutate(T004 = ifelse(T004 > p954, p954, T004)) %>%
  ungroup()

# filter columns
landuse <- landuse %>% 
  dplyr::select(id_hex, abbrev_muni, year, 
                # rename vars
                PT = P001,
                # PH, PM,
                PM = P006, PH = P007,
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



# create min and max constant for multiple years --------------------------

access_files <- dir("atlasacessibilidade/data/new/access", full.names = TRUE)
access <- lapply(access_files, readr::read_rds) %>% rbindlist()

access_extremes <- access %>%
  group_by(abbrev_muni, mode) %>%
  summarise(across(CMATT15:TMICT, min, .names = "{.col}.min"),
            across(CMATT15:TMICT, max, .names = "{.col}.max"))

readr::write_rds(access_extremes, "atlasacessibilidade/data/new/access_limits.rds")
