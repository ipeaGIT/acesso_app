
library(aopdata)
library(sf)
library(dplyr)
library(data.table)

# open access
access <- lapply(dir("atlasacessibilidade/data/new/access", full.names = TRUE),
                 readr::read_rds) %>%
  rbindlist()


# filter columns
access <- access %>% 
  dplyr::select(id_hex, name_muni, abbrev_muni, year, mode, 
                # starts_with("P001"),
                starts_with("CMA"),
                # starts_with("CMP"),
                starts_with("TMI")) %>%
  setDT()


# trazer renda
access_pop <- aopdata::read_population(city = "all")
access_pop <- access_pop[, .(id_hex, P001, P002, P003, R002, R003)]

access <- merge(access, access_pop, by = "id_hex",
                sort = FALSE)


# palma ratio ---------------------------------------------------------------------------------

# wide to long
access_palma_car_tp <- access %>%
  filter(mode %in% c("public_transport", "car")) %>%
  # filter(mode %in% c("tp", "carro")) %>%
  select(id_hex, name_muni, abbrev_muni, year, mode, P001, P002, P003, R002, R003,
         ends_with(c("30", "60", "90", "120"))) %>% setDT()

access_palma_active <- access %>%
  filter(mode %in% c("walk", "bicycle")) %>%
  # filter(mode %in% c("caminhada", "bicicleta")) %>%
  select(id_hex, name_muni, abbrev_muni, year, mode, P001, P002, P003, R002, R003,
         ends_with(c("15", "30", "45", "60"))) %>% setDT()


# to log
# access_palma_car_tp <- melt(access_palma_car_tp, 
#      id.vars = c("id_hex", "name_muni", "abbrev_muni", "year", "mode", "P001", "R003"),
#      variable.name = "tipo",
#      value.name = "valor")
# access_palma_active <- melt(access_palma_active, 
#      id.vars = c("id_hex", "name_muni", "abbrev_muni", "year", "mode", "P001", "R003"),
#      variable.name = "tipo",
#      value.name = "valor")

# access_palma_car_tp <- access_palma_car_tp %>%
#   # extract time threshld (separate at the position 5 of the string)
#   tidyr::separate(tipo, c("indicador", "tempo_viagem"), sep = 5) %>%
#   # extract activity
#   tidyr::separate(indicador, c("indicador", "atividade"), sep = 3) %>%
#   # tirar Nas
#   filter(!is.na(valor)) 
# 
# 
# access_palma_active <- access_palma_active %>%
#   # extract time threshld (separate at the position 5 of the string)
#   tidyr::separate(tipo, c("indicador", "tempo_viagem"), sep = 5) %>%
#   # extract activity
#   tidyr::separate(indicador, c("indicador", "atividade"), sep = 3) %>%
#   # tirar Nas
#   filter(!is.na(valor))


access_palma_car_tp_renda <- access_palma_car_tp %>%
  filter(R003 %in% c(1, 2, 3, 4, 10)) %>%
  # definir ricos e pobres
  mutate(classe = ifelse(R003 %in% c(1, 2, 3, 4), "pobre", "rico")) %>%
  # trazer os totais
  group_by(name_muni, abbrev_muni, year, classe, mode) %>%
  summarise(across(CMATT30:CMACT120, ~ weighted.mean(.x, P001, na.rm = TRUE), .names = "{.col}")) %>%
  ungroup() %>%
  tidyr::pivot_longer(cols = CMATT30:CMACT120, names_to = "tipo", values_to = "valor") %>%
  tidyr::pivot_wider(names_from = classe, values_from = valor) %>%
  mutate(palma_ratio = round(rico/pobre, 2)) %>%
  # extract time threshld (separate at the position 5 of the string)
  tidyr::separate(tipo, c("indicador", "tempo_viagem"), sep = 5) %>%
  # extract activity
  tidyr::separate(indicador, c("indicador", "atividade"), sep = 3)

access_palma_active_renda <- access_palma_active %>%
  filter(R003 %in% c(1, 2, 3, 4, 10)) %>%
  # definir ricos e pobres
  mutate(classe = ifelse(R003 %in% c(1, 2, 3, 4), "pobre", "rico")) %>%
  # trazer os totais
  group_by(name_muni, abbrev_muni, year, classe, mode) %>%
  summarise(across(CMATT15:CMACT60, ~ weighted.mean(.x, P001, na.rm = TRUE), .names = "{.col}")) %>%
  ungroup() %>%
  tidyr::pivot_longer(cols = CMATT15:CMACT60, names_to = "tipo", values_to = "valor") %>%
  tidyr::pivot_wider(names_from = classe, values_from = valor) %>%
  mutate(palma_ratio = round(rico/pobre, 2)) %>%
  # extract time threshld (separate at the position 5 of the string)
  tidyr::separate(tipo, c("indicador", "tempo_viagem"), sep = 5) %>%
  # extract activity
  tidyr::separate(indicador, c("indicador", "atividade"), sep = 3)


# access_palma_car_tp_renda <- access_palma_car_tp %>%
#   filter(R003 %in% c(1, 2, 3, 4, 10)) %>%
#   # definir ricos e pobres
#   mutate(classe = ifelse(R003 %in% c(1, 2, 3, 4), "pobre", "rico")) %>%
#   # selecionar variaveis
#   select(name_muni, abbrev_muni, year, P001, classe, mode, indicador, atividade, tempo_viagem, valor) %>%
#   # trazer os totais
#   # left_join(acess_totais_atividades, by = c("sigla_muni", "atividade")) %>%
#   # calcula acessibilidade total
#   # mutate(valor = as.integer(valor * total)) %>%
#   group_by(name_muni, abbrev_muni, year, classe, mode, indicador, atividade, tempo_viagem) %>%
#   summarise(acess_media = weighted.mean(valor, P001, na.rm = TRUE)) %>%
#   ungroup() %>%
#   # group_by(sigla_muni) %>%
#   tidyr::spread(classe, acess_media) %>%
#   # calcular palma ratio
#   # group_by(sigla_muni) %>%
#   mutate(palma_ratio = rico/pobre) %>%
#   mutate(palma_ratio = round(palma_ratio, 1))



# bind transport modes
access_palma_renda <- rbind(access_palma_car_tp_renda, access_palma_active_renda)

# ungroup()

# exportar palma
readr::write_rds(access_palma_renda, "atlasacessibilidade/data/new/charts/acess_palma_renda.rds")





# palma por cor -------------------------

access_palma_car_tp_cor <- rbind(
  access_palma_car_tp %>%
    group_by(name_muni, abbrev_muni, year, mode) %>%
    summarise(across(CMATT30:CMACT120, ~ weighted.mean(.x, P002, na.rm = TRUE), .names = "{.col}")) %>%
    mutate(cor = "brancos"),
  access_palma_car_tp %>%
    group_by(name_muni, abbrev_muni, year, mode) %>%
    summarise(across(CMATT30:CMACT120, ~ weighted.mean(.x, P003, na.rm = TRUE), .names = "{.col}")) %>%
    mutate(cor = "negros")
) %>%
  tidyr::pivot_longer(cols = CMATT30:CMACT120, names_to = "tipo", values_to = "valor") %>%
  tidyr::pivot_wider(names_from = cor, values_from = valor) %>%
  mutate(palma_ratio = round(brancos/negros, 2)) %>%
  # extract time threshld (separate at the position 5 of the string)
  tidyr::separate(tipo, c("indicador", "tempo_viagem"), sep = 5) %>%
  # extract activity
  tidyr::separate(indicador, c("indicador", "atividade"), sep = 3)

access_palma_active_cor <- rbind(
  access_palma_active %>%
    group_by(name_muni, abbrev_muni, year, mode) %>%
    summarise(across(CMATT15:CMACT60, ~ weighted.mean(.x, P002, na.rm = TRUE), .names = "{.col}")) %>%
    mutate(cor = "brancos"),
  access_palma_active %>%
    group_by(name_muni, abbrev_muni, year, mode) %>%
    summarise(across(CMATT15:CMACT60, ~ weighted.mean(.x, P003, na.rm = TRUE), .names = "{.col}")) %>%
    mutate(cor = "negros")
) %>%
  tidyr::pivot_longer(cols = CMATT15:CMACT60, names_to = "tipo", values_to = "valor") %>%
  tidyr::pivot_wider(names_from = cor, values_from = valor) %>%
  mutate(palma_ratio = round(brancos/negros, 2)) %>%
  # extract time threshld (separate at the position 5 of the string)
  tidyr::separate(tipo, c("indicador", "tempo_viagem"), sep = 5) %>%
  # extract activity
  tidyr::separate(indicador, c("indicador", "atividade"), sep = 3)


# bind transport modes
access_palma_cor <- rbind(access_palma_car_tp_cor, access_palma_active_cor)



# exportar palma
readr::write_rds(access_palma_cor, "atlasacessibilidade/data/new/charts/acess_palma_cor.rds")




# dumbell plot --------------------------------------------------------------------------------


acess_dumbell <- access %>%
  # filter(mode %in% c("public_transport", "car")) %>%
  select(id_hex, name_muni, abbrev_muni, year, mode, P001, P002, P003, R002, R003,
         starts_with(c("TMI"))) %>% setDT()


acess_dumbell_long <- acess_dumbell %>%
  # wide to long
  tidyr::gather(tipo, valor, TMIST:TMICT) %>%
  # extract activity
  tidyr::separate(tipo, c("indicador", "atividade"), sep = 3) %>%
  # tirar Nas
  filter(!is.na(valor)) %>%
  setDT()

# tempo medio ponderapo pela populacao de cada hexagono
acess_dumbell_long <- acess_dumbell_long[, .(Total = weighted.mean(x = valor[which(P001>0)], w = P001[which(P001>0)], na.rm=T),
                                             Negra = weighted.mean(valor[which(P003>0)], w = P003[which(P003>0)], na.rm=T),
                                             Branca = weighted.mean(valor[which(P002>0)], w = P002[which(P002>0)], na.rm=T),
                                             Q1 = weighted.mean(valor[which(R002==1)], w = P001[which(R002==1)], na.rm=T),
                                             Q5 = weighted.mean(valor[which(R002==5)], w = P001[which(R002==5)], na.rm=T)), 
                                         by= .(name_muni, abbrev_muni, year, mode, indicador, atividade)]

# tempo medio ponderapo pela populacao de cada hexagono
acess_dumbell_renda <- acess_dumbell_long %>% select(name_muni, abbrev_muni, year, mode, indicador, atividade, total = Total, low = Q1, high = Q5)
acess_dumbell_cor <- acess_dumbell_long %>% select(name_muni, abbrev_muni, year, mode, indicador, atividade, total = Total, low = Negra, high = Branca)


# exportar dumbell
readr::write_rds(acess_dumbell_renda, "atlasacessibilidade/data/new/charts/acess_dumbell_renda.rds")
readr::write_rds(acess_dumbell_cor, "atlasacessibilidade/data/new/charts/acess_dumbell_cor.rds")


# zip ----------------------------------------------------


zip::zipr(zipfile = "atlasacessibilidade/data/acess_app_data_2019.zip", 
          files = dir("atlasacessibilidade/data/", pattern = "*.rds", full.names = TRUE))
