
library(aopdata)
library(sf)
library(dplyr)
library(data.table)

my_read_access <- function(modo, ...) {
  
  aopdata::read_access(mode = modo, ...)
  
}


# download data - get walk only for testing
acess_20171 <- lapply(c("walk", "bicycle", "car"), my_read_access,
                      city = "all", 
                      year = 2017) %>%
  rbindlist(fill = TRUE) %>%
  dplyr::filter(year == 2017)

acess_20172 <- aopdata::read_access(city = c("for", "spo", "bho", "poa", "cam", "cur"), 
                                    mode = c("public_transport"), 
                                    year = 2017) %>%
  dplyr::filter(year == 2017)

acess_20181 <- lapply(c("walk", "bicycle", "car"), my_read_access,
                      city = "all", 
                      year = 2018) %>%
  rbindlist(fill = TRUE) %>%
  dplyr::filter(year == 2018)

acess_20182 <- aopdata::read_access(city = c("for", "spo", "bho", "poa", "cam", "cur", "rio"), 
                                    mode = c("public_transport"), 
                                    year = 2018) %>%
  dplyr::filter(year == 2018)


acess_20191 <- lapply(c("walk", "bicycle", "car"), my_read_access,
                      city = "all", 
                      year = 2019) %>%
  rbindlist(fill = TRUE) %>%
  dplyr::filter(year == 2019)

acess_20192 <- aopdata::read_access(city = c("for", "spo", "bho", "poa", "cam", "cur", "rio", "rec", "goi"), 
                                    mode = c("public_transport"), 
                                    year = 2019) %>%
  dplyr::filter(year == 2019)

# juntar
acess <- rbind(acess_20171, acess_20172, 
               acess_20181,acess_20182,  
               acess_20191, acess_20192,  
               fill = TRUE)

# por enquanto, so pico
acess <- acess %>% filter(peak == 1)
# filtrar hexagonos vazios
acess <- acess %>% filter(!is.na(mode))

# filter columns
acess <- acess %>% 
  dplyr::select(id_hex, name_muni, abbrev_muni, year, mode, 
                starts_with("P001"),
                starts_with("CMA"),
                starts_with("CMP"),
                starts_with("TMI"))




  # palma ratio ---------------------------------------------------------------------------------

acess_palma <- acess %>% 
  # ajeitar infinitos
  mutate_at(vars(matches("TMI")), function(x) ifelse(is.infinite(x), 120, x)) %>%
   setDT()

acess_palma_long <- acess_palma %>%
  # wide to long
  tidyr::gather(tipo, valor, CMATT15:CMACT120) %>%
  # extract time threshld (separate at the position 5 of the string)
  tidyr::separate(tipo, c("indicador", "tempo_viagem"), sep = 5) %>%
  # extract activity
  tidyr::separate(indicador, c("indicador", "atividade"), sep = 3) %>%
  # multiply percent
  # mutate(valor = valor * 100)
  # tirar Nas
  filter(!is.na(valor))

# calcular totais das atividades
# acess_totais_atividades <- acess_palma_long %>%
#   # pegar um exemplo
#   filter(modo == 'bicicleta', indicador == "CMA", atividade == "ST", tempo_viagem == "15") %>%
#   select(nome_muni, sigla_muni, T001, E001:E004, S001:S004) %>%
#   group_by(sigla_muni) %>%
#   summarise(T001 = sum(T001),
#             E001 = sum(E001),
#             E002 = sum(E002),
#             E003 = sum(E003),
#             E004 = sum(E004),
#             S001 = sum(S001),
#             S002 = sum(S002),
#             S003 = sum(S003),
#             S004 = sum(S004),
#             ) %>%
#   # to long
#   gather(atividade, total, T001:S004) %>%
#   mutate(atividade = case_when(
#     atividade == "T001" ~ "TT",
#     atividade == "E001" ~ "ET",
#     atividade == "E002" ~ "EI",
#     atividade == "E003" ~ "EF",
#     atividade == "E004" ~ "EM",
#     atividade == "S001" ~ "ST",
#     atividade == "S002" ~ "SB",
#     atividade == "S003" ~ "SM",
#     atividade == "S004" ~ "SA"
#     
#     
#   ))

acess_palma_renda <- acess_palma_long %>%
  # filter(modo == "bicicleta" & pico == 1) %>%
  # select(sigla_muni, R003, P001, CMATT30) %>%
  # pegar so decis 4 e 9
  filter(R003 %in% c(1, 2, 3, 4, 10)) %>%
  # definir ricos e pobres
  mutate(classe = ifelse(R003 %in% c(1, 2, 3, 4), "pobre", "rico")) %>%
  # selecionar variaveis
  select(name_muni, abbrev_muni, year, P001, classe, mode, indicador, atividade, tempo_viagem, valor) %>%
  # trazer os totais
  # left_join(acess_totais_atividades, by = c("sigla_muni", "atividade")) %>%
  # calcula acessibilidade total
  # mutate(valor = as.integer(valor * total)) %>%
  group_by(nome_muni, abbrev_muni, year, classe, modo, indicador, atividade, tempo_viagem) %>%
  summarise(acess_media = weighted.mean(valor, P001)) %>%
  ungroup() %>%
  # group_by(sigla_muni) %>%
  spread(classe, acess_media) %>%
  # calcular palma ratio
  # group_by(sigla_muni) %>%
  mutate(palma_ratio = rico/pobre) %>%
  mutate(palma_ratio = round(palma_ratio, 1))
# ungroup()

# exportar palma
write_rds(acess_palma_renda, "data/acess_palma_renda.rds")


# palma por cor
acess_palma_cor <- acess_palma_long %>%
  group_by(nome_muni, sigla_muni, modo, indicador, atividade, tempo_viagem) %>%
  summarise(acess_brancos = weighted.mean(valor, P002),
            acess_negros = weighted.mean(valor, P003)) %>%
  # calcular palma ratio
  mutate(palma_ratio = acess_brancos/acess_negros) %>%
  mutate(palma_ratio = round(palma_ratio, 1)) %>%
  ungroup()


# exportar palma
write_rds(acess_palma_cor, "data/acess_palma_cor.rds")




# dumbell plot --------------------------------------------------------------------------------


acess_palma <- acess %>% 
  # filtrar hexagonos vazios
  filter(!is.na(modo)) %>%
  # por enquanto, so pico
  filter(pico == 1) %>%
  # filter columns
  dplyr::select(id_hex, nome_muni, sigla_muni, P001, P002, P003, R002, R003, T001, E001:E004, S001:S004, modo, TMIST:TMIEM) %>%
  # por enquanto, nao selecionar TQ e TD
  dplyr::select(-matches("TQ|TD")) %>%
  # fix wide format
  # multiply percent
  # mutate_at(vars(matches("CMA")), function(x) x*100) %>%
  # ajeitar infinitos
  mutate_at(vars(matches("TMI")), function(x) ifelse(is.infinite(x), 90, x)) %>%
  # truncar valores acima de 30 minutos
  mutate_at(vars(matches("TMI")), function(x) ifelse(x > 30, 30, x)) %>%
  st_set_geometry(NULL) %>% setDT()

acess_palma_long <- acess_palma %>%
  # wide to long
  tidyr::gather(tipo, valor, TMIST:TMIEM) %>%
  # extract activity
  tidyr::separate(tipo, c("indicador", "atividade"), sep = 3) %>%
  # tirar Nas
  filter(!is.na(valor))


# transform to dt
acess_dumbell <- setDT(acess_palma_long)

# tempo medio ponderapo pela populacao de cada hexagono
df4 <- acess_dumbell[, .(Total = weighted.mean(x = valor[which(P001>0)], w = P001[which(P001>0)], na.rm=T),
                         Negra = weighted.mean(valor[which(P003>0)], w = P003[which(P003>0)], na.rm=T),
                         Branca = weighted.mean(valor[which(P002>0)], w = P002[which(P002>0)], na.rm=T),
                         Q1 = weighted.mean(valor[which(R002==1)], w = P001[which(R002==1)], na.rm=T),
                         Q5 = weighted.mean(valor[which(R002==5)], w = P001[which(R002==5)], na.rm=T)), 
                     by= .(nome_muni, sigla_muni, modo, indicador, atividade)]

# tempo medio ponderapo pela populacao de cada hexagono
df4_renda <- df4 %>% select(nome_muni, sigla_muni, modo, indicador, atividade, total = Total, low = Q1, high = Q5)
df4_cor <- df4 %>% select(nome_muni, sigla_muni, modo, indicador, atividade, total = Total, low = Negra, high = Branca)


# 
# # ajeitar nome das cidade
# df4 <- df4 %>%
#   mutate(sigla_muni = factor(sigla_muni, levels = munis_df$abrev_muni, labels = munis_df$name_muni))


# exportar dumbell
write_rds(df4_renda, "data/acess_dumbell_renda.rds")
write_rds(df4_cor, "data/acess_dumbell_cor.rds")


# zip ----------------------------------------------------


zip::zipr(zipfile = "atlasacessibilidade/data/acess_app_data_2019.zip", 
          files = dir("atlasacessibilidade/data/", pattern = "*.rds", full.names = TRUE))
