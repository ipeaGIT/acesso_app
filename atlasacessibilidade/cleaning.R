source("../acesso_oport/R/fun/setup.R")

# abrir base final
acess <- read_rds("../../data/output_base_final/dados2019_AcessOport_v1.0_20200116_interno.rds")

# filtrar hexagonos vazios
acess <- acess %>% filter(!is.na(modo))

# por enquanto, so pico
acess_tp <- acess %>% filter(pico == 1)

# filter columns
acess_tp <- acess_tp %>% 
  dplyr::select(id_hex, sigla_muni, P001, modo, matches("15|30|45|60|90|120"), starts_with("TMI")) %>%
  # por enquanto, nao selecionar TQ e TD
  dplyr::select(-matches("TQ|TD"))

# separate between indicators
# acess_tp_cum <- acess_tp %>% dplyr::select(id_hex, sigla_muni, modo, P001, matches("15|30|45|60|90|120"))
# acess_tp_min <- acess_tp %>% dplyr::select(id_hex, sigla_muni, modo, P001, starts_with("TMI"))


# fix wide format
acess_tp_wide <- acess_tp %>%
  # multiply percent
  mutate_at(vars(matches("CMA")), function(x) x*100) %>%
  # ajeitar infinitos
  mutate_at(vars(matches("TMI")), function(x) ifelse(is.infinite(x), 120, x)) %>%
  # truncar valores acima de 30 minutos
  mutate_at(vars(matches("TMI")), function(x) ifelse(x > 30, 30, x)) %>%
  st_set_geometry(NULL) %>% setDT()

# create  fake geom to plot brazil
acess_tp_fake <- acess_tp_wide[nrow(acess_tp_wide),] %>%
  mutate(sigla_muni = "fake")

acess_tp_wide <- acess_tp_wide %>%
  rbind(acess_tp_fake)


# salvar sem geometria - wide
write_rds(acess_tp_wide,
          # filter(sigla_muni %in% c("bho", "sal")) %>%
          # filter(nome_muni %nin% c("São Paulo", "Rio de Janeiro")), 
          "atlasacessibilidade/data/acess_wide.rds") 


# from wide to long cum
acess_tp_cum_long <- acess_tp_wide %>%
  # wide to long
  tidyr::gather(tipo, valor, CMATT15:CMAEM120) %>%
  # extract time threshld (separate at the position 5 of the string)
  tidyr::separate(tipo, c("indicador", "tempo_viagem"), sep = 5) %>%
  # extract activity
  tidyr::separate(indicador, c("indicador", "atividade"), sep = 3) %>%
  # multiply percent
  mutate(valor = valor * 100)
  
acess_tp_min_long <- acess_tp_min %>%
  # wide to long
  tidyr::gather(tipo, valor, TMIST:TMIEM) %>%
  # extract activity
  tidyr::separate(tipo, c("indicador", "atividade"), sep = 3) %>%
  # ajeitar infinitos
  mutate(valor = ifelse(is.infinite(valor), 120, valor)) %>%
  # truncar valores acima de 30 minutos
  mutate(valor = ifelse(valor > 30, 30, valor))


# salvar
# write_rds(acess_tp_cum_long,   # tirar sao paulo e rio para teste
#           # filter(nome_muni %nin% c("São Paulo", "Rio de Janeiro"))
#           "acess_tp_cum_app.rds") 
# 
# write_rds(acess_tp_min_long,  # tirar sao paulo e rio para teste
#             # filter(nome_muni %nin% c("São Paulo", "Rio de Janeiro")), 
#           "acess_tp_min_app.rds") 

# salvar sem geometria - wide
write_rds(acess_tp_cum_wide %>% 
          # filter(sigla_muni %in% c("bho", "sal")) %>%
          st_set_geometry(NULL) %>% setDT(),
          # filter(nome_muni %nin% c("São Paulo", "Rio de Janeiro")), 
          "atlasacessibilidade/data/acess_tp_cum_app_sgeo_wide.rds") 

write_rds(acess_tp_min_wide %>%  
          # filter(sigla_muni %in% c("bho", "sal")) %>%
            st_set_geometry(NULL) %>% setDT(),
          "atlasacessibilidade/data/acess_tp_min_app_sgeo_wide.rds") 

# salvar sem geometria
write_rds(acess_tp_cum_long %>% 
          # filter(sigla_muni %in% c("bho", "sal")) %>%
          st_set_geometry(NULL) %>% setDT(),
          # filter(nome_muni %nin% c("São Paulo", "Rio de Janeiro")), 
          "atlasacessibilidade/data/acess_tp_cum_app_sgeo.rds") 

write_rds(acess_tp_min_long %>%  
          filter(sigla_muni %in% c("bho", "sal")) %>%
            st_set_geometry(NULL) %>% setDT(),
          "atlasacessibilidade/data/acess_tp_min_app_sgeo.rds") 




# geometrias ----------------------------------------------------------------------------------

# dados hex agregados
hex_agreg <- lapply(dir("../data/hex_agregados/", full.names = TRUE, pattern = "09"), read_rds) %>% rbindlist(fill = TRUE)

# pegar pontos nao-vazios
points <- lapply(dir("../otp/points/", pattern = "*09.csv", full.names = TRUE), read_csv) %>% rbindlist()

# filtrar so nao-vazios nos hex
hex_n_vazios <- hex_agreg %>%
  filter(id_hex %in% points$id_hex) %>%
  # teste: so cidades de tp
  # filter(muni %in% munis_df[modo == "todos"]$abrev_muni) %>%
  # selecionar colunas
  dplyr::select(id_hex, geometry)



# salvar
write_rds(hex_n_vazios %>% setDT(), "atlasacessibilidade/data/hex.rds")


# download city centroids and limits ---------------------------------------------------------------

# get city limits


limits <- lapply(munis_df_2019$code_muni, geobr::read_municipality)

limits1 <- do.call(rbind, limits) %>%
  st_sf(crs = 4326) %>%
  left_join(munis_df_2019 %>% dplyr::select(code_muni, abrev_muni), by = c("code_muni")) %>%
  st_cast("POLYGON")


# save
write_rds(limits1, "atlasacessibilidade/data/cities_limits.rds")


# get city centroids
centroids <- do.call(rbind, limits) %>%
  st_sf(crs = 4326) %>%
  st_centroid() %>%
  sfc_as_cols() %>%
  # add abrev muni
  left_join(munis_df %>% dplyr::select(code_muni, abrev_muni), by = c("code_muni"))


# save
write_rds(centroids, "atlasacessibilidade/data/cities_centroids.rds")






# palma ratio ---------------------------------------------------------------------------------

acess_palma <- acess %>% 
# filtrar hexagonos vazios
  filter(!is.na(modo)) %>%
  # por enquanto, so pico
  filter(pico == 1) %>%
  # filter columns
  dplyr::select(id_hex, nome_muni, sigla_muni, P001, P002, P003, R003, T001, E001:E004, S001:S004, modo, matches("15|30|45|60|90|120")) %>%
  # por enquanto, nao selecionar TQ e TD
  dplyr::select(-matches("TQ|TD")) %>%
  # fix wide format
  # multiply percent
  # mutate_at(vars(matches("CMA")), function(x) x*100) %>%
  # ajeitar infinitos
  mutate_at(vars(matches("TMI")), function(x) ifelse(is.infinite(x), 120, x)) %>%
  # truncar valores acima de 30 minutos
  mutate_at(vars(matches("TMI")), function(x) ifelse(x > 30, 30, x)) %>%
  st_set_geometry(NULL) %>% setDT()

acess_palma_long <- acess_palma %>%
  # wide to long
  tidyr::gather(tipo, valor, CMATT15:CMAEM120) %>%
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
  select(nome_muni, sigla_muni, P001, classe, modo, indicador, atividade, tempo_viagem, valor) %>%
  # trazer os totais
  # left_join(acess_totais_atividades, by = c("sigla_muni", "atividade")) %>%
  # calcula acessibilidade total
  # mutate(valor = as.integer(valor * total)) %>%
  group_by(nome_muni, sigla_muni, classe, modo, indicador, atividade, tempo_viagem) %>%
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
