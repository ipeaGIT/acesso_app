source("../acesso_oport_kaue/R/fun/setup.R")

# abrir base final
acess <- read_rds("../data/output_base_final/dados2019_AcessOport_v1.0_20200116.rds")

# filtrar hexagonos vazios
acess <- acess %>% filter(!is.na(modo))

# por enquanto, so transporte publico
# acess_tp <- acess %>% filter(modo == "tp")
# por enquanto, so pico
acess_tp <- acess %>% filter(pico == 1)

# filter columns
acess_tp <- acess_tp %>% dplyr::select(id_hex, sigla_muni, P001, modo, matches("15|30|45|60|90|120"), starts_with("TMI"))

# separate between indicators
acess_tp_cum <- acess_tp %>% dplyr::select(id_hex, sigla_muni, modo, P001, matches("15|30|45|60|90|120"))
acess_tp_min <- acess_tp %>% dplyr::select(id_hex, sigla_muni, modo, P001, starts_with("TMI"))


# fix wide format
acess_tp_cum_wide <- acess_tp_cum %>%
  # multiply percent
  mutate_at(vars(matches("CMA")), function(x) x*100)
  
acess_tp_min_wide <- acess_tp_min %>%
  # ajeitar infinitos
  mutate_at(vars(matches("TMI")), function(x) ifelse(is.infinite(x), 120, x)) %>%
  # truncar valores acima de 30 minutos
  mutate_at(vars(matches("TMI")), function(x) ifelse(x > 30, 30, x))



# from wide to long cum
acess_tp_cum_long <- acess_tp_cum %>%
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
write_rds(hex_n_vazios %>% setDT(), "atlasacessibilidade/data/hex_teste.rds")


# download city centroids ---------------------------------------------------------------------

# get city centroids

limits <- lapply(munis_df$code_muni, geobr::read_municipality)

limits1 <- do.call(rbind, limits) %>%
  st_sf(crs = 4326) %>%
  st_centroid() %>%
  sfc_as_cols() %>%
  # add abrev muni
  left_join(munis_df %>% dplyr::select(code_muni, abrev_muni), by = c("code_muni"))


# save
write_rds(limits1, "atlasacessibilidade/data/cities_centroids.rds")



# zip ----------------------------------------------------


zip::zipr(zipfile = "atlasacessibilidade/data/acess_app_data_2019.zip", 
          files = dir("atlasacessibilidade/data/", pattern = "*.rds", full.names = TRUE))

                 
                

# TESTES --------------------------------------------------------------------------------------


# # para abrir
# system.time(read_rds("acess_tp_cum_app.rds")) # 36s
# 
# 
# system.time(a <- read_rds("acess_tp_cum_app_sgeo.rds")) # 9.8s 
#             
# system.time(b <- read_rds("hex_teste.rds")) # 0.7s
# 
# system.time(c <- a[nome_muni == "Fortaleza"]) # 0.01s
#   
# system.time(merge(c, b, by = "id_hex", all.x = TRUE) %>% st_sf(crs = 4326)) # 0.24s
#   