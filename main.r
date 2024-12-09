install.packages("basedosdados")
install.packages("tidyr")
install.packages("economiccomplexity")
install.packages("geobr")
install.packages("ggplot2")
install.packages("ggspatial")
install.packages("sf")


library(basedosdados)
library(tidyr)
library(dplyr)
library(economiccomplexity)
library(geobr)
library(ggplot2)
library(ggspatial)
library(sf)
library(spdep)
library(sp)
library(spatialreg)


basedosdados::set_billing_id("cloud-learning-doing")

query <- "
SELECT
    dados.id_municipio AS id_municipio,
    SUM(dados.quantidade_vinculos_ativos) as quantidade_vinculos_ativos
FROM `basedosdados.br_me_rais.microdados_estabelecimentos` AS dados
LEFT JOIN (SELECT DISTINCT sigla,nome  FROM `basedosdados.br_bd_diretorios_brasil.uf`) AS diretorio_sigla_uf
    ON dados.sigla_uf = diretorio_sigla_uf.sigla
LEFT JOIN (SELECT DISTINCT id_municipio,nome  FROM `basedosdados.br_bd_diretorios_brasil.municipio`) AS diretorio_id_municipio
    ON dados.id_municipio = diretorio_id_municipio.id_municipio
WHERE dados.ano = 2022
GROUP BY 1
"

cnae <- basedosdados::read_sql("select * from basedosdados.br_bd_diretorios_brasil.cnae_2", billing_project_id = basedosdados::get_billing_id())
rais_cnae <- basedosdados::read_sql(query, billing_project_id = basedosdados::get_billing_id())
rais_cnae$quantidade_vinculos_ativos <- as.integer(rais_cnae$quantidade_vinculos_ativos)

emprego_ms <- rais_cnae %>%
  tidyr::pivot_wider(names_from = cnae_2, values_from = quantidade_vinculos_ativos, values_fill = 0)

e.ms <- as.matrix(emprego_ms |> select(-id_municipio, -nome_municipio))

e.m <- rowSums(e.ms)
e.s <- colSums(e.ms)
e.. <- sum(e.s)

calculateQL_binary <- function(matrix) {
  e.m <- rowSums(matrix)  # Soma por município (linhas)
  e.s <- colSums(matrix)  # Soma por setor (colunas)
  e.. <- sum(e.s)         # Soma total

  ql <- matrix(0, nrow = NROW(matrix), ncol = NCOL(matrix))

  for(m in 1:NROW(matrix)) {
    for(s in 1:NCOL(matrix)) {
      ql_value <- (matrix[m, s] / e.m[m]) / (e.s[s] / e..)
      ql[m, s] <- ifelse(ql_value > 1, 1, 0)  # Se QL > 1, recebe 1, senão 0
    }
  }

  return(ql)
}

ql_2022 <- calculateQL_binary(e.ms)

rownames(ql_2022) <- emprego_ms$id_municipio
colnames(ql_2022) <- colnames(emprego_ms)[-(1:2)]

dgc_2022 <- economiccomplexity::balassa_index(ql_2022)

ice <- economiccomplexity::complexity_measures(
  dgc_2022,
  method = "eigenvalues"
)

municipios <- geobr::read_municipality()

ice_municipios <- ice$complexity_index_country

ice_municipios_df <- data.frame(
  code_muni = names(ice_municipios),
  ice = as.numeric(ice_municipios)
)

municipios <- municipios |>
  dplyr::mutate(
    code_muni = as.character(code_muni)
  ) |>
  dplyr::left_join(ice_municipios_df, by = "code_muni")

palette1 <- c("#264653", "#2a9d8f", "#e9c46a", "#f4a261","#e76f51")

####### Jump previous steps #######
municipios <- sf::read_sf("municipios/municipios.shp")
###################################

municipios <- municipios |>
  dplyr::rename(
    "code_muni" = "code_mn",
    "name_muni" = "name_mn",
  )

ice_map <- ggplot2::ggplot(municipios) +
  ggplot2::geom_sf(mapping = ggplot2::aes(fill = municipios$ice), lwd = 0, colour = NA) +
  ggplot2::scale_fill_gradientn(
    colors = palette1,
    name = "Índice de Complexidade Econômica (ICE)",
    na.value = "grey80",
  ) +
  ggplot2::theme_void() +
  ggplot2::theme(
    text = element_text(face = "plain"),
    legend.title = element_text(size = 16, face = "bold", hjust = 1, margin = margin(b = 25)),
    legend.text = element_text(size = 12, face = "plain", hjust = 1),
    legend.position = c(0.8, 0.9),  
    legend.box.margin = unit(c(0, 0, 0, 0),"cm"),
   ) +
  ggspatial::annotation_scale(location = "bl", line_width = 0.4) +
  ggspatial::annotation_north_arrow(
    location = "br",
    style = ggspatial::north_arrow_fancy_orienteering(),
    height = unit(1, "cm"),
    width = unit(1, "cm")
  )

ggsave("exports/ice.pdf", plot = ice_map, width = 5000, height = 5000, units = "px", dpi = 320)

###### ESTBAN ######

sql <- "SELECT * FROM estban.estban_agencias_geolocalizadas WHERE data_base = '2022-12-01'"

query <- bigrquery::bq_project_query(
  "cloud-learning-doing",
  sql,
)

agencias_2022 <- bigrquery::bq_table_download(query)

brasil <- geobr::read_country()

agencias_2022 <- agencias_2022 |>
  dplyr::filter(!is.na(lat) & !is.na(lng)) |>
  sf::st_as_sf(coords = c("lng", "lat"), crs = 4674) |>
  sf::st_filter(brasil)


shape_mun_agencias <- sf::st_join(municipios, agencias_2022)

sql <- "SELECT
    dados.id_municipio AS code_muni,
    dados.populacao_residente as populacao,
    dados.area_unidade_territorial as area
FROM `basedosdados.br_ibge_censo_2022.area_territorial_densidade_demografica_municipio` AS dados"


query <- bigrquery::bq_project_query(
  "cloud-learning-doing",
  sql,
)

populacao <- bigrquery::bq_table_download(query)

query <- "
SELECT
    dados.id_municipio AS code_muni,
    COUNT(*) as qtd_empresas
FROM `basedosdados.br_me_rais.microdados_estabelecimentos` AS dados
WHERE dados.ano = 2022
GROUP BY 1
"

qtd_empresas <- basedosdados::read_sql(query, billing_project_id = basedosdados::get_billing_id())

shape_joined <- shape_mun_agencias |>
  dplyr::left_join(qtd_empresas, by="code_muni") |>
  dplyr::left_join(populacao, by="code_muni")
  # dplyr::left_join(rais_cnae, by=c("code_muni" = "id_municipio"))

shape_final <- shape_joined |>
  sf::st_drop_geometry() |>
  dplyr::group_by(code_muni) |>
  dplyr::summarise(
    populacao = sum(ifelse(is.na(populacao), 0, populacao), na.rm = TRUE),
    # qtd_empresas = sum(ifelse(is.na(qtd_empresas), 0, qtd_empresas), na.rm = TRUE),
    # qtd_vinculos = sum(ifelse(is.na(quantidade_vinculos_ativos), 0, quantidade_vinculos_ativos), na.rm = TRUE),
    area = sum(ifelse(is.na(area), 0, area), na.rm = TRUE),
    disponibilidades = sum(`110`, na.rm = TRUE),
    op_cred = sum(`160`, na.rm = TRUE),
    emprestimos = sum(`161`, na.rm = TRUE),
    fin = sum(`162`, na.rm = TRUE),
    fin_agricultura_inv = sum(`163`, na.rm = TRUE),
    fin_pecuaria_inv = sum(`164`, na.rm = TRUE),
    fin_agricultura_com = sum(`165`, na.rm = TRUE),
    fin_pecuaria_com = sum(`166`, na.rm = TRUE),
    fin_agroindustrial = sum(`167_168`, na.rm = TRUE),
    fin_imobiliarios = sum(`169`, na.rm = TRUE),
    outras_op_cred = sum(`172`, na.rm = TRUE),
    outros_cred = sum(`173`, na.rm = TRUE),
    arr_mercantial = sum(`180`, na.rm = TRUE),
    provisao_arr_mercantil = sum(`184`, na.rm = TRUE),
    provisao_de_credito = sum(`174`, na.rm = TRUE),
    ativos = sum(`399`, na.rm = TRUE),
    depositos = sum(`401_402_404_411_412_413_414_415_416_417_418_419`, na.rm = TRUE),
    poupanca = sum(`420`, na.rm = TRUE),
    depositos_inter = sum(`430`, na.rm = TRUE),
    relacoes_interfinanceiras = sum(`444_445_446_447_456_458`, na.rm = TRUE),
    resultado = sum(`710`, na.rm = TRUE)
  )

shape_final[is.na(shape_final)] <- 0

shape_final$PLP <- ifelse((shape_final$depositos + shape_final$poupanca + shape_final$depositos_inter) == 0, 0, shape_final$depositos/(shape_final$depositos + shape_final$poupanca + shape_final$depositos_inter))
shape_final$PLB <- ifelse(shape_final$op_cred == 0, 0, (shape_final$depositos)/(shape_final$op_cred))

QS <- (
  (shape_final$emprestimos) +
    (shape_final$fin) +
    (shape_final$fin_agricultura_inv) +
    (shape_final$fin_pecuaria_inv) +
    (shape_final$fin_agricultura_com) +
    (shape_final$fin_pecuaria_com) +
    (shape_final$fin_agroindustrial) +
    (shape_final$fin_imobiliarios) +
    (shape_final$outras_op_cred) +
    (shape_final$outros_cred) +
    (shape_final$arr_mercantial)
)^2

SQ <- (shape_final$emprestimos)^2 +
  (shape_final$fin)^2 +
  (shape_final$fin_agricultura_inv)^2 +
  (shape_final$fin_pecuaria_inv)^2 +
  (shape_final$fin_agricultura_com)^2 +
  (shape_final$fin_pecuaria_com)^2 +
  (shape_final$fin_agroindustrial)^2 +
  (shape_final$fin_imobiliarios)^2 +
  (shape_final$outras_op_cred)^2 +
  (shape_final$outros_cred)^2 +
  (shape_final$arr_mercantial)^2

shape_final$ICO <- ifelse(QS == 0, 1, SQ/QS)
shape_final$ICO <- ifelse(shape_final$ICO > 1, 1, shape_final$ICO)

new_df <- sf::st_join(municipios, agencias_2022) |> dplyr::filter(!is.na(cnpj))
cnpjs_distintos <- paste("cnpj_", unique(new_df$cnpj), sep="")

pivoted <- new_df |>
  sf::st_drop_geometry() |>
  tidyr::pivot_wider(names_from = cnpj, values_from = `160`, names_prefix = "cnpj_") |>
  dplyr::select(all_of(c("code_muni", cnpjs_distintos))) |>
  dplyr::right_join(municipios, by = "code_muni") |>
  dplyr::mutate(across(where(is.numeric), ~ifelse(is.na(.), 0, .)))

grouped <- pivoted |>
  dplyr::group_by(code_muni) |>
  dplyr::summarise(dplyr::across(starts_with("cnpj"), sum, na.rm = TRUE))


grouped$ICB <- rowSums(grouped[, startsWith(names(grouped), "cnpj_")]^2)/rowSums(grouped[, startsWith(names(grouped), "cnpj_")])^2
grouped <- grouped |> dplyr::select("code_muni", "ICB")

shape_final <- grouped |>
  dplyr::right_join(shape_final, by = "code_muni")

shape_final$densidade_demografica <- shape_final$populacao/shape_final$area
shape_final$densidade_empresarial <- shape_final$qtd_empresas/shape_final$area

agencias_2022 <- agencias_2022 |>
  dplyr::mutate(
    banco = ifelse(
      cnpj == "00000000", "Banco do Brasil",
      ifelse(
        cnpj == "90400888", "Santander",
        ifelse(
          cnpj %in% c("60746948", "07207996"), "Bradesco",
          ifelse(
            cnpj %in% c("60701190", "17298092"), "Itaú",
            ifelse(
              cnpj == "00360305", "Caixa Econômica Federal", "Outros"
            )
          )
        )
      )
    )
)

cols <- c(
  "Banco do Brasil" = "#F9DD16",
  "Santander" = "#ec0000",
  "Bradesco" = "#633280",
  "Itaú" = "#f28500",
  "Caixa Econômica Federal" = "#1c60ab",
  "Outros" = "#333333"
)

ice_map <- ggplot2::ggplot(municipios) +
  ggplot2::geom_sf(mapping = aes(fill = municipios$ice), lwd = 0, colour = NA) +
  ggplot2::scale_fill_gradientn(
    colors = palette1,
    name = "Índice de Complexidade Econômica (ICE)",
    na.value = "grey80",
  ) +
  ggplot2::scale_size_area(
    name = "Operações de crédito (milhões de R$)",
    max_size = 6
  ) +
  ggplot2::geom_sf(agencias_2022, mapping = aes(colour = banco, size = (`160`/1e6))) +
  ggplot2::scale_colour_manual(
    values = scales::alpha(cols, 0.5),
    na.value = "grey80",
    name = "Banco"
  ) +
  theme_void() +
  theme(
    text = element_text(face = "plain"),
    legend.title = element_text(size = 14, face = "bold"),
    legend.text = element_text(size = 12, face = "plain"),
    legend.position = "left",
    legend.box.spacing = unit(0.5, "cm"),
    legend.spacing.y = unit(1, "cm")
   ) +
  annotation_scale(location = "bl", line_width = 0.4) +
  annotation_north_arrow(
    location = "br",
    style = north_arrow_fancy_orienteering(),
    height = unit(1, "cm"),
    width = unit(1, "cm")
  )

  ggsave("exports/ice2.pdf", plot = ice_map, width = 5000, height = 5000, units = "px", dpi = 320)

plot(y = shape_final$ice, x = 1 - shape_final$ICO)



####### Jump previous steps #######
shape <- dplyr::as_tibble(read.csv("data/data.csv", header = TRUE, sep=",")) |>
  dplyr::select(!X) |>
  dplyr::mutate(code_muni = as.character(code_muni))

ice_mun <- municipios |> dplyr::select("code_muni", "ice")

shape_final <- shape |>
  dplyr::left_join(ice_mun, by = c("code_muni" = "code_muni"))

shape_final$diversidade_credito <- 1 - shape_final$ICO
shape_final$diversidade_bancaria <- 1 - shape_final$ICB


sfarrow::st_write_parquet(obj = sf::st_as_sf(shape_final), dsn = "shape_final.parquet")

shape_final$log_populacao <- log(shape_final$populacao)

#### Models ####
ols <- lm(formula = "ice ~ diversidade_credito + diversidade_bancaria + PLB + PLP + log_populacao + op_cred", data=shape_final)
summary(ols)

spatial <- shape_final |>
  dplyr::filter(!code_muni %in% c("2605459", "3520400")) |>
  sf::st_as_sf() |>
  sf::as_Spatial()

shape_final <- shape_final |>
  dplyr::filter(!code_muni %in% c("2605459", "3520400"))

rainha <- spdep::poly2nb(
  shape_final$geometry,
  row.names = shape_final$code_muni,
  queen = TRUE
)

centroids <- sf::st_centroid(shape_final$geometry)
knn4 <- spdep::knn2nb(spdep::knearneigh(centroids, k=4), row.names = shape_final$code_muni)
knn5 <- spdep::knn2nb(spdep::knearneigh(centroids, k=5), row.names = shape_final$code_muni)
knn6 <- spdep::knn2nb(spdep::knearneigh(centroids, k=6), row.names = shape_final$code_muni)
knn7 <- spdep::knn2nb(spdep::knearneigh(centroids, k=7), row.names = shape_final$code_muni)


rainha_lista <- spdep::nb2listw(rainha, style="W", zero.policy = TRUE)
knn4_lista <- spdep::nb2listw(knn4, style="W", zero.policy = TRUE)
knn5_lista <- spdep::nb2listw(knn5, style="W")
knn6_lista <- spdep::nb2listw(knn6, style="W", zero.policy = TRUE)
knn7_lista <- spdep::nb2listw(knn7, style="W", zero.policy = TRUE)

i_moran <- data.frame(
  Matriz = c("Rainha 1", "KNN 4", "KNN 5", "KNN 6", "KNN 7"),
  `I de Moran` = c(
    as.numeric(moran.test(spatial$ice, rainha_lista)$estimate[1]),
    as.numeric(moran.test(spatial$ice, knn4_lista)$estimate[1]),
    as.numeric(moran.test(spatial$ice, knn5_lista)$estimate[1]),
    as.numeric(moran.test(spatial$ice, knn6_lista)$estimate[1]),
    as.numeric(moran.test(spatial$ice, knn7_lista)$estimate[1])
  ),
  `p-value` = c(
    moran.test(spatial$ice, rainha_lista)$p.value,
    moran.test(spatial$ice, knn4_lista)$p.value,
    moran.test(spatial$ice, knn5_lista)$p.value,
    moran.test(spatial$ice, knn6_lista)$p.value,
    moran.test(spatial$ice, knn7_lista)$p.value
  )
)

moran_i <- spdep::moran.test(shape_final$ice, knn5_lista)
moran_local <- spdep::localmoran(shape_final$ice, knn5_lista, alternative = "two.sided")

shape_final$ylag <- spdep::lag.listw(knn5_lista, shape_final$ice)


shape_final$quadrante <- NA
shape_final$p_value <- unname(moran_local[, 5])

# high-high quadrant
shape_final[(shape_final$ice >= 0 & 
  shape_final$ylag >= 0) & 
 (moran_local[, 5] <= 0.05), "quadrante"] <- "High-High"
# low-low quadrant
shape_final[(shape_final$ice <= 0 & 
  shape_final$ylag <= 0) & 
 (moran_local[, 5] <= 0.05), "quadrante"] <- "Low-Low"
# high-low quadrant
shape_final[(shape_final$ice >= 0 & 
  shape_final$ylag <= 0) & 
 (moran_local[, 5] <= 0.05), "quadrante"] <- "High-Low"
# low-high quadrant
shape_final[(shape_final$ice <= 0 & 
  shape_final$ylag >= 0) & 
 (moran_local[, 5] <= 0.05), "quadrante"] <- "Low-High"
# non-significant observations
shape_final[(moran_local[, 5] > 0.05), "quadrante"] <- "Não significativo"

i_moran_ice <- ggplot2::ggplot(shape_final, aes(x = ice, y = ylag)) +
  ggplot2::geom_point(
    mapping = aes(colour = factor(quadrante)),
    size = 2
  ) +
  ggplot2::scale_colour_manual(
    name = "I de Moran Local (p <= 0.05)",
    values = c(
      "Não significativo" = "grey85",
      "High-High" = "#f8766d",
      "Low-Low" = "#00b4ef",
      "High-Low" = "#00bc59",
      "Low-High" = "#bf80ff"
    )
  ) +
  ggplot2::geom_abline(slope = as.numeric(moran_i$estimate)[1], color = "#7159c1", linewidth = 1) +
  ggplot2::geom_hline(yintercept = 0, linetype = "dashed", color = "grey50") +
  ggplot2::geom_vline(xintercept = 0, linetype = "dashed", color = "grey50") +
  labs(title = paste("I de Moran: ", format(as.numeric(moran_i$estimate)[1], digits = 4)), x = "ICE", y = "lagged ICE") +
  ggplot2::coord_fixed(
    xlim = c(-10, 10),
    ylim = c(-10, 10)
  ) +
  ggplot2::theme_classic() +
  ggplot2::theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    title = element_text(size = 8, colour = "#7159c1"),
    plot.title = element_text(hjust = 0.5)
  )

ggsave()

shape <- shape_final |> sf::st_as_sf()

moranlocal_map <- ggplot2::ggplot(shape) +
  ggplot2::geom_sf(mapping = aes(fill = quadrante), lwd = 0, colour = NA) +
  ggplot2::scale_fill_manual(
    name = "ICE",
    values = c(
      "Não significativo" = "grey85",
      "High-High" = "#f8766d",
      "Low-Low" = "#00b4ef",
      "High-Low" = "#00bc59",
      "Low-High" = "#bf80ff"
    )
  ) +
  theme_void() +
  theme(
    text = element_text(face = "plain"),
  ) +
  annotation_scale(location = "bl", line_width = 0.4) +
  annotation_north_arrow(
    location = "br",
    style = north_arrow_fancy_orienteering(),
    height = unit(1, "cm"),
    width = unit(1, "cm")
  )

  residuals_imoran <- data.frame(
    Matriz = c("Rainha 1", "KNN 4", "KNN 5", "KNN 6", "KNN 7"),
    `I de Moran (Resíduos)` = c(
      as.numeric(lm.morantest(ols, listw = rainha_lista)$estimate[1]),
      as.numeric(lm.morantest(ols, listw = knn4_lista)$estimate[1]),
      as.numeric(lm.morantest(ols, listw = knn5_lista)$estimate[1]),
      as.numeric(lm.morantest(ols, listw = knn6_lista)$estimate[1]),
      as.numeric(lm.morantest(ols, listw = knn7_lista)$estimate[1])
    ),
    `p-value` = c(
      as.numeric(lm.morantest(ols, listw = rainha_lista)$p.value),
      as.numeric(lm.morantest(ols, listw = knn4_lista)$p.value),
      as.numeric(lm.morantest(ols, listw = knn5_lista)$p.value),
      as.numeric(lm.morantest(ols, listw = knn6_lista)$p.value),
      as.numeric(lm.morantest(ols, listw = knn7_lista)$p.value)
    )
  )

  testes <- lm.LMtests(ols,listw=knn5_lista, test=c("LMerr", "LMlag", "RLMerr", "RLMlag", "SARMA"))

library(reticulate)
use_python("venv/bin/python")
use_virtualenv("./venv")
source_python("main.py")

sdm2ls <- glm("ice ~ diversidade_credito + diversidade_bancaria + PLB + PLP + log_populacao + op_cred", data = shape_final)

sdm2ls$coefficients = finreg$betas
sdm2ls$residuals = finreg$u
sdm2ls$fitted.values = finreg$predy
summary(sdm2ls)$r.squared = finreg$pr2
sdm2ls$aic = finreg$aic
print(finreg$yend)

ggplot2::ggplot(shape_final,  aes(x = diversidade_credito, y = ice)) +
  ggplot2::geom_point(mapping = aes(colour = op_cred)) +
  ggplot2::geom_abline(intercept = model$coefficients[1], slope = model$coefficients[2], color = "red") +
  ggplot2::scale_color_gradientn(
    colors = palette1,
    name = "Operações de crédito",
  ) +
  ggplot2::labs(
    x = "IDO",
    y = "ICE",
  ) +
  ggplot2::theme_minimal() +
  ggplot2::theme(
    text = ggplot2::element_text(size = 10, face = "bold"),
    legend.text = ggplot2::element_text(size = 10, face = "plain")
  )
