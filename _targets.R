# _targets.R
library(targets)
library(tarchetypes)

#options(tidyverse.quiet = TRUE). le os pacotes
tar_option_set(packages = c("tidyverse", "tsibble",
                            "fera","efat",
                            "lubridate",
                            "fable", "fabletools", #"Metrics",
                            "ROracle", "trelliscopejs"))

tar_source() # funcoes auxiliares


list(
  tar_target(
    mes_ref, # mes de referência em formato 'AAAA-MM'
    '2024-10'
  ),
  
  ## carrega dados atualizados ####
  tar_force(# para verificar se houve alterações
    force = FALSE,# força o carregamento da tabela
    caes_raw, # dados originais para serem importados
    ora_le(.select = "ano, mes, cae3, 
           dmrss_rem_total REM, dmrss_nps NPS",
           .from = "V_DMRSS_FORECASTING",
           .user = keyring::key_list("oracle")[1, 2],
           .pass = keyring::key_get("oracle", keyring::key_list("oracle")[1, 2]))
    ),
  tar_target(
    caes, # dados originais em formato tibble com coluna PERIODO e identificador unique CAE
    caes_raw %>% 
      col_PERIODO()  %>%
      mutate(#CAE = paste(NIF, CAE, sep = '_'),
             id=CAE3)
  ),
  tar_target(
    caes_tsib, # tabela com CAE em formato tsibble
    caes %>% 
      select(CAE3, PERIODO, REM, NPS) %>%
      mutate(ano_mes = yearmonth(PERIODO)) %>%
      as_tsibble(index = ano_mes, key = CAE3)
  ),
  
  ## EDA ####
  tar_target(
    caes_tsib_nest, # tsibble nested por CAE
    caes_tsib %>%
      mutate(id = CAE3) %>%
      nest(data = -id) %>%  
      # identifica series incompletas
      mutate(n_row = map_dbl(data,
                             ~nrow(.x)),
             n_na = map_dbl(data,
                            ~tsibble::count_gaps(.x,.full = end()) %>%
                              pull(.n) %>% sum()),
             data_full = map(data,
                             ~tsibble::fill_gaps(.x, .full = end()) 
                             ),
             max_na_nps = map_dbl(data_full,
                              ~.x %>% 
                                pull(NPS) %>% fera::gap_max(.)),
             max_na_rem = map_dbl(data_full,
                                  ~.x %>% 
                                    pull(NPS) %>% fera::gap_max(.)),
             data_full_ks = map(data_full,
                                ~mutate(.x,
                                        PERIODO = as.Date(ano_mes),
                                        REM = imputeTS::na_kalman(REM),
                                        NPS = round(imputeTS::na_kalman(NPS),0))),
             max_data = map_chr(data_full,
                            ~max(.x$PERIODO, na.rm = T) %>% as.character())
             
             )
    ), 
  tar_target(
    caes_tsib_nest_descr, # adiciona primeiro mês para tabela descritiva
    caes_tsib_nest %>%
        mutate(min_data = map_chr(data_full,
                                  ~min(.x$PERIODO, na.rm = T) %>% as.character())) %>% 
      select(-starts_with('data'))
  )
  
  ## Identificação de outliers #####
  , tar_target(
    caes_nest, # dados nested por CAE com resultados iForest
    caes %>%
      nest(data = -id) %>% 
      mutate(n_row = map_dbl(data, 
                            ~nrow(.x)), 
             n_na = map_dbl(data, 
                           ~sum(is.na(.x$REM))),
             # identificação de outliers
             iforest_rem = map2(.x = data,
                                .y = id,
                                ~efat::iforest.out(.x,
                                                   CAE = .y,
                                                   coluna= REM,
                                                   DataLim = paste0(mes_ref, '-01'))
                                ),
             iforest_nps = map2(.x = data,
                                .y = id,
                                ~efat::iforest.out(.x,
                                                   CAE = .y,
                                                   coluna= NPS,
                                                   DataLim = paste0(mes_ref, '-01'))
                                )
             )
    ),
  tar_target(
    caes_nest_outliers, # graficos com outliers identificados
    caes_nest %>%
      mutate(n_out_rem = map_dbl(iforest_rem, 
                             ~length(.x$iout)), 
             plot_out_rem = map_plot(iforest_rem,
                                 ~.x %>% 
                                   .$plot.out +
                                   ggplot2::scale_x_date(limits = c(as.Date("2021-01-01"), 
                                                                    as.Date(paste0(mes_ref, '-01'))))
                                 ),
             n_out_nps = map_dbl(iforest_nps, 
                                 ~length(.x$iout)), 
             plot_out_nps = map_plot(iforest_nps,
                                     ~.x %>% 
                                       .$plot.out +
                                       ggplot2::scale_x_date(limits = c(as.Date("2021-01-01"), 
                                                                        as.Date(paste0(mes_ref, '-01'))))
                                     ),
             )
    )
  #####
  # corrigir outliers ??
  #####

  ## Escolhe o melhor método para o nowcast #####
  , tar_target(
    cv_nowcast, # Cross-Validation para o nowcast do mês de referencia para REM e NPS
    {
      caes_tsib_nest %>%
        filter(max_data == as.Date(paste0(mes_ref, '-01')) %m-% months(1) %>% as.character(), # seleciona os que têm m~es antrior ao mes de referencia
               n_na < 5) %>% # selecionar apenas as CAE com de 5 missings
        #slice(311) %>%
        # aplica CV para cada uma das series
        mutate(cv_rem = map(data_full_ks,
                            ~cv_now(.x, mes_ref, REM, 12, mase)),
               cv_nps = map(data_full_ks,
                            ~cv_now(.x, mes_ref, NPS, 12, mase))
        )
    }
  )
  , tar_target(
    plot_res_cv_nowcast, # graficos com os resultado de CV e nowcast do melhor modelo
    {
      cv_nowcast %>%
        select(-data, -data_full) %>% # tibble inicial com os resultados do CV e sem as colunas 'data'

        mutate(
            # identifica o melhor modelo para REM e NPS
            best_model_rem = map_chr(cv_rem,
                                     ~.x$best_model),
            best_model_nps = map_chr(cv_nps,
                                     ~.x$best_model),
            # junta os resultados dos 12 meses do CV para REM e NPS numa unica df
            df_cv_rem = map(cv_rem,
                            ~.x$df_now %>%
                              .$now_mensal %>%
                              bind_rows()),
            df_cv_nps = map(cv_nps,
                            ~.x$df_now %>%
                              .$now_mensal %>%
                              bind_rows() ),
            # nowcast do melhor modelo para o mês de referencia
            mes_now_rem = map2(.x = data_full_ks,
                               .y = best_model_rem,
                               ~mes_now(.x, mes_ref, REM) %>%
                                 filter(.model == .y)),
            mes_now_nps = map2(.x = data_full_ks,
                               .y = best_model_nps,
                               ~mes_now(.x, mes_ref, NPS) %>%
                                 filter(.model == .y))
            #graficos com os resultados finais
            , gg_cv_now_rem = trelliscopejs::pmap_plot(list(dts = data_full_ks, df_cv = df_cv_rem, dts_best_now = mes_now_rem, id = id),
                                                            ~gg_cv_now(dts = ..1, df_cv = ..2, dts_best_now = ..3, id = ..4,
                                                                       var = REM)),
            gg_cv_now_nps = trelliscopejs::pmap_plot(list(dts = data_full_ks, df_cv = df_cv_nps, dts_best_now = mes_now_nps, id = id),
                                                            ~gg_cv_now(dts = ..1, df_cv = ..2, dts_best_now = ..3, id = ..4,
                                                                       var = NPS))
        )
      }
    )
  , tar_target(res_nowcast_up, # tabela com os resultados de nowcast para upload
               {
                 rem <- plot_res_cv_nowcast %>%
                   select(id, mes_now_rem) %>%
                   .$mes_now_rem %>%
                   bind_rows() %>% 
                   mutate(aaaa_mm = as.character(as.Date(ano_mes)),
                          ANO_MES = paste0(substr(aaaa_mm,1,4), substr(aaaa_mm,6,7)), 
                          REM = round(now,2)) %>% 
                   as_tibble() %>% 
                   select(CAE3, aaaa_mm, ANO_MES, REM)
                 
                 nps <- plot_res_cv_nowcast %>%
                   select(id, mes_now_nps) %>%
                   .$mes_now_nps %>%
                   bind_rows() %>% 
                   mutate(aaaa_mm = as.character(as.Date(ano_mes)),
                          NPS = round(now,0)) %>% 
                   as_tibble() %>% 
                   select(CAE3, aaaa_mm, NPS)
                 
                 rem %>% 
                   left_join(nps) %>% 
                   mutate(D_UPDATE = ora_date()) %>% 
                   select(ANO_MES, CAE = CAE3, REM, NPS, D_UPDATE)
               }
               )
 
)