
#' @title Valor de algum mês anterior
#'
#' @description Devolve o valor observado mum n meses atrás
#' 
#' @param dts tsibble com uma coluna do tipo `Date` (`PERIODO`) e a variável a ser revista
#' @param meses número de meses que queremos substarir ao mês de referência
#' @param var variável do com o valor a rever
#' @param mes_ref mês de referência ao qual queremos subtrair n meses
#' 
#' @return um valor numérico da variável especificada

mes_antes <- function(dts, meses, var, mes_ref){
  
  mes_ref <- lubridate::ymd(paste0(mes_ref,'-01'))
  
  res <- dts %>% 
    dplyr::filter(PERIODO == mes_ref - months(meses) ) %>% 
    dplyr::pull({{var}})
  
  return(res)
}


#' @title Estima mês corrente com variação homóloga
#' @description Função que calcula a previsão do próximo mês aplicando a variação homóloga. 
#' Ou seja, Variação homóloga = (mês homólogo - mês anterior ao homólogo) / mês anterior ao homólogo
#' 
#' @param dts tibble com uma coluna do tipo `yearmonth()` (`ano_mes`) e a variável a ser prevista
#' @param mes mês de referência para a previsão no formato 'AAAA-MM'
#' @param var variável a ser prevista
#' 
#' @return tibble com a previsão do mês de referência para o modelo do da variação homóloga
#'
#' @examples
#' \dontrun{
#' var_hom(dts, "2021-01", var)
#' }
var_hom <- function(dts, var, mes_ref){
  
  
  #mes_ref <- lubridate::ymd(paste0(mes_ref,'-01'))
  
  dts0 <- dts %>% 
    dplyr::filter(ano_mes < tsibble::yearmonth(mes_ref)) %>% 
    dplyr::mutate(value = {{var}})

  m13 <- mes_antes(dts= dts0, meses = 13, var = value, mes_ref = mes_ref)
  m12 <- mes_antes(dts= dts0, meses = 12, var = value, mes_ref = mes_ref)
  var12 <- round((m12-m13)/m13, 4)

  valor <- dts %>%
    dplyr::filter(ano_mes == tsibble::yearmonth(mes_ref) - lubridate::month(1) ) %>%
    dplyr::pull({{var}})
  
  pred <- valor * (1 + var12)
  
  cae <- dts %>% slice(1) %>% pull(CAE3)
  
  real <- dts %>% filter(ano_mes == tsibble::yearmonth(mes_ref)) %>% pull({{var}})
  if(!tsibble::yearmonth(mes_ref) %in% dts$ano_mes){real <- NA_real_}
  
  nome_coluna <- enquo(var)
  
  res <- tsibble::tsibble(CAE3 = cae,
      .model = 'varhom', 
      now = pred, 
      CI95 = NA_character_,
      ano_mes = tsibble::yearmonth(mes_ref),
      real = real) %>% 
    select(CAE3, .model, ano_mes, now, CI95, real) %>% 
    rename(!!quo_name(nome_coluna) := real)

  return(res)
}

#' @title Nowcast do mês corrente
#' @description Função que calcula a previsão do próximo mês para os modelos ARIMA, ETS, SNAIVE, TSLM,  mixed (um ensemble dos 4 modelos anteriores) e aplicação da variação homóloga
#' 
#' @param dts tibble com uma coluna do tipo `yearmonth()` (`ano_mes`) e a variável a ser prevista
#' @param mes mês de referência para a previsão no formato 'AAAA-MM'
#' @param var variável a ser prevista
#' 
#' @return tibble com a previsão do mês corrente para cada um dos modelos usados
#'
#' @examples
#' \dontrun{
#' mes_now(dts, "2021-01", var)
#' }
#' 
#' @import dplyr
#' @import tsibble
#' @import fable
#' @import fabletools
#' 
#' @export
#' 
mes_now <- function(dts, mes, var) {
  # verifica que dts é ul tsibble
  if (!tsibble::is_tsibble(dts)) {
    stop("dts deve ser um tsibble")
  }
  # verifica que tem a coluna `ano_mes`
  if (!"ano_mes" %in% colnames(dts)) {
    stop("dts deve ter a coluna ano_mes")
  }
  # verifica que o input mes pode ser convertido para `yearmonth`
  if (!("yearmonth" %in% class(tsibble::yearmonth(mes)))) {
    stop("mes deve ser um ano e mês no formato 'AAAA-MM'")
  }

  # # verifica que tem a coluna `var`
  # if (!(dts |> select({{var}}) |> names() %in% colnames(dts))) {
  #   stop("dts deve ter a coluna a prever")
  # }
  # verifica que var é numérico
  if (!(dts %>% pull({{var}}) %>% is.numeric())) {
    stop(var, " deve ser numérico")
  }
  
dts0 <- dts %>% 
  dplyr::filter(ano_mes < tsibble::yearmonth(mes)) %>% 
  dplyr::mutate(value = {{var}}) 

models <- dts0 %>%
  fabletools::model(arima = fable::ARIMA(value),
        ets = fable::ETS(value),
        tslm = fable::TSLM(value ~ trend() + season()),
        snaive = fable::SNAIVE(value ~ lag("year"))
        ) %>%  
  dplyr::mutate(mixed = (arima + ets + tslm + snaive) / 4)

res_models <- models %>% 
  fabletools::forecast(h=1) %>%
  fabletools::hilo(level=95) %>% 
  dplyr::rename(dist = value,
                now = .mean) %>%
  dplyr::left_join(dts %>% 
                     dplyr::select(ano_mes, {{var}})) %>% 
  mutate(CI95 = gsub('95','',`95%`)) %>% 
  select(CAE3, .model, ano_mes, now, CI95, {{var}})

res_varhom <- var_hom(dts = dts, var = {{var}}, mes_ref = mes)

return(bind_rows(res_models,
                 res_varhom))
}

# library(testthat)
# test_that("mes_now", {
#   dados <- tsibble::tsibble(ano_mes = tsibble::make_yearmonth(year = 2021, month = 1:12), 
#                           variavel = 1:12)
#   
#   expect_error(mes_now(dts= dados, mes= "test", var= variavel))
#   expect_error(mes_now(dados, "2021-20", variavel))
#   
# })

#' @title Meses anteriores
#' 
#' @description Função que cria um vector com n meses anteriores à data de referência
#' 
#' @param ano_mes data de referência no formato 'AAAA-MM'
#' @param n número de meses anteriores
#' 
#' @return vector com n datas anteriores à data de referência no formato `yearmonth()`
#' 
#' @examples
#' vec_mes("2021-05", 12)
#' 
#' @export
#' 
vec_mes <- function(ano_mes, n) {
  
  ano_mes <- as.Date(paste0(ano_mes, "-01")) - months(n)
  
  vec <- seq(ano_mes, by = "month", length.out = n)
  
  return(tsibble::yearmonth(vec))
  
}

#' @title Cross-Validation para nowcast
#' 
#' @description faz o Cross-Validadtion para os modelos ARIMA, ETS, SNAIVE, TSLM e mixed, a partir de uma tsibble e de número pré-definido de meses
#' 
#' @param dts tsibble com uma coluna do tipo `yearmonth()` (`ano_mes`) e a variável a ser prevista
#' @param mes_ref mês de referência para a previsão no formato 'AAAA-MM'
#' @param var variável a ser prevista
#' @param n número de meses anteriores a `mes_ref` para treinar os modelos
#' @param metrica métrica a ser usada para escolher o melhor modelo
#' 
#' @return lista com os dataframes `df_now` (com as previsões para os meses de CV), `df_metrics` (com os resultados das métricas para cada modelo) e `best_model` (com o melhor modelo)
#' 
#' @examples
#' \dontrun{
#' cv_now(dts, "2021-01", var, 12, mape)
#' }
#' 
#' @import dplyr
#' @import purrr
#' @import Metrics
#' @import tsibble
#' @import fable
#' @import fabletools
#' 
#' @export
#' 
cv_now <- function(dts, mes_ref, var, n, metrica) {
  
  df <- vec_mes(mes_ref, n) %>%  
    tsibble::as_tibble() %>% 
    dplyr::rename(mes = value)
  
  df_now <- df %>% 
    dplyr::mutate(now_mensal = purrr::map(mes,
                            ~mes_now(dts, .x, {{var}})))

  
  df_metrics <- df_now %>%
    .$now_mensal %>% 
    dplyr::bind_rows() %>%
    tsibble::as_tibble() %>%
    dplyr::group_by(.model) %>%
    dplyr::summarise(rrse = Metrics::rrse({{var}}, now),
                    mape = Metrics::mape({{var}}, now),
                    rmse = Metrics::rmse({{var}}, now),
                    mase = Metrics::mase({{var}}, now)) %>% 
    ungroup()
  
  res_metrics <- df_metrics %>% pull({{metrica}}) %>% min()
  
  if(is.nan(res_metrics) | is.infinite(res_metrics) ){
    best_model <- 'varhom'
  } else {
  best_model <- df_metrics %>% 
    dplyr::filter({{metrica}} == min({{metrica}})) %>% 
    dplyr::pull(.model)
  }

  list(df_now = df_now, df_metrics = df_metrics, best_model = best_model)
  
}


#' @title Gráficos com o Nowcast
#'
#' @description Função que faz o plot dos resultados do Cross-Validation e do nowcast
#' 
#' @param dts tsibble com uma coluna do tipo `yearmonth()` (`ano_mes`) e a variável a ser prevista
#' @param df_cv dataframe com os resultados CV para os modelos ARIMA, ETS, SNAIVE, TSLM e mixed
#' @param dts_best_now dataframe com o nowcast para o melhor modelo
#' @param id identificador da série temporal
#' @param var variável do nowcast
#' 
#' @return gráfico com os resultados do Cross-Validation e do nowcast

gg_cv_now <- function(dts, df_cv, dts_best_now, id, var) {
  
  # serie temporal do id
  ggplot(data = dts, aes(x = as.Date(ano_mes), y = {{var}})) + 
    ggplot2::geom_point() +
    ggplot2::geom_line(linetype = "dashed") +
    
    # resultados do treino com CV
    ggplot2::geom_line(data = df_cv, aes(x = as.Date(ano_mes), y = now, color = .model)) +
    ggplot2::scale_color_manual(values = c("#4285f4", "#dd4b39", "#fbbc05", "#34a853", "#673ab7", "#00ff83")) +
    
    # resultado do nowcast para o melhor modelo
    ggplot2::geom_point(data = dts_best_now ,
                        aes(x = as.Date(ano_mes), y = now),
                        color = 'red') +
    
    # labels e temas
    ggplot2::labs(title = id,
                  #subtitle = "Cross-Validation",
                  x = "Ano",
                  y = enquo(var)) +
    ggplot2::theme_minimal() +
    ggplot2::theme(legend.position = "bottom")
}

#' @title Data ORACLE
#'
#' @description data - hora do sistema em formato usado no ORACLE
#' 
ora_date <- function() {
  today <- Sys.time()
  return(format(today, format = "%d-%m-%Y %H:%M:%S"))
}
