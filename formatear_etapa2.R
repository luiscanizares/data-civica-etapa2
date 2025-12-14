# ============================================================
# Data Cívica / RNPDNO - Etapa 2 (PARTE A)
# Autor: Luis Cañizares
#
# Objetivo:
#   1) Leer los 32 JSON por entidad federativa generados por Rust:
#        ./salida/data_civica_etapa2/estados/1.json ... 32.json
#   2) Extraer la distribución por edad y sexo
#   3) Agregar a 5 grupos de edad
#   4) Construir un data frame único y exportar CSV.
#   5) Unir nombre de entidad desde: ./diccionarios/estados.json
#   6) Generar CSV de entrega
# ============================================================


# ============================================================
# 0) CONFIGURACIÓN (rutas)
# ============================================================
path_estados <- "C:/DataCivica/reqrnpdno/salida/data_civica_etapa2/estados"
path_dic     <- "C:/DataCivica/reqrnpdno/diccionarios"
path_salida  <- "C:/DataCivica/reqrnpdno/salida/data_civica_etapa2"

# Archivos de salida
out_csv_base    <- file.path(path_salida, "desapariciones_2024_mexicana_sexo_grupoedad_por_entidad.csv")
out_csv_nombres <- file.path(path_salida, "desapariciones_2024_mexicana_sexo_grupoedad_por_entidad_con_nombres.csv")
out_csv_entrega <- file.path(path_salida, "ENTREGA_final.csv")


# ============================================================
# 1) PAQUETES
# ============================================================
pkgs <- c("jsonlite","dplyr","tidyr","stringr","purrr","tibble","readr")
new  <- pkgs[!pkgs %in% rownames(installed.packages())]
if (length(new) > 0) install.packages(new)
invisible(lapply(pkgs, library, character.only = TRUE))

# Validaciones de rutas
stopifnot(dir.exists(path_estados))
stopifnot(dir.exists(path_dic))
stopifnot(dir.exists(path_salida))


# ============================================================
# 2) FUNCIONES AUXILIARES
# ============================================================

# Convierte strings a número de forma robusta (quita comas y caracteres)
parse_num <- function(x) {
  x <- as.character(x)
  x <- gsub(",", "", x)
  suppressWarnings(as.numeric(gsub("[^0-9.-]", "", x)))
}

# Asigna grupo de edad
grupo_edad <- function(n) {
  dplyr::case_when(
    is.na(n) ~ NA_character_,
    n <= 9   ~ "Infancias (0-9)",
    n <= 19  ~ "Adolescentes (10-19)",
    n <= 35  ~ "Jóvenes (20-35)",
    n <= 59  ~ "Adultos (36-59)",
    TRUE     ~ "Adultos mayores (60+)"
  )
}

# Transforma lista de registros a tibble.
as_tbl_if_records <- function(obj) {
  if (is.list(obj) && length(obj) > 0 &&
      all(purrr::map_lgl(obj, is.list)) &&
      all(purrr::map_lgl(obj, ~ !is.null(names(.x))))) {
    out <- suppressWarnings(dplyr::bind_rows(obj))
    if (is.data.frame(out) && ncol(out) > 0) return(tibble::as_tibble(out))
  }
  NULL
}

# recolecta todas las tablas.
collect_tables <- function(x) {
  out <- list()
  
  rec <- function(obj) {
    if (is.data.frame(obj)) out[[length(out)+1]] <<- tibble::as_tibble(obj)
    
    tbl <- as_tbl_if_records(obj)
    if (!is.null(tbl)) out[[length(out)+1]] <<- tbl
    
    if (is.list(obj)) {
      nms <- names(obj)
      if (is.null(nms)) {
        for (i in seq_along(obj)) rec(obj[[i]])
      } else {
        for (nm in nms) rec(obj[[nm]])
      }
    }
  }
  
  rec(x)
  out
}

# Identifica la tabla que corresponde a edades.
pick_age_table <- function(tabs) {
  score_age_cols <- function(df) {
    cn <- names(df)
    sum(!is.na(suppressWarnings(as.integer(cn))))
  }
  scores <- purrr::map_int(tabs, score_age_cols)
  if (all(scores == 0)) return(NULL)
  tabs[[which.max(scores)]]
}

# Etiqueta filas de la tabla de edades:
# - Se asume (Hombres, Mujeres, Indeterminado).
label_rows_sexo <- function(age_tab, age_cols) {
  mat <- suppressWarnings(as.matrix(dplyr::select(age_tab, dplyr::all_of(age_cols))))
  storage.mode(mat) <- "numeric"
  rs <- rowSums(mat, na.rm = TRUE)
  
  if (nrow(age_tab) == 3) {
    idx_ind  <- if (any(rs == 0)) which(rs == 0)[1] else which.min(rs)
    idx_rest <- setdiff(1:3, idx_ind)
    
    ord <- idx_rest[order(rs[idx_rest], decreasing = TRUE)]
    lab <- rep(NA_character_, 3)
    lab[ord[1]]  <- "Hombres"
    lab[ord[2]]  <- "Mujeres"
    lab[idx_ind] <- "Indeterminado"
    return(lab)
  }
  
  if (nrow(age_tab) == 2) {
    ord <- order(rs, decreasing = TRUE)
    lab <- rep(NA_character_, 2)
    lab[ord[1]] <- "Hombres"
    lab[ord[2]] <- "Mujeres"
    return(lab)
  }
  
  # Caso raro: si no son 2 o 3 filas, se etiqueta genérico
  paste0("fila_", seq_len(nrow(age_tab)))
}

# Lee 1 archivo de estado y devuelve:
# entidad_id x sexo(H/M) x grupo_edad x total
read_one_estado <- function(f, debug = FALSE) {
  # entidad_id viene del nombre del archivo
  entidad_id <- as.integer(stringr::str_extract(basename(f), "\\d+"))
  
  # Carga de JSON como lista
  x <- jsonlite::fromJSON(f, simplifyVector = FALSE)
  
  # Extrae tablas contenidas en el JSON y toma la tabla de edades
  tabs <- collect_tables(x)
  age_tab <- pick_age_table(tabs)
  
  if (is.null(age_tab)) {
    warning("No encontré tabla de edades en: ", basename(f))
    return(tibble(entidad_id=integer(), sexo=character(), grupo_edad=character(), total=double()))
  }
  
  # Toma solo columnas de edad numéricas (excluye "Sin edad de referencia")
  age_cols <- names(age_tab)[!is.na(suppressWarnings(as.integer(names(age_tab))))]
  
  # Etiquetas filas como sexo
  sexo_labels <- label_rows_sexo(age_tab, age_cols)
  age_tab <- dplyr::mutate(age_tab, sexo = sexo_labels)
  
  if (debug) {
    cat("\nDEBUG:", basename(f), "\n")
    cat("  sexo_labels:", paste(sexo_labels, collapse=", "), "\n")
    cat("  nrow:", nrow(age_tab), " | edad_cols:", length(age_cols), "\n")
  }
  
  # Pasa a formato largo, asigna grupo de edad y suma a los 5 grupos
  age_tab %>%
    tidyr::pivot_longer(dplyr::all_of(age_cols), names_to = "edad_raw", values_to = "total_raw") %>%
    dplyr::mutate(
      edad_num   = as.integer(edad_raw),
      grupo_edad = grupo_edad(edad_num),
      total      = parse_num(total_raw)
    ) %>%
    dplyr::filter(sexo %in% c("Hombres","Mujeres"), !is.na(grupo_edad)) %>%
    dplyr::group_by(sexo, grupo_edad) %>%
    dplyr::summarise(total = sum(total, na.rm = TRUE), .groups = "drop") %>%
    dplyr::mutate(entidad_id = entidad_id) %>%
    dplyr::select(entidad_id, sexo, grupo_edad, total)
}


# ============================================================
# 3) CONSTRUIR df
# ============================================================

# Lista de archivos esperados
files_32 <- file.path(path_estados, paste0(1:32, ".json"))
files_32 <- files_32[file.exists(files_32)]
cat("Archivos encontrados (1..32):", length(files_32), "\n")

# Debug sobre el primer JSON para verificación
tmp <- read_one_estado(files_32[1], debug = TRUE)
print(tmp)

# Procesa todos los estados
df_raw <- purrr::map_dfr(files_32, read_one_estado)

# Verifica las combinaciones
grid <- tidyr::expand_grid(
  entidad_id = 1:32,
  sexo = c("Hombres","Mujeres"),
  grupo_edad = c("Infancias (0-9)","Adolescentes (10-19)","Jóvenes (20-35)","Adultos (36-59)","Adultos mayores (60+)")
)

df_final <- grid %>%
  dplyr::left_join(df_raw, by=c("entidad_id","sexo","grupo_edad")) %>%
  dplyr::mutate(total = dplyr::if_else(is.na(total), 0, total)) %>%
  dplyr::arrange(entidad_id, grupo_edad, sexo)

cat("nrow(df_final) =", nrow(df_final), " (esperado 320)\n")

# Exporta CSV (sin nombres)
readr::write_csv(df_final, out_csv_base)
cat("CSV base guardado en:", out_csv_base, "\n")


# ============================================================
# 4) AGREGAR NOMBRE DE ESTADO (diccionarios/estados.json)
# ============================================================
fp_est <- file.path(path_dic, "estados.json")
stopifnot(file.exists(fp_est))

raw_est <- jsonlite::fromJSON(fp_est, simplifyVector = TRUE)
dict <- if (is.data.frame(raw_est)) tibble::as_tibble(raw_est) else dplyr::bind_rows(raw_est) %>% tibble::as_tibble()
names(dict) <- tolower(names(dict))

dict_est <- dict %>%
  tidyr::pivot_longer(cols = everything(), names_to = "entidad_nombre", values_to = "entidad_id") %>%
  dplyr::mutate(
    entidad_nombre = stringr::str_squish(entidad_nombre),
    entidad_id     = suppressWarnings(as.integer(entidad_id))
  ) %>%
  dplyr::filter(!is.na(entidad_id), entidad_nombre != "--todos--") %>%
  dplyr::distinct(entidad_id, .keep_all = TRUE) %>%
  dplyr::arrange(entidad_id)

df_final_nombres <- df_final %>%
  dplyr::left_join(dict_est, by="entidad_id") %>%
  dplyr::relocate(entidad_nombre, .after = entidad_id)

cat("nrow(df_final_nombres) =", nrow(df_final_nombres), "\n")

# Exporta CSV con nombres
readr::write_csv(df_final_nombres, out_csv_nombres)
cat("CSV con nombres guardado en:", out_csv_nombres, "\n")


# ============================================================
# 5) CSV FINAL DE ENTREGA
# ============================================================
df_entrega <- df_final_nombres %>%
  dplyr::mutate(entidad_nombre = tools::toTitleCase(entidad_nombre)) %>%
  dplyr::arrange(entidad_id, entidad_nombre, sexo, grupo_edad)

# Exporta CSV de entrega
readr::write_csv(df_entrega, out_csv_entrega)
cat("ENTREGA_final.csv guardado en:", out_csv_entrega, "\n")
