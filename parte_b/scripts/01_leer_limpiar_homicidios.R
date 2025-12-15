# ============================================================
# Parte B - Script 01: Leer microdatos EDR 2024 (INEGI) y
# filtrar PRESUNTOS HOMICIDIOS (CIE-10: X85–Y09 y Y87.1)
# + corregir EDAD codificada Uxxx (ej: 4030 = 30 años)
#
# Output:
#   parte_b/data_clean/homicidios_2024_limpio.rds
# ============================================================

# ---------------------------
# 0) Paquetes
# ---------------------------
pkgs <- c("foreign", "dplyr", "stringr", "readr", "tibble", "fs", "purrr")
new  <- pkgs[!pkgs %in% rownames(installed.packages())]
if (length(new) > 0) install.packages(new)
invisible(lapply(pkgs, library, character.only = TRUE))

# ---------------------------
# 1) Rutas (PORTABLES)
#    Corre esto desde la raíz del repo: reqrnpdno/
# ---------------------------
root <- fs::path_abs(".")
path_raw   <- fs::path(root, "parte_b", "data_raw")
path_clean <- fs::path(root, "parte_b", "data_clean")
fs::dir_create(path_clean)

if (!dir_exists(path_raw)) {
  stop("No existe: ", path_raw,
       "\nAsegúrate de tener los DBF en parte_b/data_raw/ (o subcarpetas).")
}

# Buscar DEFUN24.dbf aunque esté en subcarpetas
f_def <- list.files(
  path_raw,
  pattern = "^DEFUN24\\.dbf$",
  full.names = TRUE,
  recursive = TRUE,
  ignore.case = TRUE
)

if (length(f_def) == 0) {
  stop("No encontré DEFUN24.dbf dentro de: ", path_raw,
       "\nEjemplos de archivos encontrados (primeros 60):\n",
       paste(head(list.files(path_raw, recursive = TRUE), 60), collapse = "\n"))
}
f_def <- f_def[1]
message("Usando archivo: ", f_def)

# ---------------------------
# 2) Leer DEFUN24
# ---------------------------
message("Leyendo DEFUN24 (puede tardar)...")
def <- foreign::read.dbf(f_def, as.is = TRUE)
def <- tibble::as_tibble(def)
names(def) <- toupper(names(def))

# ---------------------------
# 3) Helpers: detectar columnas
# ---------------------------
pick_col <- function(df, candidates) {
  hit <- intersect(toupper(candidates), names(df))
  if (length(hit) == 0) {
    stop("No encontré ninguna de estas columnas: ",
         paste(candidates, collapse = ", "),
         "\nColumnas disponibles (primeras 80): ",
         paste(head(names(df), 80), collapse = ", "))
  }
  hit[1]
}

col_sexo  <- pick_col(def, c("SEXO"))
col_edad  <- pick_col(def, c("EDAD"))
# causa básica de defunción (puede variar por layout)
col_causa <- pick_col(def, c("CAUSA_DEF", "CAUSABAS", "CAUSA", "CVE_CAUSA", "CAUSA_BAS", "CVECAUSA"))

# ---------------------------
# 4) CIE-10: homicidio (agresiones)
#    X85–Y09 y (opcional) Y87.1 secuelas
# ---------------------------
norm_causa <- function(x) {
  x <- toupper(trimws(as.character(x)))
  x <- gsub("\\.", "", x)  # Y87.1 -> Y871
  x
}

is_homicidio <- function(causa) {
  c <- norm_causa(causa)
  stringr::str_detect(c, "^(X8[5-9]|X9[0-9]|Y0[0-9])") | stringr::str_detect(c, "^Y871")
}

# ---------------------------
# 5) EDAD INEGI codificada Uxxx:
#    unidad = EDAD %/% 1000
#      4=años, 3=meses, 2=días, 1=horas
#    valor = EDAD %% 1000
#    998/999 = no especificado (comúnmente)
# ---------------------------
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

decode_edad_anios <- function(edad_cod) {
  edad_num <- suppressWarnings(as.integer(edad_cod))
  unidad   <- edad_num %/% 1000
  valor    <- edad_num %% 1000
  
  # códigos típicos "sin edad"
  valor[valor %in% c(998, 999)] <- NA_integer_
  
  edad_anios <- dplyr::case_when(
    unidad == 4 ~ as.numeric(valor),                 # años
    unidad == 3 ~ as.numeric(valor) / 12,            # meses
    unidad == 2 ~ as.numeric(valor) / 365.25,        # días
    unidad == 1 ~ as.numeric(valor) / (365.25 * 24), # horas
    TRUE        ~ NA_real_
  )
  
  # plausibilidad
  edad_anios[edad_anios < 0 | edad_anios > 120] <- NA_real_
  edad_anios
}

# ---------------------------
# 6) Filtrar homicidios + limpiar variables clave
# ---------------------------
message("Filtrando presuntos homicidios…")

def_hom <- def %>%
  mutate(
    causa_norm   = norm_causa(.data[[col_causa]]),
    es_homicidio = is_homicidio(causa_norm),
    
    sexo = dplyr::case_when(
      .data[[col_sexo]] %in% c(1, "1") ~ "Hombres",
      .data[[col_sexo]] %in% c(2, "2") ~ "Mujeres",
      TRUE ~ "No especificado"
    ),
    
    edad_anios     = decode_edad_anios(.data[[col_edad]]),
    edad_anios_int = floor(edad_anios),
    grupo_edad     = grupo_edad(edad_anios_int)
  ) %>%
  filter(es_homicidio, sexo %in% c("Hombres", "Mujeres"), !is.na(grupo_edad))

# ---------------------------
# 7) Validaciones (muy importantes)
# ---------------------------
message("OK. Registros de homicidio (con edad válida): ", nrow(def_hom))
print(def_hom %>% count(sexo, grupo_edad, sort = TRUE))

# Diagnóstico: cuántos EDAD sin referencia (998/999) había dentro de homicidios
diag_edad <- def %>%
  mutate(
    causa_norm   = norm_causa(.data[[col_causa]]),
    es_homicidio = is_homicidio(causa_norm),
    edad_num     = suppressWarnings(as.integer(.data[[col_edad]])),
    unidad       = edad_num %/% 1000,
    valor        = edad_num %% 1000
  ) %>%
  filter(es_homicidio) %>%
  count(unidad, valor, sort = TRUE) %>%
  head(25)

message("Diagnóstico EDAD (top 25) dentro de homicidios (antes de filtrar edad):")
print(diag_edad)

# ---------------------------
# 8) Guardar RDS limpio
# ---------------------------
out_rds <- fs::path(path_clean, "homicidios_2024_limpio.rds")
saveRDS(def_hom, out_rds)
message("Guardado en: ", out_rds)
