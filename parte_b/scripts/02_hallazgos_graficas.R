# ============================================================
# Parte B - Script 02: 3 hallazgos + 3 gráficas + 3 párrafos
# Usa el output del Script 01:
#   parte_b/data_clean/homicidios_2024_limpio.rds
#
# Outputs:
#   parte_b/figuras/fig1_sexo_grupo_edad.png
#   parte_b/figuras/fig2_top_estados.png
#   parte_b/figuras/fig3_indig_o_migr.png
#   parte_b/entrega/parrafos_hallazgos.txt
# ============================================================

# ---------------------------
# 0) Paquetes
# ---------------------------
pkgs <- c("dplyr","stringr","readr","tibble","fs","ggplot2","scales","purrr")
new  <- pkgs[!pkgs %in% rownames(installed.packages())]
if (length(new) > 0) install.packages(new)
invisible(lapply(pkgs, library, character.only = TRUE))

# ---------------------------
# 1) Rutas (portables)
# ---------------------------
root <- fs::path_abs(".")
path_clean <- fs::path(root, "parte_b", "data_clean")
path_fig   <- fs::path(root, "parte_b", "figuras")
path_out   <- fs::path(root, "parte_b", "entrega")
fs::dir_create(path_fig)
fs::dir_create(path_out)

f_rds <- fs::path(path_clean, "homicidios_2024_limpio.rds")
if (!file.exists(f_rds)) stop("No existe: ", f_rds, "\nCorre primero el Script 01.")

df <- readRDS(f_rds)

stopifnot(all(c("sexo","grupo_edad") %in% names(df)))

# ---------------------------
# 2) Hallazgo 1: sexo × grupo de edad (distribución)
# ---------------------------
sum1 <- df %>%
  count(sexo, grupo_edad, name="n") %>%
  group_by(sexo) %>%
  mutate(pct = n / sum(n)) %>%
  ungroup()

# Métrica narrativa: % de víctimas hombres 20-35
tot <- nrow(df)
n_h_joven <- sum(df$sexo == "Hombres" & df$grupo_edad == "Jóvenes (20-35)")
pct_h_joven <- n_h_joven / tot

p1 <- ggplot(sum1, aes(x = grupo_edad, y = n, fill = sexo)) +
  geom_col(position = position_dodge(width = 0.8), width = 0.7) +
  scale_y_continuous(labels = scales::comma) +
  labs(
    title = "Presuntos homicidios 2024: distribución por sexo y grupo de edad",
    x = NULL, y = "Número de víctimas", fill = NULL
  ) +
  theme_minimal(base_size = 12) +
  theme(axis.text.x = element_text(angle = 20, hjust = 1))

f1 <- fs::path(path_fig, "fig1_sexo_grupo_edad.png")
ggsave(f1, p1, width = 10, height = 5.5, dpi = 300)

parrafo1 <- paste0(
  "Hallazgo 1 — La violencia homicida se concentra en hombres jóvenes. ",
  "En 2024, los hombres de 20 a 35 años representan ",
  scales::percent(pct_h_joven, accuracy = 0.1),
  " de todas las víctimas registradas como presuntos homicidios (con edad válida). ",
  "Esto sugiere que la prevención y la reducción de homicidios requiere estrategias focalizadas en juventudes masculinas, ",
  "incluyendo contextos comunitarios y territoriales donde la violencia se intensifica."
)

# ---------------------------
# 3) Hallazgo 2: concentración territorial (Top 10 entidades)
#    Detecta variable de entidad automáticamente.
# ---------------------------
find_col_regex <- function(df, patterns) {
  nm <- names(df)
  for (pat in patterns) {
    hit <- nm[str_detect(nm, regex(pat, ignore_case = TRUE))]
    if (length(hit) > 0) return(hit[1])
  }
  NA_character_
}

# intentamos: ocurrencia > residencia > registro
col_ent <- find_col_regex(df, c("^ENT_OCU", "^ENT_OCURR", "^ENT_RES", "^ENT_REG", "^ENTID"))
if (is.na(col_ent)) {
  message("No encontré columna de entidad (ENT_OCURR/ENT_RES/ENT_REG). Haré hallazgo 2 con edad (fallback).")
}

# Catálogo básico 1..32 (para etiquetar sin depender de catálogos)
mx_estados <- tibble::tibble(
  entidad_id = 1:32,
  entidad = c(
    "Aguascalientes","Baja California","Baja California Sur","Campeche","Coahuila","Colima",
    "Chiapas","Chihuahua","Ciudad de México","Durango","Guanajuato","Guerrero","Hidalgo",
    "Jalisco","Estado de México","Michoacán","Morelos","Nayarit","Nuevo León","Oaxaca",
    "Puebla","Querétaro","Quintana Roo","San Luis Potosí","Sinaloa","Sonora","Tabasco",
    "Tamaulipas","Tlaxcala","Veracruz","Yucatán","Zacatecas"
  )
)

if (!is.na(col_ent)) {
  tmp2 <- df %>%
    mutate(entidad_id = suppressWarnings(as.integer(.data[[col_ent]]))) %>%
    filter(!is.na(entidad_id), entidad_id >= 1, entidad_id <= 32) %>%
    left_join(mx_estados, by = "entidad_id") %>%
    count(entidad, sexo, name = "n") %>%
    group_by(sexo) %>%
    mutate(rank = dense_rank(desc(n))) %>%
    filter(rank <= 10) %>%
    ungroup()
  
  p2 <- ggplot(tmp2, aes(x = reorder(entidad, n), y = n, fill = sexo)) +
    geom_col(position = position_dodge(width = 0.8), width = 0.7) +
    coord_flip() +
    scale_y_continuous(labels = scales::comma) +
    labs(
      title = "Top 10 entidades por presuntos homicidios en 2024 (según variable disponible)",
      x = NULL, y = "Número de víctimas", fill = NULL
    ) +
    theme_minimal(base_size = 12)
  
  f2 <- fs::path(path_fig, "fig2_top_estados.png")
  ggsave(f2, p2, width = 10, height = 6, dpi = 300)
  
  top_total <- df %>%
    mutate(entidad_id = suppressWarnings(as.integer(.data[[col_ent]]))) %>%
    filter(!is.na(entidad_id), entidad_id >= 1, entidad_id <= 32) %>%
    left_join(mx_estados, by="entidad_id") %>%
    count(entidad, name="n") %>%
    arrange(desc(n))
  
  top1 <- top_total$entidad[1]
  top1_n <- top_total$n[1]
  top1_pct <- top1_n / sum(top_total$n)
  
  parrafo2 <- paste0(
    "Hallazgo 2 — La violencia homicida se concentra territorialmente. ",
    "La entidad con más presuntos homicidios en 2024 es ", top1, " (",
    scales::comma(top1_n), " casos), lo que equivale a ",
    scales::percent(top1_pct, accuracy = 0.1),
    " del total registrado en la variable territorial usada. ",
    "Este patrón de concentración sugiere que las políticas públicas deben priorizar territorios específicos, ",
    "combinando prevención social, fortalecimiento institucional y acciones de reducción de riesgos."
  )
} else {
  # Fallback simple (si no hay entidad): nada de fig2 territorial, reusamos edad×sexo con enfoque distinto
  f2 <- fs::path(path_fig, "fig2_top_estados.png")
  parrafo2 <- "Hallazgo 2 — (Pendiente): No se encontró una columna de entidad (ENT_OCURR/ENT_RES/ENT_REG) en el microdato limpio. Revisa el nombre exacto de la variable territorial para generar el Top 10 por entidades."
}

# ---------------------------
# 4) Hallazgo 3: enfoque en población indígena o migrante (si hay variable)
#    - intenta detectar “lengua indígena / habla lengua”
#    - si no, intenta “país de nacimiento”
# ---------------------------
# detectar posibles columnas
col_lengua_bin <- find_col_regex(df, c("HABLA.*LENG", "LENGUA.*HABLA", "HLENG"))
col_lengua_cod <- find_col_regex(df, c("^LENGUA$", "LENGUA_"))
col_pais_nac   <- find_col_regex(df, c("PAIS.*NAC", "NACIO", "PAISNAC"))

tmp3 <- NULL
tipo3 <- NULL

# Caso A: variable binaria habla lengua indígena (1/2)
if (!is.na(col_lengua_bin)) {
  tmp3 <- df %>%
    mutate(
      habla_lengua_indig = case_when(
        .data[[col_lengua_bin]] %in% c(1,"1","SI","SÍ","Si","Sí") ~ "Habla lengua indígena",
        .data[[col_lengua_bin]] %in% c(2,"2","NO","No") ~ "No habla lengua indígena",
        TRUE ~ NA_character_
      )
    ) %>%
    filter(!is.na(habla_lengua_indig)) %>%
    count(sexo, habla_lengua_indig, name="n") %>%
    group_by(sexo) %>%
    mutate(pct = n/sum(n)) %>%
    ungroup()
  tipo3 <- "lengua"
}

# Caso B: solo código de lengua (asumimos que no-missing implica lengua registrada)
if (is.null(tmp3) && !is.na(col_lengua_cod)) {
  tmp3 <- df %>%
    mutate(
      lengua_cod = suppressWarnings(as.integer(.data[[col_lengua_cod]])),
      habla_lengua_indig = case_when(
        is.na(lengua_cod) ~ NA_character_,
        lengua_cod %in% c(998,999,0) ~ "No lengua/No especificado",
        TRUE ~ "Lengua registrada (posible indígena)"
      )
    ) %>%
    filter(!is.na(habla_lengua_indig)) %>%
    count(sexo, habla_lengua_indig, name="n") %>%
    group_by(sexo) %>%
    mutate(pct = n/sum(n)) %>%
    ungroup()
  tipo3 <- "lengua_cod"
}

# Caso C: país de nacimiento (migrantes)
if (is.null(tmp3) && !is.na(col_pais_nac)) {
  # intentamos identificar "México" por código 484 (común) o texto "MEX"
  pais_cod <- suppressWarnings(as.integer(df[[col_pais_nac]]))
  
  tmp3 <- df %>%
    mutate(
      pais_nac = suppressWarnings(as.integer(.data[[col_pais_nac]])),
      migrante = case_when(
        is.na(pais_nac) ~ NA_character_,
        pais_nac %in% c(484, 1) ~ "Nacido en México", # 484 es estándar ISO num; 1 a veces es México en catálogos locales
        TRUE ~ "Nacido en el extranjero"
      )
    ) %>%
    filter(!is.na(migrante)) %>%
    count(sexo, migrante, name="n") %>%
    group_by(sexo) %>%
    mutate(pct = n/sum(n)) %>%
    ungroup()
  tipo3 <- "migrante"
}

if (is.null(tmp3)) {
  # fallback
  tmp3 <- tibble(sexo = character(), grupo = character(), n = integer(), pct = double())
  tipo3 <- "ninguno"
}

if (tipo3 != "ninguno") {
  col_group <- if (tipo3 == "migrante") "migrante" else "habla_lengua_indig"
  
  p3 <- ggplot(tmp3, aes(x = .data[[col_group]], y = pct, fill = sexo)) +
    geom_col(position = position_dodge(width = 0.8), width = 0.7) +
    scale_y_continuous(labels = scales::percent_format(accuracy = 0.1)) +
    labs(
      title = "Presuntos homicidios 2024: población indígena o migrante (según variable disponible)",
      x = NULL, y = "Porcentaje dentro de cada sexo", fill = NULL
    ) +
    theme_minimal(base_size = 12)
  
  f3 <- fs::path(path_fig, "fig3_indig_o_migr.png")
  ggsave(f3, p3, width = 10, height = 5.5, dpi = 300)
  
  # párrafo 3 (con número)
  if (col_group == "habla_lengua_indig") {
    # toma el % (Hombres, habla)
    vH <- tmp3 %>% filter(sexo=="Hombres", .data[[col_group]] != "No habla lengua indígena") %>% summarise(p=sum(pct)) %>% pull(p)
    vM <- tmp3 %>% filter(sexo=="Mujeres", .data[[col_group]] != "No habla lengua indígena") %>% summarise(p=sum(pct)) %>% pull(p)
    parrafo3 <- paste0(
      "Hallazgo 3 — La violencia también debe leerse con lentes de desigualdad. ",
      "Entre las víctimas registradas, la proporción con lengua indígena (según la variable disponible) es de ",
      scales::percent(vH, accuracy=0.1), " en hombres y ",
      scales::percent(vM, accuracy=0.1), " en mujeres. ",
      "Este tipo de desagregación es clave porque la violencia se cruza con exclusiones históricas; ",
      "visibilizar a poblaciones racializadas ayuda a orientar respuestas más justas y culturalmente pertinentes."
    )
  } else {
    vH <- tmp3 %>% filter(sexo=="Hombres", migrante=="Nacido en el extranjero") %>% pull(pct)
    vM <- tmp3 %>% filter(sexo=="Mujeres", migrante=="Nacido en el extranjero") %>% pull(pct)
    vH <- ifelse(length(vH)==0, NA, vH)
    vM <- ifelse(length(vM)==0, NA, vM)
    parrafo3 <- paste0(
      "Hallazgo 3 — Migración y violencia: mirar la vulnerabilidad. ",
      "La proporción de víctimas nacidas en el extranjero (según la variable disponible) es de ",
      scales::percent(vH, accuracy=0.1), " en hombres y ",
      scales::percent(vM, accuracy=0.1), " en mujeres. ",
      "Aunque la proporción pueda ser baja, es relevante porque personas migrantes pueden enfrentar barreras adicionales ",
      "de acceso a justicia y protección; desagregar así ayuda a no invisibilizar riesgos específicos."
    )
  }
} else {
  f3 <- fs::path(path_fig, "fig3_indig_o_migr.png")
  parrafo3 <- "Hallazgo 3 — (Pendiente): No se encontró variable clara de lengua indígena ni país de nacimiento. Revisa nombres de columnas (ej. HABLA_LENGUA*, LENGUA*, PAIS_NAC*) para construir un hallazgo con enfoque en comunidades racializadas o migrantes."
}

# ---------------------------
# 5) Guardar párrafos (para pegar en Word/PDF)
# ---------------------------
out_txt <- fs::path(path_out, "parrafos_hallazgos.txt")
writeLines(c(parrafo1, "", parrafo2, "", parrafo3), out_txt)
message("Listo. Figuras guardadas en: ", path_fig)
message("Párrafos guardados en: ", out_txt)
message("Archivos: ", f1, " | ", f2, " | ", f3)
