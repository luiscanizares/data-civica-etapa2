use reqrnpdno::{cliente, extractora, parameters, parameters::Parametros, utilidades};
use std::error::Error;

fn main() -> Result<(), Box<dyn Error>> {
    // =========================================================
    // ETAPA 2 - PARTE A (Data Cívica / RNPDNO)
    // Autor: Luis Cañizares
    //
    // Objetivo:
    //  - Extraer datos del RNPDNO para el periodo:
    //      2024-01-01 a 2025-01-01
    //  - Filtrar por nacionalidad: "Mexicana" (id obtenido del catálogo)
    //  - Generar 1 archivo JSON por entidad federativa en:
    //      ./salida/data_civica_etapa2/estados/
    //
    // Nota:
    //  - La desagregación final en 5 grupos de edad:
    //      0–9, 10–19, 20–35, 36–59, 60+
    //    se realiza posteriormente en R.
    // =========================================================

    // ---------------------------------------------------------
    // 1) Crear carpeta de salida
    // ---------------------------------------------------------
    // Crea la carpeta dentro del repo:
    let ruta_salida = utilidades::crear_directorio("./salida/", "data_civica_etapa2")?;

    // ---------------------------------------------------------
    // 2) Inicializar cliente HTTP
    // ---------------------------------------------------------
    // Consulta catálogos y realiza peticiones al RNPDNO.
    let cli = cliente::cliente_nuevo()?;

    // ---------------------------------------------------------
    // 3) Obtener el ID de la nacionalidad "mexicana" desde el catálogo
    // ---------------------------------------------------------
    let nacionalidades = parameters::get_nacionalidades(&cli)?;

    // Filtra entradas
    let candidatos: Vec<(String, String)> = nacionalidades
        .iter()
        .filter(|(nombre, _id)| nombre.to_uppercase().contains("MEXIC"))
        .map(|(nombre, id)| (nombre.clone(), id.clone()))
        .collect();

    if candidatos.is_empty() {
        return Err("No se encontró una nacionalidad que contenga 'MEXIC' en el catálogo".into());
    }

    // Si hay más de una coincidencia, avisamos y usamos la primera.
    if candidatos.len() > 1 {
        eprintln!("Encontré varias nacionalidades que contienen 'MEXIC':");
        for (n, id) in &candidatos {
            eprintln!("  - {n} => {id}");
        }
        eprintln!("Usaré la primera coincidencia.");
    }

    let id_mexicana = candidatos[0].1.clone();
    println!(
        "ID nacionalidad usada: {} (catálogo: {})",
        id_mexicana, candidatos[0].0
    );

    // ---------------------------------------------------------
    // 4) Definir parámetros del filtro
    // ---------------------------------------------------------
    let mut p = Parametros::new();

    // Título (solo descriptivo para la salida)
    p.titulo = "RNPDNO - Nacionalidad mexicana (2024-01-01 a 2025-01-01)".to_string();

    // Rango de fechas solicitado
    p.fecha_inicio = "2024-01-01".to_string();
    p.fecha_fin = "2025-01-01".to_string();

    // Filtro de nacionalidad
    p.id_nacionalidad = id_mexicana;

    // Importante:
    // - NO se filtra por estatus de víctima.
    // - extraer_por_estados() itera por entidades y crea:
    //     <ruta_salida>/estados/<id>.json
    println!("Ruta de salida: {ruta_salida}");
    println!("Extrayendo datos por estados (esto puede tardar unos minutos)...");

    // ---------------------------------------------------------
    // 5) Ejecutar extracción por estados
    // ---------------------------------------------------------
    extractora::extraer_por_estados(&p, &ruta_salida)?;

    println!("Listo. Revisa: {ruta_salida}/estados/");
    Ok(())
}
