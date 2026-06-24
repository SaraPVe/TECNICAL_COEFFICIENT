# TECHNICAL COEFFICIENT

Cálculo de coeficientes técnicos e import shares a partir de matrices input-output (datos UNIZAR / WILIAM).

## Estructura del repositorio

```
├── Data/                          # Datos de entrada (.RData)
│   ├── Data_origin_UNIZAR.RData   # Matriz input-output origen (UNIZAR)
│   ├── Data_origin_WILIAM.RData   # Matriz input-output origen (WILIAM)
│   ├── mis_sectores.RData         # Vectores de sectores y países
│   └── matrix_io_from_excel.RData
│
├── Generar_outputs_generales.R    # Ejecuta TC + BIS + BISO y consolida salidas
│
├── Coeficientes_tecnicos/         # Coeficientes técnicos
│   └── Coeficientes técnicos.R
│
├── Base_Import_Share/             # Import shares agregados
│   └── Base_Import_Shares_BIS.R
│
├── Base_Import_share_by_origin/   # Import shares por país de origen
│   └── Base Import Share by Origin
```

## Scripts

### 0. Flujo general (`Generar_outputs_generales.R`)

Ejecuta los tres bloques del repositorio y genera dos libros consolidados en la raíz:

1. **Comprobaciones generales** → `Comprobaciones_generales_TECHNICAL_COEFFICIENT.xlsx`.
2. **Resultados WILIAM** → `Resultados_WILIAM_TECHNICAL_COEFFICIENT.xlsx`.

El libro de resultados contiene las hojas:

- `EXO_Technical_coefficients_UNIZ` → para `Production.xlsx`.
- `BASE_IMPORT_SHARES_UNIZ` → para `Trade.xlsx`.
- `EXO_Import_origin_shares_UNIZ` → para `Trade.xlsx`.

También crea los rangos nombrados que WILIAM espera para TC, BIS y BISO (`*_UNIZ`). El libro consolidado no sustituye automáticamente `Production.xlsx` y `Trade.xlsx`: deja las hojas y rangos listos para copiar a esos ficheros o para cambiar las rutas del modelo.

Por defecto, el flujo general elimina los Excel intermedios generados por los scripts parciales y deja solo los dos libros consolidados. Para conservarlos temporalmente:

```bash
TC_KEEP_INTERMEDIATE_XLSX=1 Rscript Generar_outputs_generales.R
```

### 1. Coeficientes técnicos (`Coeficientes_tecnicos/Coeficientes técnicos.R`)

Calcula la matriz de coeficientes técnicos a partir de la tabla input-output.

1. **Carga** la matriz IO (`Data_origin_UNIZAR.RData`) con 2 206 filas (país × sector).
2. **Suma por columna** — calcula el output total usando las 2 206 filas.
3. **Suma por sector** — agrega las filas de los 62 sectores intermedios.
4. **División** — divide el consumo intermedio de cada sector entre el output total de la columna.
5. **Limita la matriz** — exporta únicamente las 62 columnas intermedias; la demanda final no forma parte de la tecnología de producción.
6. **Sectores sin producción en UNIZAR** — mantiene a cero las columnas país-sector sin producción.
7. **Validación WILIAM** — reproduce la regla oficial de WILIAM con tecnología media; `HYDROGEN_PRODUCTION` usa la tecnología media de `MANUFACTURE_CHEMICAL`.
8. **Validación UNIZAR** — comprueba cada coeficiente técnico individual frente al rango `[0,0.85]` y documenta las columnas mantenidas a cero.
9. **Exporta comprobaciones parciales** → `Coeficientes_tecnicos/Comprobaciones/Comprobaciones_TC.xlsx`.
10. **Exporta resultados parciales WILIAM** → `Coeficientes_tecnicos/Resultados_WILIAM_TC.xlsx`, con la hoja `EXO_Technical_coefficients_UNIZ` y los rangos nombrados `A_MATRIX_TOTAL_*_UNIZ`.

### 2. Base Import Share (`Base_Import_Share/Base_Import_Shares_BIS.R`)

Calcula la proporción de importaciones de cada sector por país (import share).

1. **Carga** la matriz IO y filtra las filas de impuestos y valor añadido.
2. **Numerador** — para cada fila (país-sector), suma las columnas del mismo país excluyendo la propia fila (importaciones de los demás países para ese sector).
3. **Denominador** — uso total del producto: producción doméstica más importaciones.
4. **Import share** = importaciones / uso total (con denominador cero → 0).
5. **Transposición** — extrae submatrices por país y las apila para obtener el formato final.
6. **Exporta salida parcial** → `Base_Import_Share_R.xlsx`.

### 3. Base Import Share by Origin (`Base_Import_share_by_origin/Base Import Share by Origin`)

Calcula la proporción de importaciones desglosada por país de origen.

1. **Carga** la matriz IO y la convierte a formato largo (`pivot_longer` + `separate`).
2. **Numerador** — suma los valores agrupados por (país fila, sector, país columna, sector limpio).
3. **Denominador** — para cada grupo (país columna, sector), suma los valores de todos los países excepto el propio (diagonal a 0).
4. **Import share** = numerador / denominador, con `case_when` para manejar diagonal y divisiones por cero.
5. **Pivota a ancho** — vuelve al formato matricial, manteniendo el orden original de sectores en las columnas.
6. **NA → 0** y ordena por país y sector.
7. **Validación WILIAM** — conserva el cálculo MRIO crudo y muestra sus diferencias frente a `Trade.xlsx`; no sustituye silenciosamente los valores.
8. **Demanda final WILIAM** — se documenta por separado porque la referencia oficial procede de `PP_to_BP.xlsx` a precios de comprador.
9. **Exporta salidas parciales** → `BISO.xlsx` y `Comprobaciones_BISO.xlsx`.

## Comprobaciones

- Todas las diferencias se calculan con valor absoluto.
- Los coeficientes técnicos de UNIZAR se comprueban frente al rango `[0,0.85]`; los shares de UNIZAR se comprueban frente al rango `[0,1]`.
- Los coeficientes técnicos de UNIZAR se comprueban individualmente; no se suman para este control.
- BIS y BISO de UNIZAR comprueban que los grupos con flujo sumen 1 y los grupos sin flujo sumen 0.
- El libro `Comprobaciones_generales_TECHNICAL_COEFFICIENT.xlsx` consolida TC, BIS y BISO en un único documento.
- El libro `Resultados_WILIAM_TECHNICAL_COEFFICIENT.xlsx` consolida las hojas operativas para WILIAM.
- Si se ejecutan los scripts individuales, estos generan Excel parciales; si se ejecuta `Generar_outputs_generales.R`, se consolidan y se eliminan al final.

## Datos de entrada

| Archivo | Contenido |
|---|---|
| `Data_origin_UNIZAR.RData` | Matriz input-output con 2 206 filas (35 países × ~63 sectores) |
| `mis_sectores.RData` | Vectores auxiliares: `sectores_finales`, `sectores_prioritarios`, `sectores_columna`, `Country` |

## Requisitos

- **R** ≥ 4.0
- Paquetes: `tidyverse`, `readxl`, `openxlsx`, `writexl`
