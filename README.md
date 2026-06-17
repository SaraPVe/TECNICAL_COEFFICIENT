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

### 1. Coeficientes técnicos (`Coeficientes_tecnicos/Coeficientes técnicos.R`)

Calcula la matriz de coeficientes técnicos a partir de la tabla input-output.

1. **Carga** la matriz IO (`Data_origin_UNIZAR.RData`) con 2 206 filas (país × sector).
2. **Suma por columna** — calcula el output total usando las 2 206 filas.
3. **Suma por sector** — agrega las filas de los 62 sectores intermedios.
4. **División** — divide el consumo intermedio de cada sector entre el output total de la columna.
5. **Limita la matriz** — exporta únicamente las 62 columnas intermedias; la demanda final no forma parte de la tecnología de producción.
6. **Sectores sin producción** — aplica la tecnología media del mismo sector entre países, excluyendo ceros.
7. **Hidrógeno** — reproduce la regla de WILIAM: `HYDROGEN_PRODUCTION` usa la tecnología media de `MANUFACTURE_CHEMICAL`.
8. **Validación** — compara el cálculo WILIAM con `Production.xlsx` usando diferencias absolutas, aplica el método validado a UNIZAR y comprueba cada coeficiente técnico individual frente al rango `[0,0.85]`.
9. **Exporta** → `Final_matrix_CT_1.xlsx`, `Tecnical coefficients final.xlsx` y el libro de comprobaciones.

### 2. Base Import Share (`Base_Import_Share/Base_Import_Shares_BIS.R`)

Calcula la proporción de importaciones de cada sector por país (import share).

1. **Carga** la matriz IO y filtra las filas de impuestos y valor añadido.
2. **Numerador** — para cada fila (país-sector), suma las columnas del mismo país excluyendo la propia fila (importaciones de los demás países para ese sector).
3. **Denominador** — uso total del producto: producción doméstica más importaciones.
4. **Import share** = importaciones / uso total (con denominador cero → 0).
5. **Transposición** — extrae submatrices por país y las apila para obtener el formato final.
6. **Exporta** → `Base_Import_Share_R.xlsx`

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
9. **Exporta** → `BISO.xlsx` y `Comprobaciones_BISO.xlsx`.

## Comprobaciones

- Todas las diferencias se calculan con valor absoluto.
- Los coeficientes técnicos de UNIZAR se comprueban frente al rango `[0,0.85]`; los shares de UNIZAR se comprueban frente al rango `[0,1]`.
- Los coeficientes técnicos de UNIZAR se comprueban individualmente; no se suman para este control.
- BIS y BISO de UNIZAR comprueban que los grupos con flujo sumen 1 y los grupos sin flujo sumen 0.
- El libro `Comprobacion_integral_MRIO.xlsx` resume la validación de TC, BIS y BISO, incluida la revisión manual de Austria.

## Datos de entrada

| Archivo | Contenido |
|---|---|
| `Data_origin_UNIZAR.RData` | Matriz input-output con 2 206 filas (35 países × ~63 sectores) |
| `mis_sectores.RData` | Vectores auxiliares: `sectores_finales`, `sectores_prioritarios`, `sectores_columna`, `Country` |

## Requisitos

- **R** ≥ 4.0
- Paquetes: `tidyverse`, `readxl`, `openxlsx`, `writexl`
