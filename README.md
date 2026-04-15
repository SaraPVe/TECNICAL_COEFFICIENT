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
2. **Suma por columna** — calcula el total de producción por cada columna (sector-país).
3. **Suma por sector** — agrega las filas por sector (`group_by(Sector)`).
4. **División** — divide cada columna entre su total de producción (`sweep`), obteniendo los coeficientes técnicos.
5. **Transposición** — pivota el resultado a formato largo (país, sector, valores) usando `pivot_longer`.
6. **Ordenación** — ordena por país y sector según un orden predefinido (35 países × 62 sectores).
7. **Exporta** → `Tecnical coefficients final.xlsx`

### 2. Base Import Share (`Base_Import_Share/Base_Import_Shares_BIS.R`)

Calcula la proporción de importaciones de cada sector por país (import share).

1. **Carga** la matriz IO y filtra las filas de impuestos y valor añadido.
2. **Numerador** — para cada fila (país-sector), suma las columnas del mismo país excluyendo la propia fila (importaciones de los demás países para ese sector).
3. **Denominador** — igual pero incluyendo todas las filas del mismo sector (total de importaciones).
4. **Import share** = numerador / denominador (con NAs → 0).
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
7. **Exporta** → `BISO.xlsx`

## Datos de entrada

| Archivo | Contenido |
|---|---|
| `Data_origin_UNIZAR.RData` | Matriz input-output con 2 206 filas (35 países × ~63 sectores) |
| `mis_sectores.RData` | Vectores auxiliares: `sectores_finales`, `sectores_prioritarios`, `sectores_columna`, `Country` |

## Requisitos

- **R** ≥ 4.0
- Paquetes: `tidyverse`, `readxl`, `openxlsx`, `writexl`