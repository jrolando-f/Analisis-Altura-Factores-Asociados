# ANÁLISIS DE ALTURA Y FACTORES ASOCIADOS

## Pregunta de investigación
¿Qué factores influyen en la altura de las personas? ¿Cómo cambia a lo largo de los años?

La estatura humana es un rasgo parcialmente hereditario. Sin embargo, queremos analizar si existen factores no genéticos que influyan en la distribución de la estatura entre la población. Por ello, formulamos la hipótesis de que las variaciones de estatura en el mundo indican no sólo diferencias genéticas, sino también diferencias generales en el nivel de vida.

## Posibles factores
Los factores que podrían influir en la altura de las personas incluyen:
- **PBI per cápita**: Indicador del nivel económico de un país.
- **Alimentación**: Específicamente, el consumo de proteína animal.
- **Tasa de mortalidad de niños menores a 5 años**: Indicador de condiciones de salud y nutrición en la infancia.
- **Expectativa de vida**: Reflejo general de la calidad de vida.

> **Nota**: Aunque la genética es un factor determinante en la altura, este análisis se enfoca en factores no genéticos.

## Datos utilizados
Se utilizaron datasets de las siguientes fuentes:
- [Our World in Data](https://ourworldindata.org)
- [World Bank DataBank](https://databank.worldbank.org)

Las variables principales del análisis incluyen:
- **Altura** (cm)
- **Consumo de proteínas** (g/persona/día)
- **Continente** (categoría)
- **Expectativa de vida** (años)
- **Tasa de mortalidad infantil** (número de muertes por cada 1,000 nacimientos vivos)

## Análisis realizado
Este proyecto incluye:
1. **Análisis descriptivo**: Explorar la relación entre altura y los factores mencionados.
2. **Modelos estadísticos**: Evaluar la contribución de cada factor en la variación de la altura entre diferentes regiones y a lo largo del tiempo.
3. **Visualización de datos**: Gráficos para ilustrar patrones y tendencias.

## Requisitos del sistema
Este proyecto fue desarrollado en R. Para reproducir el análisis, se necesitan las siguientes bibliotecas:
- `tidyverse`
- `ggplot2`
- `dplyr`
- `readr`
  
