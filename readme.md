# Datos de género del Sistema de Información Municipal (Sinim)

Obtención y procesamiento de datos en lote desde el [Sistema de Información Municipal (SINIM)](https://datos.sinim.gov.cl) de la Subsecretaría de Desarrollo Regional y Administrativo (Subdere).

Se obtienen los datos del área de Género almacenados en Sinim.

## Datos
Datos disponibles en formato `csv` y `parquet` en la carpeta `datos/`, desde 2019 a 2023.

## Variables

|Variable                                                                                        |medida |genero  |calificacion   |tipo            |
|:-----------------------------------------------------------------------------------------------|:------|:-------|:--------------|:---------------|
|Porcentaje de mujeres funcionarias municipales                                                  |%      |Mujeres |Otros          |Funcionarios/as |
|Porcentaje de mujeres profesionales sobre el total de funcionarias municipales Mujeres          |%      |Total   |Profesional    |Funcionarios/as |
|Porcentaje de mujeres en escalafón directivo sobre total de funcionarias municipales mujeres.   |%      |Total   |Otros          |Directivos/as   |
|Número de hombres planta                                                                        |N      |Hombres |Otros          |Planta          |
|Número de mujeres planta                                                                        |N      |Mujeres |Otros          |Planta          |
|Número de hombres a contrata                                                                    |N      |Hombres |Otros          |Contrata        |
|Número de mujeres a contrata                                                                    |N      |Mujeres |Otros          |Contrata        |
|Mujeres pertenecientes a escalafón directivo y profesional (planta y contrata)                  |N      |Mujeres |Profesional    |Directivos/as   |
|Nº de hombres a honorarios (Subtítulo 21.03.000)                                                |N      |Hombres |Otros          |Honorarios      |
|Nº de mujeres a honorarios, destinadas a programas comunitarios (Subtítulo 21.04.004) (1 más 2) |N      |Mujeres |Otros          |Honorarios      |
|Nº de mujeres a honorarios (Subtítulo 21.03.000)                                                |N      |Mujeres |Otros          |Honorarios      |
|Nº de hombres a honorarios, destinados a programas comunitarios (Subtítulo 21.04.004) (1 más 2) |N      |Hombres |Otros          |Honorarios      |
|Nº de mujeres profesionales a contrata                                                          |N      |Mujeres |Profesional    |Contrata        |
|Nº de mujeres no profesionales a contrata (sin título profesional)                              |N      |Mujeres |No profesional |Contrata        |
|Nº de hombres profesionales a contrata                                                          |N      |Hombres |Profesional    |Contrata        |
|Nº de hombres no profesionales a contrata (sin título profesional)                              |N      |Hombres |No profesional |Contrata        |
|Nº de mujeres profesionales de planta                                                           |N      |Mujeres |Profesional    |Planta          |
|Nº de hombres profesionales de planta                                                           |N      |Hombres |Profesional    |Planta          |
|Nº de mujeres no profesionales de planta (sin título profesional)                               |N      |Mujeres |No profesional |Planta          |
|Nº de hombres no profesionales de planta (sin título profesional)                               |N      |Hombres |No profesional |Planta          |
|Número total de hombres y mujeres a honorarios (Subtítulo 215.21.03                             |N      |Total   |Otros          |Honorarios      |
|Número total de hombres y mujeres, destinado a programas comunitarios (Subtítulo 215.21.04.004) |N      |Total   |Otros          |Honorarios      |

### Fuentes
- [Sistema de Información Municipal (SINIM)](https://datos.sinim.gov.cl), Subsecretaría de Desarrollo Regional y Administrativo (Subdere)
- Código basado en el repositorio [`sinim_datos_comunales`](https://github.com/bastianolea/sinim_datos_comunales)