# Mi entrega — Competencia DMEyF

Soy Nicky Baranek. Mi predicción elegida es `SINCIERRES_PROB10_K10375.csv`.

## Mi secuencia de ejecución

1. Descomprimo esta carpeta y mantengo su estructura. Ubico el dataset crudo en `datos/competencia_01_crudo.csv`, o defino `DMEYF_DATOS` con su ruta absoluta.
2. Abro `DMEyF_Nicky_Entrega_Final.ipynb` en esa carpeta. Ejecuto la sección 1 con Python. Cambio al kernel R y ejecuto la sección 2 en orden: configuración, funciones, preparación, entrenamiento y ensamble, experimentos y cortes.
3. Dejo `reproducir_final=TRUE` y todos los demás switches en `FALSE`. Mantengo seis hilos en LightGBM. No activo los experimentos descartados para generar mi entrega.
4. Obtengo el archivo en `salida/final/SINCIERRES_PROB10_K10375.csv`. Mi código verifica el SHA256 y se detiene si no coincide con el archivo entregado.

También puedo ejecutar la misma ruta con R, sin abrir Jupyter:

```bash
DMEYF_DATOS=/ruta/competencia_01_crudo.csv Rscript --vanilla REPRODUCIR_FINAL.R
```

Ejecuto ese comando desde la carpeta de entrega. Si necesito otra ubicación para las salidas, defino `DMEYF_SALIDA`; para cambiar la carpeta del código, defino `DMEYF_ENTREGA`.

## Mi modelo elegido

- Reconstruyo el target por presencia mensual: BAJA+1/BAJA+2 = 1 y CONTINUA = 0.
- Corrijo `ccajas_depositos` de 202105 asignando NA.
- Calculo lags calendario de orden 1 y 2 y sus deltas; mantengo ausencias mensuales como NA.
- Mantengo el orden exacto de las columnas. Incluyo `numero_de_cliente` y `foto_mes` como predictores, igual que en la corrida original.
- Durante la BO de origen retiro préstamos personales y sus lags/deltas. En producción los restauro y retiro la familia completa de `Visa_fultimo_cierre` y `Master_fultimo_cierre`: diez columnas.
- Conservo `Visa_status`, `Master_status` y sus lags/deltas. No agrego transiciones adicionales, rankings, flags zero, tendencias ni etiquetas artificiales al modelo final.
- Entreno 202103, 202104, 202105 y 202106 sin undersampling: 654.066 filas y 752 predictores. Predigo 202108: 164.647 clientes.
- Uso 1907 iteraciones, 77 hojas y Hessian mínimo 2,83699883991516. Conservo la métrica `average_precision` del entrenamiento que efectivamente generó la entrega. No uso early stopping.
- Entreno las diez semillas 100613, 100267, 101611, 102481, 103651, 104729, 105019, 105613, 106013 y 106619.
- Promedio sus probabilidades con pesos iguales, ordeno de mayor a menor y desempato por ID creciente. Selecciono 10.375 IDs y escribo una columna, sin encabezado, con salto de línea final.

Mi SHA256 esperado es:

`7715cbb9568462c2a1f9b0534528fbce3f77c54cd43a255c59476dc9a37b5caa`

## Mi optimización de origen

Incluyo `BO_Origen.ipynb`, el código del motor y los archivos FIRMA/HISTORIAL/TERMINADO de la BO que produjo los HP usados. La BO ORIGINAL_LAG2 es `52fc1b5d908fb495ce06d1ed3038defbaf9a2609d2ea0bc80904d2ba8b58c6ed`.

Entrené marzo–abril y evalué junio con ganancia en K11500, cuatro puntos iniciales y ocho propuestas de mejora esperada. Elijo los HP del historial efectivo, no de una búsqueda posterior. Si quiero volver a correr esa búsqueda, ejecuto `BO_Origen.ipynb` en una salida vacía; esa notebook activa `bo_origen=TRUE` y apaga producción.

No uso la BO posterior de Sin cierres, de 1130 iteraciones: su ensayo público dio 98,89 M en K10500 y la descarté. La conservo como experimento.

## Mi ambiente

Reproduzco en Linux x86_64 con R 4.6.1, LightGBM 4.7.0.99 y data.table 1.18.6.1. El código verifica esas versiones antes de entrenar. No actualizo LightGBM durante la réplica ni lo reemplazo silenciosamente por 4.7.0.

Para la ruta final necesito `data.table`, `lightgbm`, `digest`, `jsonlite` y `filelock`. Para las BO necesito además `mlrMBO` 1.1.6, `DiceKriging` 1.6.1, `mlr`, `ParamHelpers` y `smoof`. Uso Python para la carga y R para preparar, entrenar y predecir. No necesito servicios externos, credenciales de GCP ni checkpoints de otra VM para ejecutar la ruta final.

Guardo `sessionInfo()` en la salida. Para comprobar sintaxis en mi ambiente ejecuto:

```bash
Rscript --vanilla VALIDAR_SINTAXIS.R
```

## Mis experimentos

Conservo los resultados originales en `evidencia/`, los historiales de BO en `procedencia_bo/` y las fuentes anteriores en `procedencia/`. Documenté mis decisiones en `REGISTRO_EXPERIMENTOS.csv`.

Mis switches incluyen control, rankings completos, ranking de montos, etiquetas artificiales originales y con rankings, target BAJA+2, tarjetas juntas, Visa sola, Master sola, vinculación, Sin cierres histórico, ranking con zero/tendencias, BO posterior, mezcla control/ranking, ensambles de cinco, comparaciones y cortes históricos.

Si activo una variante, construyo sus controles en `salida/laboratorio` y no reemplazo mi CSV final. Para comparaciones de Visa activo `visa_comparaciones`; para las de Sin cierres activo `sin_cierres_comparaciones`. Esas opciones activan sus históricos necesarios. Para los cortes de junio activo `cortes_historicos`, que activa Sin cierres histórico. No ejecuto todas las variantes para entregar: consumen muchas horas.

En `rank_zero_tend3` corrijo el parámetro anidado de Dataset que había emitido un warning; conservo la receta y un hilo. Distingo esta corrección de sintaxis del resultado histórico registrado.

Mis grupos ENS5 comparten modelos; no los interpreto como veinte pruebas independientes. Junio fue reutilizado para desarrollar y seleccionar. El público no permite conocer la curva privada.

## Mi estado de verificación

Verifiqué la estructura de las notebooks, la sintaxis Python, la sintaxis R y la procedencia de los HP. La reproducción desde el crudo queda confirmada únicamente cuando obtengo `salida/final/VERIFICACION_FINAL.txt` con el SHA256 esperado. Hasta entonces no presento la revisión estática como una ejecución completa.

El archivo de `referencia/` permite contrastar la salida y su hash; no lo utilizo para generar las probabilidades ni para sustituir el entrenamiento.


## Mi reproducción confirmada

Verifiqué la ejecución desde el crudo el 11/10/2026 a las 22:45:49 de Buenos Aires. Los diez modelos generaron exactamente el CSV elegido. Dejo la constancia en VERIFICACION_FINAL.txt.
