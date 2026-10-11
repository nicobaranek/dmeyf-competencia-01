# Dejo anotado: CELDA 03 — Fijo mi plan y los interruptores.
PARAM <- list(
 semilla=100613L,
 semillas=c(100613L,100267L,101611L,102481L,103651L,104729L,105019L,105613L,106013L,106619L),
 threads=6L,
 train=c(202103L,202104L), train_final=c(202103L,202104L,202105L,202106L),
 valid=202106L, future=202108L, corte=11500L,
 orden_lags=2L, incluir_id_mes=TRUE,
 bo_evaluaciones=12L, bo_iniciales=4L,
 bounds=list(iter=c(512L,4096L),leaves=c(8L,256L),logh=c(-3,1)),
 top_configuraciones=1L,
 rankings=FALSE, canaritos=FALSE, ablaciones=FALSE,
 negativos_nuevo=FALSE, pseudoetiquetas=FALSE,
 # Uso 0.5 como hipótesis de este experimento, no como umbral óptimo ni como cita del profesor.
 pseudo_umbral=0.50,
 hp_seleccion=list(num_iterations=2180L,num_leaves=30L,min_sum_hessian_in_leaf=.1141634103),
 ablaciones_top=3L,
 producir_agosto=FALSE, control_produccion_sin_prestamos=TRUE,
 modo_ensemble='raw'
)
stopifnot(PARAM$bo_evaluaciones>PARAM$bo_iniciales,length(unique(PARAM$semillas))>=10L,
 PARAM$orden_lags%in%c(0L,1L,2L),PARAM$corte>=8000L,PARAM$corte<=15000L,
 PARAM$modo_ensemble%in%c('raw','prob'))
cat('Primera receta: DQ, historia',PARAM$orden_lags,'; BO',PARAM$bo_evaluaciones,'; semillas',length(PARAM$semillas),'\n')
# Fijo explícitamente las variables de monto existentes en el artefacto auditado.
PARAM$rank_campos <- c("Master_madelantodolares","Master_madelantopesos","Master_mconsumosdolares","Master_mconsumospesos","Master_mconsumototal","Master_mfinanciacion_limite","Master_mlimitecompra","Master_mpagado","Master_mpagominimo","Master_mpagosdolares","Master_mpagospesos","Master_msaldodolares","Master_msaldopesos","Master_msaldototal","Visa_madelantodolares","Visa_madelantopesos","Visa_mconsumosdolares","Visa_mconsumospesos","Visa_mconsumototal","Visa_mfinanciacion_limite","Visa_mlimitecompra","Visa_mpagado","Visa_mpagominimo","Visa_mpagosdolares","Visa_mpagospesos","Visa_msaldodolares","Visa_msaldopesos","Visa_msaldototal","mactivos_margen","matm","matm_other","mautoservicio","mcaja_ahorro","mcaja_ahorro_adicional","mcaja_ahorro_dolares","mcajeros_propios_descuentos","mcheques_depositados","mcheques_depositados_rechazados","mcheques_emitidos","mcheques_emitidos_rechazados","mcomisiones","mcomisiones_mantenimiento","mcomisiones_otras","mcuenta_corriente","mcuenta_corriente_adicional","mcuenta_debitos_automaticos","mcuentas_saldo","mextraccion_autoservicio","mforex_buy","mforex_sell","minversion1_dolares","minversion1_pesos","minversion2","mpagodeservicios","mpagomiscuentas","mpasivos_margen","mpayroll","mpayroll2","mplazo_fijo_dolares","mplazo_fijo_pesos","mprestamos_hipotecarios","mprestamos_personales","mprestamos_prendarios","mrentabilidad","mrentabilidad_annual","mtarjeta_master_consumo","mtarjeta_master_descuentos","mtarjeta_visa_consumo","mtarjeta_visa_descuentos","mtransferencias_emitidas","mtransferencias_recibidas","mttarjeta_master_debitos_automaticos","mttarjeta_visa_debitos_automaticos")

# Conservo los resultados previos antes de actualizar mis resúmenes.
raiz_previa <- Sys.getenv('DMEYF_RAIZ')
baseline_path <- file.path(raiz_previa,'resultados','CONF_ORIGINAL_LAG2_1.csv')
stopifnot(file.exists(baseline_path))
BASELINE_RANK <- data.table::fread(baseline_path)
stopifnot(nrow(BASELINE_RANK)==1L,BASELINE_RANK$receta=='ORIGINAL_LAG2')
respaldo <- file.path(raiz_previa,'respaldo_antes_rank_montos_20261008')
if(!dir.exists(respaldo)) {
 dir.create(respaldo,recursive=TRUE)
 ff <- list.files(file.path(raiz_previa,'resultados'),full.names=TRUE)
 stopifnot(all(file.copy(ff,respaldo,recursive=TRUE)))
}

# Mantengo todas las ramas; activo sólo el experimento actual.
PARAM$ejecutar_original <- TRUE
PARAM$mezclar_configuraciones <- FALSE

# Activo sólo la confirmación HP2 en mi celda 14.
PARAM$solo_hp2 <- TRUE
