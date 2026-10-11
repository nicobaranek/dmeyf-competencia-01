if(Sys.getenv("DMEYF_INTEGRAL_BACKGROUND")=="1" && Sys.getenv("DMEYF_INTEGRAL_AUTORUN")!="1") {
 cat("Esta celda se ejecuta en el Rscript integral. Monitorear CELDA26.\n")
} else {
# Dejo anotado: CELDA 25 — Cierre de cálculo, registro completo y código reproducible; no equivale a envío final.
regs<-list.files(file.path(RAIZ,'modelos'),pattern='_registro[.]csv$',full.names=TRUE)
registro_total<-rbindlist(lapply(regs,fread),fill=TRUE)
csv(registro_total,file.path(DIR_INT,'entrega','REGISTRO_MODELOS_COMPLETO.csv'))
activadas<-data.table(prueba=c('control_original_prob20','rank_todas_despues_lags','artificiales_original',
 'artificiales_rank','canaritos','ablaciones','negativos_dudosos','target_baja2','mezcla_HP1_HP2','factorial_mayo_lag2','AutoGluon','clustering'),
 estado=c('CONTROL_ORIGINAL'%in%CFG$recetas,'RANK_TODAS'%in%CFG$recetas,'ARTIFICIALES_ORIGINAL'%in%CFG$recetas,
 'ARTIFICIALES_RANK'%in%CFG$recetas,isTRUE(CFG$extras)&&isTRUE(CFG$canaritos),isTRUE(CFG$extras)&&isTRUE(CFG$ablaciones),
 FALSE,FALSE,FALSE,isTRUE(CFG$extras)&&isTRUE(CFG$factorial_mayo_lag2),FALSE,FALSE),
 observacion=c('Media simple de todas las probabilidades; conserva RAW anterior',
 'Nuevo: todas las variables originales/lag/delta, ranking después de historia',
 'Obligatorio por instrucción de Nicky; BO 4 meses y producción 6',
 'Interacción disponible; opcional por presupuesto',
 'Última clase lo posterga por memoria a competencia 2; ramas SD1/SD2 conservadas',
 'Cribado con HP fijos y luego BO propia; no es mínimo de esta clase',
 'Ya probado en otra notebook; se conserva evidencia, no repito aquí',
 'CELDA17 conserva experimento terminado: 458.15 vs 465.85 en junio a 11500',
 'CELDAS14/15 preservadas; RAW junio mejoró, público 96.9925. No lo repito',
 'Exploración, no reemplaza el control temporal marzo-abril / junio',
 'Propuesta exploratoria, sin código ni benchmark definitivo del docente; no activa',
 'Hipótesis de reordenamiento, no parte de la receta mínima; requiere asignación por ID y validación'))
csv(activadas,file.path(DIR_INT,'entrega','PRUEBAS_ACTIVADAS_Y_ANTECEDENTES.csv'))
atomic(list(cfg=CFG,raw=RAW_HASH,historia=lapply(HIST_INT,function(z)list(plan=z$plan,hp=z$hp,modelos=z$modelos,resumen=z$resumen)),
 produccion=lapply(PROD_INT,function(z)list(plan=z$plan,modelos=z$modelos,ensemble=z$ensemble$id)),fecha=Sys.time()),
 file.path(DIR_INT,'CIERRE_CALCULO_INTEGRAL.rds'))
writeLines(c('CORRIDA INTEGRAL — CLASE 08/10/2026',paste('Finalización cálculo:',Sys.time()),paste('Raíz:',DIR_INT),
 'Todos los registros de primer mes se conservan, aunque no tengan lag.',
 'Sin undersampling. BO sin préstamos/históricos, producción REENTRENADA con préstamos/históricos.',
 'Probabilidades de cada modelo, metadatos, HP y semillas están guardados.',
 'Artificios en Y, nunca en X. La regla numérica queda explícita como hipótesis propia.',
 'La nueva representación de rankings reemplaza todas las variables originales y sus lags/deltas después de calcularlos.',
 'Los cortes se comparan con lotes homogéneos y ensambles; no se busca la mejor semilla del público.',
 'Julio no tiene H2 completo; agosto no posee target real. No reconstruyo etiquetas reales para evaluar agosto.',
 'Cierre de cálculo: NO significa entrega final ni éxito en el privado.',
 paste('Tabla pública a completar:',file.path(DIR_INT,'publico','RESPUESTAS_PARA_COMPLETAR.csv')),
 paste('Mapa:',file.path(DIR_INT,'MAPA_EXPERIMENTOS.html'))),file.path(DIR_INT,'entrega','RESULTADOS_PARA_CHAT.txt'))
writeLines(c('# Reproducción',
 'Abrir la notebook maestra. En Google Cloud ejecutar CELDA01, CELDA02 y CELDA18.',
 'CELDA18 exporta las celdas R, verifica sintaxis y lanza Rscript independiente del navegador.',
 'Conservar PLAN_CONGELADO.json, hashes, versiones, fuente z494 y esta notebook.',
 'Publicar el código en el GitHub indicado por la materia; no subir datos privados ni credenciales.',
 'El crudo debe estar en la ruta DMEYF_DATOS. Los checkpoints permiten recuperar trabajo compatible.',
 'Los archivos grandes modelos/scores se mantienen fuera del ZIP pequeño de resultados.'),
 file.path(DIR_INT,'entrega','README_REPRODUCCION.txt'))
mapa_int()
writeLines(capture.output(sessionInfo()),file.path(DIR_INT,'entrega','SESSION_INFO.txt'))
if(nzchar(Sys.which('zip'))) {
 previo<-getwd();setwd(DIR_INT)
 archivos<-c(list.files('auditoria',full.names=TRUE),list.files('entrega',full.names=TRUE),
   list.files('planes',full.names=TRUE),list.files('publico',full.names=TRUE),
   list.files('historia',recursive=TRUE,full.names=TRUE),
   'ESTADO_EXPERIMENTOS.csv','MAPA_EXPERIMENTOS.html','PLAN_CONGELADO.json','CIERRE_CALCULO_INTEGRAL.rds')
 # No incluye los ZIP de candidatos dentro del ZIP resumen ni matrices gigantes.
 archivos<-archivos[!grepl('[.]zip$|PROBABILIDADES',archivos)&file.exists(archivos)&!dir.exists(archivos)]
 zz<-tryCatch(utils::zip(file.path(DIR_INT,'RESUMEN_INTEGRAL.zip'),archivos,flags='-q9X'),finally=setwd(previo))
 stopifnot(zz==0L)
}
log_('CIERRE CALCULO INTEGRAL',DIR_INT)
cat('Cálculo terminado. Los lotes y ensambles están en:',file.path(DIR_INT,'lotes'),'\n')

}
