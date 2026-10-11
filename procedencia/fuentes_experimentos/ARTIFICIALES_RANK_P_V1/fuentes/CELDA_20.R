if(Sys.getenv("DMEYF_INTEGRAL_BACKGROUND")=="1" && Sys.getenv("DMEYF_INTEGRAL_AUTORUN")!="1") {
 cat("Esta celda se ejecuta en el Rscript integral. Monitorear CELDA26.\n")
} else {
# Dejo anotado: CELDA 20 — Auditoría, fuentes, representación completa y ramas pendientes.
paso_int('AUDITORIA',function() {
 d<-cargar_base()
 stopifnot(nrow(d[foto_mes==202103L])>0L,all(is.na(d[foto_mes==202105L,ccajas_depositos])),
   !anyNA(d[foto_mes%in%PARAM$train_final,clase_ternaria]))
 # Dejo anotado: NA se conserva como señal. No reemplazo estados 9 por NA ni invento etiquetas reales en julio/agosto.
 clases<-d[,.N,by=.(foto_mes,clase_ternaria)]
 csv(clases,file.path(DIR_INT,'auditoria','CLASES_POR_MES.csv'))
 vars<-setdiff(names(d),c('numero_de_cliente','foto_mes','clase_ternaria'))
 contingencia<-rbindlist(lapply(vars,function(v) {
  x<-d[[v]];tipo<-ifelse(is.na(x),'NA',ifelse(x==0,'CERO',ifelse(x<0,'NEGATIVO','POSITIVO')))
  z<-data.table(foto_mes=d$foto_mes,clase_ternaria=d$clase_ternaria,tipo=tipo)
  z[foto_mes<=202106L,.N,by=.(foto_mes,clase_ternaria,tipo)][,variable:=v]
 }))
 csv(contingencia,file.path(DIR_INT,'auditoria','CONTINGENCIA_CERO_SIGNO_NA.csv'))
 dd<-readRDS(file.path(RAIZ,'auditoria',paste0(hash(list(componente='drift_todos_v1',raw=RAW_HASH)),'.rds')))$valor
 csv(dd,file.path(DIR_INT,'auditoria','DISTRIBUCIONES_TODAS_VARIABLES.csv'))
 if(nrow(dd)) {
  drift<-dcast(dd,variable~foto_mes,value.var=c('missing','ceros','mediana','p25','p75'))
  csv(drift,file.path(DIR_INT,'auditoria','DRIFT_COMPARACION_MENSUAL.csv'))
 }
 catvars<-attr(d,'categoricas')
 requisitos<-data.table(requisito=c('DQ ccajas_depositos mayo=NA','Target BAJA+1/BAJA+2','Training sin undersampling',
 'Lag calendario exacto 1 y 2','Deltas 1 y 2 en campos numéricos','Conservar primeros meses con NA',
 'Préstamos y sus históricos fuera de BO','Préstamos reincorporados al REENTRENAR producción',
 'ID y mes fuera de rankings','Media simple de probabilidades por ID'),
 estado='IMPLEMENTADO')
 csv(requisitos,file.path(DIR_INT,'auditoria','REQUISITOS_MINIMOS.csv'))
 fuente<-data.table(material=c('Clase anterior Pabellon 7','Tutoría Avenida del Libertador 21','Clase nueva audios 10,11,13','Pizarrones IMG_1036 a IMG_1047','z494 original','z495 GustavoPasquini'),
 alcance=c('Etiquetas artificiales Y; mecanismo histórico y producción, 02:19–19:47',
 'DQ, retirar préstamos en BO/restaurar al entrenar; FF por nodo .20; registro y cortes',
 'Ranking luego de lags; 20 semillas; media de probabilidades; lotes mismo corte; código reproducible',
 'Flujo, contingencias cero/NA, veinte probabilidades y media, separación temporal',
 'Fuente original preservada en fuentes; motor mlrMBO',
 'Referencia revisada; su target incluye BAJA+1 y BAJA+2; no afirmo copiar experimentos no presentes'))
 csv(fuente,file.path(DIR_INT,'auditoria','FUENTES_Y_ALCANCE.csv'))
 writeLines(c('Decisiones explícitas que NO fueron números dictados por el profesor:',
 paste('Semillas nuevas: primos consecutivos después de las diez previas. Total:',CFG$n_semillas),
 paste('Pseudoetiquetas: regla',CFG$pseudo_regla,'; top_k:',CFG$pseudo_top_k,'; seed:',CFG$pseudo_seed),
 'La transcripción explica pintar positivos según la predicción, pero no explicita umbral ni cantidad. No atribuyo top_k=10500 al profesor ni a Germán.',
 paste('Presupuesto BO:',CFG$bo_evaluaciones,'evaluaciones:',CFG$bo_iniciales,'iniciales y el resto por EI.'),
 'Los resultados reales de junio jamás se usan para crear etiquetas artificiales de mayo/junio.',
 'Junio sí se usa para evaluar y seleccionar: es evaluación retrospectiva, no test independiente.',
 'Los resultados públicos son una muestra; no garantizan desempeño privado.',
 'El conjunto final es una lista de IDs de agosto; los lotes no identifican al mejor CSV individual.'),
 file.path(DIR_INT,'auditoria','DECISIONES_NO_DICTADAS.txt'))
 rm(d);gc(FALSE)
 if(any(c('RANK_TODAS','ARTIFICIALES_RANK')%in%CFG$recetas)) {
  rep<-rank_todas_int(rec_control_int$rep)
  rec<-rec_control_int;rec$rep<-rep;rec$nombre<-'RANK_TODAS_POST_LAG2'
  REC_INT$RANK_TODAS<<-rec
 }
 if('ARTIFICIALES_ORIGINAL'%in%CFG$recetas) {
  rec<-rec_control_int;rec$nombre<-'ARTIFICIALES_ORIGINAL_LAG2';rec$estrategia<-'ARTIFICIALES_4H_6P'
  REC_INT$ARTIFICIALES_ORIGINAL<<-rec
 }
 if('ARTIFICIALES_RANK'%in%CFG$recetas) {
  rec<-REC_INT$RANK_TODAS;rec$nombre<-'ARTIFICIALES_RANK_LAG2';rec$estrategia<-'ARTIFICIALES_4H_6P'
  REC_INT$ARTIFICIALES_RANK<<-rec
 }
 csv(rbindlist(lapply(names(REC_INT),function(n)data.table(receta=n,representacion=REC_INT[[n]]$rep$id,
   N_features_BO=length(campos_receta(REC_INT[[n]],FALSE,FALSE)),
   N_features_produccion=length(campos_receta(REC_INT[[n]],TRUE,TRUE))))),file.path(DIR_INT,'planes','RECETAS.csv'))
 for(pair in list(c('RANK_TODAS','RANK_H','RANK_P'),c('ARTIFICIALES_ORIGINAL','PSEUDO_H','PSEUDO_P')))
  if(!pair[1L]%in%CFG$recetas)for(n in pair[-1L])estado_int(n,'NO_ACTIVADO')
 if(!isTRUE(CFG$extras))estado_int('EXTRAS','NO_ACTIVADO','Canaritos, ablaciones, filtros y 2x2 permanecen disponibles; no son la ruta mínima de esta clase.')
 TRUE
})

}
