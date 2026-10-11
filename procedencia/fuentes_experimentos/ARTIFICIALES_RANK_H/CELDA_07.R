# Dejo anotado: CELDA 07 — Creo las recetas activas. No agrego las columnas auxiliares como predictores.
RECETAS <- list()
for(nom in names(REP)) {
 rec <- list(nombre=paste0(nom,'_LAG',PARAM$orden_lags),rep=REP[[nom]],estrategia='COMPLETO',umbral=NULL,seleccion=NULL,excluir=character(),planes=NULL)
 RECETAS[[rec$nombre]] <- rec
 if(PARAM$canaritos) for(corte_sd in c(1,2)) {
  sel <- canarios(rec,corte_sd); rr <- rec; rr$seleccion <- sel$campos
  rr$nombre <- paste0(rec$nombre,'_CAN',corte_sd); RECETAS[[rr$nombre]] <- rr
 }
}
csv(rbindlist(lapply(RECETAS,function(r)data.table(receta=r$nombre,features_BO=length(campos_receta(r)),features_produccion=length(campos_receta(r,TRUE,TRUE)),estrategia=r$estrategia))),file.path(RAIZ,'resultados','RECETAS_ACTIVAS.csv'))
print(names(RECETAS))
