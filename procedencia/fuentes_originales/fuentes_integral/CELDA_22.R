if(Sys.getenv("DMEYF_INTEGRAL_BACKGROUND")=="1" && Sys.getenv("DMEYF_INTEGRAL_AUTORUN")!="1") {
 cat("Esta celda se ejecuta en el Rscript integral. Monitorear CELDA26.\n")
} else {
# Dejo anotado: CELDA 22 — Producción. Préstamos reincorporados ANTES de reentrenar; todos los modelos se guardan.
if(isTRUE(CFG$producir_agosto))for(nom in CFG$recetas) {
 nodo<-if(nom=='CONTROL_ORIGINAL')'CONTROL_P' else if(nom=='RANK_TODAS')'RANK_P' else 'PSEUDO_P'
 resultado<-paso_int(nodo,function() {
  hist<-HIST_INT[[nom]];rec<-hist$receta;hp<-hist$hp
  if(grepl('^ARTIFICIALES',nom)) {
   rec$planes$prod<-plan_pseudo_int(rec,TRUE)
   stopifnot(identical(as.integer(rec$planes$prod$meses),202103L:202108L))
  }
  campos<-campos_receta(rec,TRUE,TRUE)
  stopifnot(all(PRESTAMOS%in%campos),!any(c('clase_ternaria','clase01','prob_docente','etiqueta_artificial')%in%campos))
  od<-file.path(DIR_INT,'produccion',nom);dir.create(od,recursive=TRUE,showWarnings=FALSE)
  plan<-list(nombre=nom,hp=hp,semillas=SEEDS_INT,rep=rec$rep$id,modo='prob',prestamos=TRUE,
    meses=if(is.null(rec$planes))PARAM$train_final else rec$planes$prod$meses,
    target=if(grepl('^ARTIFICIALES',nom))'Mar-jun real + jul-ago artificial' else 'Mar-jun real')
  atomic(plan,file.path(od,'PLAN.rds'))
  xs<-lapply(SEEDS_INT,function(s) {
   z<-fit_score(rec,hp,s,TRUE,TRUE)
   # Dejo anotado: Cada seed queda recuperable antes de comenzar la siguiente, incluso con agosto sin etiqueta.
   log_('INTEGRAL AGOSTO',nom,s,z$id);z
  })
  en<-promediar_prob_int(xs,paste0(nom,'_AGOSTO_PROB',length(xs)))
  csv(en$probabilidades,file.path(od,'PROBABILIDADES_AGOSTO.csv'))
  reg<-rbindlist(lapply(xs,function(z)data.table(receta=nom,semilla=z$seed,modelo=z$id,
    archivo_modelo=z$modelo,scores=file.path(RAIZ,'scores',paste0(z$id,'.rds')),hp=toJSON(z$hp,auto_unbox=TRUE))))
  csv(reg,file.path(od,'REGISTRO_MODELOS.csv'))
  z<-list(nombre=nom,plan=plan,modelos=reg,ensemble=en$en)
  guardar_cierre_int(nom,z,'produccion');rm(xs,en);gc(FALSE);z
 })
 PROD_INT[[nom]]<-resultado
}
if(!isTRUE(CFG$producir_agosto))for(n in c('CONTROL_P','RANK_P','PSEUDO_P'))
 estado_int(n,'NO_ACTIVADO','Histórico solamente. Activar producir_agosto en CELDA 18 para producción.')
atomic(list(plan=CFG,recetas=PROD_INT,fecha=Sys.time()),file.path(DIR_INT,'produccion','CIERRE_PRODUCCION.rds'))

}
