if(Sys.getenv("DMEYF_INTEGRAL_BACKGROUND")=="1" && Sys.getenv("DMEYF_INTEGRAL_AUTORUN")!="1") {
 cat("Esta celda se ejecuta en el Rscript integral. Monitorear CELDA26.\n")
} else {
# Dejo anotado: CELDA 21 — BO propia por receta y confirmación de todas las semillas. No elijo semillas ganadoras.
for(nom in CFG$recetas) {
 nodo<-if(nom=='CONTROL_ORIGINAL')'CONTROL_H' else if(nom=='RANK_TODAS')'RANK_H' else 'PSEUDO_H'
 resultado<-paso_int(nodo,function() {
  rec<-REC_INT[[nom]];stopifnot(!is.null(rec))
  if(grepl('^ARTIFICIALES',nom)) {
   rec$planes<-list(hist=plan_pseudo_int(rec,FALSE),prod=NULL)
   stopifnot(identical(as.integer(rec$planes$hist$meses),202103L:202106L))
  }
  od<-file.path(DIR_INT,'historia',nom);dir.create(od,recursive=TRUE,showWarnings=FALSE)
  plan<-list(receta=nom,rep=rec$rep$id,estrategia=rec$estrategia,raw=RAW_HASH,
   semillas=SEEDS_INT,prestamos=FALSE,modo='prob',corte_BO=PARAM$corte,
   meses=if(is.null(rec$planes))PARAM$train else rec$planes$hist$meses,
   plan_Y=if(is.null(rec$planes))NULL else hash(rec$planes$hist),base=par_base(PARAM$semilla))
  atomic(plan,file.path(od,'PLAN.rds'))
  bo<-bayes(rec);hp<-bo$optimos[[1L]]
  atomic(bo,file.path(od,'BO.rds'))
  # Dejo anotado: BO inicial 4 + 8 propuestas inteligentes: no son doce aleatorias por defecto.
  xs<-lapply(SEEDS_INT,function(s)fit_score(rec,hp,s,FALSE,FALSE))
  en<-promediar_prob_int(xs,paste0(nom,'_JUNIO_PROB',length(xs)))
  resumen<-resumen_junio_int(xs,en$en,nom)
  csv(en$probabilidades,file.path(od,'PROBABILIDADES_JUNIO.csv'))
  z<-list(nombre=nom,receta=rec,bo=bo,hp=hp,modelos=vapply(xs,`[[`,character(1),'id'),ensemble=en$en,resumen=resumen,plan=plan)
  guardar_cierre_int(nom,z,'historia')
  rm(xs,en);gc(FALSE);z
 })
 HIST_INT[[nom]]<-resultado
}
comparacion_int<-rbindlist(lapply(HIST_INT,`[[`,'resumen'))
base_int<-comparacion_int[receta=='CONTROL_ORIGINAL',.(corte,base=ganancia_ensemble)]
comparacion_int<-merge(comparacion_int,base_int,by='corte',all.x=TRUE)
comparacion_int[,delta_vs_control:=ganancia_ensemble-base]
csv(comparacion_int,file.path(DIR_INT,'historia','COMPARACION_RECETAS.csv'))
for(nom in names(HIST_INT))if(nom!='CONTROL_ORIGINAL') {
 delta<-comparacion_int[receta==nom & corte==10500L,delta_vs_control]
 if(length(delta)==1L)estado_int(if(nom=='RANK_TODAS')'RANK_H' else 'PSEUDO_H',
   if(delta>0)'EXITO_HISTORICO' else 'SIN_MEJORA',paste(nom,'delta junio a 10500:',round(delta,2),'millones; no es garantía de agosto'))
}
print(comparacion_int)
atomic(list(plan=CFG,recetas=HIST_INT,fecha=Sys.time()),file.path(DIR_INT,'historia','CIERRE_HISTORIA.rds'))

# Dejo anotado: Ramas adicionales explícitas. No las hago pasar por pruebas ejecutadas si están apagadas.
if(isTRUE(CFG$extras))paso_int('EXTRAS',function() {
 rec<-rec_control_int;bo<-HIST_INT$CONTROL_ORIGINAL$bo;extras<-list()
 if(isTRUE(CFG$canaritos))for(k in c(1,2)) {
  sel<-canarios(rec,k);rr<-rec;rr$nombre<-paste0('CANARITOS_SD',k);rr$seleccion<-sel$campos
  extras[[rr$nombre]]<-rr
 }
 if(isTRUE(CFG$ablaciones))extras<-c(extras,ablaciones(rec,bo))
 # Dejo anotado: El 2x2 es una exploración sugerida en el diálogo, no la separación temporal mínima.
 if(isTRUE(CFG$factorial_mayo_lag2))for(mayo in c(FALSE,TRUE))for(lag2 in c(FALSE,TRUE)) {
  rr<-rec;rr$nombre<-paste0('FACTORIAL_MAYO_',mayo,'_LAG2_',lag2)
  if(!lag2)rr$seleccion<-rr$rep$campos[!grepl('_(delta_)?lag2$',rr$rep$campos)]
  meses<-if(mayo)202103L:202105L else PARAM$train;d<-leer_meses(rr$rep,meses)
  rr$planes<-list(hist=list(key=paste(d$numero_de_cliente,d$foto_mes),y=as.integer(d$clase_ternaria%in%c('BAJA+1','BAJA+2')),meses=meses),prod=NULL)
  extras[[rr$nombre]]<-rr;rm(d);gc(FALSE)
 }
 for(n in names(extras)) {
  rr<-extras[[n]];b<-bayes(rr);xs<-lapply(BASE_SEEDS,function(s)fit_score(rr,b$optimos[[1L]],s))
  en<-promediar_prob_int(xs,n);res<-resumen_junio_int(xs,en$en,n)
  guardar_cierre_int(n,list(bo=b,resumen=res,ensemble=en$en),'historia');rm(xs,en);gc(FALSE)
 }
 csv(data.table(receta=names(extras),estado='HISTORIA TERMINADA'),file.path(DIR_INT,'historia','EXTRAS.csv'))
 TRUE
})

}
