# Entreno la receta que elegí sin modificar el orden de las columnas.
if(SW$reproducir_final) {
 out <- file.path(RAIZ,'final');dir.create(out,recursive=TRUE,showWarnings=FALSE)
 tr <- leer_meses(REP,PARAM$train_final)
 va <- leer_meses(REP,202108L)
 campos_h <- campos_receta(REC_CONTROL,FALSE,FALSE)
 prestamos <- grep('^(cprestamos_personales|mprestamos_personales)($|_)',names(tr),value=TRUE)
 cierres <- grep('^(Visa_fultimo_cierre|Master_fultimo_cierre)($|_)',campos_h,value=TRUE)
 campos <- setdiff(unique(c(campos_h,prestamos)),cierres)
 referencia <- fread(file.path(CARPETA,'referencia','FEATURES.csv'))$campo
 stopifnot(length(campos_h)==752L,length(prestamos)==10L,length(cierres)==10L,
  length(campos)==752L,identical(campos,referencia),nrow(tr)==654066L,
  nrow(va)==164647L,!anyNA(tr$clase_ternaria),!anyDuplicated(va$numero_de_cliente))
 setDTthreads(1L)
 x <- data.matrix(tr[,..campos]);xv <- data.matrix(va[,..campos])
 x[!is.finite(x)] <- NA_real_;xv[!is.finite(xv)] <- NA_real_
 y <- as.integer(tr$clase_ternaria%in%c('BAJA+1','BAJA+2'))
 ids <- va$numero_de_cliente
 rm(tr,va);gc(FALSE)
 ds <- lgb.Dataset(x,label=y,params=list(max_bin=31L,
  feature_pre_filter=FALSE,num_threads=6L))
 rm(x);gc(FALSE)
 P <- matrix(NA_real_,length(ids),length(PARAM$semillas))
 for(j in seq_along(PARAM$semillas)) {
  s <- PARAM$semillas[j]
  pars <- modifyList(par_base(s),HP_FINAL)
  firma <- list(raw=RAW_HASH,representacion=REP$id,campos=campos,
   meses=PARAM$train_final,seed=s,pars=pars,versiones=VERSIONES)
  sp <- file.path(out,paste0('SCORES_',s,'.rds'))
  mp <- file.path(out,paste0('MODELO_',s,'.txt'))
  if(file.exists(sp)) {
   anterior <- readRDS(sp)
   stopifnot(identical(anterior$firma,firma),identical(anterior$ids,ids),file.exists(mp))
   pr <- anterior$prob
  } else {
   log_('ENTRENO FINAL',j,'/',length(PARAM$semillas),'semilla',s)
   m <- lgb.train(params=pars,data=ds,nrounds=HP_FINAL$num_iterations)
   pr <- predict(m,xv,params=list(num_threads=6L))
   stopifnot(length(pr)==length(ids),all(is.finite(pr)))
   lgb.save(m,mp)
   atomic(list(firma=firma,ids=ids,prob=pr),sp)
   rm(m);gc(FALSE)
  }
  P[,j] <- pr
 }
 # Promedio las probabilidades antes de decidir a quién contacto.
 prob <- rowMeans(P)
 ix <- order(-prob,ids)[seq_len(CORTE_FINAL)]
 elegido <- ids[ix]
 stopifnot(length(elegido)==10375L,!anyDuplicated(elegido))
 archivo <- file.path(out,'SINCIERRES_PROB10_K10375.csv')
 writeLines(format(elegido,scientific=FALSE,trim=TRUE),archivo)
 sha <- digest(file=archivo,algo='sha256')
 fwrite(data.table(numero_de_cliente=ids,prob=prob),file.path(out,'PROBABILIDADES_AGOSTO.csv'))
 atomic(list(ids=ids,prob=prob,campos=campos,hp=HP_FINAL,
  semillas=PARAM$semillas,sha256=sha),file.path(out,'ENSAMBLE_FINAL.rds'))
 # Dejo anotado: Detengo mi entrega si no reproduje exactamente el archivo enviado.
 if(sha!=SHA_FINAL)stop('CSV distinto al entregado. SHA256 obtenido: ',sha,
  '. No debo subir esta reproducción sin revisar la diferencia.')
 writeLines(c('REPRODUCCION EXACTA: OK',paste('SHA256:',sha),
  paste('Archivo:',archivo)),file.path(out,'VERIFICACION_FINAL.txt'))
 log_('REPRODUCCION EXACTA VERIFICADA',sha)
 rm(ds,xv,P);gc(FALSE)
}
