if(Sys.getenv("DMEYF_INTEGRAL_BACKGROUND")=="1" && Sys.getenv("DMEYF_INTEGRAL_AUTORUN")!="1") {
 cat("Esta celda se ejecuta en el Rscript integral. Monitorear CELDA26.\n")
} else {
# Dejo anotado: CELDA 23 — Lotes homogéneos de 20 CSV individuales y un CSV del ensamble completo.
if(isTRUE(CFG$producir_agosto))paso_int('LOTES',function() {
 manifiestos<-list();ensambles<-list()
 guardar_ids<-function(tab,k,destino) {
  stopifnot(!anyDuplicated(tab$numero_de_cliente),all(is.finite(tab$score)),nrow(tab)>=k,k>0L)
  o<-order(-tab$score,tab$numero_de_cliente)
  ids<-format(tab$numero_de_cliente[head(o,k)],scientific=FALSE,trim=TRUE,digits=22)
  stopifnot(length(ids)==k,!anyDuplicated(ids),all(grepl('^[0-9]+$',ids)))
  tmp<-paste0(destino,'.tmp_',Sys.getpid());writeLines(ids,tmp)
  if(!file.rename(tmp,destino))stop('No pude escribir ',destino)
  stopifnot(identical(readLines(destino),ids))
  list(archivo=destino,sha256=unname(digest::digest(file=destino,algo='sha256')),N=k)
 }
 for(nom in names(PROD_INT)) {
  z<-PROD_INT[[nom]];stopifnot(nrow(z$modelos)==CFG$n_semillas)
  for(k in as.integer(CFG$cortes)) {
   # Dejo anotado: Se separan recetas, representación, HP y corte. No mezclo distintas cantidades de contactos en un lote.
   bloques<-split(seq_len(nrow(z$modelos)),ceiling(seq_len(nrow(z$modelos))/as.integer(CFG$max_csv_lote)))
   for(b in seq_along(bloques)) {
    ix<-bloques[[b]];lote<-paste0('LOT_',nom,'_K',k,'_B',b)
    ld<-file.path(DIR_INT,'lotes',lote);dir.create(ld,recursive=TRUE,showWarnings=FALSE)
    reg<-rbindlist(lapply(ix,function(j) {
     score<-readRDS(z$modelos$scores[j])$valor
     stopifnot(score$mes==202108L,isTRUE(score$prestamos),all(score$scores$foto_mes==202108L))
     tab<-data.table(numero_de_cliente=score$scores$numero_de_cliente,score=score$scores$prob)
     f<-file.path(ld,paste0('seed_',score$seed,'_K',k,'.csv'));ver<-guardar_ids(tab,k,f)
     data.table(lote=lote,receta=nom,semilla=score$seed,modelo=score$id,corte=k,
       archivo=ver$archivo,sha256=ver$sha256,N=ver$N,tipo='INDIVIDUALES',n_archivos=length(ix))
    }))
    csv(reg,file.path(ld,'MANIFIESTO.csv'))
    mensaje<-paste('submit',lote)
    writeLines(c(mensaje,'','Adjuntar en este MISMO mensaje únicamente los CSV seed_*.csv de esta carpeta.',
      paste('Archivos:',nrow(reg),'; todos con',k,'IDs; sin encabezado.'),
      'El ZIP es para descargar y descomprimir. No adjuntar el ZIP como candidato.',
      'La respuesta del bot da media y desviación del lote; no identifica la ganancia de cada semilla.'),
      file.path(ld,'INSTRUCCION_ZULIP.txt'))
    if(nzchar(Sys.which('zip'))) {
     previo<-getwd();setwd(ld)
     zz<-tryCatch(utils::zip(file.path(DIR_INT,'entrega',paste0(lote,'.zip')),
       files=c(basename(reg$archivo),'MANIFIESTO.csv','INSTRUCCION_ZULIP.txt'),flags='-q9X'),finally=setwd(previo))
     stopifnot(zz==0L)
    }
    manifiestos[[lote]]<-reg
   }
   # Dejo anotado: Una lista distinta: se ordena DESPUÉS de promediar todas las probabilidades, no se votan IDs.
   etiqueta<-paste0('INT_',nom,'_PROB',CFG$n_semillas)
   fila<-submit(z$ensemble,etiqueta,k)
   fila[,`:=`(receta=nom,tipo='ENSAMBLE_COMPLETO',n_archivos=1L,
     sha256=unname(digest::digest(file=archivo,algo='sha256')))]
   ensambles[[paste(nom,k)]]<-fila
  }
 }
 manifest<-rbindlist(manifiestos);ensemble<-rbindlist(ensambles)
 csv(manifest,file.path(DIR_INT,'lotes','MANIFIESTO_LOTES.csv'))
 csv(ensemble,file.path(DIR_INT,'lotes','CANDIDATOS_ENSAMBLE.csv'))
 # Dejo anotado: Plantilla de respuestas: el usuario registra la respuesta real; no genero ganancias ficticias.
 plantilla<-unique(manifest[,.(referencia=lote,tipo,receta,corte,n_archivos)])
 plantilla<-rbindlist(list(plantilla,ensemble[,.(referencia=nombre,tipo,receta,corte,n_archivos)]))
 # Dejo anotado: nombre ensamble no contiene corte: agrego el corte para que sea una clave única y el comando inequívoco.
 plantilla[tipo=='ENSAMBLE_COMPLETO',referencia:=paste0(referencia,'_K',corte)]
 plantilla[,`:=`(public_mean=NA_real_,public_sd=NA_real_,submission_id=NA_character_,fecha=NA_character_)]
 fp<-file.path(DIR_INT,'publico','RESPUESTAS_PARA_COMPLETAR.csv')
 if(file.exists(fp)) {
  previa<-fread(fp,colClasses=list(numeric=c('public_mean','public_sd'),character=c('submission_id','fecha')))
  nuevas<-plantilla[!referencia%in%previa$referencia]
  if(nrow(nuevas))csv(rbindlist(list(previa,nuevas),fill=TRUE),fp)
 }else csv(plantilla,fp)
 orden<-rbindlist(lapply(CFG$recetas,function(nom) {
  data.table(paso=seq_len(2),receta=nom,corte=10500L,
    tipo=c('INDIVIDUALES','ENSAMBLE_COMPLETO'),
    accion=c(paste('submit',paste0('LOT_',nom,'_K10500_B1')),
      paste('submit',paste0('INT_',nom,'_PROB',CFG$n_semillas,'_K10500'))))
 }))
 orden[,orden:=.I]
 csv(orden,file.path(DIR_INT,'lotes','ORDEN_ENVIO_SUGERIDO.csv'))
 atomic(list(lotes=manifest,ensambles=ensemble,fecha=Sys.time()),file.path(DIR_INT,'lotes','CIERRE_LOTES.rds'))
 rp<-fread(fp,colClasses=list(numeric=c('public_mean','public_sd'),character=c('submission_id','fecha')))
 estado_int('PUBLICO',if(any(!is.na(rp$public_mean)))'EN_CURSO' else 'PENDIENTE',
   'Comparar recetas al mismo corte 10500; registrar mean, sd e ID. No envío automáticamente.')
 print(orden);TRUE
})
if(isTRUE(CFG$producir_agosto)) {
 total_plan<-if(exists('CFG_COMPLETA'))length(CFG_COMPLETA$recetas) else length(CFG$recetas)
 if(length(PROD_INT)<total_plan)estado_int('LOTES','EN_CURSO',paste(length(PROD_INT),'de',total_plan,'recetas con lotes disponibles'))
}

# Dejo anotado: Hasta que haya evaluación, no afirmo que el ensamble 20 sea el actual mejor público 105.2425.
if(file.exists(file.path(RAIZ,'submits','submit_HP1_CON_PRESTAMOS_10500.csv'))) {
 prev<-file.path(RAIZ,'submits','submit_HP1_CON_PRESTAMOS_10500.csv')
 csv(data.table(archivo=prev,sha256=unname(digest::digest(file=prev,algo='sha256')),
   public_gain=105.2425,submission_id='1537-1791499630854908',
   fuente='Respuesta Zulip copiada por Nicky; HP1 con préstamos, 10 semillas RAW, 10500 IDs'),
   file.path(DIR_INT,'publico','REFERENCIA_PUBLICA_PREVIA.csv'))
}

}
