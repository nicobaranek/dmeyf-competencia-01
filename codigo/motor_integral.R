rank_todas_int <- function(rep) {
 firma<-list(componente='RANK_TODAS_DESPUES_LAGS_v1',base=rep$firma,
  regla='por mes: ceros fijos; negativos y positivos por separado; NA conservado; reemplazo valores',
  excluir=c('numero_de_cliente','foto_mes','clase_ternaria'))
 rd<-file.path(RAIZ,'features',hash(firma));dir.create(rd,recursive=TRUE,showWarnings=FALSE)
 vars<-setdiff(rep$campos,c('numero_de_cliente','foto_mes','clase_ternaria'))
 for(m in 202103L:202108L) {
  fp<-file.path(rd,paste0(m,'.rds'))
  if(file.exists(fp)) {zz<-readRDS(fp);stopifnot(identical(zz$firma,firma));rm(zz);next}
  z<-leer_meses(rep,m);aud<-list()
  for(v in vars) {
   x<-z[[v]];stopifnot(is.numeric(x));r<-rank_cero(x)
   stopifnot(identical(is.na(x),is.na(r)),all(r[x==0 & !is.na(x)]==0),
    all(r[x>0 & !is.na(x)]>0),all(r[x<0 & !is.na(x)]<0),all(abs(r[!is.na(r)])<=1))
   set(z,j=v,value=r)
   aud[[v]]<-data.table(foto_mes=m,variable=v,n_na=sum(is.na(r)),ceros=sum(r==0,na.rm=TRUE))
  }
  csv(rbindlist(aud),file.path(rd,paste0('AUDITORIA_RANK_',m,'.csv')))
  atomic(list(firma=firma,mes=m,datos=z),fp);rm(z);gc(FALSE)
  log_('RANK TODAS bloque terminado',m,length(vars),'variables; ranking después de lags y deltas')
 }
 csv(data.table(variable=rep$campos,ranking=rep$campos%in%vars),file.path(rd,'CATALOGO_FEATURES.csv'))
 atomic(firma,file.path(rd,'FIRMA.rds'))
 list(id=hash(firma),dir=rd,campos=rep$campos,rank=TRUE,firma=firma)
}

predecir_mes_int <- function(modelo,rep,mes) {
 sig<-list(componente='pred_mes_integral_v1',modelo=modelo$id,representacion=rep$id,mes=as.integer(mes))
 cache('scores',sig,function() {
  stopifnot(file.exists(modelo$modelo));m<-lightgbm::lgb.load(modelo$modelo)
  df<-leer_meses(rep,mes);xx<-data.matrix(df[,modelo$campos,with=FALSE]);pr<-predict(m,xx)
  stopifnot(length(pr)==nrow(df),all(is.finite(pr)),all(pr>=0 & pr<=1))
  tab<-data.table(numero_de_cliente=df$numero_de_cliente,foto_mes=as.integer(mes),prob=pr)
  rm(m,df,xx);gc(FALSE);tab
 })
}
etiquetar_int <- function(tab,reales,regla=CFG$pseudo_regla,k=CFG$pseudo_top_k,seed=CFG$pseudo_seed) {
 t<-copy(tab);stopifnot(!anyDuplicated(t[,.(numero_de_cliente,foto_mes)]),
  all(is.finite(t$prob)),all(t$prob>=0 & t$prob<=1))
 prevalencia<-mean(reales$clase_ternaria%in%c('BAJA+1','BAJA+2'))
 t[,etiqueta_artificial:=0L]
 for(m in sort(unique(t$foto_mes))) {
  ii<-which(t$foto_mes==m);p<-t$prob[ii]
  if(regla=='bernoulli') {
   set.seed(as.integer(seed+m),kind="L'Ecuyer-CMRG");y<-as.integer(runif(length(ii))<p)
  }else {
   npos<-if(regla=='top_k')as.integer(k) else as.integer(round(length(ii)*prevalencia))
   stopifnot(npos>0L,npos<length(ii));y<-integer(length(ii))
   y[head(order(-p,t$numero_de_cliente[ii]),npos)]<-1L
  }
  set(t,ii,'etiqueta_artificial',y)
 }
 t[,regla:=regla];t[,origen:='ARTIFICIAL; no se lee el target real de estos meses']
 t
}
plan_pseudo_int <- function(rec,prod=FALSE) {
 reales<-if(prod)PARAM$train_final else PARAM$train
 arti<-if(prod)c(202107L,202108L) else c(202105L,202106L)
 # Dejo anotado: Docente con buenos HP fijos ya existentes. No corro una BO adicional para el docente.
 docente<-rec_control_int
 if(rec$rep$id!=rec_control_int$rep$id) {
  docente$rep<-rec$rep;docente$nombre<-'DOCENTE_RANK_TODAS'
 }
 seed_doc<-head(BASE_SEEDS,as.integer(CFG$pseudo_docente_semillas))
 modelos<-lapply(seed_doc,function(s)fit_score(docente,HP_DOCENTE_INT,s,prod=prod,prestamos=prod))
 sig<-list(componente='PSEUDO_CLASE_PREVIA_v2',raw=RAW_HASH,rep=rec$rep$id,reales=reales,artificiales=arti,
  modelos=vapply(modelos,`[[`,character(1),'id'),regla=CFG$pseudo_regla,k=CFG$pseudo_top_k,
  seed=CFG$pseudo_seed,versiones=VERSIONES)
 cache('seleccion',sig,function() {
  pred<-rbindlist(lapply(arti,function(m) {
   ts<-lapply(modelos,predecir_mes_int,rep=rec$rep,mes=m);ref<-ts[[1L]]
   probs<-lapply(ts,function(t){ix<-match(ref$numero_de_cliente,t$numero_de_cliente);stopifnot(!anyNA(ix));t$prob[ix]})
   ref[,prob:=Reduce('+',probs)/length(probs)];ref
  }))
  r<-leer_meses(rec$rep,reales)
  stopifnot(!anyNA(r$clase_ternaria))
  art<-etiquetar_int(pred,r)
  audit<-art[,.(N=.N,positivos=sum(etiqueta_artificial),proporcion=mean(etiqueta_artificial),
    prob_min_positivo=if(any(etiqueta_artificial==1L))min(prob[etiqueta_artificial==1L]) else NA_real_),by=foto_mes]
  tag<-paste0(rec$nombre,'_',if(prod)'prod' else 'hist','_',substr(hash(sig),1,16))
  csv(art,file.path(DIR_INT,'planes',paste0('ETIQUETAS_',tag,'.csv')))
  csv(audit,file.path(DIR_INT,'planes',paste0('AUDITORIA_ETIQUETAS_',tag,'.csv')))
  # Dejo anotado: Las únicas etiquetas reales leídas pertenecen a reales. Y se guarda fuera de X.
  list(key=c(paste(r$numero_de_cliente,r$foto_mes),paste(art$numero_de_cliente,art$foto_mes)),
    y=c(as.integer(r$clase_ternaria%in%c('BAJA+1','BAJA+2')),art$etiqueta_artificial),
    meses=c(reales,arti),firma=sig,auditoria=audit)
 })
}

# Promedio por ID; nunca promedio listas ordenadas de personas diferentes ni uso votos/pesos por ganancia.
promediar_prob_int <- function(xs,nombre) {
 stopifnot(length(xs)>0L)
 ref<-copy(xs[[1L]]$scores[,.(numero_de_cliente,foto_mes,clase_ternaria)])
 stopifnot(!anyDuplicated(ref$numero_de_cliente),uniqueN(ref$foto_mes)==1L)
 matriz<-matrix(NA_real_,nrow(ref),length(xs))
 ids<-vapply(xs,`[[`,character(1),'id');seeds<-vapply(xs,`[[`,integer(1),'seed')
 stopifnot(!anyDuplicated(ids),!anyDuplicated(seeds))
 for(j in seq_along(xs)) {
  t<-xs[[j]]$scores;stopifnot(!anyDuplicated(t$numero_de_cliente),nrow(t)==nrow(ref))
  ix<-match(ref$numero_de_cliente,t$numero_de_cliente)
  stopifnot(!anyNA(ix),identical(ref$foto_mes,t$foto_mes[ix]))
  if(all(!is.na(ref$clase_ternaria)))stopifnot(identical(ref$clase_ternaria,t$clase_ternaria[ix]))
  matriz[,j]<-t$prob[ix]
 }
 stopifnot(all(is.finite(matriz)),all(matriz>=0 & matriz<=1))
 tab<-copy(ref);tab[,score:=rowMeans(matriz)]
 id<-hash(list(componente='ENS_PROB_INTEGRAL_v1',modelos=ids,modo='prob',orden_clientes=hash(ref$numero_de_cliente)))
 en<-list(id=id,nombre=nombre,modo='prob',tabla=tab,modelos=ids,semillas=seeds)
 atomic(en,file.path(RAIZ,'scores',paste0('ENS_',id,'.rds')))
 wide<-copy(ref[,.(numero_de_cliente,foto_mes)])
 for(j in seq_along(xs))set(wide,j=paste0('prob_',seeds[j]),value=matriz[,j])
 wide[,prob_ensemble:=rowMeans(matriz)]
 list(en=en,probabilidades=wide)
}
resumen_junio_int <- function(xs,en,nombre) {
 tab<-en$tabla;stopifnot(all(tab$foto_mes==202106L),!anyNA(tab$clase_ternaria))
 cv<-curva(tab$numero_de_cliente,tab$score,tab$clase_ternaria)
 csv(cv,file.path(DIR_INT,'historia',paste0('CURVA_',nombre,'.csv')))
 cortes<-cv[corte%in%as.integer(CFG$cortes)]
 semillas<-rbindlist(lapply(xs,function(x){c<-curva(x$scores$numero_de_cliente,x$scores$prob,x$scores$clase_ternaria)
  c[corte%in%as.integer(CFG$cortes)][,`:=`(semilla=x$seed,modelo=x$id)]}))
 stats<-semillas[,.(media=mean(ganancia),sd=sd(ganancia),min=min(ganancia),max=max(ganancia)),by=corte]
 r<-merge(cortes,stats,by='corte');setnames(r,'ganancia','ganancia_ensemble');r[,receta:=nombre]
 r[,media_meseta:=vapply(corte,function(k)mean(cv[corte>=k-250 & corte<=k+250,ganancia]),numeric(1))]
 csv(semillas,file.path(DIR_INT,'historia',paste0('SEMILLAS_',nombre,'.csv')))
 csv(r,file.path(DIR_INT,'historia',paste0('RESUMEN_',nombre,'.csv')))
 r
}
guardar_cierre_int <- function(nombre,x,carpeta) {
 atomic(x,file.path(DIR_INT,carpeta,paste0('CIERRE_',nombre,'.rds')))
}
