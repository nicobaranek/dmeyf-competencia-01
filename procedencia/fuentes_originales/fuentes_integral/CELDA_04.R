# Dejo anotado: CELDA 04 — Defino mis funciones y la persistencia.
# Mantengo el cálculo en R y guardo cada resultado costoso antes de avanzar.
suppressPackageStartupMessages({library(data.table);library(digest);library(jsonlite);library(filelock)})
options(scipen=999, datatable.print.nrows=30)
setDTthreads(PARAM$threads)
CODE <- 'MAESTRA_CATEDRA_20261007_v1'
RAIZ <- Sys.getenv('DMEYF_RAIZ'); RAW <- Sys.getenv('DMEYF_DATOS')
stopifnot(nzchar(RAIZ),file.exists(RAW))
for (s in c('auditoria','datos','features','datasets','modelos','scores','bo','seleccion','resultados','submits','figuras','fuentes')) dir.create(file.path(RAIZ,s),recursive=TRUE,showWarnings=FALSE)
hash <- function(x) digest(x,algo='sha256')
log_ <- function(...) {s<-paste(format(Sys.time(),'%Y-%m-%d %H:%M:%S %Z'),paste(...,collapse=' '));cat(s,'\n');cat(s,'\n',file=file.path(RAIZ,'PROGRESO.log'),append=TRUE)}
atomic <- function(x,path) {tmp<-paste0(path,'.tmp_',Sys.getpid());saveRDS(x,tmp,compress=FALSE);if(!file.rename(tmp,path)){if(!file.copy(tmp,path,overwrite=TRUE))stop('No pude persistir ',path);unlink(tmp)};invisible(path)}
csv <- function(x,path) {tmp<-paste0(path,'.tmp_',Sys.getpid());fwrite(x,tmp);if(!file.rename(tmp,path)){if(!file.copy(tmp,path,overwrite=TRUE))stop('No pude persistir ',path);unlink(tmp)}}
registro <- function(etapa,archivo,accion,motivo){p<-file.path(RAIZ,'auditoria','AUDITORIA_REUTILIZACION.csv');d<-data.table(fecha=as.character(Sys.time()),etapa=etapa,archivo=archivo,accion=accion,motivo=motivo);fwrite(d,p,append=file.exists(p),col.names=!file.exists(p))}
# Dejo anotado: Los locks de cálculo viven en disco local; todos los procesos de esta VM comparten la ruta.
# No ejecuto simultáneamente esta misma RAIZ desde dos VM distintas.
LOCK_DIR<-file.path(path.expand('~/.cache/dmeyf_locks'),hash(RAIZ));dir.create(LOCK_DIR,recursive=TRUE,showWarnings=FALSE)
cache <- function(etapa,firma,fun){
 p<-file.path(RAIZ,etapa,paste0(hash(firma),'.rds'))
 lk<-filelock::lock(file.path(LOCK_DIR,paste0(hash(p),'.lock')),timeout=0)
 if(is.null(lk))stop('Otra ejecución está usando este checkpoint: ',p)
 on.exit(filelock::unlock(lk))
 if(file.exists(p)){z<-readRDS(p);stopifnot(identical(z$firma,firma));registro(etapa,p,'REUTILIZAR','Firma exacta');return(z$valor)}
 log_('CALCULO',etapa,basename(p));v<-fun();atomic(list(firma=firma,valor=v,fecha=Sys.time()),p);registro(etapa,p,'CALCULAR','No existe checkpoint compatible para esta firma');v
}
VERSIONES<-list(R=as.character(getRversion()),lightgbm=as.character(packageVersion('lightgbm')),data.table=as.character(packageVersion('data.table')))
RAW_HASH<-unname(tools::md5sum(RAW));writeLines(capture.output(sessionInfo()),file.path(RAIZ,'auditoria','SESSION_INFO.txt'))
atomic(PARAM,file.path(RAIZ,'PLAN_ACTUAL.rds'));atomic(VERSIONES,file.path(RAIZ,'auditoria','VERSIONES.rds'))
PRESTAMOS<-c('cprestamos_personales','mprestamos_personales')
SEMILLAS<-as.integer(PARAM$semillas)
par_base<-function(seed){list(boosting='gbdt',objective='binary',metric='average_precision',feature_pre_filter=FALSE,
 first_metric_only=FALSE,boost_from_average=TRUE,force_row_wise=TRUE,deterministic=TRUE,verbosity=-1,
 seed=as.integer(seed),num_threads=PARAM$threads,max_depth=-1L,min_gain_to_split=0,lambda_l1=0,lambda_l2=0,max_bin=31L,
 bagging_fraction=1,bagging_freq=0L,pos_bagging_fraction=1,neg_bagging_fraction=1,is_unbalance=FALSE,scale_pos_weight=1,
 extra_trees=FALSE,min_data_in_leaf=0L,feature_fraction=.50,feature_fraction_bynode=.20,learning_rate=.005)}
periodo<-function(x) as.integer(x%/%100L*12L+x%%100L)
rank_cero<-function(x){z<-rep(NA_real_,length(x));z[which(x==0)]<-0;ip<-which(is.finite(x)&x>0);ineg<-which(is.finite(x)&x<0)
 if(length(ip))z[ip]<-frank(x[ip],ties.method='average')/length(ip)
 if(length(ineg))z[ineg]<- -frank(abs(x[ineg]),ties.method='average')/length(ineg)
 z}
# Audito lo que existe, sin cargar modelos ni recalcular resultados anteriores.
inventario_previo<-function(){
 roots<-unique(c(Sys.getenv('DMEYF_ANTERIORES'),file.path(dirname(RAIZ),'HOJA_RUTA_AGOSTO'),RAIZ));roots<-roots[nzchar(roots)&dir.exists(roots)]
 out<-rbindlist(lapply(roots,function(root){ff<-list.files(root,recursive=TRUE,full.names=TRUE);ff<-ff[grepl('\\.(csv|rds|txt|json|bin|ipynb)$',ff)];if(!length(ff))return(NULL);fi<-file.info(ff);data.table(archivo=ff,bytes=fi$size,fecha=as.character(fi$mtime),estado='LOCALIZADO; compatibilidad pendiente, no equivale a reutilizado')}),fill=TRUE)
 csv(out,file.path(RAIZ,'auditoria','INVENTARIO_PREVIO.csv'));log_('Inventario',nrow(out),'archivos. No cambio ni borro artefactos anteriores.');out
}
cargar_base<-function(){
 firma<-list(componente='base_DQ_target_v1',raw=RAW_HASH)
 cache('datos',firma,function(){
  d<-fread(RAW);stopifnot(all(c('numero_de_cliente','foto_mes')%in%names(d)),!anyNA(d$numero_de_cliente),!anyNA(d$foto_mes))
  if(anyDuplicated(d[,.(numero_de_cliente,foto_mes)]))stop('Duplicados cliente x mes')
  if(!all(202103L:202108L%in%unique(d$foto_mes)))stop('No están marzo-agosto de 2021')
  csv(data.table(filas=nrow(d),columnas=ncol(d),clientes=uniqueN(d$numero_de_cliente),md5=RAW_HASH),file.path(RAIZ,'auditoria','DIMENSIONES.csv'))
  setorder(d,numero_de_cliente,foto_mes)
  q<-d[,.(numero_de_cliente,foto_mes,p=periodo(foto_mes))];ult<-max(q$p)
  q[,c('p1','p2'):=shift(p,n=1:2,type='lead'),by=numero_de_cliente]
  q[,reconstruida:=fifelse(p<ult-1L,'CONTINUA',NA_character_)]
  q[p<ult & (is.na(p1)|p1>p+1L),reconstruida:='BAJA+1']
  q[p<ult-1L & !is.na(p1)&p1==p+1L & (is.na(p2)|p2>p+2L),reconstruida:='BAJA+2']
  if('clase_ternaria'%in%names(d)){
   q[,original:=as.character(d$clase_ternaria)]
   csv(q[, .N,by=.(foto_mes,original,reconstruida)],file.path(RAIZ,'auditoria','CONCORDANCIA_TARGET.csv'))
   ex<-q[!is.na(original)&!is.na(reconstruida)&original!=reconstruida]
   csv(ex,file.path(RAIZ,'auditoria','EXCEPCIONES_TARGET.csv'))
   if(nrow(ex))stop('Target original y reconstruido discrepan. Revisar EXCEPCIONES_TARGET.csv')
   d[,clase_ternaria:=as.character(clase_ternaria)]
   d[is.na(clase_ternaria),clase_ternaria:=q$reconstruida[is.na(d$clase_ternaria)]]
  }else{d[,clase_ternaria:=q$reconstruida];writeLines('Sin etiqueta original; uso la reconstrucción del notebook de cátedra, no una concordancia verificada.',file.path(RAIZ,'auditoria','FUENTE_TARGET.txt'))}
  if(anyNA(d[foto_mes<=202106L,clase_ternaria]))stop('Hay etiquetas faltantes en training/evaluación completos')
  csv(d[,.N,by=.(foto_mes,clase_ternaria)],file.path(RAIZ,'auditoria','CLASES_POR_MES.csv'))
  if(!'ccajas_depositos'%in%names(d))stop('No existe ccajas_depositos')
  csv(d[foto_mes==202105L,.(N=.N,ceros=sum(ccajas_depositos==0,na.rm=TRUE),missing=sum(is.na(ccajas_depositos)))],file.path(RAIZ,'auditoria','DQ_MAYO.csv'))
  d[foto_mes==202105L,ccajas_depositos:=NA_real_]
  campos<-setdiff(names(d),c('numero_de_cliente','foto_mes','clase_ternaria'))
  catvars<-campos[!vapply(d[,..campos],is.numeric,logical(1))]
  mapa<-list()
  for(v in catvars){lev<-sort(unique(as.character(d[[v]][!is.na(d[[v]])])));mapa[[v]]<-lev;set(d,j=v,value=match(as.character(d[[v]]),lev))}
  atomic(mapa,file.path(RAIZ,'auditoria','MAPA_CATEGORIAS.rds'))
  nums<-setdiff(campos,catvars)
  for(v in nums){ii<-which(!is.finite(d[[v]])&!is.na(d[[v]]));if(length(ii))set(d,ii,v,NA_real_)}
  attr(d,'categoricas')<-catvars
  d
 })
}
# Fijo la transformación sin usar el target; los percentiles se calculan por mes en toda la cartera.
preparar_features<-function(d,rankear=FALSE){
 campos<-setdiff(names(d),c('numero_de_cliente','foto_mes','clase_ternaria'));cats<-attr(d,'categoricas')
 ii_discovery<-which(d$foto_mes%in%PARAM$train)
 elegibles<-if(rankear)PARAM$rank_campos else character()
 if(rankear)stopifnot(length(elegibles)>0L,all(elegibles%in%campos),!any(elegibles%in%cats),all(vapply(d[,..elegibles],is.numeric,logical(1))))
 f<-list(componente=if(rankear)'historia_rank_montos_explicitos_v2' else 'historia_mensual_v1',raw=RAW_HASH,dq='ccajas_depositos_202105_NA',orden=PARAM$orden_lags,rank_cero=elegibles,categoricas=cats)
 k<-hash(f);rdir<-file.path(RAIZ,'features',k);dir.create(rdir,recursive=TRUE,showWarnings=FALSE)
 # Reutilizo cada bloque terminado aunque se haya interrumpido otro mes.
 meses<-202103L:202108L
 faltan<-meses[!file.exists(file.path(rdir,paste0(meses,'.rds')))]
 if(length(faltan)){
  log_('RECALCULO IMPRESCINDIBLE: bloques faltantes',paste(faltan,collapse=','),'representación',k)
  log_('Motivo: falta firma exacta de DQ, lags y ranking. Reutilizo base, bloques existentes y cálculos posteriores compatibles.')
  b<-copy(d)
  for(v in elegibles){set(b,j=v,value=as.numeric(b[[v]]));b[,(v):=rank_cero(get(v)),by=foto_mes]}
  pp<-periodo(b$foto_mes);ids<-b$numero_de_cliente
  # Localizo lag calendario exacto: no confundo una ausencia mensual con lag1.
  key<-paste(ids,pp,sep=':');lagidx<-lapply(seq_len(PARAM$orden_lags),function(l)match(paste(ids,pp-l,sep=':'),key))
  for(m in faltan){
   ix<-which(b$foto_mes==m);z<-copy(b[ix])
   for(l in seq_len(PARAM$orden_lags))for(v in campos){
    lagv<-b[[v]][lagidx[[l]][ix]];set(z,j=paste0(v,'_lag',l),value=lagv)
    if(!v%in%cats)set(z,j=paste0(v,'_delta_lag',l),value=b[[v]][ix]-lagv)
   }
   atomic(list(firma=f,mes=m,datos=z),file.path(rdir,paste0(m,'.rds')));log_('Bloque guardado',m,'features',ncol(z)-3L);rm(z);gc(FALSE)
  }
  rm(b);gc(FALSE)
 }
 campos_todos<-c(campos,unlist(lapply(seq_len(PARAM$orden_lags),function(l)c(paste0(campos,'_lag',l),paste0(setdiff(campos,cats),'_delta_lag',l)))))
 if(PARAM$incluir_id_mes)campos_todos<-c('numero_de_cliente','foto_mes',campos_todos)
 cat<-data.table(variable=campos_todos,ranking=FALSE,temporal=grepl('_lag[12]$',campos_todos))
 cat[,origen:=sub('_(delta_)?lag[12]$','',variable)];cat[,ranking:=origen%in%elegibles]
 csv(cat,file.path(rdir,'CATALOGO_FEATURES.csv'));atomic(f,file.path(rdir,'FIRMA.rds'))
 list(id=k,dir=rdir,campos=campos_todos,rank=rankear,firma=f)
}
leer_meses<-function(rep,meses){rbindlist(lapply(meses,function(m){z<-readRDS(file.path(rep$dir,paste0(m,'.rds')));stopifnot(identical(z$firma,rep$firma),identical(z$mes,as.integer(m)));z$datos}),use.names=TRUE)}
campos_receta<-function(rec,produccion=FALSE,prestamos=FALSE){
 c<-rec$rep$campos
 if(!prestamos)c<-c[!sub('_(delta_)?lag[12]$','',c)%in%PRESTAMOS]
 if(!is.null(rec$seleccion)){lo<-c[sub('_(delta_)?lag[12]$','',c)%in%PRESTAMOS];c<-intersect(c,rec$seleccion);if(prestamos)c<-unique(c(c,lo))}
 if(length(rec$excluir))c<-c[!sub('_(delta_)?lag[12]$','',c)%in%rec$excluir]
 c
}
curva<-function(ids,prob,y){stopifnot(length(ids)==length(prob),length(y)==length(ids),!anyNA(y),all(is.finite(prob)));ix<-order(-prob,ids);data.table(corte=seq_along(ix),ganancia=cumsum(ifelse(y[ix]=='BAJA+2',1072500,-27500))/1e6)}
# Construyo el dataset LightGBM una vez por diseño; lo guardo en binario y libero la matriz densa.
materializar<-function(rec,prod=FALSE,prestamos=FALSE){
 meses<-if(prod)PARAM$train_final else PARAM$train
 if(!is.null(rec$planes)&&!is.null(rec$planes[[if(prod)'prod' else 'hist']]$meses))meses<-rec$planes[[if(prod)'prod' else 'hist']]$meses
 campos<-campos_receta(rec,prod,prestamos)
 firma<-list(componente='dataset_v1',raw=RAW_HASH,representacion=rec$rep$id,campos=campos,meses=meses,estrategia=rec$estrategia,umbral=rec$umbral,versiones=VERSIONES,
  plan=if(is.null(rec$planes))NULL else hash(rec$planes[[if(prod)'prod' else 'hist']]))
 key<-hash(firma);fp<-file.path(RAIZ,'datasets',paste0(key,'.rds'));bin<-file.path(RAIZ,'datasets',paste0(key,'.bin'))
 if(file.exists(fp)&&file.exists(bin)){z<-readRDS(fp);stopifnot(identical(z$firma,firma));registro('dataset',bin,'REUTILIZAR','Firma exacta');return(z$valor)}
 d<-leer_meses(rec$rep,meses);y<-as.integer(d$clase_ternaria%in%c('BAJA+1','BAJA+2'))
 if(!is.null(rec$planes)){
  plan<-rec$planes[[if(prod)'prod' else 'hist']]
  keys<-paste(d$numero_de_cliente,d$foto_mes);pi<-match(plan$key,keys);stopifnot(!anyNA(pi));d<-d[pi];y<-plan$y
 }
 stopifnot(length(campos)>0,nrow(d)>0,!anyNA(y),all(y%in%c(0,1)),uniqueN(y)==2L)
 # Calculo un límite conservador antes de materializar para evitar otra caída de RAM.
 mi<-readLines('/proc/meminfo');av<-as.numeric(sub('.*?([0-9]+).*','\\1',mi[grepl('^MemAvailable:',mi)]))*1024
 estimado<-as.double(nrow(d))*length(campos)*8*3
 if(length(av)&&is.finite(av)&&estimado>av*.85)stop('Memoria insuficiente para esta matriz: necesito margen aproximado ',round(estimado/2^30,1),' GiB. Checkpoints conservados.')
 log_('Dataset',rec$nombre,if(prod)'PRODUCCION' else 'HISTORICO','filas',nrow(d),'features',length(campos),'préstamos',prestamos)
 x<-data.matrix(d[,..campos]);dm<-lightgbm::lgb.Dataset(x,label=y,free_raw_data=TRUE,params=list(max_bin=31L,feature_pre_filter=FALSE));dm$construct()
 tmp<-paste0(bin,'.tmp_',Sys.getpid());lightgbm::lgb.Dataset.save(dm,tmp);if(!file.rename(tmp,bin))stop('No pude persistir dataset binario')
 meta<-list(bin=bin,campos=campos,n=nrow(d),y_hash=hash(y),filas_hash=hash(d[,.(numero_de_cliente,foto_mes)]),firma=firma,id=key)
 atomic(list(firma=firma,valor=meta),fp);rm(x,dm,d);gc(FALSE);meta
}
fit_score<-function(rec,hp,seed,prod=FALSE,prestamos=FALSE){
 meta<-materializar(rec,prod,prestamos);mes<-if(prod)202108L else 202106L;p<-modifyList(par_base(seed),hp)
 sig<-list(componente='fit_v1',dataset=meta$firma,pars=p,seed=seed,score_mes=mes,score_rep=rec$rep$id)
 key<-hash(sig);mp<-file.path(RAIZ,'modelos',paste0(key,'.txt'));sp<-file.path(RAIZ,'scores',paste0(key,'.rds'))
 if(file.exists(sp)&&file.exists(mp)){z<-readRDS(sp);stopifnot(identical(z$firma,sig));registro('modelo_scores',sp,'REUTILIZAR','Firma exacta');return(z$valor)}
 # Dejo anotado: Si el modelo terminó pero se cortó antes de guardar scores, lo reutilizo.
 mf<-paste0(mp,'.firma.rds');m<-NULL
 if(file.exists(mp)&&file.exists(mf)){stopifnot(identical(readRDS(mf),sig));m<-lightgbm::lgb.load(mp);log_('RECUPERO modelo terminado',key)}
 if(is.null(m)){
  log_('ENTRENO',rec$nombre,seed,'mes',mes,'iteraciones',p$num_iterations)
  dt<-lightgbm::lgb.Dataset(meta$bin,params=list(max_bin=31L,feature_pre_filter=FALSE))
  m<-lightgbm::lgb.train(params=p,data=dt,nrounds=p$num_iterations)
  tmp<-paste0(mp,'.tmp');lightgbm::lgb.save(m,tmp);if(!file.rename(tmp,mp))stop('No pude guardar modelo');atomic(sig,mf);rm(dt);gc(FALSE)
 }
 df<-leer_meses(rec$rep,mes);xx<-data.matrix(df[,meta$campos,with=FALSE]);pr<-predict(m,xx);raw<-predict(m,xx,type='raw')
 stopifnot(length(pr)==nrow(df),all(is.finite(pr)),all(is.finite(raw)))
 out<-list(id=key,receta=rec$nombre,seed=seed,mes=mes,prestamos=prestamos,hp=hp,campos=meta$campos,modelo=mp,
 scores=data.table(numero_de_cliente=df$numero_de_cliente,foto_mes=mes,clase_ternaria=df$clase_ternaria,prob=pr,raw=raw),ganancia=NA_real_)
 im<-as.data.table(lightgbm::lgb.importance(m));im<-merge(data.table(Feature=meta$campos),im,by='Feature',all.x=TRUE);csv(im,file.path(RAIZ,'modelos',paste0(key,'_importancia.csv')))
 if(!prod){cv<-curva(df$numero_de_cliente,pr,df$clase_ternaria);out$ganancia<-cv[corte==PARAM$corte,ganancia];csv(cv,file.path(RAIZ,'resultados',paste0('CURVA_',key,'.csv')))}
 atomic(list(firma=sig,valor=out,fecha=Sys.time()),sp)
 rp<-data.table(id=key,receta=rec$nombre,semilla=seed,mes=mes,prestamos=prestamos,N_train=meta$n,features=length(meta$campos),ganancia=out$ganancia,hp=toJSON(hp,auto_unbox=TRUE),modelo=mp,scores=sp)
 csv(rp,file.path(RAIZ,'modelos',paste0(key,'_registro.csv')));log_('GUARDADO',rec$nombre,seed,if(prod)'agosto' else paste('ganancia',out$ganancia));rm(m,df,xx);gc(FALSE);out
}
# Mantengo mlrMBO/DiceKriging, motor de la notebook de cátedra.
# Dejo anotado: Cada evaluación tiene caché propio: incluso si el estado del optimizador queda atrasado no reentreno el mismo punto.
bayes<-function(rec){
 meta<-materializar(rec,FALSE,FALSE)
 sig<-list(componente='bo_catedra_v1',dataset=meta$firma,base=par_base(PARAM$semilla),limites=PARAM$bounds,presupuesto=PARAM$bo_evaluaciones,corte=PARAM$corte,seed=PARAM$semilla,
  versiones=c(VERSIONES,list(mlrMBO=as.character(packageVersion('mlrMBO')),DiceKriging=as.character(packageVersion('DiceKriging')))))
 bd<-file.path(RAIZ,'bo',hash(sig));dir.create(bd,recursive=TRUE,showWarnings=FALSE);terminado<-file.path(bd,'TERMINADO.rds');histp<-file.path(bd,'HISTORIAL.rds');estado<-file.path(bd,'ESTADO.RData')
 if(file.exists(terminado)){z<-readRDS(terminado);stopifnot(identical(z$firma,sig));log_('BO RECUPERADA',rec$nombre);return(z$valor)}
 atomic(sig,file.path(bd,'FIRMA.rds'))
 objetivo<-function(x){
  hp<-list(num_iterations=as.integer(x$num_iterations),num_leaves=as.integer(x$num_leaves),min_sum_hessian_in_leaf=10^x$log10_min_sum_hessian_in_leaf)
  r<-fit_score(rec,hp,PARAM$semilla);g<-r$ganancia;if(!is.finite(g))stop('Ganancia no finita; no agrego evaluación inválida')
  h<-if(file.exists(histp))readRDS(histp) else data.table()
  if(!nrow(h)||!r$id%in%h$modelo){h<-rbindlist(list(h,data.table(evaluacion=nrow(h)+1L,num_iterations=hp$num_iterations,num_leaves=hp$num_leaves,log10_min_sum_hessian_in_leaf=x$log10_min_sum_hessian_in_leaf,min_sum_hessian_in_leaf=hp$min_sum_hessian_in_leaf,ganancia=g,modelo=r$id)),fill=TRUE);atomic(h,histp);csv(h,file.path(bd,'HISTORIAL.csv'))}
  log_('BO',rec$nombre,'evaluaciones únicas',nrow(h),'/',PARAM$bo_evaluaciones,'ganancia',g);g
 }
 ps<-ParamHelpers::makeParamSet(ParamHelpers::makeIntegerParam('num_iterations',lower=PARAM$bounds$iter[1],upper=PARAM$bounds$iter[2]),
  ParamHelpers::makeIntegerParam('num_leaves',lower=PARAM$bounds$leaves[1],upper=PARAM$bounds$leaves[2]),
  ParamHelpers::makeNumericParam('log10_min_sum_hessian_in_leaf',lower=PARAM$bounds$logh[1],upper=PARAM$bounds$logh[2]))
 obj<-smoof::makeSingleObjectiveFunction(name=rec$nombre,fn=objetivo,minimize=FALSE,noisy=TRUE,par.set=ps,has.simple.signature=FALSE)
 mlr::configureMlr(show.learner.output=FALSE)
 ctrl<-mlrMBO::makeMBOControl(save.on.disk.at.time=0,save.file.path=estado)
 ctrl<-mlrMBO::setMBOControlTermination(ctrl,iters=PARAM$bo_evaluaciones-PARAM$bo_iniciales)
 ctrl<-mlrMBO::setMBOControlInfill(ctrl,crit=mlrMBO::makeMBOInfillCritEI())
 learner<-mlr::makeLearner('regr.km',predict.type='se',covtype='matern3_2',control=list(trace=FALSE),nugget.estim=TRUE)
 set.seed(PARAM$semilla,kind="L'Ecuyer-CMRG")
 # Dejo anotado: El resultado final se decide con ganancias monetarias originales, nunca con scores estandarizados.
 if(file.exists(histp)&&nrow(readRDS(histp))>=PARAM$bo_evaluaciones){h<-readRDS(histp)}else{
  tryCatch({if(file.exists(estado)){mlrMBO::mboContinue(estado)}else{design<-ParamHelpers::generateDesign(n=PARAM$bo_iniciales,par.set=ps);mlrMBO::mbo(obj,design=design,learner=learner,control=ctrl)}} ,error=function(e){log_('BO interrumpida; historial/modelos conservados:',conditionMessage(e));stop('No cambio el método ni vuelvo a entrenar puntos guardados. Reejecutar celda 08 retoma; revisar el error anterior.')})
  h<-readRDS(histp)
 }
 setorder(h,-ganancia,modelo);h[,orden:=.I];csv(h,file.path(bd,'ORDENADOS.csv'))
 opt<-lapply(seq_len(min(PARAM$top_configuraciones,nrow(h))),function(i)as.list(h[i,.(num_iterations,num_leaves,min_sum_hessian_in_leaf)]))
 res<-list(nombre=rec$nombre,historial=h,optimos=opt,directorio=bd);atomic(list(firma=sig,valor=res),terminado);res
}
# Dejo anotado: Canaritos: incluyo TODAS las variables, incluso las de importancia cero.
canarios<-function(rec,multiplo){
 f<-list(componente='canarios_v1',rep=rec$rep$id,train=PARAM$train,seed=PARAM$semilla,fraccion=.20,hp=PARAM$hp_seleccion)
 resultado<-cache('seleccion',f,function(){
  d<-leer_meses(rec$rep,PARAM$train);cam<-campos_receta(rec);x<-data.matrix(d[,..cam]);set.seed(PARAM$semilla)
  ncan<-ceiling(ncol(x)*.20);nom<-paste0('canarito_',seq_len(ncan));x<-cbind(x,matrix(runif(nrow(x)*ncan),nrow=nrow(x),dimnames=list(NULL,nom)))
  y<-as.integer(d$clase_ternaria%in%c('BAJA+1','BAJA+2'));dt<-lightgbm::lgb.Dataset(x,label=y,free_raw_data=TRUE)
  m<-lightgbm::lgb.train(params=modifyList(par_base(PARAM$semilla),PARAM$hp_seleccion),data=dt,nrounds=PARAM$hp_seleccion$num_iterations)
  imp<-merge(data.table(Feature=colnames(x)),as.data.table(lightgbm::lgb.importance(m)),by='Feature',all.x=TRUE);imp[is.na(Gain),Gain:=0]
  imp[,es_canarito:=Feature%in%nom];set.seed(PARAM$semilla);imp[,desempate:=runif(.N)];setorder(imp,-Gain,desempate);imp[,id:=.I]
  list(imp=imp,canaritos=ncan)
 })
 imp<-copy(resultado$imp);corte<-mean(imp[es_canarito==TRUE,id])+multiplo*sd(imp[es_canarito==TRUE,id]);imp[,conservar:=!es_canarito&id<corte]
 csv(imp,file.path(RAIZ,'seleccion',paste0('CANARITOS_',hash(f),'_SD',multiplo,'.csv')))
 list(campos=imp[conservar==TRUE,Feature],corte=corte,canaritos=resultado$canaritos)
}
# Dejo anotado: Filtro opcional, separado del experimento que continúa en Colab.
# Mantengo 30/70 por filas, auxiliares fuera del 70%, positivos conservados dentro del 70%.
planes_negativos<-function(rec,umbral=NULL){
 auxp<-modifyList(par_base(PARAM$semilla),list(feature_fraction_bynode=1,num_iterations=2180L,num_leaves=30L,min_sum_hessian_in_leaf=.1141634103))
 lapply(setNames(c(FALSE,TRUE),c('hist','prod')),function(prod){
  meses<-if(prod)PARAM$train_final else PARAM$train
  cache('seleccion',list(componente='filtro30_v1',rep=rec$rep$id,meses=meses,umbral=umbral,aux=auxp,seed=PARAM$semilla),function(){
   d<-leer_meses(rec$rep,meses);d[,y:=as.integer(clase_ternaria%in%c('BAJA+1','BAJA+2'))];set.seed(PARAM$semilla,kind="L'Ecuyer-CMRG")
   d[,fold:=sample(rep(c(rep(1L,7),rep(2L,3)),length.out=.N)),by=.(foto_mes,clase_ternaria)]
   i30<-which(d$fold==2L);i70<-which(d$fold==1L);stopifnot(length(i30)>0L,length(i70)>0L,length(intersect(i30,i70))==0L)
   pro<-rep(NA_real_,length(i70))
   if(!is.null(umbral)){
    pro<-cache('seleccion',list(componente='aux30_pred_v1',rep=rec$rep$id,meses=meses,aux=auxp,i30=hash(i30),i70=hash(i70)),function(){
     campos<-campos_receta(rec);dt<-lightgbm::lgb.Dataset(data.matrix(d[i30,..campos]),label=d$y[i30],free_raw_data=TRUE)
     m<-lightgbm::lgb.train(params=auxp,data=dt,nrounds=2180L);p<-predict(m,data.matrix(d[i70,..campos]));rm(dt,m);gc(FALSE);p
    })
   }
   keep<-if(is.null(umbral))rep(TRUE,length(i70)) else d$y[i70]==1L|pro<umbral
   aud<-data.table(numero_de_cliente=d$numero_de_cliente[i70],foto_mes=d$foto_mes[i70],y=d$y[i70],prob_aux=pro,conservar=keep)
   csv(aud,file.path(RAIZ,'seleccion',paste0('FILTRO_',if(prod)'prod' else 'hist','_',if(is.null(umbral))'control' else umbral,'.csv')))
   list(key=paste(d$numero_de_cliente[i70][keep],d$foto_mes[i70][keep]),y=d$y[i70][keep])
  })
 })
}
# Dejo anotado: Pseudoetiquetas: mantengo el target verdadero separado; las etiquetas auxiliares nunca son predictors.
planes_pseudo<-function(rec,hp_docente){
 lapply(setNames(c(FALSE,TRUE),c('hist','prod')),function(prod){
  reales<-if(prod)PARAM$train_final else PARAM$train;artificiales<-if(prod)c(202107L,202108L) else c(202105L,202106L)
  cache('seleccion',list(componente='pseudo_v1',rep=rec$rep$id,reales=reales,artificiales=artificiales,hp=hp_docente,corte=PARAM$pseudo_umbral,seed=PARAM$semilla),function(){
   meta<-materializar(rec,prod,FALSE);dt<-lightgbm::lgb.Dataset(meta$bin,params=list(max_bin=31L,feature_pre_filter=FALSE));p<-modifyList(par_base(PARAM$semilla),hp_docente)
   m<-lightgbm::lgb.train(params=p,data=dt,nrounds=p$num_iterations)
   a<-leer_meses(rec$rep,artificiales);pr<-predict(m,data.matrix(a[,meta$campos,with=FALSE]));yr<-as.integer(pr>=PARAM$pseudo_umbral)
   r<-leer_meses(rec$rep,reales)
   csv(data.table(numero_de_cliente=a$numero_de_cliente,foto_mes=a$foto_mes,prob_docente=pr,etiqueta_artificial=yr),file.path(RAIZ,'seleccion',paste0('PSEUDO_',if(prod)'prod' else 'hist','_',rec$rep$id,'.csv')))
   list(key=c(paste(r$numero_de_cliente,r$foto_mes),paste(a$numero_de_cliente,a$foto_mes)),y=c(as.integer(r$clase_ternaria%in%c('BAJA+1','BAJA+2')),yr),meses=c(reales,artificiales))
  })
 })
}
resumir_ensemble<-function(resultados,nombre){
 stopifnot(length(resultados)>0L)
 ref<-resultados[[1L]]$scores;ids<-ref$numero_de_cliente
 for(z in resultados)stopifnot(identical(z$scores$numero_de_cliente,ids),identical(z$scores$foto_mes,ref$foto_mes))
 modos<-c('raw','prob');out<-list()
 for(modo in modos){
  s<-Reduce('+',lapply(resultados,function(z)z$scores[[modo]]))/length(resultados)
  tab<-copy(ref[,.(numero_de_cliente,foto_mes,clase_ternaria)]);tab[,score:=s]
  id<-hash(list(modelos=vapply(resultados,`[[`,character(1),'id'),modo=modo));dest<-file.path(RAIZ,'scores',paste0('ENS_',id,'.rds'));atomic(list(nombre=nombre,modo=modo,modelos=vapply(resultados,`[[`,character(1),'id'),tabla=tab),dest)
  gan<-NA_real_
  if(unique(tab$foto_mes)==202106L){
   cv<-curva(ids,s,tab$clase_ternaria);gan<-cv[corte==PARAM$corte,ganancia];csv(cv,file.path(RAIZ,'resultados',paste0('CURVA_ENS_',id,'.csv')))
   png(file.path(RAIZ,'figuras',paste0('GANANCIA_',id,'.png')),width=1300,height=750)
   plot(cv$corte,cv$ganancia,type='l',xlim=c(8000,15000),xlab='Clientes contactados',ylab='Ganancia junio (millones)',main=paste(nombre,modo));abline(v=PARAM$corte,lty=2);dev.off()
   cortes<-cv[corte%in%seq(8000L,15000L,250L)]
   cortes[,ganancia_media_vecindad:=vapply(corte,function(k)mean(cv[corte>=k-250L & corte<=k+250L,ganancia]),numeric(1))]
   csv(cortes,file.path(RAIZ,'resultados',paste0('MESETA_',id,'.csv')))
  }
  out[[modo]]<-list(id=id,nombre=nombre,modo=modo,tabla=tab,ganancia=gan,modelos=vapply(resultados,`[[`,character(1),'id'))
 }
 out
}
confirmar<-function(rec,bo){
 res<-list()
 for(j in seq_along(bo$optimos)){
  hp<-bo$optimos[[j]];xs<-lapply(SEMILLAS,function(s)fit_score(rec,hp,s))
  en<-resumir_ensemble(xs,paste0(rec$nombre,'_HP',j))
  resumen<-data.table(receta=rec$nombre,configuracion=j,n_semillas=length(xs),media=mean(vapply(xs,`[[`,numeric(1),'ganancia')),sd=sd(vapply(xs,`[[`,numeric(1),'ganancia')),ensemble_raw=en$raw$ganancia,ensemble_prob=en$prob$ganancia)
  csv(resumen,file.path(RAIZ,'resultados',paste0('CONF_',rec$nombre,'_',j,'.csv')))
  # Dejo anotado: Gráfico conjunto: ganancias individuales y ganancia del promedio de predicciones.
  png(file.path(RAIZ,'figuras',paste0('SEMILLAS_',rec$nombre,'_',j,'.png')),width=1300,height=750)
  cr<-curva(en$raw$tabla$numero_de_cliente,en$raw$tabla$score,en$raw$tabla$clase_ternaria);plot(cr$corte,cr$ganancia,type='n',xlim=c(8000,15000),xlab='Contactos',ylab='Ganancia junio (millones)',main=paste(rec$nombre,'configuración',j))
  for(x in xs){ci<-curva(x$scores$numero_de_cliente,x$scores$prob,x$scores$clase_ternaria);lines(ci$corte,ci$ganancia,col=adjustcolor('grey40',.4))};lines(cr$corte,cr$ganancia,col='blue',lwd=2);dev.off()
  res[[as.character(j)]]<-list(receta=rec$nombre,hp=hp,individuales=xs,ensemble=en,resumen=resumen)
 }
 res
}
submit<-function(en,etiqueta,corte=PARAM$corte){
 d<-copy(en$tabla);stopifnot(all(d$foto_mes==202108L),nrow(d)>=corte,all(is.finite(d$score)),!anyDuplicated(d$numero_de_cliente))
 setorder(d,-score,numero_de_cliente);d[,ranking:=.I]
 csv(d,file.path(RAIZ,'resultados',paste0('SCORES_AGOSTO_',etiqueta,'.csv')))
 dest<-file.path(RAIZ,'submits',paste0('submit_',etiqueta,'_',corte,'.csv'))
 # Dejo anotado: Formateo IDs sin convertirlos a integer de 32 bits ni notación científica.
 ids<-format(d$numero_de_cliente[seq_len(corte)],scientific=FALSE,trim=TRUE,digits=22)
 stopifnot(length(ids)==corte,!anyDuplicated(ids),all(grepl('^[0-9]+$',ids)))
 writeLines(ids,paste0(dest,'.tmp'));if(!file.rename(paste0(dest,'.tmp'),dest))stop('No pude guardar submit')
 stopifnot(identical(readLines(dest),ids));log_('SUBMIT VERIFICADO',dest)
 data.table(nombre=etiqueta,corte=corte,archivo=dest,ensemble=en$id,public_gain=NA_real_,estado='NO ENVIADO; evaluación externa pendiente')
}
# Dejo anotado: Reviso todas las variables originales mediante ablaciones de su familia (nivel + históricos).
# Primero filtro con parámetros fijos; después ajusto sólo las mejores, sin confundir ambas pruebas.
ablaciones<-function(rec,bo){
 hp<-bo$optimos[[1]];base<-fit_score(rec,hp,PARAM$semilla)$ganancia
 variables<-sort(unique(sub('_(delta_)?lag[12]$','',campos_receta(rec))))
 tab<-rbindlist(lapply(variables,function(v){rr<-rec;rr$nombre<-paste0('DROP_',v);rr$excluir<-v;x<-fit_score(rr,hp,PARAM$semilla);data.table(variable=v,ganancia=x$ganancia,delta=x$ganancia-base,modelo=x$id)}))
 setorder(tab,-delta,variable);csv(tab,file.path(RAIZ,'resultados',paste0('ABLACIONES_',rec$nombre,'.csv')))
 mejores<-tab[delta>0,head(variable,PARAM$ablaciones_top)]
 candidatos<-list()
 for(v in mejores){rr<-rec;rr$nombre<-paste0(rec$nombre,'_SIN_',v);rr$excluir<-v;candidatos[[rr$nombre]]<-rr}
 if(length(mejores)>1L){rr<-rec;rr$nombre<-paste0(rec$nombre,'_SIN_CONJUNTO');rr$excluir<-mejores;candidatos[[rr$nombre]]<-rr}
 candidatos
}
# Audito cambio de distribución de TODOS los campos numéricos, sin usar la baja como filtro.
auditar_drift<-function(d){
 vars<-setdiff(names(d),c('numero_de_cliente','foto_mes','clase_ternaria'));out<-rbindlist(lapply(vars,function(v){d[,{
  x<-get(v);q<-if(all(is.na(x)))rep(NA_real_,3) else as.numeric(quantile(x,c(.25,.50,.75),na.rm=TRUE));list(n=.N,missing=mean(is.na(x)),ceros=mean(x==0,na.rm=TRUE),p25=q[1],mediana=q[2],p75=q[3],unicos=uniqueN(x,na.rm=TRUE))
 },by=foto_mes][,variable:=v]}))
 csv(out,file.path(RAIZ,'auditoria','DISTRIBUCIONES_TODAS_VARIABLES.csv'));out
}
stopifnot(identical(rank_cero(c(NA,0,-20,-10,10,20,20)),c(NA_real_,0,-1,-.5,1/3,5/6,5/6)))
