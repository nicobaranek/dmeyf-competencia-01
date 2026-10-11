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

# Dejo anotado: CELDA 05 — Audito evidencia existente, cargo el crudo y verifico el target.
INVENTARIO <- inventario_previo()
D <- cargar_base()
print(D[,.N,by=.(foto_mes,clase_ternaria)][order(foto_mes,clase_ternaria)])
DRIFT <- cache('auditoria',list(componente='drift_todos_v1',raw=RAW_HASH),function()auditar_drift(D))
cat('El target de training es BAJA+1/BAJA+2; la ganancia remunera solamente BAJA+2.\n')
cat('Julio no ofrece H2 completo; agosto no se evalúa con etiquetas reconstruidas.\n')
# Dejo anotado: CELDA 06 — Materializo sólo las representaciones activas.
REP <- list()
if(PARAM$ejecutar_original) REP$ORIGINAL <- preparar_features(D,FALSE)
if(PARAM$rankings) REP$RANK_MONTOS_CERO <- preparar_features(D,TRUE)
# Dejo anotado: La matriz incluye préstamos; los retiro al crear el dataset de BO, no destruyo sus columnas.
rm(D);gc(full=TRUE)
cat('Representaciones disponibles:',paste(names(REP),collapse=', '),'\n')
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
if(Sys.getenv("DMEYF_INTEGRAL_BACKGROUND")=="1" && Sys.getenv("DMEYF_INTEGRAL_AUTORUN")!="1") {
 cat("Esta celda se ejecuta en el Rscript integral. Monitorear CELDA26.\n")
} else {
# Dejo anotado: CELDA 19 — Motor integral. Conservo las funciones y firmas de los controles previos.
suppressPackageStartupMessages(library(data.table))
CFG <- jsonlite::fromJSON(Sys.getenv('DMEYF_INTEGRAL_CONFIG'),simplifyVector=TRUE)
stopifnot(CFG$version=='INTEGRAL_CLASE_20261008_v1',length(CFG$recetas)>0L,
          'ARTIFICIALES_ORIGINAL'%in%CFG$recetas,CFG$n_semillas>=20L,
          CFG$bo_iniciales<CFG$bo_evaluaciones,CFG$pseudo_regla%in%c('top_k','prevalencia','bernoulli'))
DIR_INT <- file.path(RAIZ,'integral_clase_20261008',hash(CFG))
for(s in c('planes','historia','produccion','lotes','auditoria','publico','entrega'))
 dir.create(file.path(DIR_INT,s),recursive=TRUE,showWarnings=FALSE)
writeLines(DIR_INT,file.path(RAIZ,'DIR_INTEGRAL_ACTUAL.txt'))
atomic(CFG,file.path(DIR_INT,'PLAN_CONGELADO.rds'))
writeLines(toJSON(CFG,pretty=TRUE,auto_unbox=TRUE),file.path(DIR_INT,'PLAN_CONGELADO.json'))
PARAM$bo_evaluaciones <- as.integer(CFG$bo_evaluaciones)
PARAM$bo_iniciales <- as.integer(CFG$bo_iniciales)
PARAM$top_configuraciones <- 1L
PARAM$corte <- 11500L # Comparabilidad con BO/controles terminados; los cortes públicos son independientes.
PARAM$threads <- as.integer(CFG$threads)
setDTthreads(PARAM$threads)
if(Sys.getenv('DMEYF_INTEGRAL_AUTORUN')=='1') {
 INTEGRAL_LOCK <- filelock::lock(file.path(LOCK_DIR,'integral_clase.lock'),timeout=0)
 if(is.null(INTEGRAL_LOCK))stop('Otra corrida integral sigue activa. No entreno en paralelo la misma RAIZ.')
}
es_primo_int <- function(n) {
 if(n<2L)return(FALSE)
 if(n==2L)return(TRUE)
 !any(n%%seq.int(2L,as.integer(floor(sqrt(n))))==0L)
}
BASE_SEEDS <- as.integer(c(100613,100267,101611,102481,103651,104729,105019,105613,106013,106619))
SEEDS_INT <- BASE_SEEDS
ns <- max(SEEDS_INT)+1L
while(length(SEEDS_INT)<CFG$n_semillas) {
 if(es_primo_int(ns))SEEDS_INT<-c(SEEDS_INT,ns)
 ns<-ns+1L
}
stopifnot(!anyDuplicated(SEEDS_INT))
csv(data.table(semilla=SEEDS_INT,origen=ifelse(SEEDS_INT%in%BASE_SEEDS,'PREVIA','PRIMO_CONSECUTIVO')),
 file.path(DIR_INT,'planes','SEMILLAS.csv'))
# Dejo anotado: Las diez nuevas son una elección reproducible de esta implementación, no una lista dictada en clase.
INT_ESTADO <- data.table(
 id=c('AUDITORIA','CONTROL_H','RANK_H','PSEUDO_H','EXTRAS','CONTROL_P','RANK_P','PSEUDO_P','LOTES','PUBLICO','FINAL','GITHUB'),
 titulo=c('DQ y distribuciones','Control: junio','Ranking completo: junio','Artificiales: junio','Experimentos adicionales',
 'Control: agosto','Ranking: agosto','Artificiales: agosto','CSV y ensambles','Evaluación externa','Entrega final','Código reproducible'),
 depende=c('','AUDITORIA','AUDITORIA','CONTROL_H','CONTROL_H','CONTROL_H','RANK_H','PSEUDO_H','CONTROL_P,RANK_P,PSEUDO_P','LOTES','PUBLICO','FINAL'),
 estado='PENDIENTE',detalle='')
esc_html <- function(s) {
 s<-gsub('&','&amp;',as.character(s),fixed=TRUE)
 s<-gsub('<','&lt;',s,fixed=TRUE);gsub('>','&gt;',s,fixed=TRUE)
}
mapa_int <- function() {
 path<-file.path(DIR_INT,'ESTADO_EXPERIMENTOS.csv');csv(INT_ESTADO,path)
 d<-copy(INT_ESTADO)
 for(i in seq_len(nrow(d)))if(d$estado[i]=='PENDIENTE'&&nzchar(d$depende[i])) {
  dep<-strsplit(d$depende[i],',',fixed=TRUE)[[1L]]
  if(any(!INT_ESTADO[id%in%dep,estado]%in%c('TERMINADO','EXITO_HISTORICO','SIN_MEJORA','NO_ACTIVADO')))
   d$estado[i]<-'BLOQUEADO'
 }
 colores<-c(PENDIENTE='#f1f5f9',BLOQUEADO='#e2e8f0',EN_CURSO='#fef08a',TERMINADO='#bbf7d0',
  EXITO_HISTORICO='#86efac',SIN_MEJORA='#fed7aa',ERROR='#fecaca',NO_ACTIVADO='#ddd6fe')
 pos<-data.table(id=d$id,x=rep(c(15,315,615),each=4),y=rep(c(20,125,230,335),3))
 svg<-'<svg viewBox="0 0 915 455" xmlns="http://www.w3.org/2000/svg"><defs><marker id="a" markerWidth="8" markerHeight="8" refX="7" refY="4" orient="auto"><path d="M0,0 L8,4 L0,8" fill="#64748b"/></marker></defs>'
 for(i in seq_len(nrow(d)))if(nzchar(d$depende[i]))for(dep in strsplit(d$depende[i],',',fixed=TRUE)[[1L]]) {
  a<-pos[id==dep];b<-pos[id==d$id[i]]
  svg<-paste0(svg,sprintf('<path d="M %d %d L %d %d" stroke="#94a3b8" fill="none" marker-end="url(#a)"/>',a$x+135,a$y+75,b$x+135,b$y))
 }
 for(i in seq_len(nrow(d))) {
  p<-pos[id==d$id[i]];col<-unname(colores[d$estado[i]])
  svg<-paste0(svg,sprintf('<rect x="%d" y="%d" width="270" height="75" rx="8" fill="%s" stroke="#475569"/><text x="%d" y="%d" font-size="14">%s</text><text x="%d" y="%d" font-size="11">%s</text>',
    p$x,p$y,col,p$x+10,p$y+24,esc_html(d$titulo[i]),p$x+10,p$y+50,esc_html(d$estado[i])))
 }
 rows<-paste(vapply(seq_len(nrow(d)),function(i)paste0('<tr><td>',esc_html(d$titulo[i]),'</td><td>',esc_html(d$estado[i]),'</td><td>',esc_html(d$detalle[i]),'</td></tr>'),character(1)),collapse='')
 html<-paste0('<!doctype html><meta charset="utf-8"><title>Mapa de experimentos</title><style>body{font:16px system-ui;margin:24px;max-width:1100px}svg{width:100%}table{border-collapse:collapse;width:100%}td,th{padding:9px;border-bottom:1px solid #ddd;text-align:left}</style><h1>Experimentos y dependencias</h1><p>Actualizado: ',esc_html(Sys.time()),'. Verde: terminado; naranja: sin mejora histórica; amarillo: en curso; gris: bloqueado; rojo: error. La ganancia pública se registra por separado.</p>',svg,'</svg><table><tr><th>Experimento</th><th>Estado</th><th>Resultado</th></tr>',rows,'</table>')
 writeLines(html,file.path(DIR_INT,'MAPA_EXPERIMENTOS.html'))
}
# Evito colisión de nombres en data.table mediante selección por índice.
estado_int <- function(nodo,nuevo,nota='') {
 ii<-which(INT_ESTADO$id==nodo);stopifnot(length(ii)==1L)
 set(INT_ESTADO,ii,'estado',nuevo);set(INT_ESTADO,ii,'detalle',nota);mapa_int()
}
PASO_ACTUAL <- 'AUDITORIA'
paso_int <- function(nodo,fun) {
 PASO_ACTUAL<<-nodo;estado_int(nodo,'EN_CURSO');log_('INTEGRAL INICIO',nodo)
 tryCatch({v<-fun();estado_int(nodo,'TERMINADO');log_('INTEGRAL FIN',nodo);v},error=function(e){
  estado_int(nodo,'ERROR',conditionMessage(e));atomic(list(nodo=nodo,error=conditionMessage(e),fecha=Sys.time()),file.path(DIR_INT,'ERROR_ULTIMO.rds'))
  log_('INTEGRAL ERROR',nodo,conditionMessage(e));stop(e)
 })
}
# Dejo anotado: Nunca doy por compatibles los controles por su nombre: comparo firma, semilla, parámetros y representación.
rec_control_int <- RECETAS[['ORIGINAL_LAG2']]
stopifnot(!is.null(rec_control_int),PARAM$orden_lags==2L,is.null(rec_control_int$planes))
rec_control_int$excluir<-character();rec_control_int$seleccion<-NULL
rec_control_int$estrategia<-'COMPLETO'
control_ens_path <- file.path(RAIZ,'scores','ENS_7e4d3206f7971dba787743c06ea0add795129842997372e078bf007813ff6e98.rds')
if(!file.exists(control_ens_path))stop('Falta el control histórico auditado. No lo recreo silenciosamente.')
control_ens_int <- readRDS(control_ens_path)
meta_int <- materializar(rec_control_int,FALSE,FALSE)
control_previo_int <- lapply(control_ens_int$modelos,function(mid) {
 z<-readRDS(file.path(RAIZ,'scores',paste0(mid,'.rds')));v<-z$valor
 esperado<-list(componente='fit_v1',dataset=meta_int$firma,pars=modifyList(par_base(v$seed),v$hp),
   seed=v$seed,score_mes=202106L,score_rep=rec_control_int$rep$id)
 if(!identical(z$firma,esperado)||!file.exists(v$modelo))
  stop('Control incompatible: ',mid,'. No repito entrenamiento anterior; revisar versión/plan antes de continuar.')
 v
})
HP_DOCENTE_INT <- control_previo_int[[1L]]$hp
stopifnot(length(control_previo_int)==10L,
 setequal(vapply(control_previo_int,`[[`,integer(1),'seed'),BASE_SEEDS),
 all(vapply(control_previo_int,function(z)identical(z$hp,HP_DOCENTE_INT),logical(1))))
REC_INT <- list(CONTROL_ORIGINAL=rec_control_int)
HIST_INT <- list();PROD_INT<-list()

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
mapa_int()
log_('PLAN INTEGRAL',DIR_INT,'recetas',paste(CFG$recetas,collapse=','),'semillas',length(SEEDS_INT))
# Dejo anotado: Pruebas sin entrenamiento: ranking, etiquetas sin target futuro y alineación de ensambles por ID.
stopifnot(identical(rank_cero(c(NA,0,-20,-10,10,20,20)),c(NA_real_,0,-1,-.5,1/3,5/6,5/6)))
local({
 t<-data.table(numero_de_cliente=c(3L,1L,2L,4L),foto_mes=202105L,prob=c(.2,.9,.8,.1))
 rr<-data.table(clase_ternaria=c('CONTINUA','BAJA+1','CONTINUA','BAJA+2'))
 a<-etiquetar_int(t,rr,'top_k',2L,100613L)
 stopifnot(setequal(a[etiqueta_artificial==1L,numero_de_cliente],c(1L,2L)))
 b<-etiquetar_int(t,rr,'bernoulli',2L,100613L)
 stopifnot(identical(b$etiqueta_artificial,etiquetar_int(t,rr,'bernoulli',2L,100613L)$etiqueta_artificial))
 fake<-function(ids,p,s)list(id=paste0('SELFTEST_',s),seed=as.integer(s),scores=data.table(numero_de_cliente=as.integer(ids),foto_mes=202108L,clase_ternaria=NA_character_,prob=p))
 z<-promediar_prob_int(list(fake(c(1,2),c(.2,.8),1),fake(c(2,1),c(.4,.6),2)),'SELFTEST')
 stopifnot(isTRUE(all.equal(z$en$tabla$score,c(.4,.6))))
 # Dejo anotado: El selftest no se registra como modelo entrenado ni como candidato.
 unlink(file.path(RAIZ,'scores',paste0('ENS_',z$en$id,'.rds')))
})
csv(data.table(prueba=c('rank_cero_signo_NA','pseudo_top_k','pseudo_azar_reproducible','ensamble_alineado_por_ID'),estado='OK'),
 file.path(DIR_INT,'auditoria','PRUEBAS_FUNCIONALES.csv'))

}

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


CFG_COMPLETA <- CFG

for(RAMA_INTEGRAL in CFG_COMPLETA$recetas) {

 CFG$recetas <- RAMA_INTEGRAL; CFG$extras <- FALSE

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


}

CFG <- CFG_COMPLETA

if(isTRUE(CFG$extras)) {

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


}

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


if(exists("INTEGRAL_LOCK"))filelock::unlock(INTEGRAL_LOCK)

cat("\nCORRIDA INTEGRAL TERMINADA\n")
