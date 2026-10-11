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
