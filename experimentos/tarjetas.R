
.libPaths(unique(c("/home/ds/.local/lib/R/site-library",.libPaths())))
library(data.table)
library(lightgbm)
setDTthreads(1L)
out <- Sys.getenv("DMEYF_STATUS_OUT")
lk <- filelock::lock(file.path(Sys.getenv("DMEYF_LAB_BASE"),".dmeyf_status_training.lock"),timeout=0)
if(is.null(lk))stop("Otro proceso de tarjetas está activo.")

log <- function(...) {
 cat(format(Sys.time()),...,"\n")
 flush.console()
}
atomic <- function(x,p) {
 tmp <- paste0(p,".tmp")
 saveRDS(x,tmp,compress=FALSE)
 if(!file.rename(tmp,p))stop("No pude guardar ",p)
}
guardar <- function(x,n) fwrite(x,file.path(out,n))
raiz <- Sys.getenv("DMEYF_LAB_RAIZ")
previo <- file.path(raiz,"integral_clase_20261008",
 "0b6b06569ceee877ea42e00a072112f6b536e82c366f16427290dbd4457a2440")
h <- readRDS(file.path(previo,"historia","CIERRE_CONTROL_ORIGINAL.rds"))
seeds <- as.integer(h$plan$semillas)[1:10]
stopifnot(length(seeds)==10L,!anyDuplicated(seeds))

controles <- lapply(h$modelos,function(id)
 readRDS(file.path(raiz,"scores",paste0(id,".rds"))))
ss <- vapply(controles,function(z)as.integer(z$valor$seed),integer(1))
ii <- match(seeds,ss)
stopifnot(!anyNA(ii))
controles <- controles[ii]
base <- controles[[1]]
campos <- base$firma$dataset$campos
rep_id <- base$firma$dataset$representacion
versiones <- list(
 R=as.character(getRversion()),
 lightgbm=as.character(packageVersion("lightgbm")),
 data.table=as.character(packageVersion("data.table"))
)
stopifnot(identical(versiones,base$firma$dataset$versiones),
          versiones$lightgbm=="4.7.0.99",
          identical(as.integer(base$firma$dataset$meses),c(202103L,202104L)),
          !any(grepl("^(cprestamos_personales|mprestamos_personales)",campos)))
for(z in controles) {
 stopifnot(identical(z$firma$dataset,base$firma$dataset),
           identical(z$valor$hp,base$valor$hp),
           identical(z$firma$pars$num_threads,6L),
           z$valor$mes==202106L)
}

raw <- Sys.getenv("DMEYF_DATOS")
stopifnot(unname(tools::md5sum(raw))==base$firma$dataset$raw)
d <- fread(raw,select=c("numero_de_cliente","foto_mes",
                        "Visa_status","Master_status"),nThread=1L)
stopifnot(!anyDuplicated(d[,.(numero_de_cliente,foto_mes)]))
mescal <- 12L*(d$foto_mes%/%100L)+d$foto_mes%%100L
keys <- paste(d$numero_de_cliente,mescal,sep=":")
f <- d[,.(numero_de_cliente,foto_mes)]

# Dejo anotado: Solo datos del mes actual y anteriores; no utiliza etiquetas.
for(lag in 1:2) {
 ix <- match(paste(d$numero_de_cliente,mescal-lag,sep=":"),keys)
 existe <- !is.na(ix)
 f[,(paste0("ST_historia_L",lag)):=as.integer(existe)]
 for(v in c("Visa_status","Master_status")) {
  actual <- d[[v]]
  anterior <- actual[ix]
  marca <- if(v=="Visa_status")"VISA" else "MASTER"
  eventos <- list(
   paso_NA=!is.na(anterior) & is.na(actual),
   salida_NA=is.na(anterior) & !is.na(actual),
   NA_persistente=is.na(anterior) & is.na(actual),
   cambio=!is.na(anterior) & !is.na(actual) & anterior!=actual,
   estable=!is.na(anterior) & !is.na(actual) & anterior==actual
  )
  for(n in names(eventos)) {
   valor <- as.integer(eventos[[n]])
   valor[!existe] <- NA_integer_
   set(f,j=paste0("ST_",marca,"_L",lag,"_",n),value=valor)
  }
  # Auditoría de los lags utilizados al crear estos indicadores.
  set(f,j=paste0("AUD_",v,"_lag",lag),value=anterior)
 }
 a <- f[[paste0("ST_VISA_L",lag,"_paso_NA")]]
 b <- f[[paste0("ST_MASTER_L",lag,"_paso_NA")]]
 set(f,j=paste0("ST_ambas_paso_NA_L",lag),value=as.integer(a==1L & b==1L))
 set(f,j=paste0("ST_alguna_paso_NA_L",lag),value=as.integer(a==1L | b==1L))
}
f[,ST_ambas_NA_actual:=as.integer(is.na(d$Visa_status) & is.na(d$Master_status))]
nuevos <- grep("^ST_",names(f),value=TRUE)
stopifnot(!any(nuevos %in% campos))
guardar(data.table(campo=nuevos),"FEATURES_NUEVAS.csv")
key_f <- paste(f$numero_de_cliente,f$foto_mes,sep=":")

leer <- function(m) {
 z <- readRDS(file.path(raiz,"features",rep_id,paste0(m,".rds")))
 stopifnot(z$mes==m,z$firma$raw==base$firma$dataset$raw)
 t <- as.data.table(z$datos)
 stopifnot(all(t$foto_mes==m),all(campos %in% names(t)),
           !anyNA(t$clase_ternaria))
 idx <- match(paste(t$numero_de_cliente,t$foto_mes,sep=":"),key_f)
 stopifnot(!anyNA(idx))
 for(v in c("Visa_status","Master_status"))
  for(lag in 1:2)
   stopifnot(isTRUE(all.equal(
    as.numeric(t[[paste0(v,"_lag",lag)]]),
    as.numeric(f[[paste0("AUD_",v,"_lag",lag)]][idx]),
    check.attributes=FALSE)))
 for(n in nuevos)set(t,j=n,value=f[[n]][idx])
 t
}
log("VALIDANDO BLOQUES Y TRANSICIONES")
train <- rbindlist(lapply(c(202103L,202104L),leer),use.names=TRUE)
valid <- leer(202106L)
stopifnot(nrow(train)==326184L,nrow(valid)==164114L,
          sum(valid$clase_ternaria=="BAJA+2")==1098L)
y <- as.integer(train$clase_ternaria %in% c("BAJA+1","BAJA+2"))
stopifnot(sum(y)==4082L)
todos <- c(campos,nuevos)
x <- data.matrix(train[,..todos])
xv <- data.matrix(valid[,..todos])
ref <- valid[,.(numero_de_cliente,foto_mes,clase_ternaria)]

# Compruebo que las columnas originales reproducen al control guardado.
modelo_control <- lgb.load(base$valor$modelo)
pc <- predict(modelo_control,xv[,seq_along(campos),drop=FALSE],
              num_threads=1L)
sc <- base$valor$scores
idx <- match(ref$numero_de_cliente,sc$numero_de_cliente)
stopifnot(!anyNA(idx),max(abs(pc-sc$prob[idx]))<1e-10)
rm(modelo_control,pc,sc,train,valid,d,f)
gc(verbose=FALSE)

plan <- list(
 experimento="STATUS_TRANSICIONES_H_V1",semillas=seeds,
 train=c(202103L,202104L),valid=202106L,
 target="BAJA+1/BAJA+2=1; CONTINUA=0",
 ganancia="BAJA+2:1072500; resto:-27500",
 campos_originales=campos,campos_nuevos=nuevos,
 hp=base$valor$hp,threads=1L,threads_control=6L,
 versiones=versiones,raw=base$firma$dataset$raw,
 modelos_control=vapply(controles,function(z)z$valor$id,character(1)),
 codigo=digest::digest(file=Sys.getenv("DMEYF_STATUS_SCRIPT"),algo="sha256")
)
pp <- file.path(out,"PLAN.rds")
if(file.exists(pp))stopifnot(identical(readRDS(pp),plan)) else atomic(plan,pp)

log("DATOS VERIFICADOS. FEATURES NUEVAS",length(nuevos),
    "SEMILLAS",length(seeds),"THREADS 1")
ds <- lgb.Dataset(x,label=y,free_raw_data=TRUE,
 params=list(max_bin=31L,feature_pre_filter=FALSE,num_threads=1L))
ds$construct()
rm(x);gc(verbose=FALSE)

evaluar <- function(p,tipo,seed) {
 o <- order(-p,ref$numero_de_cliente)
 cv <- cumsum(ifelse(ref$clase_ternaria[o]=="BAJA+2",1072500,-27500))/1e6
 data.table(tipo=tipo,semilla=seed,K10500=cv[10500L],
            meseta_10000_11500=mean(cv[10000:11500]),
            meseta_10250_10750=mean(cv[10250:10750]))
}
P <- matrix(NA_real_,nrow(ref),length(seeds))
C <- P
resultados <- list()
for(j in seq_along(seeds)) {
 s <- seeds[j]
 control <- controles[[j]]
 sc <- control$valor$scores
 idx <- match(ref$numero_de_cliente,sc$numero_de_cliente)
 stopifnot(!anyNA(idx),
  identical(as.character(ref$clase_ternaria),as.character(sc$clase_ternaria[idx])))
 C[,j] <- sc$prob[idx]
 pars <- control$firma$pars
 pars$num_threads <- 1L
 sig <- list(plan=plan,seed=s,pars=pars)
 sp <- file.path(out,paste0("SCORES_",s,".rds"))
 mp <- file.path(out,paste0("MODELO_",s,".txt"))
 fp <- file.path(out,paste0("FIRMA_",s,".rds"))
 if(file.exists(sp)) {
  z <- readRDS(sp)
  stopifnot(identical(z$firma,sig),file.exists(mp))
  p <- z$prob
  log("REUTILIZO",s)
 } else {
  inicio <- Sys.time()
  if(file.exists(mp) && file.exists(fp)) {
   stopifnot(identical(readRDS(fp),sig))
   m <- lgb.load(mp)
   log("RECUPERO MODELO",s)
  } else {
   log("ENTRENO TARJETAS",s,"MODELO",j,"/ 10")
   m <- lgb.train(params=pars,data=ds,nrounds=pars$num_iterations)
   lgb.save(m,paste0(mp,".tmp"))
   stopifnot(file.rename(paste0(mp,".tmp"),mp))
   atomic(sig,fp)
  }
  p <- predict(m,xv,num_threads=1L)
  stopifnot(length(p)==nrow(ref),all(is.finite(p)),all(p>=0 & p<=1))
  atomic(list(firma=sig,prob=p,ids=ref$numero_de_cliente),sp)
  guardar(as.data.table(lgb.importance(m)),paste0("IMPORTANCIA_",s,".csv"))
  rm(m);gc(verbose=FALSE)
  log("GUARDADO TARJETAS",s,"MINUTOS",
      round(as.numeric(difftime(Sys.time(),inicio,units="mins")),1))
 }
 stopifnot(length(p)==nrow(ref),all(is.finite(p)))
 P[,j] <- p
 resultados[[length(resultados)+1L]] <- evaluar(C[,j],"CONTROL",s)
 resultados[[length(resultados)+1L]] <- evaluar(P[,j],"TARJETAS",s)
 tabla <- rbindlist(resultados)
 guardar(tabla,"RESULTADOS_PARCIALES.csv")
 pares <- merge(tabla[tipo=="CONTROL"],tabla[tipo=="TARJETAS"],
                by="semilla",suffixes=c("_control","_tarjetas"))
 pares[,delta_K10500:=K10500_tarjetas-K10500_control]
 pares[,delta_meseta:=meseta_10000_11500_tarjetas-meseta_10000_11500_control]
 guardar(pares,"DIFERENCIAS_PAREADAS.csv")
 print(pares[,.(semilla,delta_K10500,delta_meseta)])
}
comparacion <- rbindlist(list(
 evaluar(rowMeans(C),"CONTROL_PROB10",0L),
 evaluar(rowMeans(P),"TARJETAS_PROB10",0L)))
guardar(comparacion,"COMPARACION_ENSAMBLES.csv")
atomic(list(plan=plan,comparacion=comparacion,
           semillas=rbindlist(resultados),ref=ref,
           prob_control=rowMeans(C),prob_tarjetas=rowMeans(P)),
       file.path(out,"TERMINADO.rds"))
print(comparacion)
log("TARJETAS HISTORICO 10 SEMILLAS TERMINADO. AGOSTO NO EJECUTADO.")
filelock::unlock(lk)
