
.libPaths(unique(c("/home/ds/.local/lib/R/site-library",.libPaths())))
library(data.table)
library(lightgbm)
setDTthreads(1L)
out <- Sys.getenv("DMEYF_STATUS_OUT")
lk <- filelock::lock(file.path("/home/ds",".dmeyf_sin_cierres_training.lock"),timeout=0)
if(is.null(lk))stop("Otro proceso de sin_cierres está activo.")

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
raiz <- "/home/ds/buckets/b1/exp/DMEyF_MAESTRA_20261007"
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


nuevos <- character()
excluidos <- grep(
 "^(Visa|Master)_fultimo_cierre($|_)",
 campos,value=TRUE
)
stopifnot(length(campos)==752L,length(excluidos)==10L)
usados <- setdiff(campos,excluidos)
stopifnot(length(usados)==742L)
guardar(data.table(campo=excluidos),"FEATURES_EXCLUIDAS.csv")
guardar(data.table(campo=usados),"FEATURES_USADAS.csv")

leer <- function(m) {
 z <- readRDS(file.path(raiz,"features",rep_id,paste0(m,".rds")))
 stopifnot(z$mes==m,z$firma$raw==base$firma$dataset$raw)
 t <- as.data.table(z$datos)
 stopifnot(all(t$foto_mes==m),all(campos %in% names(t)),
           !anyNA(t$clase_ternaria),
           !anyDuplicated(t$numero_de_cliente))
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
              params=list(num_threads=1L))
sc <- base$valor$scores
idx <- match(ref$numero_de_cliente,sc$numero_de_cliente)
stopifnot(!anyNA(idx),max(abs(pc-sc$prob[idx]))<1e-10)
rm(modelo_control,pc,sc,train,valid)
# Primero reproduzco el control con sus 752 columnas.
# Recién entonces retiro las 10 columnas del experimento.
x <- x[,match(usados,todos),drop=FALSE]
xv <- xv[,match(usados,todos),drop=FALSE]
stopifnot(ncol(x)==742L,ncol(xv)==742L)
gc(verbose=FALSE)

plan <- list(
 experimento="SIN_CIERRES_H_V1",semillas=seeds,
 train=c(202103L,202104L),valid=202106L,
 target="BAJA+1/BAJA+2=1; CONTINUA=0",
 ganancia="BAJA+2:1072500; resto:-27500",
 campos_originales=campos,campos_excluidos=excluidos,campos_usados=usados,
 hp=base$valor$hp,threads=1L,threads_control=6L,
 versiones=versiones,raw=base$firma$dataset$raw,
 modelos_control=vapply(controles,function(z)z$valor$id,character(1)),
 codigo=digest::digest(file=Sys.getenv("DMEYF_STATUS_SCRIPT"),algo="sha256")
)
pp <- file.path(out,"PLAN.rds")
if(file.exists(pp))stopifnot(identical(readRDS(pp),plan)) else atomic(plan,pp)

log("DATOS VERIFICADOS. FEATURES CONSERVADAS",length(usados),
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
   log("ENTRENO SIN_CIERRES",s,"MODELO",j,"/ 10")
   m <- lgb.train(params=pars,data=ds,nrounds=pars$num_iterations)
   lgb.save(m,paste0(mp,".tmp"))
   stopifnot(file.rename(paste0(mp,".tmp"),mp))
   atomic(sig,fp)
  }
  p <- predict(m,xv,params=list(num_threads=1L))
  stopifnot(length(p)==nrow(ref),all(is.finite(p)),all(p>=0 & p<=1))
  atomic(list(firma=sig,prob=p,ids=ref$numero_de_cliente),sp)
  guardar(as.data.table(lgb.importance(m)),paste0("IMPORTANCIA_",s,".csv"))
  rm(m);gc(verbose=FALSE)
  log("GUARDADO SIN_CIERRES",s,"MINUTOS",
      round(as.numeric(difftime(Sys.time(),inicio,units="mins")),1))
 }
 stopifnot(length(p)==nrow(ref),all(is.finite(p)))
 P[,j] <- p
 resultados[[length(resultados)+1L]] <- evaluar(C[,j],"CONTROL",s)
 resultados[[length(resultados)+1L]] <- evaluar(P[,j],"SIN_CIERRES",s)
 tabla <- rbindlist(resultados)
 guardar(tabla,"RESULTADOS_PARCIALES.csv")
 pares <- merge(tabla[tipo=="CONTROL"],tabla[tipo=="SIN_CIERRES"],
                by="semilla",suffixes=c("_control","_sin_cierres"))
 pares[,delta_K10500:=K10500_sin_cierres-K10500_control]
 pares[,delta_meseta:=meseta_10000_11500_sin_cierres-meseta_10000_11500_control]
 guardar(pares,"DIFERENCIAS_PAREADAS.csv")
 print(pares[,.(semilla,delta_K10500,delta_meseta)])
}
comparacion <- rbindlist(list(
 evaluar(rowMeans(C),"CONTROL_PROB10",0L),
 evaluar(rowMeans(P),"SIN_CIERRES_PROB10",0L)))
guardar(comparacion,"COMPARACION_ENSAMBLES.csv")
atomic(list(plan=plan,comparacion=comparacion,
           semillas=rbindlist(resultados),ref=ref,
           prob_control=rowMeans(C),prob_sin_cierres=rowMeans(P)),
       file.path(out,"TERMINADO.rds"))
print(comparacion)
log("SIN_CIERRES HISTORICO 10 SEMILLAS TERMINADO. AGOSTO NO EJECUTADO.")
filelock::unlock(lk)
