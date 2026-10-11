
.libPaths(unique(c("/home/ds/.local/lib/R/site-library",.libPaths())))
library(data.table)
library(lightgbm)
library(digest)
setDTthreads(1L)
stopifnot(as.character(packageVersion("lightgbm"))=="4.7.0.99")
raiz <- "/home/ds/buckets/b1/exp/DMEyF_MAESTRA_20261007"
base <- "/home/ds/buckets/b1/exp/DMEyF_ENSAMBLES_20261010"
out <- file.path(base,"SIN_CIERRES_P_V1")
log <- function(...) {cat(format(Sys.time()),...,"\n");flush.console()}
h <- readRDS(file.path(base,"SIN_CIERRES_H_V1","TERMINADO.rds"))
viejo <- file.path(raiz,"integral_clase_20261008",
 "0b6b06569ceee877ea42e00a072112f6b536e82c366f16427290dbd4457a2440",
 "historia","CIERRE_CONTROL_ORIGINAL.rds")
control <- readRDS(viejo)
modelos <- lapply(control$modelos,function(id)
 readRDS(file.path(raiz,"scores",paste0(id,".rds"))))
seeds <- c(100613L,100267L,101611L,102481L,103651L,
           104729L,105019L,105613L,106013L,106619L)
buscar <- function(seed) {
 i <- which(vapply(modelos,function(z)z$valor$seed==seed,logical(1)))
 stopifnot(length(i)==1L)
 modelos[[i]]
}
primero <- buscar(seeds[1])
rep <- primero$firma$score_rep
campos_h <- primero$valor$campos
stopifnot(length(campos_h)==752L)
leer <- function(m) {
 p <- file.path(raiz,"features",rep,paste0(m,".rds"))
 stopifnot(file.exists(p))
 z <- readRDS(p)
 stopifnot(z$mes==m)
 as.data.table(z$datos)
}
tr <- rbindlist(lapply(c(202103L,202104L,202105L,202106L),leer))
va <- leer(202108L)
stopifnot(!anyDuplicated(tr[,.(numero_de_cliente,foto_mes)]),
          !anyNA(tr$clase_ternaria),
          nrow(va)==164647L,!anyDuplicated(va$numero_de_cliente))
prestamos <- grep("^(cprestamos_personales|mprestamos_personales)($|_)",
                 names(tr),value=TRUE)
cierres <- grep("^(Visa_fultimo_cierre|Master_fultimo_cierre)($|_)",
               campos_h,value=TRUE)
stopifnot(length(prestamos)==10L,length(cierres)==10L)
campos <- setdiff(unique(c(campos_h,prestamos)),cierres)
stopifnot(length(campos)==752L,all(campos %in% names(va)),
          !any(c("clase_ternaria","clase_binaria") %in% campos))
hp <- primero$valor$hp
firma <- list(receta="SIN_CIERRES_P_V1",campos=campos,
 train=c(202103L,202104L,202105L,202106L),score=202108L,
 semillas=seeds,hp=hp,representacion=rep,
 control=control$modelos,threads=6L)
planp <- file.path(out,"PLAN.rds")
if(file.exists(planp)) {
 stopifnot(identical(readRDS(planp),firma))
} else {
 saveRDS(firma,planp)
}
fwrite(data.table(campo=campos),file.path(out,"FEATURES.csv"))
fwrite(data.table(campo=cierres),file.path(out,"EXCLUIDAS.csv"))
x <- data.matrix(tr[,..campos])
xv <- data.matrix(va[,..campos])
x[!is.finite(x)] <- NA_real_
xv[!is.finite(xv)] <- NA_real_
y <- as.integer(tr$clase_ternaria %in% c("BAJA+1","BAJA+2"))
ids <- va$numero_de_cliente
log("DATOS VERIFICADOS",nrow(tr),"TRAIN",length(campos),"FEATURES",
    "PRESTAMOS RESTAURADOS; CIERRES EXCLUIDOS")
rm(tr,va);gc(FALSE)
ds <- lgb.Dataset(x,label=y,params=list(max_bin=31L,
 feature_pre_filter=FALSE,num_threads=6L))
rm(x);gc(FALSE)
probabilidades <- matrix(NA_real_,length(ids),length(seeds))
for(j in seq_along(seeds)) {
 s <- seeds[j]
 zc <- buscar(s)
 stopifnot(identical(zc$valor$hp,hp))
 pars <- zc$firma$pars
 pars$num_threads <- 6L
 p <- file.path(out,paste0("SCORES_",s,".rds"))
 mp <- file.path(out,paste0("MODELO_",s,".txt"))
 sig <- list(plan=firma,seed=s,pars=pars)
 if(file.exists(p)) {
  z <- readRDS(p)
  stopifnot(identical(z$firma,sig),identical(z$ids,ids),
            file.exists(mp))
  pr <- z$prob
  log("REUTILIZADO",s)
 } else {
  log("ENTRENO",j,"/10 SEMILLA",s)
  m <- lgb.train(params=pars,data=ds,nrounds=hp$num_iterations)
  pr <- predict(m,xv,params=list(num_threads=6L))
  stopifnot(length(pr)==length(ids),all(is.finite(pr)))
  lgb.save(m,mp)
  tmp <- paste0(p,".tmp")
  saveRDS(list(firma=sig,ids=ids,prob=pr),tmp)
  stopifnot(file.rename(tmp,p))
  rm(m);gc(FALSE)
  log("GUARDADO",j,"/10",s)
 }
 probabilidades[,j] <- pr
 saveRDS(data.table(orden=seq_len(j),semilla=seeds[seq_len(j)]),
         file.path(out,"PROGRESO_MODELOS.rds"))
}
emitir <- function(prob,nombre,carpeta) {
 dir.create(file.path(out,carpeta),showWarnings=FALSE)
 ix <- order(-prob,ids)[seq_len(10500L)]
 p <- file.path(out,carpeta,paste0(nombre,".csv"))
 fwrite(data.table(id=ids[ix]),p,col.names=FALSE)
 stopifnot(length(unique(ids[ix]))==10500L)
 data.table(archivo=p,clientes=10500L,
            sha256=digest(file=p,algo="sha256"))
}
manifesto <- list()
for(j in seq_along(seeds))
 manifesto[[length(manifesto)+1L]] <- emitir(probabilidades[,j],
 paste0("SINCIERRES_V1_INDIV_S",seeds[j],"_K10500"),
 "LOT_SINCIERRES_V1_INDIV_K10500")
set.seed(100613L)
comb <- combn(seq_along(seeds),5L)
grupos <- comb[,sample.int(ncol(comb),20L),drop=FALSE]
fwrite(rbindlist(lapply(1:20,function(g)
 data.table(grupo=g,semilla=seeds[grupos[,g]]))),
 file.path(out,"GRUPOS_ENS5.csv"))
for(g in 1:20)
 manifesto[[length(manifesto)+1L]] <- emitir(
 rowMeans(probabilidades[,grupos[,g],drop=FALSE]),
 sprintf("SINCIERRES_V1_ENS5_G%02d_K10500",g),
 "LOT_SINCIERRES_V1_ENS5_K10500")
promedio <- rowMeans(probabilidades)
manifesto[[length(manifesto)+1L]] <- emitir(promedio,
 "SINCIERRES_V1_PROB10_COMPLETO_K10500","ENSAMBLE_COMPLETO")
fwrite(data.table(numero_de_cliente=ids,prob=promedio),
 file.path(out,"PROBABILIDADES_AGOSTO.csv"))
fwrite(rbindlist(manifesto),file.path(out,"MANIFIESTO.csv"))
saveRDS(list(plan=firma,ids=ids,prob=promedio),
 file.path(out,"TERMINADO.rds"))
log("PRODUCCION COMPLETA: 10 MODELOS, 20 ENS5, PROB10")
