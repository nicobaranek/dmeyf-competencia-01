
.libPaths(unique(c("/home/ds/.local/lib/R/site-library",.libPaths())))
suppressPackageStartupMessages(library(data.table))
suppressPackageStartupMessages(library(lightgbm))
stopifnot(as.character(packageVersion("lightgbm"))=="4.7.0.99")
out <- Sys.getenv("DMEYF_PROD_OUT")
src <- file.path(out,"fuentes")
for(n in c(3,4,5,6,7,19,20))
 source(file.path(src,sprintf("CELDA_%02d.R",n)),local=.GlobalEnv)

# Dejo anotado: Utilizo el motor auditado sin ejecutar la celda de BO histórica.
h <- readRDS(Sys.getenv("DMEYF_CIERRE_HIST"))
stopifnot(h$nombre=="ARTIFICIALES_RANK",
          identical(as.integer(h$plan$semillas),as.integer(SEEDS_INT)),
          length(h$modelos)==20L,
          h$plan$raw==RAW_HASH,
          identical(as.integer(PARAM$train_final),202103L:202106L),
          CFG$pseudo_regla=="top_k",CFG$pseudo_top_k==10500,
          PARAM$threads==6L)
rec <- REC_INT$ARTIFICIALES_RANK
stopifnot(rec$rep$id==h$plan$rep,
          identical(campos_receta(rec,FALSE,FALSE),
                    readRDS(file.path(RAIZ,"scores",
                    paste0(h$modelos[1],".rds")))$firma$dataset$campos))
hp <- h$hp
stopifnot(hp$num_iterations==2404L,hp$num_leaves==191L)

# Dejo anotado: Salidas de esta producción en carpeta propia.
DIR_INT <- file.path(out,"resultados")
for(n in c("planes","produccion","lotes","auditoria","historia","entrega"))
 dir.create(file.path(DIR_INT,n),recursive=TRUE,showWarnings=FALSE)
writeLines(DIR_INT,file.path(out,"DIRECTORIO_RESULTADOS.txt"))
atomic(CFG,file.path(out,"CONFIG_EJECUTADA.rds"))
atomic(list(hp=hp,semillas=SEEDS_INT,fuente=Sys.getenv("DMEYF_CIERRE_HIST")),
       file.path(out,"PLAN_PRODUCCION.rds"))

log_("PRODUCCION ARTIFICIALES_RANK: DOCENTE Y ETIQUETAS JULIO/AGOSTO")
plan_prod <- plan_pseudo_int(rec,TRUE)
stopifnot(identical(as.integer(plan_prod$meses),202103L:202108L),
          identical(as.integer(plan_prod$firma$reales),202103L:202106L),
          identical(as.integer(plan_prod$firma$artificiales),c(202107L,202108L)),
          all(plan_prod$auditoria$positivos==10500L))
rec$planes <- list(hist=NULL,prod=plan_prod)
atomic(plan_prod,file.path(out,"PLAN_ETIQUETAS_PRODUCCION.rds"))
csv(plan_prod$auditoria,file.path(out,"AUDITORIA_ETIQUETAS.csv"))

xs <- vector("list",length(SEEDS_INT))
for(j in seq_along(SEEDS_INT)) {
 log_("PRODUCCION ALUMNO",j,"/20 SEMILLA",SEEDS_INT[j])
 xs[[j]] <- fit_score(rec,hp,SEEDS_INT[j],prod=TRUE,prestamos=TRUE)
 stopifnot(xs[[j]]$mes==202108L,
           isTRUE(xs[[j]]$prestamos),
           length(xs[[j]]$campos)==762L)
 atomic(data.table(orden=seq_len(j),
        semilla=SEEDS_INT[seq_len(j)],
        id=vapply(xs[seq_len(j)],`[[`,character(1),"id")),
        file.path(out,"PROGRESO_MODELOS.rds"))
}
en <- promediar_prob_int(xs,"ARTIFICIALES_RANK_AGOSTO_PROB20")
stopifnot(nrow(en$en$tabla)==164647L,
          all(en$en$tabla$foto_mes==202108L))
csv(en$probabilidades,file.path(out,"PROBABILIDADES_AGOSTO.csv"))
atomic(list(receta=rec,hp=hp,plan=plan_prod,
            modelos=vapply(xs,`[[`,character(1),"id"),ensemble=en$en),
       file.path(out,"CIERRE_PRODUCCION.rds"))

# Dejo anotado: Grupos definidos sin utilizar ganancias públicas ni históricas.
set.seed(100613)
grupos <- replicate(20L,sample.int(20L,5L),simplify=FALSE)
csv(rbindlist(lapply(seq_along(grupos),function(i)
 data.table(grupo=i,semilla=SEEDS_INT[grupos[[i]]]))),
 file.path(out,"GRUPOS_ENS5.csv"))

ref <- en$en$tabla$numero_de_cliente
P <- vapply(xs,function(x) {
 ix <- match(ref,x$scores$numero_de_cliente)
 stopifnot(!anyNA(ix))
 x$scores$prob[ix]
},numeric(length(ref)))
stopifnot(max(abs(rowMeans(P)-en$en$tabla$score))<1e-12)
manifest <- list()

emitir <- function(p,k,tipo,identificador,subdir) {
 d <- file.path(out,"submits",subdir)
 dir.create(d,recursive=TRUE,showWarnings=FALSE)
 nombre <- paste0("ARANK_V1_",tipo,"_",identificador,"_K",k,".csv")
 archivo <- file.path(d,nombre)
 ix <- order(-p,ref)[seq_len(k)]
 ids <- format(ref[ix],scientific=FALSE,trim=TRUE,digits=22)
 stopifnot(length(ids)==k,!anyDuplicated(ids),
           all(grepl("^[0-9]+$",ids)))
 if(file.exists(archivo)) {
  stopifnot(identical(readLines(archivo),ids))
 } else {
  writeLines(ids,paste0(archivo,".tmp"))
  stopifnot(file.rename(paste0(archivo,".tmp"),archivo))
 }
 stopifnot(identical(readLines(archivo),ids))
 manifest[[length(manifest)+1L]] <<- data.table(
  receta="ARTIFICIALES_RANK",tipo=tipo,id=identificador,corte=k,
  archivo=archivo,sha256=digest::digest(file=archivo,algo="sha256"))
}
for(k in c(9000L,9500L,10000L,10500L)) {
 for(j in seq_along(SEEDS_INT))
  emitir(P[,j],k,"INDIVIDUAL",paste0("S",SEEDS_INT[j]),
         paste0("LOT_ARANK_V1_INDIVIDUALES_K",k))
 for(j in seq_along(grupos))
  emitir(rowMeans(P[,grupos[[j]],drop=FALSE]),k,"ENS5",
         sprintf("G%02d",j),paste0("LOT_ARANK_V1_ENS5_K",k))
 emitir(rowMeans(P),k,"PROB20","COMPLETO","ENSAMBLES_COMPLETOS")
}
csv(rbindlist(manifest),file.path(out,"MANIFIESTO.csv"))
writeLines(c(
 "Receta: ARTIFICIALES_RANK; regla artificial top_k=10500.",
 "Docente: diez semillas, representación rankings, HP del control.",
 "Alumno: veinte semillas, HP elegidos en junio; BO no repetida.",
 "Reales marzo-junio; artificiales julio-agosto; préstamos incluidos.",
 "Ensamble: promedio simple de probabilidades alineadas por ID.",
 "Cada lote contiene veinte CSV del mismo tipo y corte.",
 "Los grupos ENS5 se superponen; no son observaciones independientes.",
 "Ningún archivo fue enviado a Zulip por este proceso."
),file.path(out,"LEEME.txt"))
atomic(list(fecha=Sys.time(),ensemble=en$en$id),
       file.path(out,"TERMINADO.rds"))
if(exists("INTEGRAL_LOCK"))filelock::unlock(INTEGRAL_LOCK)
log_("PRODUCCION Y CSV ARTIFICIALES_RANK TERMINADOS")
