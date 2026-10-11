
.libPaths(unique(c("/home/ds/.local/lib/R/site-library", .libPaths())))
library(data.table)
setDTthreads(1L)
base <- Sys.getenv("FINAL_BASE")
raiz <- Sys.getenv("FINAL_RAIZ")
out <- Sys.getenv("FINAL_OUT")
src <- file.path(base,"SIN_CIERRES_P_V1")

stopifnot(file.exists(file.path(src,"TERMINADO.json")))
z <- readRDS(file.path(src,"TERMINADO.rds"))
ids <- z$ids
stopifnot(length(ids)==164647L, !anyDuplicated(ids))

semillas <- c(100613L,100267L,101611L,102481L,103651L,
              104729L,105019L,105613L,106013L,106619L)
sc <- lapply(semillas,function(s)
  readRDS(file.path(src,paste0("SCORES_",s,".rds"))))

alinear <- function(v) {
  stopifnot(!anyDuplicated(v$ids),setequal(ids,v$ids))
  p <- v$prob[match(ids,v$ids)]
  stopifnot(length(p)==length(ids),all(is.finite(p)),
            all(p>=0 & p<=1))
  p
}
ps <- do.call(cbind,lapply(sc,alinear))
stopifnot(max(abs(rowMeans(ps)-z$prob))<1e-10)

# Dejo anotado: Los mismos grupos que se generaron para Sin cierres.
set.seed(100613L)
combinaciones <- combn(seq_along(semillas),5L)
grupos <- combinaciones[,sample(ncol(combinaciones),20L),drop=FALSE]

exportar <- function(p,nombre,k) {
  stopifnot(length(p)==length(ids),all(is.finite(p)))
  elegidos <- ids[order(-p,ids)][seq_len(k)]
  stopifnot(length(elegidos)==k,!anyDuplicated(elegidos))
  writeLines(format(elegidos,scientific=FALSE,trim=TRUE),
             file.path(out,paste0(nombre,".csv")))
}

# Dejo anotado: Grilla pequeña, fijada antes de mirar estos resultados públicos.
cortes <- c(10000L,10250L,10500L,10750L,11000L,11500L)
for(k in cortes)
  exportar(rowMeans(ps),paste0("SINCIERRES_PROB10_K",k),k)

# Busco control de agosto compatible, sin recalcular.
normalizar <- function(p) {
  p$num_threads <- NULL
  p[order(names(p))]
}
objetivo <- normalizar(sc[[1L]]$firma$pars)
campos_sin <- sc[[1L]]$firma$plan$campos
if(is.null(campos_sin)) {
  plan <- readRDS(file.path(src,"PLAN.rds"))
  campos_sin <- plan$campos
}
stopifnot(length(campos_sin)==752L)

hallados <- vector("list",length(semillas))
origenes <- rep(NA_character_,length(semillas))
archivos <- list.files(file.path(raiz,"scores"),
                       pattern="^[a-f0-9]+\\.rds$",full.names=TRUE)

for(archivo in archivos) {
  a <- tryCatch(readRDS(archivo),error=function(e) NULL)
  if(is.null(a) || is.null(a$valor)) next
  v <- a$valor
  if(length(v$seed)!=1L || is.na(v$seed)) next
  j <- match(v$seed,semillas)
  if(is.na(j) || !is.null(hallados[[j]])) next
  if(!identical(as.integer(v$mes),202108L)) next
  if(!identical(normalizar(a$firma$pars),normalizar(sc[[j]]$firma$pars))) next
  if(!identical(as.integer(a$firma$dataset$meses),
                c(202103L,202104L,202105L,202106L))) next

  campos <- v$campos
  cierres <- grep("^(Visa_fultimo_cierre|Master_fultimo_cierre)($|_)",
                  campos,value=TRUE)
  if(length(cierres)!=10L ||
     !setequal(setdiff(campos,cierres),campos_sin)) next
  if(is.null(v$scores) ||
     !all(c("numero_de_cliente","prob") %in% names(v$scores))) next

  pred <- list(ids=v$scores$numero_de_cliente,
               prob=v$scores$prob)
  p <- tryCatch(alinear(pred),error=function(e) NULL)
  if(is.null(p)) next
  hallados[[j]] <- p
  origenes[j] <- archivo
}

fwrite(data.table(semilla=semillas,archivo=origenes),
       file.path(out,"CONTROL_TRAZABILIDAD.csv"))

if(all(vapply(hallados,function(x)!is.null(x),logical(1)))) {
  pc <- do.call(cbind,hallados)
  exportar(rowMeans(pc),"CONTROL_PROB10_K10500",10500L)
  for(g in seq_len(ncol(grupos)))
    exportar(rowMeans(pc[,grupos[,g],drop=FALSE]),
      sprintf("CONTROL_ENS5_G%02d_K10500",g),10500L)
  writeLines("CONTROL COMPATIBLE: 10 semillas, mismos parámetros y meses.",
             file.path(out,"ESTADO_CONTROL.txt"))
} else {
  writeLines(paste("CONTROL NO EXPORTADO. Encontrados:",
                  sum(!is.na(origenes)),"/10. Revisar trazabilidad."),
             file.path(out,"ESTADO_CONTROL.txt"))
}

fwrite(data.table(grupo=rep(1:20,each=5),
                 semilla=as.vector(matrix(semillas[grupos],nrow=5))),
       file.path(out,"GRUPOS_CONTROL_ENS5.csv"))
cat(readLines(file.path(out,"ESTADO_CONTROL.txt")),"\n")
cat("CORTES SIN CIERRES VERIFICADOS:",paste(cortes,collapse=", "),"\n")
