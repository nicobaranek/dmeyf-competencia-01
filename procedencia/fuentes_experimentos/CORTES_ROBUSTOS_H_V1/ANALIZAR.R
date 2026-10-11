
.libPaths(unique(c("/home/ds/.local/lib/R/site-library", .libPaths())))
library(data.table)
library(digest)
library(filelock)
out <- Sys.getenv("CORTES_OUT")
raiz <- Sys.getenv("CORTES_RAIZ")
base <- Sys.getenv("CORTES_BASE")
lk <- lock("/home/ds/.dmeyf_cortes_robustos.lock", timeout=0)
if(is.null(lk)) stop("El análisis ya está activo.")
on.exit <- NULL

viejo <- file.path(raiz,"integral_clase_20261008",
 "0b6b06569ceee877ea42e00a072112f6b536e82c366f16427290dbd4457a2440",
 "historia")
p <- file.path(base,"ARTIFICIALES_RANK_H","DIRECTORIO_RESULTADOS.txt")
stopifnot(file.exists(p))
nuevo <- file.path(trimws(readLines(p, warn=FALSE)[1]),"historia")

nombres <- c("CONTROL_ORIGINAL","RANK_TODAS",
             "ARTIFICIALES_ORIGINAL","ARTIFICIALES_RANK")
grilla <- seq(8000L,12000L,250L)
curva <- function(ids,prob,y) {
 stopifnot(!anyDuplicated(ids), !anyNA(y), all(is.finite(prob)))
 ix <- order(-prob,ids)
 cumsum(ifelse(y[ix]=="BAJA+2",1072500,-27500))/1e6
}
tablas <- list()
selecciones <- list()
auditoria <- list()
referencia <- NULL

for(nom in nombres) {
 carpeta <- if(nom=="ARTIFICIALES_RANK") nuevo else viejo
 cierre <- file.path(carpeta,paste0("CIERRE_",nom,".rds"))
 stopifnot(file.exists(cierre))
 h <- readRDS(cierre)
 stopifnot(length(h$modelos)==20L, !anyDuplicated(h$modelos))
 zs <- lapply(h$modelos,function(id) {
  fp <- file.path(raiz,"scores",paste0(id,".rds"))
  stopifnot(file.exists(fp))
  z <- readRDS(fp)$valor
  t <- as.data.table(z$scores)
  stopifnot(all(c("numero_de_cliente","foto_mes",
                  "clase_ternaria","prob") %in% names(t)),
            all(t$foto_mes==202106L), !anyDuplicated(t$numero_de_cliente))
  setorder(t,numero_de_cliente)
  list(seed=z$seed,tabla=t)
 })
 semillas <- vapply(zs,function(z)as.integer(z$seed),integer(1))
 stopifnot(length(unique(semillas))==20L)
 ref <- zs[[1]]$tabla
 stopifnot(sum(ref$clase_ternaria=="BAJA+2")==1098L)
 for(z in zs) stopifnot(
  identical(z$tabla$numero_de_cliente,ref$numero_de_cliente),
  identical(z$tabla$clase_ternaria,ref$clase_ternaria))
 if(is.null(referencia)) {
  referencia <- list(ids=ref$numero_de_cliente,y=ref$clase_ternaria,
                     seeds=sort(semillas))
 } else stopifnot(
  identical(referencia$ids,ref$numero_de_cliente),
  identical(referencia$y,ref$clase_ternaria),
  identical(referencia$seeds,sort(semillas)))

 probs <- do.call(cbind,lapply(zs,function(z)z$tabla$prob))
 ce <- curva(ref$numero_de_cliente,rowMeans(probs),ref$clase_ternaria)
 cs <- vapply(seq_len(ncol(probs)),function(j)
  curva(ref$numero_de_cliente,probs[,j],ref$clase_ternaria),
  numeric(nrow(ref)))

 t <- data.table(receta=nom,corte=grilla,
                 ganancia_ensemble=ce[grilla])
 t[,meseta_500:=vapply(corte,function(k)
    mean(ce[(k-500L):(k+500L)]),numeric(1))]
 t[,peor_corte_vecino:=vapply(corte,function(k)
    min(ce[(k-500L):(k+500L)]),numeric(1))]
 t[,media_semillas:=rowMeans(cs[grilla,,drop=FALSE])]
 t[,sd_semillas:=apply(cs[grilla,,drop=FALSE],1,sd)]
 t[,min_semillas:=apply(cs[grilla,,drop=FALSE],1,min)]
 t[,max_semillas:=apply(cs[grilla,,drop=FALSE],1,max)]

 # Dejo anotado: Centro por mayor promedio de la ventana; empate: menor corte.
 # Dejo anotado: Centro interior para que sus dos vecinos estén en la grilla.
 centro <- t[corte>=8250L & corte<=11750L][
   order(-meseta_500,corte)][1,corte]
 s <- copy(t[corte %in% c(centro-250L,centro,centro+250L)])
 s[,motivo:=ifelse(corte==centro,"CENTRO_MESETA","VECINO_PREFIJADO")]
 tablas[[nom]] <- t
 selecciones[[nom]] <- s
 auditoria[[nom]] <- data.table(receta=nom,cierre=cierre,
   sha256_cierre=digest(file=cierre,algo="sha256"),
   modelo=h$modelos,semilla=semillas)
 cat("\n",nom,"CENTRO:",centro,"\n")
 print(s[,.(corte,ganancia_ensemble,meseta_500,sd_semillas,motivo)])
 rm(probs,cs,zs); gc(FALSE)
}
fwrite(rbindlist(tablas),file.path(out,"COMPARACION_CORTES.csv"))
fwrite(rbindlist(selecciones),file.path(out,"CORTES_PREFIJADOS.csv"))
fwrite(rbindlist(auditoria),file.path(out,"TRAZABILIDAD.csv"))
writeLines(c(
 "Análisis retrospectivo de junio; junio ya intervino en BO.",
 "Ensamble: promedio igual de probabilidades de las 20 semillas.",
 "Grilla 8000:12000 cada 250; meseta usa todos los cortes en +/-500.",
 "Selección: centro interior con mayor meseta; empate menor corte.",
 "Tres cortes por receta: centro y vecinos +/-250.",
 "La dispersión entre semillas no estima la incertidumbre del privado.",
 "No se incorporan resultados públicos a esta selección.",
 "No se entrena, no se modifica producción y no se envía a Zulip.",
 "Los CSV de agosto se generan después de verificar su producción."
),file.path(out,"PROTOCOLO.txt"))
saveRDS(list(fecha=Sys.time(),recetas=nombres,
             cortes=rbindlist(selecciones)),
        file.path(out,"TERMINADO.rds"))
unlock(lk)
cat("\nANALISIS TERMINADO. CORTES PREFIJADOS Y TRAZABILIDAD GUARDADOS.\n")
