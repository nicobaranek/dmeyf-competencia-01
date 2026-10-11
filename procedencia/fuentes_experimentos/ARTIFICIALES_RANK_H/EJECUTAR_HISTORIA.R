
.libPaths(unique(c(.libPaths(), path.expand("~/R/dmeyf-library"))))
run <- Sys.getenv("DMEYF_RANK_ART_RUN")
cargar <- function(n) source(
 file.path(run,sprintf("CELDA_%02d.R",n)), local=.GlobalEnv)

for(n in c(3,4,5,6,7,19,20)) cargar(n)

stopifnot(!isTRUE(CFG$producir_agosto),
          !isTRUE(CFG$extras),
          length(SEEDS_INT)==20L,
          !is.null(REC_INT$ARTIFICIALES_RANK))

previo <- file.path(RAIZ,"integral_clase_20261008",
 "0b6b06569ceee877ea42e00a072112f6b536e82c366f16427290dbd4457a2440")

# Incorporo resultados previos como controles, sin volver a entrenarlos.
for(nom in c("CONTROL_ORIGINAL","RANK_TODAS","ARTIFICIALES_ORIGINAL")) {
 z <- readRDS(file.path(previo,"historia",paste0("CIERRE_",nom,".rds")))
 stopifnot(z$plan$raw==RAW_HASH,
           identical(as.integer(z$plan$semillas),as.integer(SEEDS_INT)))
 HIST_INT[[nom]] <- z
}

writeLines(DIR_INT,file.path(run,"DIRECTORIO_RESULTADOS.txt"))
CFG_PLAN <- CFG
CFG$recetas <- "ARTIFICIALES_RANK"
cat("\nINICIO ARTIFICIALES_RANK: SOLO HISTORIA JUNIO\n")
cargar(21)
CFG <- CFG_PLAN

stopifnot("ARTIFICIALES_RANK"%in%names(HIST_INT),
          length(HIST_INT$ARTIFICIALES_RANK$modelos)==20L)

file.copy(file.path(DIR_INT,"historia","COMPARACION_RECETAS.csv"),
          file.path(run,"COMPARACION_RECETAS.csv"),overwrite=TRUE)
atomic(list(plan=CFG,resultado=HIST_INT$ARTIFICIALES_RANK,
            fecha=Sys.time()),file.path(run,"TERMINADO.rds"))
if(exists("INTEGRAL_LOCK")) filelock::unlock(INTEGRAL_LOCK)
cat("\nARTIFICIALES_RANK HISTORICO TERMINADO. AGOSTO NO EJECUTADO.\n")
