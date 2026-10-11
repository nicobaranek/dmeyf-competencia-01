
.libPaths(unique(c("/home/ds/.local/lib/R/site-library", .libPaths())))
stopifnot(as.character(packageVersion("lightgbm")) == "4.7.0.99")
cat("ENTORNO RESTAURADO. LightGBM:",
    as.character(packageVersion("lightgbm")), "\n")
source(file.path(Sys.getenv("DMEYF_RANK_ART_RUN"),
                 "EJECUTAR_HISTORIA.R"), local=.GlobalEnv)
