# Dejo anotado: CELDA 06 — Materializo sólo las representaciones activas.
REP <- list()
if(PARAM$ejecutar_original) REP$ORIGINAL <- preparar_features(D,FALSE)
if(PARAM$rankings) REP$RANK_MONTOS_CERO <- preparar_features(D,TRUE)
# Dejo anotado: La matriz incluye préstamos; los retiro al crear el dataset de BO, no destruyo sus columnas.
rm(D);gc(full=TRUE)
cat('Representaciones disponibles:',paste(names(REP),collapse=', '),'\n')
