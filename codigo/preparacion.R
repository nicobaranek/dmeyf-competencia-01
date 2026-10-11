# Reconstruyo el target, corrijo mayo y calculo la historia calendario exacta.
D <- cargar_base()
REP <- preparar_features(D,FALSE)
stopifnot(REP$id=='c9527308c55d2d52c924dc64ce5594db3d240bce96c8a548c6f48a06239c71c8')
rm(D)
gc(full=TRUE)
# Dejo anotado: Recupero la procedencia de los HP sin hacer pasar una BO nueva por la original.
ORIGEN <- readRDS(file.path(CARPETA,'procedencia_bo',
 '52fc1b5d908fb495ce06d1ed3038defbaf9a2609d2ea0bc80904d2ba8b58c6ed',
 'TERMINADO.rds'))
HP_FINAL <- ORIGEN$valor$optimos[[1L]]
stopifnot(HP_FINAL$num_iterations==1907L,HP_FINAL$num_leaves==77L,
 abs(HP_FINAL$min_sum_hessian_in_leaf-2.83699883991516)<1e-12)
REC_CONTROL <- list(nombre='ORIGINAL_LAG2',rep=REP,estrategia='COMPLETO',
 umbral=NULL,seleccion=NULL,excluir=character(),planes=NULL)
# Dejo anotado: Si activo la BO de origen, la ejecuto en otra raíz sin reutilizar su resultado.
if(SW$bo_origen) {
 for(paquete in c('mlrMBO','mlr','ParamHelpers','smoof','DiceKriging'))
  if(!requireNamespace(paquete,quietly=TRUE))stop('Debo instalar ',paquete)
 b <- bayes(REC_CONTROL)
 atomic(b,file.path(RAIZ,'BO_ORIGEN_REEJECUTADA.rds'))
}
