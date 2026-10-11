# Dejo anotado: Si lo activo, comparo los cortes históricos sin mirar etiquetas de agosto.
if(SW$cortes_historicos) {
 od <- file.path(Sys.getenv('DMEYF_LAB_BASE'),'SIN_CIERRES_H_V1')
 cierre <- file.path(od,'TERMINADO.rds')
 if(!file.exists(cierre))stop('Debo activar sin_cierres_historico antes de cortes_historicos.')
 h <- readRDS(cierre);ref <- h$ref
 P <- sapply(h$plan$semillas,function(s) {
  a <- readRDS(file.path(od,paste0('SCORES_',s,'.rds')))
  stopifnot(setequal(a$ids,ref$numero_de_cliente))
  a$prob[match(ref$numero_de_cliente,a$ids)]
 })
 ce <- curva(ref$numero_de_cliente,rowMeans(P),ref$clase_ternaria)
 tabla <- rbindlist(lapply(c(10000L,10250L,10375L,10500L,10750L,11000L,11500L),function(k)
  data.table(corte=k,ganancia=ce$ganancia[k],
   meseta=mean(ce$ganancia[(k-250L):(k+250L)]))))
 fwrite(tabla,file.path(RAIZ,'CORTES_HISTORICOS.csv'));print(tabla)
}
