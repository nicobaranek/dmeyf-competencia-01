# Fijo mi entrega y dejo apagadas las variantes que no elegí.
PARAM <- list(
 semilla=100613L,
 semillas=c(100613L,100267L,101611L,102481L,103651L,104729L,
            105019L,105613L,106013L,106619L),
 threads=6L,train=c(202103L,202104L),
 train_final=c(202103L,202104L,202105L,202106L),
 valid=202106L,future=202108L,corte=11500L,orden_lags=2L,
 incluir_id_mes=TRUE,bo_evaluaciones=12L,bo_iniciales=4L,
 bounds=list(iter=c(512L,4096L),leaves=c(8L,256L),logh=c(-3,1)),
 top_configuraciones=1L,rank_campos=character()
)
SW <- list(
 reproducir_final=TRUE,bo_origen=FALSE,
 control=FALSE,rankings=FALSE,artificiales_original=FALSE,
 artificiales_rank=FALSE,rank_montos=FALSE,target_baja2=FALSE,
 tarjetas=FALSE,solo_visa=FALSE,solo_master=FALSE,
 vinculacion=FALSE,sin_cierres_historico=FALSE,
 rank_zero_tend3=FALSE,bo_sin_cierres_nueva=FALSE,
 diagnosticos_ens5=FALSE,cortes_historicos=FALSE,
 mezcla_control_rank=FALSE,visa_comparaciones=FALSE,
 sin_cierres_comparaciones=FALSE
)
CORTE_FINAL <- 10375L
SHA_FINAL <- '7715cbb9568462c2a1f9b0534528fbce3f77c54cd43a255c59476dc9a37b5caa'
CARPETA <- normalizePath(Sys.getenv('DMEYF_ENTREGA',unset=getwd()),mustWork=TRUE)
if(!file.exists(file.path(CARPETA,'codigo','motor.R')))
 stop('Debo ejecutar desde la carpeta de entrega o definir DMEYF_ENTREGA.')
RAIZ_NUEVA <- Sys.getenv('DMEYF_SALIDA',unset=file.path(CARPETA,'salida'))
RAW_NUEVO <- Sys.getenv('DMEYF_DATOS',unset=file.path(CARPETA,'datos','competencia_01_crudo.csv'))
if(!file.exists(RAW_NUEVO)) stop('Debo indicar DMEYF_DATOS con el dataset crudo.')
if(unname(tools::md5sum(RAW_NUEVO))!='b6f759764c5d04cbf068548517ae1249')
 stop('El crudo no coincide con el dataset de la corrida final.')
Sys.setenv(DMEYF_RAIZ=RAIZ_NUEVA,DMEYF_DATOS=RAW_NUEVO)
if(dir.exists('/home/ds/.local/lib/R/site-library'))
 .libPaths(unique(c('/home/ds/.local/lib/R/site-library',.libPaths())))
paquetes <- c('data.table','lightgbm','digest','jsonlite','filelock')
faltan <- paquetes[!vapply(paquetes,requireNamespace,logical(1),quietly=TRUE)]
if(length(faltan)) stop('Debo instalar estos paquetes: ',paste(faltan,collapse=', '))
library(lightgbm)
stopifnot(as.character(getRversion())=='4.6.1',
 as.character(packageVersion('lightgbm'))=='4.7.0.99',
 as.character(packageVersion('data.table'))=='1.18.6.1')
source(file.path(CARPETA,'codigo','motor.R'),local=.GlobalEnv)
