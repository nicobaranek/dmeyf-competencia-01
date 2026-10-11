# Dejo anotado: Armo los controles que necesitan mis experimentos en una raíz separada.
EXTRAS <- any(unlist(SW[setdiff(names(SW),c('reproducir_final','bo_origen'))]))
if(EXTRAS) {
 if(SW$cortes_historicos || SW$sin_cierres_comparaciones)
  SW$sin_cierres_historico <- TRUE
 if(SW$visa_comparaciones)SW$solo_visa <- TRUE
 if(SW$mezcla_control_rank) {
  SW$control <- TRUE
  SW$rankings <- TRUE
 }
 raiz_final <- RAIZ
 laboratorio <- file.path(raiz_final,'laboratorio')
 RAIZ <- file.path(laboratorio,'maestra')
 BASE_LAB <- file.path(laboratorio,'experimentos')
 dir.create(RAIZ,recursive=TRUE,showWarnings=FALSE)
 dir.create(BASE_LAB,recursive=TRUE,showWarnings=FALSE)
 for(n in c('auditoria','datos','datasets','modelos','scores','bo','seleccion','resultados','submits','figuras'))
  dir.create(file.path(RAIZ,n),showWarnings=FALSE)
 if(!dir.exists(file.path(RAIZ,'features')))
  stopifnot(file.symlink(file.path(raiz_final,'features'),file.path(RAIZ,'features')))
 Sys.setenv(DMEYF_LAB_RAIZ=RAIZ,DMEYF_LAB_BASE=BASE_LAB)
 BASE_SEEDS <- PARAM$semillas
 SEEDS_INT <- c(BASE_SEEDS,106621L,106627L,106637L,106649L,106657L,
               106661L,106663L,106669L,106681L,106693L)
 CFG <- list(pseudo_regla='top_k',pseudo_top_k=10500L,
             pseudo_seed=100613L,pseudo_docente_semillas=10L)
 DIR_INT <- file.path(laboratorio,'integral')
 for(n in c('planes','historia','produccion','lotes','auditoria'))
  dir.create(file.path(DIR_INT,n),recursive=TRUE,showWarnings=FALSE)
 source(file.path(CARPETA,'codigo','motor_integral.R'),local=.GlobalEnv)
 rec_control_int <- REC_CONTROL
 HP_DOCENTE_INT <- HP_FINAL
 previo <- file.path(RAIZ,'integral_clase_20261008',
  '0b6b06569ceee877ea42e00a072112f6b536e82c366f16427290dbd4457a2440')
 dir.create(file.path(previo,'historia'),recursive=TRUE,showWarnings=FALSE)
 REC_INT <- list(CONTROL_ORIGINAL=REC_CONTROL)
 HIST_INT <- list();PROD_INT <- list()
 necesito_rank <- SW$rankings || SW$artificiales_rank || SW$rank_zero_tend3
 if(necesito_rank) {
  rr <- REC_CONTROL;rr$rep <- rank_todas_int(REP)
  rr$nombre <- 'RANK_TODAS_POST_LAG2'
  REC_INT$RANK_TODAS <- rr
 }
 ejecutar_receta <- function(nom,rec,hp,prod=FALSE) {
  xs <- lapply(SEEDS_INT,function(s)fit_score(rec,hp,s,prod,prod))
  en <- promediar_prob_int(xs,paste0(nom,if(prod)'_AGOSTO' else '_JUNIO'))
  plan <- list(semillas=SEEDS_INT,raw=RAW_HASH,rep=rec$rep$id)
  z <- list(nombre=nom,receta=rec,hp=hp,
   modelos=vapply(xs,`[[`,character(1),'id'),ensemble=en$en,
   plan=plan,resumen=if(prod)NULL else resumen_junio_int(xs,en$en,nom))
  if(prod) PROD_INT[[nom]] <<- z else {
   HIST_INT[[nom]] <<- z
   atomic(z,file.path(previo,'historia',paste0('CIERRE_',nom,'.rds')))
  }
  z
 }
 ejecutar_receta('CONTROL_ORIGINAL',REC_CONTROL,HP_FINAL,FALSE)
 if(SW$control || SW$diagnosticos_ens5)
  ejecutar_receta('CONTROL_ORIGINAL',REC_CONTROL,HP_FINAL,TRUE)
 if(necesito_rank) {
  hp_rank <- readRDS(file.path(CARPETA,'procedencia_bo',
   '37fa6c85a784b6a214de0590f8f9e40c037228d73d6dfc2aed3763b62b93d972',
   'TERMINADO.rds'))$valor$optimos[[1L]]
  ejecutar_receta('RANK_TODAS',REC_INT$RANK_TODAS,hp_rank,FALSE)
  if(SW$rankings || SW$diagnosticos_ens5)
   ejecutar_receta('RANK_TODAS',REC_INT$RANK_TODAS,hp_rank,TRUE)
 }
 for(nom in c('ARTIFICIALES_ORIGINAL','ARTIFICIALES_RANK')) {
  activo <- if(nom=='ARTIFICIALES_ORIGINAL')SW$artificiales_original else SW$artificiales_rank
  if(!activo)next
  rr <- if(nom=='ARTIFICIALES_ORIGINAL')REC_CONTROL else REC_INT$RANK_TODAS
  rr$nombre <- paste0(nom,'_LAG2');rr$estrategia <- 'ARTIFICIALES_4H_6P'
  rr$planes <- list(hist=plan_pseudo_int(rr,FALSE),prod=NULL)
  bid <- if(nom=='ARTIFICIALES_ORIGINAL')
   'd3d28cc4bd4d5336fb1bd7e8d6b47de26375209806f59252be5d44873d477efa' else
   '0621c1e8942c1632c00f8ae4c7b80adcfd58c304a43bbb334088afbf63ff8b3c'
  hp <- readRDS(file.path(CARPETA,'procedencia_bo',bid,'TERMINADO.rds'))$valor$optimos[[1L]]
  ejecutar_receta(nom,rr,hp,FALSE)
  rr$planes$prod <- plan_pseudo_int(rr,TRUE)
  ejecutar_receta(nom,rr,hp,TRUE)
 }
 if(SW$rank_montos) {
  D <- cargar_base()
  bo <- readRDS(file.path(CARPETA,'procedencia_bo',
   '2595ac2f643b4afb3166b2701db84bc24e786246b65cabc9c73025d1ba5f66a8',
   'TERMINADO.rds'))
  PARAM$rank_campos <- c("Master_madelantodolares","Master_madelantopesos","Master_mconsumosdolares","Master_mconsumospesos","Master_mconsumototal","Master_mfinanciacion_limite","Master_mlimitecompra","Master_mpagado","Master_mpagominimo","Master_mpagosdolares","Master_mpagospesos","Master_msaldodolares","Master_msaldopesos","Master_msaldototal","Visa_madelantodolares","Visa_madelantopesos","Visa_mconsumosdolares","Visa_mconsumospesos","Visa_mconsumototal","Visa_mfinanciacion_limite","Visa_mlimitecompra","Visa_mpagado","Visa_mpagominimo","Visa_mpagosdolares","Visa_mpagospesos","Visa_msaldodolares","Visa_msaldopesos","Visa_msaldototal","mactivos_margen","matm","matm_other","mautoservicio","mcaja_ahorro","mcaja_ahorro_adicional","mcaja_ahorro_dolares","mcajeros_propios_descuentos","mcheques_depositados","mcheques_depositados_rechazados","mcheques_emitidos","mcheques_emitidos_rechazados","mcomisiones","mcomisiones_mantenimiento","mcomisiones_otras","mcuenta_corriente","mcuenta_corriente_adicional","mcuenta_debitos_automaticos","mcuentas_saldo","mextraccion_autoservicio","mforex_buy","mforex_sell","minversion1_dolares","minversion1_pesos","minversion2","mpagodeservicios","mpagomiscuentas","mpasivos_margen","mpayroll","mpayroll2","mplazo_fijo_dolares","mplazo_fijo_pesos","mprestamos_hipotecarios","mprestamos_personales","mprestamos_prendarios","mrentabilidad","mrentabilidad_annual","mtarjeta_master_consumo","mtarjeta_master_descuentos","mtarjeta_visa_consumo","mtarjeta_visa_descuentos","mtransferencias_emitidas","mtransferencias_recibidas","mttarjeta_master_debitos_automaticos","mttarjeta_visa_debitos_automaticos")
  rr <- REC_CONTROL;rr$rep <- preparar_features(D,TRUE)
  rr$nombre <- 'RANK_MONTOS_CERO_LAG2'
  ejecutar_receta('RANK_MONTOS',rr,bo$valor$optimos[[1L]],FALSE)
  rm(D);gc(FALSE)
 }
 if(SW$target_baja2) {
  rr <- REC_CONTROL;rr$nombre <- 'ORIGINAL_LAG2_TARGET_BAJA2'
  d <- leer_meses(REP,PARAM$train)
  rr$planes <- list(hist=list(key=paste(d$numero_de_cliente,d$foto_mes),
   y=as.integer(d$clase_ternaria=='BAJA+2'),meses=PARAM$train),prod=NULL)
  hp <- readRDS(file.path(CARPETA,'procedencia_bo',
   'bf654ea508b68620dba3a8ffa6f1aeb91e2886174dceeca3fd104ac91f0fd675',
   'TERMINADO.rds'))$valor$optimos[[1L]]
  ejecutar_receta('TARGET_BAJA2',rr,hp,FALSE)
 }
 mapa <- c(tarjetas='STATUS_TRANSICIONES_H_V1',
  solo_visa='STATUS_ABLACIONES_H_V1/SOLO_VISA',
  solo_master='STATUS_ABLACIONES_H_V1/SOLO_MASTER',
  vinculacion='VINCULACION_H_V1',sin_cierres_historico='SIN_CIERRES_H_V1',
  rank_zero_tend3='RANK_ZERO_TEND3_H_V1',bo_sin_cierres_nueva='SIN_CIERRES_BO_FINAL_V1')
 for(nom in names(mapa)) {
  if(!SW[[nom]])next
  od <- file.path(BASE_LAB,mapa[[nom]])
  dir.create(od,recursive=TRUE,showWarnings=FALSE)
  sp <- file.path(CARPETA,'experimentos',paste0(nom,'.R'))
  if(nom=='bo_sin_cierres_nueva') {
   fd <- file.path(BASE_LAB,'SIN_CIERRES_P_V1')
   dir.create(fd,recursive=TRUE,showWarnings=FALSE)
   fp <- readRDS(file.path(raiz_final,'final','SCORES_100613.rds'))
   fp$firma$plan <- list(campos=fp$firma$campos)
   atomic(fp,file.path(fd,'SCORES_100613.rds'))
  }
  Sys.setenv(DMEYF_STATUS_OUT=od,DMEYF_STATUS_SCRIPT=sp,BO_FINAL_OUT=od)
  source(sp,local=new.env(parent=.GlobalEnv))
 }
 # Dejo los diagnósticos aparte del entrenamiento final.
 if(SW$diagnosticos_ens5) {
  for(nom in names(HIST_INT)) {
   a <- HIST_INT[[nom]]
   xs <- lapply(a$modelos,function(id)readRDS(file.path(RAIZ,'scores',paste0(id,'.rds')))$valor)
   ref <- xs[[1]]$scores
   P <- sapply(xs,function(v)v$scores$prob[match(ref$numero_de_cliente,v$scores$numero_de_cliente)])
   set.seed(100613L);comb <- combn(seq_len(ncol(P)),5L)
   grupos <- comb[,sample.int(ncol(comb),20L),drop=FALSE]
   tablas <- lapply(seq_len(ncol(grupos)),function(g) {
    cv <- curva(ref$numero_de_cliente,rowMeans(P[,grupos[,g],drop=FALSE]),ref$clase_ternaria)
    data.table(receta=nom,grupo=g,K10500=cv$ganancia[10500],
               meseta=mean(cv$ganancia[10000:11500]))
   })
   fwrite(rbindlist(tablas),file.path(BASE_LAB,paste0('ENS5_',nom,'.csv')))
  }
 }
 # Comparo mis ensambles de tarjetas con los controles de las mismas semillas.
 for(nom in c('visa_comparaciones','sin_cierres_comparaciones')) {
  if(!SW[[nom]])next
  od <- file.path(BASE_LAB,if(nom=='visa_comparaciones')
   'VISA_COMPARACIONES_H_V1' else 'SIN_CIERRES_COMPARACIONES_H_V1')
  dir.create(od,recursive=TRUE,showWarnings=FALSE)
  Sys.setenv(DMEYF_VISA_AUDIT_OUT=od,DMEYF_SIN_CIERRES_AUDIT_OUT=od)
  sp <- file.path(CARPETA,'experimentos',paste0(nom,'.R'))
  source(sp,local=new.env(parent=.GlobalEnv))
 }
 if(SW$mezcla_control_rank) {
  a <- PROD_INT$CONTROL_ORIGINAL$ensemble$tabla
  b <- PROD_INT$RANK_TODAS$ensemble$tabla
  ii <- match(a$numero_de_cliente,b$numero_de_cliente)
  stopifnot(!anyNA(ii))
  pr <- (a$score+b$score[ii])/2
  elegido <- a$numero_de_cliente[order(-pr,a$numero_de_cliente)][1:10500]
  writeLines(format(elegido,scientific=FALSE,trim=TRUE),
   file.path(BASE_LAB,'CONTROL_RANK_PROB40_K10500.csv'))
 }
 RAIZ <- raiz_final
}
