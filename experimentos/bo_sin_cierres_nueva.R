
.libPaths(unique(c("/home/ds/.local/lib/R/site-library",.libPaths())))
library(data.table)
library(lightgbm)
library(mlrMBO)
library(ParamHelpers)
library(smoof)
library(digest)
library(filelock)
setDTthreads(1L)

out <- Sys.getenv("BO_FINAL_OUT")
raiz <- Sys.getenv("DMEYF_LAB_RAIZ")
base <- Sys.getenv("DMEYF_LAB_BASE")
lk <- lock(file.path(out,"ejecucion.lock"),timeout=0)
stopifnot(!is.null(lk))

log <- function(...) {
 cat(format(Sys.time(),"%Y-%m-%d %H:%M:%S %Z"),...,"\n")
 flush.console()
}
atomic <- function(x,p) {
 tmp <- paste0(p,".tmp")
 saveRDS(x,tmp)
 stopifnot(file.rename(tmp,p))
}
stopifnot(as.character(packageVersion("lightgbm"))=="4.7.0.99")

src <- file.path(base,"SIN_CIERRES_P_V1")
seeds <- c(100613L,100267L,101611L,102481L,103651L,
           104729L,105019L,105613L,106013L,106619L)
anterior <- readRDS(file.path(src,"SCORES_100613.rds"))
pars_base <- anterior$firma$pars
hp_actual <- pars_base[c("num_iterations","num_leaves",
                        "min_sum_hessian_in_leaf")]
campos_prod <- anterior$firma$plan$campos
if(is.null(campos_prod))
 campos_prod <- readRDS(file.path(src,"PLAN.rds"))$campos

stopifnot(length(campos_prod)==752L)
campos_hist <- campos_prod[
 !grepl("^(cprestamos_personales|mprestamos_personales)($|_)",
        campos_prod)]
stopifnot(length(campos_hist)==742L,
 !any(grepl("^(Visa_fultimo_cierre|Master_fultimo_cierre)($|_)",
            campos_prod)))

viejo <- file.path(raiz,"integral_clase_20261008",
 "0b6b06569ceee877ea42e00a072112f6b536e82c366f16427290dbd4457a2440",
 "historia","CIERRE_CONTROL_ORIGINAL.rds")
control <- readRDS(viejo)
v <- readRDS(file.path(raiz,"scores",
                      paste0(control$modelos[[1L]],".rds")))
rep <- v$firma$score_rep

leer <- function(meses,campos) {
 rbindlist(lapply(meses,function(m) {
   p <- file.path(raiz,"features",rep,paste0(m,".rds"))
   a <- readRDS(p)$datos
   stopifnot(all(c("numero_de_cliente","foto_mes","clase_ternaria",
                   campos)%in%names(a)))
   a[,c("numero_de_cliente","foto_mes","clase_ternaria",campos),
     with=FALSE]
 }))
}
matriz <- function(d,campos) {
 x <- data.matrix(d[,campos,with=FALSE])
 x[!is.finite(x)] <- NA_real_
 x
}

tr <- leer(c(202103L,202104L),campos_hist)
va <- leer(202106L,campos_hist)
stopifnot(nrow(tr)==326184L,nrow(va)==164114L,
          !anyNA(tr$clase_ternaria),
          sum(va$clase_ternaria=="BAJA+2")==1098L)

x <- matriz(tr,campos_hist)
xv <- matriz(va,campos_hist)
ids <- va$numero_de_cliente
clase <- va$clase_ternaria
ds <- lgb.Dataset(x,label=as.integer(
 tr$clase_ternaria%in%c("BAJA+1","BAJA+2")),
 params=list(max_bin=31L,feature_pre_filter=FALSE),
 free_raw_data=TRUE)
rm(tr,x)
gc()

medir <- function(p) {
 cv <- cumsum(ifelse(clase[order(-p,ids)]=="BAJA+2",
                      1072500,-27500))/1e6
 c(K10500=cv[10500L],meseta=mean(cv[10000L:11500L]))
}

ajustar <- function(hp,s,data=ds,score=xv,etapa="H") {
 firma <- list(hp=hp,seed=s,etapa=etapa,
               campos=if(etapa=="H")campos_hist else campos_prod,
               metric="auc")
 p <- file.path(out,paste0(etapa,"_",digest(firma),".rds"))
 if(file.exists(p)) {
   z <- readRDS(p)
   stopifnot(identical(z$firma,firma))
   return(z$prob)
 }
 pars <- modifyList(pars_base,hp)
 pars$seed <- s
 pars$num_threads <- 6L
 pars$metric <- "auc"
 log("ENTRENO",etapa,"SEMILLA",s,"ITER",hp$num_iterations)
 m <- lgb.train(params=pars,data=data,nrounds=hp$num_iterations)
 pr <- predict(m,score)
 stopifnot(all(is.finite(pr)),all(pr>=0 & pr<=1))
 lgb.save(m,sub("\\.rds$",".txt",p))
 atomic(list(firma=firma,prob=pr),p)
 log("GUARDADO",etapa,"SEMILLA",s)
 pr
}

log("DATOS BO VERIFICADOS. 742 FEATURES, SEIS EVALUACIONES.")

pset <- makeParamSet(
 makeIntegerParam("num_iterations",lower=512L,upper=4096L),
 makeIntegerParam("num_leaves",lower=8L,upper=256L),
 makeNumericParam("logh",lower=-3,upper=1))

objetivo <- makeSingleObjectiveFunction(
 name="SIN_CIERRES_GANANCIA",par.set=pset,minimize=TRUE,
 has.simple.signature=FALSE,fn=function(x) {
   a <- x
   hp <- list(num_iterations=as.integer(a$num_iterations),
              num_leaves=as.integer(a$num_leaves),
              min_sum_hessian_in_leaf=10^a$logh)
   met <- medir(ajustar(hp,seeds[1]))
   fila <- data.table(num_iterations=hp$num_iterations,
     num_leaves=hp$num_leaves,
     min_sum_hessian_in_leaf=hp$min_sum_hessian_in_leaf,
     K10500=unname(met["K10500"]),
     meseta=unname(met["meseta"]))
   fp <- file.path(out,"BO_EVALUACIONES.csv")
   fwrite(fila,fp,append=file.exists(fp))
   log("BO RESULTADO",met["K10500"],"MESETA",met["meseta"])
   -unname(met["K10500"])
 })

set.seed(100613L)
design <- generateDesign(3L,pset)
design <- rbind(data.frame(
 num_iterations=as.integer(hp_actual$num_iterations),
 num_leaves=as.integer(hp_actual$num_leaves),
 logh=log10(hp_actual$min_sum_hessian_in_leaf)),design)

ctrl <- makeMBOControl()
ctrl <- setMBOControlTermination(ctrl,iters=2L)
ctrl <- setMBOControlInfill(ctrl,crit=makeMBOInfillCritEI())

atomic(list(campos_hist=campos_hist,campos_prod=campos_prod,
 hp_actual=hp_actual,seeds=seeds,design=design),
 file.path(out,"PLAN.rds"))

res <- mbo(objetivo,design=design,control=ctrl,
 learner=mlr::makeLearner("regr.km",predict.type="se"))
atomic(res,file.path(out,"BO_TERMINADA.rds"))

a <- res$x
hp <- list(num_iterations=as.integer(a$num_iterations),
 num_leaves=as.integer(a$num_leaves),
 min_sum_hessian_in_leaf=10^a$logh)
atomic(hp,file.path(out,"HP_ELEGIDOS.rds"))

log("CONFIRMACION PAREADA: TRES SEMILLAS")
pn <- sapply(seeds[1:3],function(s) ajustar(hp,s))
pa <- sapply(seeds[1:3],function(s) ajustar(hp_actual,s))
mn <- medir(rowMeans(pn))
ma <- medir(rowMeans(pa))

tabla <- rbind(
 data.table(receta="HP_ACTUALES",
            K10500=unname(ma[1]),meseta=unname(ma[2])),
 data.table(receta="HP_BO",
            K10500=unname(mn[1]),meseta=unname(mn[2])))
fwrite(tabla,file.path(out,"CONFIRMACION.csv"))
print(tabla)

aporta <- unname(mn["meseta"]>ma["meseta"] &&
                mn["K10500"]>=ma["K10500"])

if(!aporta) {
 atomic(list(estado="SIN_MEJORA_CONFIRMADA",hp=hp,
             comparacion=tabla),file.path(out,"TERMINADO.rds"))
 log("SIN MEJORA CONFIRMADA. CONSERVO CANDIDATO ANTERIOR.")
} else {
 log("MEJORA CONFIRMADA. INICIO PRODUCCION DE DIEZ SEMILLAS")
 rm(ds,xv,va,pn,pa)
 gc()

 tr <- leer(c(202103L,202104L,202105L,202106L),campos_prod)
 fu <- leer(202108L,campos_prod)
 stopifnot(nrow(tr)==654066L,nrow(fu)==164647L,
           !anyNA(tr$clase_ternaria))

 xf <- matriz(fu,campos_prod)
 idf <- fu$numero_de_cliente
 xp <- matriz(tr,campos_prod)
 dp <- lgb.Dataset(xp,label=as.integer(
 tr$clase_ternaria%in%c("BAJA+1","BAJA+2")),
 params=list(max_bin=31L,feature_pre_filter=FALSE),
 free_raw_data=TRUE)
 rm(tr,fu,xp)
 gc()

 pp <- sapply(seeds,function(s)
   ajustar(hp,s,data=dp,score=xf,etapa="P"))
 prob <- rowMeans(pp)
 fwrite(data.table(numero_de_cliente=idf,prob=prob),
        file.path(out,"PROBABILIDADES_AGOSTO.csv"))

 for(k in c(10000L,10250L,10500L,10750L)) {
   elegidos <- idf[order(-prob,idf)][seq_len(k)]
   stopifnot(!anyDuplicated(elegidos))
   writeLines(format(elegidos,scientific=FALSE,trim=TRUE),
    file.path(out,paste0("SINCIERRES_BO_PROB10_K",k,".csv")))
 }
 atomic(list(estado="PRODUCCION_TERMINADA",hp=hp,ids=idf,
             prob=prob,comparacion=tabla),
        file.path(out,"TERMINADO.rds"))
 log("PRODUCCION COMPLETA. CSV LISTOS; NO SE ENVIARON SUBMITS.")
}
