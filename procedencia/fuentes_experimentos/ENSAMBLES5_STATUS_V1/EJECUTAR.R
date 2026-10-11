
.libPaths(unique(c("/home/ds/.local/lib/R/site-library", .libPaths())))
library(data.table)
setDTthreads(1L)
options(warn=1)
out <- Sys.getenv("DMEYF_DIAGNOSTICO")
raiz <- "/home/ds/buckets/b1/exp/DMEyF_MAESTRA_20261007"
previo <- file.path(raiz,"integral_clase_20261008",
 "0b6b06569ceee877ea42e00a072112f6b536e82c366f16427290dbd4457a2440")
cierre <- readRDS(file.path(previo,"CIERRE_CALCULO_INTEGRAL.rds"))
recetas <- c("CONTROL_ORIGINAL","RANK_TODAS","ARTIFICIALES_ORIGINAL")
guardar <- function(x,n) fwrite(x,file.path(out,n))
log <- function(...) {cat(format(Sys.time()), ..., "\n");flush.console()}

h0 <- readRDS(file.path(previo,"historia","CIERRE_CONTROL_ORIGINAL.rds"))
seeds <- sort(as.integer(h0$plan$semillas))
stopifnot(length(seeds)==20L,!anyDuplicated(seeds))
set.seed(20261010)
perm <- sample(seeds)
grupos <- lapply(0:19,function(i) perm[((i+0:4)%%20)+1L])
plan <- rbindlist(lapply(1:20,function(g)
 data.table(grupo=g,semilla=grupos[[g]],
            bloque_sin_solapamiento=g %in% c(1L,6L,11L,16L))))
guardar(plan,"GRUPOS_PREDEFINIDOS.csv")
stopifnot(all(table(plan$semilla)==5L))
writeLines(c(
 "Grupos fijados antes de evaluar; mismos grupos en las tres recetas.",
 "Cada semilla participa en cinco grupos.",
 "Grupos 1,6,11,16 no comparten semillas entre sí.",
 "Los 20 grupos se superponen: su SD no es incertidumbre del privado.",
 "Junio ya se uso para optimizar: evaluacion retrospectiva.",
 "No se selecciona el mejor grupo ni se optimiza el corte publico."
),file.path(out,"PROTOCOLO.txt"))

cargar <- function(ids,ref,mes) {
 ref <- copy(as.data.table(ref))
 setorder(ref,numero_de_cliente)
 stopifnot(!anyDuplicated(ref$numero_de_cliente),all(ref$foto_mes==mes))
 mat <- matrix(NA_real_,nrow(ref),length(seeds))
 encontrados <- integer()
 for(id in ids) {
  z <- readRDS(file.path(raiz,"scores",paste0(id,".rds")))$valor
  s <- as.integer(z$seed)
  j <- match(s,seeds)
  stopifnot(!is.na(j),!s %in% encontrados,z$mes==mes)
  t <- as.data.table(z$scores)
  stopifnot(nrow(t)==nrow(ref),!anyDuplicated(t$numero_de_cliente),
            all(t$foto_mes==mes))
  ix <- match(ref$numero_de_cliente,t$numero_de_cliente)
  stopifnot(!anyNA(ix))
  p <- t$prob[ix]
  stopifnot(length(p)==nrow(ref),all(is.finite(p)),all(p>=0 & p<=1))
  if(mes==202106L)
   stopifnot(identical(as.character(t$clase_ternaria[ix]),
                       as.character(ref$clase_ternaria)))
  mat[,j] <- p
  encontrados <- c(encontrados,s)
 }
 stopifnot(setequal(encontrados,seeds),!anyNA(mat),
           max(abs(rowMeans(mat)-ref$score))<1e-12)
 list(tabla=ref,mat=mat)
}

evaluar <- function(p,ref,receta,tamano,grupo) {
 orden <- order(-p,ref$numero_de_cliente)
 stopifnot(!anyNA(ref$clase_ternaria))
 cv <- cumsum(ifelse(ref$clase_ternaria[orden]=="BAJA+2",
                    1072500,-27500))/1e6
 cortes <- c(9000L,seq.int(10000L,11500L,250L))
 data.table(receta=receta,tamano=tamano,grupo=grupo,corte=cortes,
   ganancia=cv[cortes],
   meseta_10000_11500=mean(cv[10000:11500]),
   meseta_10250_10750=mean(cv[10250:10750]))
}

resultados <- list()
manifiesto <- list()
modelos <- list()
for(nom in recetas) {
 log("PROCESANDO",nom)
 h <- readRDS(file.path(previo,"historia",paste0("CIERRE_",nom,".rds")))
 stopifnot(setequal(as.integer(h$plan$semillas),seeds))
 a <- cargar(h$modelos,h$ensemble$tabla,202106L)
 if(nom=="CONTROL_ORIGINAL") control_junio <- copy(a$tabla)

 for(j in seq_along(seeds))
  resultados[[length(resultados)+1L]] <-
   evaluar(a$mat[,j],a$tabla,nom,1L,seeds[j])
 for(g in 1:20) {
  ix <- match(grupos[[g]],seeds)
  resultados[[length(resultados)+1L]] <-
   evaluar(rowMeans(a$mat[,ix,drop=FALSE]),a$tabla,nom,5L,g)
 }
 resultados[[length(resultados)+1L]] <-
  evaluar(rowMeans(a$mat),a$tabla,nom,20L,0L)
 rm(a);gc(verbose=FALSE)

 pr <- cierre$produccion[[nom]]
 en <- readRDS(file.path(raiz,"scores",paste0("ENS_",pr$ensemble,".rds")))
 b <- cargar(pr$modelos$modelo,en$tabla,202108L)
 modelos[[nom]] <- list(historia=h$modelos,produccion=pr$modelos)
 lote <- paste0("LOT_ENS5_",nom,"_K10500")
 carpeta <- file.path(out,lote)
 dir.create(carpeta,showWarnings=FALSE)

 for(g in 1:20) {
  p <- rowMeans(b$mat[,match(grupos[[g]],seeds),drop=FALSE])
  ids <- b$tabla$numero_de_cliente[
    head(order(-p,b$tabla$numero_de_cliente),10500L)]
  texto <- format(ids,scientific=FALSE,trim=TRUE,digits=22)
  stopifnot(length(texto)==10500L,!anyDuplicated(texto),
            all(grepl("^[0-9]+$",texto)))
  nombre <- sprintf("ENS5_%02d_K10500.csv",g)
  archivo <- file.path(carpeta,nombre)
  writeLines(texto,archivo)
  stopifnot(identical(readLines(archivo),texto))
  manifiesto[[length(manifiesto)+1L]] <- data.table(
    lote=lote,receta=nom,grupo=g,archivo=nombre,N=length(texto),
    semillas=paste(grupos[[g]],collapse=";"),
    sha256=digest::digest(file=archivo,algo="sha256"))
 }
 log("LOTE VERIFICADO",lote)
 rm(b,en,h,pr);gc(verbose=FALSE)
}
res <- rbindlist(resultados)
guardar(res,"GANANCIAS_JUNIO.csv")
resumen <- res[corte==10500L,
 .(cantidad=.N,media=mean(ganancia),sd=sd(ganancia),
   minimo=min(ganancia),maximo=max(ganancia),
   meseta_amplia=mean(meseta_10000_11500)),
 by=.(receta,tamano)]
guardar(resumen,"RESUMEN_JUNIO.csv")
guardar(rbindlist(manifiesto),"MANIFIESTO.csv")
saveRDS(modelos,file.path(out,"MODELOS_ORIGEN.rds"))
print(resumen)

log("AUDITORIA VISA Y MASTERCARD")
raw <- "/home/ds/buckets/b1/datasets/competencia_01_crudo.csv"
d <- fread(raw,select=c("numero_de_cliente","foto_mes",
                       "clase_ternaria","Visa_status","Master_status"),
           nThread=1L)
stopifnot(!anyDuplicated(d[,.(numero_de_cliente,foto_mes)]))
d[,mes_cal:=12L*(foto_mes%/%100L)+foto_mes%%100L]
claves <- paste(d$numero_de_cliente,d$mes_cal,sep="_")
completos <- d$foto_mes %in% 202103:202106
positivos <- !is.na(d$clase_ternaria) & d$clase_ternaria=="BAJA+2"
baja1 <- !is.na(d$clase_ternaria) & d$clase_ternaria=="BAJA+1"

d[,seleccion_control:=NA_integer_]
ix <- match(d$numero_de_cliente[d$foto_mes==202106L],
            control_junio$numero_de_cliente)
stopifnot(!anyNA(ix))
top <- head(order(-control_junio$score,
                  control_junio$numero_de_cliente),10500L)
d[d$foto_mes==202106L,seleccion_control:=as.integer(ix %in% top)]

tablas <- list()
agregar <- function(nombre,valor) {
 t <- data.table(foto_mes=d$foto_mes,variable=nombre,
   estado=valor,completo=completos,b2=positivos,b1=baja1,
   elegido=d$seleccion_control)
 r <- t[,.(N=.N,N_evaluables=sum(completo),
   BAJA2=sum(b2 & completo),BAJA1=sum(b1 & completo),
   seleccion_control_junio=sum(elegido==1L,na.rm=TRUE),
   BAJA2_fuera_control_junio=sum(b2 & elegido==0L,na.rm=TRUE)),
   by=.(foto_mes,variable,estado)]
 r[,tasa_BAJA2:=fifelse(N_evaluables>0,BAJA2/N_evaluables,NA_real_)]
 r[,tasa_BAJA1:=fifelse(N_evaluables>0,BAJA1/N_evaluables,NA_real_)]
 tablas[[length(tablas)+1L]] <<- r
}
for(v in c("Visa_status","Master_status")) {
 x <- d[[v]]
 actual <- ifelse(is.na(x),"NA",as.character(x))
 agregar(v,actual)
 for(lag in 1:2) {
  ix <- match(paste(d$numero_de_cliente,d$mes_cal-lag,sep="_"),claves)
  existe <- !is.na(ix)
  anterior <- x[ix]
  previo_estado <- ifelse(!existe,"SIN_HISTORIA",
                    ifelse(is.na(anterior),"NA",as.character(anterior)))
  agregar(paste0(v,"_transicion_lag",lag),
          paste(previo_estado,actual,sep=" -> "))
  bandera <- ifelse(!existe,"SIN_HISTORIA",
              ifelse(!is.na(anterior) & is.na(x),"PASO_A_NA",
                ifelse(is.na(anterior) & !is.na(x),"SALIO_DE_NA",
                  ifelse(is.na(anterior) & is.na(x),"NA_PERSISTENTE",
                    ifelse(anterior==x,"MISMO_ESTADO","CAMBIO_ESTADO")))))
  agregar(paste0(v,"_evento_lag",lag),bandera)
 }
}
agregar("ESTADO_CONJUNTO",
 paste(ifelse(is.na(d$Master_status),"NA",d$Master_status),
       ifelse(is.na(d$Visa_status),"NA",d$Visa_status),sep=" MC / VISA "))
guardar(rbindlist(tablas),"STATUS_TRANSICIONES_POR_MES.csv")

campos <- names(control_junio)
z <- readRDS(file.path(raiz,"scores",paste0(h0$modelos[1],".rds")))
guardar(data.table(campo=z$valor$campos[
 grepl("Visa_status|Master_status",z$valor$campos)]),
 "STATUS_YA_INCLUIDOS_EN_CONTROL.csv")
writeLines(c(
 "HIPOTESIS: transiciones explicitas agregan valor sobre status y lags existentes.",
 "Brazo A: CONTROL_ORIGINAL. Brazo B: A + transiciones Visa/Master.",
 "Conservar estados originales y NA; no imponer escala ordinal de riesgo.",
 "Historial por mes calendario exacto; SIN_HISTORIA separado de NA observado.",
 "Features candidatas: paso a NA, salida de NA, cambio y persistencia, lag1 y lag2.",
 "Transiciones categoricas con mapa aprendido solo en training; desconocidos separados.",
 "Agregar interacciones entre eventos de ambas tarjetas, sin utilizar meses futuros.",
 "Primera comparacion: mismos HP del control, mismas 20 semillas y meses.",
 "Junio: ganancia K10500 y mesetas; revisar diferencias pareadas.",
 "Junio reutilizado: evidencia retrospectiva, no test independiente.",
 "No usar tasas BAJA ni tablas de contingencia como features.",
 "Si aporta: BO propia y confirmacion antes de produccion.",
 "Este proceso solo audita y prepara: no entreno el brazo STATUS."
),file.path(out,"PLAN_EXPERIMENTO_STATUS.txt"))
log("ENSAMBLES Y AUDITORIA TERMINADOS")
