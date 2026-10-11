
.libPaths(unique(c("/home/ds/.local/lib/R/site-library",.libPaths())))
library(data.table)
setDTthreads(1L)
options(warn=2)
out <- Sys.getenv("DMEYF_AUDITORIA_CORREGIDA")
raiz <- "/home/ds/buckets/b1/exp/DMEyF_MAESTRA_20261007"
previo <- file.path(raiz,"integral_clase_20261008",
 "0b6b06569ceee877ea42e00a072112f6b536e82c366f16427290dbd4457a2440")
h0 <- readRDS(file.path(previo,"historia","CIERRE_CONTROL_ORIGINAL.rds"))
control_junio <- copy(as.data.table(h0$ensemble$tabla))
setorder(control_junio,numero_de_cliente)
guardar <- function(x,n) fwrite(x,file.path(out,n))
log <- function(...) {cat(format(Sys.time()),...,"\n");flush.console()}

raw <- "/home/ds/buckets/b1/datasets/competencia_01_crudo.csv"
d <- fread(raw,select=c("numero_de_cliente","foto_mes",
                       "Visa_status","Master_status"),
           nThread=1L)
stopifnot(!anyDuplicated(d[,.(numero_de_cliente,foto_mes)]))

stopifnot(!anyDuplicated(control_junio$numero_de_cliente),
          all(control_junio$foto_mes==202106L),
          !anyNA(control_junio$clase_ternaria))
filas <- which(d$foto_mes==202106L)
stopifnot(length(filas)==nrow(control_junio),
          setequal(d$numero_de_cliente[filas],
                   control_junio$numero_de_cliente))
idx <- match(d$numero_de_cliente[filas],
             control_junio$numero_de_cliente)
d[,clase_ternaria:=NA_character_]
set(d,i=filas,j="clase_ternaria",
    value=as.character(control_junio$clase_ternaria[idx]))
stopifnot(!anyNA(d$clase_ternaria[filas]),
          sum(d$clase_ternaria[filas]=="BAJA+2")==1098L,
          sum(d$clase_ternaria[filas]=="BAJA+1")==874L)
cat("ETIQUETAS JUNIO VERIFICADAS POR ID: OK\n")

d[,mes_cal:=12L*(foto_mes%/%100L)+foto_mes%%100L]
claves <- paste(d$numero_de_cliente,d$mes_cal,sep="_")
completos <- d$foto_mes==202106L
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
log("AUDITORIA CORREGIDA TERMINADA: TASAS SOLO EN JUNIO")

t <- fread(file.path(out,"STATUS_TRANSICIONES_POR_MES.csv"))
stopifnot(all(t[foto_mes!=202106L,N_evaluables]==0L),
          all(is.na(t[foto_mes!=202106L,tasa_BAJA2])))
cat("\nCAMPOS QUE YA USA EL CONTROL:\n")
print(fread(file.path(out,"STATUS_YA_INCLUIDOS_EN_CONTROL.csv")))
cat("\nEVENTOS EN JUNIO:\n")
print(t[foto_mes==202106L & grepl("_evento_lag1$",variable),
 .(variable,estado,N,BAJA2,tasa_BAJA2,BAJA2_fuera_control_junio)])
writeLines("OK: etiquetas verificadas; tasas exclusivamente de junio.",
           file.path(out,"TERMINADO.txt"))
