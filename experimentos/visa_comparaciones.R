
.libPaths(unique(c("/home/ds/.local/lib/R/site-library",.libPaths())))
library(data.table)
setDTthreads(1L)
out <- Sys.getenv("DMEYF_VISA_AUDIT_OUT")
lk <- filelock::lock("/home/ds/.dmeyf_visa_comparaciones.lock",timeout=0)
if(is.null(lk))stop("Otra comparación VISA está activa.")
guardar <- function(x,n) fwrite(x,file.path(out,n))
log <- function(...) {cat(format(Sys.time()),...,"\n");flush.console()}
base <- Sys.getenv("DMEYF_LAB_BASE")
raiz <- Sys.getenv("DMEYF_LAB_RAIZ")
src <- file.path(base,"STATUS_ABLACIONES_H_V1","SOLO_VISA")
z <- readRDS(file.path(src,"TERMINADO.rds"))
ref <- as.data.table(z$ref)
semillas <- as.integer(z$plan$semillas)
ids_control <- z$plan$modelos_control
stopifnot(length(semillas)==10L,length(ids_control)==10L,
          !anyDuplicated(semillas),!anyDuplicated(ref$numero_de_cliente),
          nrow(ref)==164114L,all(ref$foto_mes==202106L),
          !anyNA(ref$clase_ternaria),
          sum(ref$clase_ternaria=="BAJA+2")==1098L)
controles <- lapply(ids_control,function(id)
 readRDS(file.path(raiz,"scores",paste0(id,".rds"))))
orden <- match(semillas,vapply(controles,function(a)
 as.integer(a$valor$seed),integer(1)))
stopifnot(!anyNA(orden))
controles <- controles[orden]
C <- V <- matrix(NA_real_,nrow(ref),10L)
for(j in 1:10) {
 sc <- controles[[j]]$valor$scores
 ix <- match(ref$numero_de_cliente,sc$numero_de_cliente)
 stopifnot(!anyDuplicated(sc$numero_de_cliente),!anyNA(ix),
  identical(as.character(ref$clase_ternaria),
            as.character(sc$clase_ternaria[ix])))
 C[,j] <- sc$prob[ix]
 v <- readRDS(file.path(src,paste0("SCORES_",semillas[j],".rds")))
 ix <- match(ref$numero_de_cliente,v$ids)
 stopifnot(!anyDuplicated(v$ids),!anyNA(ix))
 V[,j] <- v$prob[ix]
}
stopifnot(all(is.finite(C)),all(is.finite(V)),
          all(C>=0 & C<=1),all(V>=0 & V<=1))
pc <- rowMeans(C);pv <- rowMeans(V)
stopifnot(length(z$prob_control)==nrow(ref),
          max(abs(pc-z$prob_control))<1e-10)
campo <- intersect(c("prob_tarjetas","prob_solo_visa","prob_visa"),names(z))
stopifnot(length(campo)>=1L,
          max(abs(pv-z[[campo[1]]]))<1e-10)
log("PREDICCIONES Y ENSAMBLE ORIGINAL VERIFICADOS")

curva <- function(p) {
 ix <- order(-p,ref$numero_de_cliente)
 cumsum(ifelse(ref$clase_ternaria[ix]=="BAJA+2",1072500,-27500))/1e6
}
resumir <- function(a,b) data.table(
 delta_K10500=b[10500]-a[10500],
 delta_meseta_amplia=mean(b[10000:11500]-a[10000:11500]),
 delta_meseta_cercana=mean(b[10250:10750]-a[10250:10750]))
a <- curva(pc);b <- curva(pv)
tabla <- data.table(corte=seq_along(a),control=a,visa=b,delta=b-a)
guardar(tabla,"CURVAS_COMPLETAS.csv")
guardar(tabla[corte>=7500 & corte<=12500 & corte%%250==0],
        "CURVAS_CADA_250.csv")
total <- resumir(a,b)
guardar(total,"RESUMEN_ENSAMBLE10.csv")

set.seed(100613)
opciones <- combn(1:10,5)
grupos <- opciones[,sample.int(ncol(opciones),20),drop=FALSE]
guardar(data.table(grupo=rep(1:20,each=5),
 semilla=semillas[as.vector(grupos)]),"GRUPOS_ENS5.csv")
pares <- list()
entra <- sale <- integer(nrow(ref))
for(i in 1:20) {
 j <- grupos[,i]
 c <- rowMeans(C[,j,drop=FALSE])
 v <- rowMeans(V[,j,drop=FALSE])
 pares[[i]] <- cbind(data.table(grupo=i),resumir(curva(c),curva(v)))
 ic <- order(-c,ref$numero_de_cliente)[1:10500]
 iv <- order(-v,ref$numero_de_cliente)[1:10500]
 e <- setdiff(iv,ic);s <- setdiff(ic,iv)
 entra[e] <- entra[e]+1L;sale[s] <- sale[s]+1L
 log("GRUPO",i,"/20")
}
pares <- rbindlist(pares)
guardar(pares,"COMPARACION_ENS5_PAREADA.csv")
resumen <- rbindlist(lapply(setdiff(names(pares),"grupo"),function(n) {
 d <- pares[[n]]
 data.table(metrica=n,media=mean(d),mediana=median(d),
 minimo=min(d),maximo=max(d),grupos_mejora=sum(d>0),
 grupos_empata=sum(d==0),grupos_empeora=sum(d<0))
}))
guardar(resumen,"RESUMEN_ENS5.csv")
ic <- order(-pc,ref$numero_de_cliente)[1:10500]
iv <- order(-pv,ref$numero_de_cliente)[1:10500]
e <- setdiff(iv,ic);s <- setdiff(ic,iv)
intercambio <- data.table(entran=length(e),salen=length(s),
 baja2_entran=sum(ref$clase_ternaria[e]=="BAJA+2"),
 baja2_salen=sum(ref$clase_ternaria[s]=="BAJA+2"),
 comunes=length(intersect(ic,iv)),
 jaccard=length(intersect(ic,iv))/length(union(ic,iv)))
stopifnot(abs((intercambio$baja2_entran-intercambio$baja2_salen)*1.1-
 total$delta_K10500)<1e-8)
guardar(intercambio,"RESUMEN_INTERCAMBIO.csv")
clientes <- copy(ref)
clientes[,`:=`(prob_control=pc,prob_visa=pv,
 seleccionado_control=seq_len(.N)%in%ic,
 seleccionado_visa=seq_len(.N)%in%iv,
 veces_entra_ENS5=entra,veces_sale_ENS5=sale)]
guardar(clientes,"CLIENTES_Y_ESTABILIDAD.csv")
writeLines(c(
 "No se entrenaron modelos nuevos. Evaluacion retrospectiva en junio.",
 "Veinte grupos superpuestos de cinco semillas: no son muestras independientes.",
 "No inferir incertidumbre del privado a partir de esta dispersion."
),file.path(out,"ALCANCE.txt"))
saveRDS(list(resumen=total,ens5=resumen,intercambio=intercambio,
 grupos=grupos,semillas=semillas),file.path(out,"TERMINADO.rds"))
print(total);print(resumen);print(intercambio)
log("COMPARACIONES VISA TERMINADAS")
filelock::unlock(lk)
