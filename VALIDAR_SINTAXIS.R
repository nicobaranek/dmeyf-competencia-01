# Reviso la sintaxis antes de iniciar cualquier entrenamiento.
archivos <- list.files('.',pattern='[.]R$',recursive=TRUE,full.names=TRUE)
for(archivo in archivos) {
 invisible(parse(file=archivo))
 cat('SINTAXIS OK:',archivo,'\n')
}
cat('TODOS LOS ARCHIVOS R PASARON LA REVISION DE SINTAXIS.\n')
