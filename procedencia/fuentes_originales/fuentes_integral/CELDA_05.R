# Dejo anotado: CELDA 05 — Audito evidencia existente, cargo el crudo y verifico el target.
INVENTARIO <- inventario_previo()
D <- cargar_base()
print(D[,.N,by=.(foto_mes,clase_ternaria)][order(foto_mes,clase_ternaria)])
DRIFT <- cache('auditoria',list(componente='drift_todos_v1',raw=RAW_HASH),function()auditar_drift(D))
cat('El target de training es BAJA+1/BAJA+2; la ganancia remunera solamente BAJA+2.\n')
cat('Julio no ofrece H2 completo; agosto no se evalúa con etiquetas reconstruidas.\n')
