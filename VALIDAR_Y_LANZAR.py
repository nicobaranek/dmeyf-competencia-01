# Valido mi entrega y lanzo la réplica en una carpeta vacía.
import os, json, subprocess, hashlib, ast
from pathlib import Path

carpeta = Path(__file__).resolve().parent
os.chdir(carpeta)
crudo = Path(os.environ.get('DMEYF_DATOS', '/home/ds/buckets/b1/datasets/competencia_01_crudo.csv'))
assert crudo.is_file(), 'Debo indicar la ruta del crudo.'
with crudo.open('rb') as f:
    assert hashlib.file_digest(f, 'md5').hexdigest() == 'b6f759764c5d04cbf068548517ae1249'
for nombre in ['DMEyF_Nicky_Entrega_Final.ipynb', 'BO_Origen.ipynb']:
    nb = json.loads((carpeta / nombre).read_text())
    assert nb['nbformat'] == 4
    for i, celda in enumerate(nb['cells']):
        if celda['cell_type'] != 'code':
            continue
        texto = ''.join(celda['source'])
        if 'from pathlib import Path' in texto:
            ast.parse(texto)
        else:
            subprocess.run(['Rscript','--vanilla','-e',
                'invisible(parse(file="stdin"))'],input=texto,text=True,check=True)
subprocess.run(['Rscript','--vanilla','VALIDAR_SINTAXIS.R'],check=True)
salida = carpeta / 'salida_verificacion'
assert not salida.exists(), 'Uso una carpeta vacía para verificar desde el crudo. No duplico la réplica.'
env = os.environ.copy()
env.update(DMEYF_DATOS=str(crudo),DMEYF_ENTREGA=str(carpeta),
           DMEYF_SALIDA=str(salida),R_LIBS_USER='/home/ds/.local/lib/R/site-library',
           OMP_NUM_THREADS='6',OPENBLAS_NUM_THREADS='1',MKL_NUM_THREADS='1')
subprocess.run(['Rscript','--vanilla','-e',
    "for(p in c('data.table','lightgbm','digest','jsonlite','filelock'))stopifnot(requireNamespace(p,quietly=TRUE));"
    "stopifnot(as.character(getRversion())=='4.6.1',as.character(packageVersion('lightgbm'))=='4.7.0.99',as.character(packageVersion('data.table'))=='1.18.6.1');"
    "cat('VERSIONES Y PAQUETES VERIFICADOS\\n')"],env=env,check=True)
salida.mkdir()
log = carpeta / 'REPLICA.log'
with log.open('w') as f:
    proc = subprocess.Popen(['Rscript','--vanilla','REPRODUCIR_FINAL.R'],
        env=env,stdout=f,stderr=f,start_new_session=True)
(carpeta / 'RUNNER_REPLICA.json').write_text(json.dumps({
    'pid':proc.pid,'log':str(log),'salida':str(salida)},indent=2))
print('SINTAXIS Y CRUDO VERIFICADOS. REPLICA LANZADA PID:',proc.pid)
print('LOG:',log)
print('La reproducción exacta se confirma con VERIFICACION_FINAL.txt, no con este lanzamiento.')
