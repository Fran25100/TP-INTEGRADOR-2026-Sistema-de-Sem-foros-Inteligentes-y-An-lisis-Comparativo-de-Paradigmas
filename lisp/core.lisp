
*** - LOAD: A file with name $file does not exist
Break 1 [3]>;==========================================================================================================
#| 
Funcion: Transicion
NATURALEZA: Pura
ESTRATEGIA: estructura condicional (implementada con COND)
IMPACTO: No destructiva
|#
;=============================================================================================================================
( defun transicion (color-actual cambiar-a)
    (cond
        ((and (eq color-actual 'en-rojo) (eq cambiar-a 'en-rojo-intermitente)) (list color-actual "CAMBIAR-A-ROJO-INTERMITENTE" ))
        ((and (eq color-actual 'en-rojo-intermitente) (eq cambiar-a 'en-verde)) (list color-actual "CAMBIAR-A-VERDE" ))
        ((and (eq color-actual 'en-verde) (eq cambiar-a 'en-verde-intermitente)) (list color-actual "CAMBIAR-A-Verde-INTERMITENTE" ))
        ((and (eq color-actual 'en-verde-intermitente) (eq cambiar-a 'en-amarillo)) (list color-actual "CAMBIAR-A-AMARILLO" ))
        ((and (eq color-actual 'en-amarillo) (eq cambiar-a 'en-amarillo-intermitente)) (list color-actual "CAMBIAR-A-AMARILLO-INTERMITENTE" ))
        ((and (eq color-actual 'en-amarillo-intermitente) (eq cambiar-a 'en-rojo)) (list color-actual "CAMBIAR-A-ROJO" ))
        (t (list color-actual 'accion-por-defecto ))
    )
)

TRANSICION
Break 1 [3]> (transicion 'en-verde 'CAMBIAR-A-VERDE-INTERMITENTE)

(EN-VERDE "CAMBIAR-A-VERDE-INTERMITENTE")
Break 1 [3]> (transicion 'en-rojo 'CAMBIAR-A-ROJO-INTERMITENTE)

(EN-ROJO "CAMBIAR-A-ROJO-INTERMITENTE")
Break 1 [3]> (transicion 'en-amarillo 'CAMBIAR-A-AMARILLO-INTERMITENTE)

(EN-AMARILLO "CAMBIAR-A-AMARILLO-INTERMITENTE")
Break 1 [3]> (transicion 'en-verde-intermitente 'CAMBIAR-A-ROJO)

(EN-VERDE-INTERMITENTE "CAMBIAR-A-ROJO")
Break 1 [3]> (transicion 'en-rojo-intermitente 'CAMBIAR-A-VERDE)

(EN-ROJO-INTERMITENTE "CAMBIAR-A-VERDE")
Break 1 [3]> (transicion 'en-amarillo-intermitente 'CAMBIAR-A-ROJO)

(EN-AMARILLO-INTERMITENTE "CAMBIAR-A-ROJO")
break 1 [3]> (transicion 'EN-AMARILLO 'ACCION-POR-DEFECTO)

(EN-AMARILLO ACCION-POR-DEFECTO)

Break 3 [5]> ;-------------------------------------------------------------------------------------------------------------------
#| Funcion: timer
				Naturaleza: Impura (escribe en pantalla segun los datos de entrada)
				Estrategia: Orden Superior (implementada con format)
				Impacto En Memoria: No Destructiva, no realiza cambios
 |#
;-------------------------------------------------------------------------------------------------------------------

( defun timer (timestap )
    (let ((segundos (mod timestap 225)))
        (cond
            ((and (>= segundos 0) (<= segundos 89)) 'en-rojo )
            ((and (>= segundos 90) (<= segundos 92)) 'en-rojo-intermitente )
            ((and (>= segundos 93) (<= segundos 212)) 'en-verde )
            ((and (>= segundos 213) (<= segundos 215)) 'en-verde-intermitente )
            ((and (>= segundos 216) (<= segundos 221)) 'en-amarillo )
            ((and (>= segundos 222) (<= segundos 224)) 'en-amarillo-intermitente ) 
        )
    )
)

Break 3 [5]> ;-------------------------------------------------------------------------------------------------------------------
#| Funcion: LogginLights
				Naturaleza: Impura (escribe en pantalla segun los datos de entrada)
				Estrategia: Orden Superior (implementada con format)
				Impacto En Memoria: No Destructiva, no realiza cambios
 |#
;-------------------------------------------------------------------------------------------------------------------

#| (defun LogginLights (timeUnix color-actual cambio-color)
					(format t "Tiempo ~D: la luz ah cambiado de color ~S a ~S" timeUnix color-actual cambio-color)
)
LOGGINLIGHTS
Break 3 [5]> (LogginLights 56095 'rojo 'verde)
Tiempo 56095: la luz ah cambiado de color ROJO a VERDE
NIL|#
;correcion: se automatizo el tiempo para que sea calculado directamente dentro de esta funcion, en vez de un tiempo que puede estar
;desactualizado, restamos al tiempo el cual esta dado desde 1970 unos 70 años para que se sicronicen correctamente. 
(defun LogginLights (color-actual cambio-color)
					(format t "Tiempo ~D: la luz ah cambiado de color ~S a ~S" (- (get-universal-time) 2208988800) color-actual cambio-color)
)

Break 3 [5]> ;-----------------------------------------------------------------------------------------
;requerimiento 4.a
;; ========================================================
;; FUNCIÓN: duracion-ciclo
;; NATURALEZA: Pura
;; ESTRATEGIA: Simple (no recursiva, no predicado, no utiliza funciones de orden superior)
;; IMPACTO: No destructiva
;; ========================================================
(defun duracion-ciclo ()
    (+ 120 90 6)
)

;requerimiento 4.b
;; ========================================================
;; FUNCIÓN: recomendacion-ciclo
;; NATURALEZA: Pura
;; ESTRATEGIA: alternativa doble (condicional IF)
;; IMPACTO: No destructiva
;; ========================================================
(defun recomendacion-ciclo (duracion)
    (if (and (>= duracion 35) (<= duracion 150)); preferi no ser tan especifico y cambiarlo a un if
		"ciclo optimo"
		"ciclo no optimo"
    )
)
(print (recomendacion-ciclo (duracion-ciclo)))
;esta en hecho en visual code por eso capaz es distinto a los demas funciones de mis compañeros

;requisito 5
;; ========================================================
;; FUNCIÓN: ciclos-por-tiempo
;; NATURALEZA: Impura (escribe en pantalla un resultado dependiendo de los minutos de entrada)
;; ESTRATEGIA: estructura secuencial (no presenta recursion en su implementacion)
;; IMPACTO: No destructiva
;; ========================================================

(defun ciclos-por-tiempo(minutos)
    (print "La cantidad de ciclos es de:")
    (print(truncate(/ (* minutos 60) 216 )))  ;truncate toma el resultado de una operacion y elimina el decimal, si el resultado es 28.9, quedaria 28 
 )
;--------------------------------------------------------------------------------------------------------------------------------------
;requisito 6
#|======================================================================================   
FUNCION AUXILIAR: CalcularRestoIni
NATURALEZA: pura (dependiendo del resto que recibe, retorna un resultado)
ESTRATEGIA: alternativa Multiple (cond)
IMPACTO: No destrutiva
========================================================================================
|#
(defun calcularRestoIni (restoIni)
					(cond ((<= 0 restoIni 90) (list (- 90 restoIni) 6 120)) ;(-90 restoIni) --> indica lo consumido por rojo
					((<= 90 restoIni 96) (list 0 (- 216 restoIni 120) 120)) ;(- 216 restoIni 120) --> indica lo consumido por amarillo
					(t (list 0 0 (- 216 restoIni)) ) ;(- 216 restoIni) -->indica lo consumido por verde
)
)
#|======================================================================================   
FUNCION AUXILIAR: CalcularRestoFin
NATURALEZA: pura (dependiendo del resto que recibe, retorna un resultado)
ESTRATEGIA: alternativa Multiple (cond)
IMPACTO: No destrutiva
========================================================================================
|#
(defun calcularRestoFin (restoFin)
				(cond ((<= 0 restoFin 90) (list restoFin 0 0)) ; restoFin --> indica lo consumido por rojo
					((<= 90 restoFin 96) (list 90 (- restoFin 90) 0)) ;(- restoFin 90) --> indica lo consumido por amarillo
					(t (list 90 6 (- restoFin 90 6)) ) ; (- restoFin 90 6) --> indica lo consumido por verde
)
)
#|=================================================================================================  
FUNCION AUXILIAR: calcularPorcentajes
NATURALEZA: Pura (devuelve una lista con los porcentajes de los restos de cada color del semaforo)
ESTRATEGIA: alternativa Multiple (cond)
IMPACTO: No destrutiva
==================================================================================================
|#
(defun calcularPorcentajes (ListaIni ListaFin)
				(list (float(/(*(+ 1440 (car ListaFin) (car ListaIni))100) 3600)) ;rojo
				(float(/(*(+ 96 (cadr ListaFin) (cadr ListaIni))100)3600)) ;amarillo
				(float(/(*(+ 1920 (caddr ListaFin) (caddr ListaIni))100)3600)) ;verde
)
)
#|======================================================================================   
FUNCION: calcularPorcentajes
NATURALEZA: impura (imprime en pantalla los resultados de los porcentajes)
ESTRATEGIA: alternativa Doble (if)
IMPACTO: No destrutiva
========================================================================================
|#
#| CASO FALLIDO:
(defun distribucionTemp (unix)
				(if (zerop (mod unix 216)) "| 55,5% verde| 41,6% rojo | 2,7% amarillo|" ------> el mod nos muestra
	donde estamos parados, si es = 0 son 16 verdes completos, 16 amarillos y 16,6 rojos. en porcentajes de tiempo serian:
	55,5% V, 41,6% R y 2,7% A.
				;((not(zerop (mod unix 216) 0)) 
				(format t "| ~A% rojo| ~A% amarillo | ~A% verde|"
						(car (calcularPorcentajes (calcularRestoIni (mod unix 216)) (calcularRestoFin (mod (- 3600 (mod unix 216)) 216) ) ))
			;entra al primer elemento (porcentaje del rojo) de la lista, calcularPorcentajes recibe 2 parametros
				;el 1er, devuelve una lista de los restos CONSUMIDOS al inicio extremo de la hora
				;el 2do, devuelve una lista con los restos CONSUMIDOS en el extremo final de la hora, el calculo "(mod (- 3600 (mod unix 216)) 216)"
						;es el resultado de: (- 3600 (mod unix 216) = "tiempo Acotado" que es lo que me queda del tiempo en la 2da hora (sacando el consumido
						;del resto inicial), y el mod externo nos da el extremo final de esa hora.
					(cadr (calcularPorcentajes (calcularRestoIni (mod unix 216)) (calcularRestoFin (mod (- 3600 (mod unix 216)) 216) ) ))
			;entra al 2do elemento de la lista de los porcentajes (especificamente el amarillo) y hace el mismo proceso anterior
				(caddr (calcularPorcentajes (calcularRestoIni (mod unix 216)) (calcularRestoFin (mod (- 3600 (mod unix 216)) 216) ) ))
			;entra al 3er elemento de la lista de los porcentajes (verde), sigue el mismo proceso
	)
)					
)|#
				
;CORRECTO
(defun distribucionTemp (unix)
				(if (zerop (mod unix 216)) "| 55,5% verde| 41,6% rojo | 2,7% amarillo|" #|------> el mod nos muestra
	donde estamos parados, si es = 0 son 16 verdes completos, 16 amarillos y 16,6 rojos. en porcentajes de tiempo serian:
	55,5% V, 41,6% R y 2,7% A.|#
				
				;((not(zerop (mod unix 216) 0)) 
				(format t "| ~A% rojo| ~A% amarillo | ~A% verde|"
						(car (calcularPorcentajes (calcularRestoIni (mod unix 216)) (calcularRestoFin (mod (- 3600 (- 216(mod unix 216))) 216) ) ))
						;entra al primer elemento (porcentaje del rojo) de la lista, calcularPorcentajes recibe 2 parametros
				;el 1er, devuelve una lista de los restos CONSUMIDOS al inicio extremo de la hora
				;el 2do, devuelve una lista con los restos CONSUMIDOS en el extremo final de la hora, el calculo "(mod (- 3600 (- 216(mod unix 216))) 216)"
						;es el resultado de: (- 3600 (- 216(mod unix 216))) = "tiempo Acotado" que es lo que me queda del tiempo en la 2da hora (sacando el consumido
						;del resto inicial), y el mod externo nos da el extremo final de esa hora.
					(cadr (calcularPorcentajes (calcularRestoIni (mod unix 216)) (calcularRestoFin (mod (- 3600 (- 216(mod unix 216))) 216) ) ))
					;entra al 2do elemento de la lista de los porcentajes (especificamente el amarillo) y hace el mismo proceso anterior
				(caddr (calcularPorcentajes (calcularRestoIni (mod unix 216)) (calcularRestoFin (mod (- 3600 (- 216(mod unix 216))) 216) ) ))
				;entra al 3er elemento de la lista de los porcentajes (verde), sigue el mismo proceso
	)
)					
)
;--------------------------------------------------------------------------------------------------------------------------------------



