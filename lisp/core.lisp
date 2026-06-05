
*** - LOAD: A file with name $file does not exist
Break 1 [3]> #| Funcion: Transicion
				Naturaleza: Impura (escribe en pantalla segun los datos de entrada)
				Estrategia: Orden Superior
				Impacto En Memoria: No Destructiva, no realiza cambios

 |#

(defun transicion (color-actual cambiar-a)
			(cond ((and (equal color-actual 'en-verde) (equal cambiar-a 'amarillo)) (list color-actual "cambiar-a-amarillo"))
			((and (equal color-actual 'en-amarillo) (equal cambiar-a 'rojo)) (list color-actual "cambair-a-rojo")) 
			((and (equal color-actual 'en-rojo) (equal cambiar-a 'verde)) (list color-actual "cambiar-a-verde"))
			(t (list color-actual 'accion-por-defecto))
)
)

TRANSICION
Break 1 [3]> (transicion 'en-rojo 'verde)

(EN-ROJO "cambiar-a-verde")
Break 1 [3]> ;-------------------------------------------------------------------------------------------------------------------
#| Funcion: timer
				Naturaleza: Impura (escribe en pantalla segun los datos de entrada)
				Estrategia: Orden Superior (implementada con cond, and y mod)
				Impacto En Memoria: No Destructiva, no realiza cambios

 |#
;-------------------------------------------------------------------------------------------------------------------

(defun time (unix)
				(cond (and (>= (mod unix 216) 0) (<= (mod unix 216) 89) "rojo")
				((and (>= (mod unix 216) 90) (<= (mod unix 216) 95)) "amarillo")
				((and (>= (mod unix 216) 96) (<= (mod unix 216) 216)) "verde")
)
)

** - Continuable Error
DEFUN/DEFMACRO(TIME): #<PACKAGE COMMON-LISP> is locked
Si contin■a (tecleando `continue'): Ignore the lock and proceed
The following restarts are also available:
ABORT          :R1      Abort debug loop
Break 2 [4]> (defun time-unix (unix)
				(cond (and (>= (mod unix 216) 0) (<= (mod unix 216) 89) "rojo")
				((and (>= (mod unix 216) 90) (<= (mod unix 216) 95)) "amarillo")
				((and (>= (mod unix 216) 96) (<= (mod unix 216) 216)) "verde")
)
)

TIME-UNIX
Break 2 [4]> (time-unix 216)

*** - COND: variable AND has no value
Es posible continuar en los siguientes puntos:
USE-VALUE      :R1      Input a value to be used instead of AND.
STORE-VALUE    :R2      Input a new value for AND.
ABORT          :R3      Abort debug loop
ABORT          :R4      Abort debug loop
Break 3 [5]> (defun time-unix (unix)
				(cond ((and (>= (mod unix 216) 0) (<= (mod unix 216) 89)) "rojo")
				((and (>= (mod unix 216) 90) (<= (mod unix 216) 95)) "amarillo")
				((and (>= (mod unix 216) 96) (<= (mod unix 216) 216)) "verde")
)
)


TIME-UNIX
Break 3 [5]> (time-unix 216)

"rojo"
Break 3 [5]> (time-unix 509)

"rojo"
Break 3 [5]> (time-unix 1024)

"verde"
Break 3 [5]> (time-unix 894)

"rojo"
Break 3 [5]> (time-unix 1000)

"verde"
Break 3 [5]> (time-unix 985)

"verde"
Break 3 [5]> (time-unix 935)

"rojo"
Break 3 [5]> (time-unix 945)

"rojo"
Break 3 [5]> (time-unix 955)

"amarillo"
Break 3 [5]> (time-unix 951)

"rojo"
Break 3 [5]> (time-unix 952)

"rojo"
Break 3 [5]> (time-unix 953)

"rojo"
Break 3 [5]> (time-unix 954)

"amarillo"
Break 3 [5]> (time-unix 864)

"rojo"
Break 3 [5]> (time-unix 863)

"verde"
Break 3 [5]> #| como se puede observar, el limite inferior de rojo en este ejemplo es de 953, por lo que al restar 90s, se obtiene
el valor de "verde", pero si en vez de eso se restara un valor menor a 90s, se obtiene el valor de amarillo. esto debido
al ciclo del semaforo: verde->amarillo->rojo->verde...|#
(print "hola")

"hola" 
"hola"
Break 3 [5]> (time-unix 1034)

"verde"
Break 3 [5]> (time-unix 1033)

"verde"
Break 3 [5]> (time-unix 954)

"amarillo"
Break 3 [5]> (time-unix 953)

"rojo"
Break 3 [5]> (time-unix 953)

"rojo"
Break 3 [5]> #|1-CORRECCION:como se puede observar, el limite SUPERIOR de rojo en este ejemplo es de 953, por lo que al restar 90s, se obtiene
el valor de "verde", pero si en vez de eso se sumara un valor de hasta maximo a 6s, se obtiene el valor de amarillo. esto debido
al ciclo del semaforo: verde->amarillo->rojo->verde... al restar 90 nos estariamos ubicando en forma de ciclo contrario
						verde->rojo->amarillo->verde|#
(print "NUEVA ACTUALIZACION")

"NUEVA ACTUALIZACION" 
"NUEVA ACTUALIZACION"
;-------------------------------------------------------------------------------------------------------------------
#| Funcion: timer
				Naturaleza: Impura (escribe en pantalla segun los datos de entrada)
				Estrategia: Orden Superior (implementada con cond, and y mod)
				Impacto En Memoria: No Destructiva, no realiza cambios

 |#
;-------------------------------------------------------------------------------------------------------------------
Break 3 [5]> (defun timer (timeUnix)
				(cond ((and (>= (mod unix 216) 0) (<= (mod unix 216) 89)) "rojo")
				((and (>= (mod unix 216) 96) (<= (mod unix 216) 216)) "verde")
)
)

TIMER
Break 3 [5]> ;-------------------------------------------------------------------------------------------------------------------
#| Funcion: LogginLights
				Naturaleza: Impura (escribe en pantalla segun los datos de entrada)
				Estrategia: Orden Superior (implementada con format)
				Impacto En Memoria: No Destructiva, no realiza cambios
 |#
;-------------------------------------------------------------------------------------------------------------------

(defun LogginLights (timeUnix color-actual cambio-color)
					(format t "Tiempo ~D: la luz ah cambiado de color ~S a ~S" timeUnix color-actual cambio-color)
)

LOGGINLIGHTS
Break 3 [5]> (LogginLights 56095 'rojo 'verde)
Tiempo 56095: la luz ah cambiado de color ROJO a VERDE
NIL
Break 3 [5]> 


; requerimiento 4 en proceso, 4a
(defun duracion-ciclo(duracion) 
	(if (and (>= duracion 35) (<= duracion 150) )
		"La duracion esta en el rango optimo"
		"La duracion NO SE en encuentra en el rango optimo" 
	  )
	)
; requisito 4b aun en proceso
(defun recomendacion-ciclo(duracion) 


	)
;requisito 5
;; ========================================================
;; FUNCIÓN: ciclos-por-tiempo
;; NATURALEZA: Pura (dependiendo del tiempo que recibe, retorna un resultado)
;; ESTRATEGIA: es una funcion que retorna un resultado
;; IMPACTO: No destructiva
;; ========================================================

(defun ciclos-por-tiempo(minutos)
    (print "La cantidad de ciclos es de:")
    (print(truncate(/ (* minutos 60) 216 )))  ;truncate toma el resultado de una operacion y elimina el decimal, si el resultado es 28.9, quedaria 28 
 )
