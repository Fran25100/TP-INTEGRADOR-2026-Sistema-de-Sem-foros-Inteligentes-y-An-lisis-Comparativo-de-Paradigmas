
*** - LOAD: A file with name $file does not exist
Break 1 [3]>;==========================================================================================================
#| 
Funcion: Transicion
NATURALEZA: Pura
ESTRATEGIA: Simple (utiliza COND)
IMPACTO: No destructiva
|#
;=============================================================================================================================
(defun transicion (color-actual cambiar-a)
			(cond ((or (and (equal color-actual 'en-verde) (equal cambiar-a 'amarillo)) (and (equal color-actual 'en-rojo) (equal cambiar-a 'amarillo))) (list color-actual "cambiar-a-amarillo"))
			((and (equal color-actual 'en-amarillo) (equal cambiar-a 'rojo)) (list color-actual "cambair-a-rojo")) 
			((and (equal color-actual 'en-rojo) (equal cambiar-a 'verde)) (list color-actual "cambiar-a-verde"))
			(t (list color-actual 'accion-por-defecto))
)
)

TRANSICION
Break 1 [3]> (transicion 'en-verde 'amarillo)

(EN-VERDE "cambiar-a-amarillo")
Break 1 [3]> (transicion 'en-rojo 'amarillo)

(EN-ROJO "cambiar-a-amarillo")
Break 1 [3]> (transicion 'en-amarillo 'amarillo)

(EN-AMARILLO ACCION-POR-DEFECTO)

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
(print "actualizacion")

"actualizacion" 
"actualizacion"
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
Break 3 [5]> #|(defun timer (timeUnix)
				(cond ((and (>= (mod unix 216) 0) (<= (mod unix 216) 89)) "rojo")
				((and (>= (mod unix 216) 96) (<= (mod unix 216) 216)) "verde")
)
)|#
(defun timer (timeUnix)
				(cond ((and (>= (mod timeUnix 216) 0) (<= (mod timeUnix 216) 89)) "rojo")
				((and (>= (mod timeUnix 216) 90) (<= (mod timeUnix 216) 95)) "amarillo")
				(t "verde")
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
( defun duracion-ciclo ()
    (+ 120 90 6)
)

;requerimiento 4.b
;; ========================================================
;; FUNCIÓN: recomendacion-ciclo
;; NATURALEZA: Pura
;; ESTRATEGIA: Simple (condicional IF)
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
;; NATURALEZA: Pura (dependiendo del tiempo que recibe, retorna un resultado)
;; ESTRATEGIA: es una funcion que retorna un resultado
;; IMPACTO: No destructiva
;; ========================================================

(defun ciclos-por-tiempo(minutos)
    (print "La cantidad de ciclos es de:")
    (print(truncate(/ (* minutos 60) 216 )))  ;truncate toma el resultado de una operacion y elimina el decimal, si el resultado es 28.9, quedaria 28 
 )

;requisito 6, avisen si hace falta cambios o no
(defun distribuciontemporal(segundos) ;3600 segundos, osea 1 hora
       (print"Los porcentajes son:")
       (print"Porcentaje del rojo")
       (print(/ (* (* 90.0 (/ segundos 216.0)) 100.0) segundos)) ;rojo
       (print "Porcentaje de amarillo")
       (print(/ (* (* 6.0 (/ segundos 216.0)) 100.0) segundos)) ;amarillo
       (print "Porcentaje de verde")
       (print(/ (* (* 120.0 (/ segundos 216.0)) 100.0) segundos)) ;verde
  )
(distribuciontemporal 3600)

