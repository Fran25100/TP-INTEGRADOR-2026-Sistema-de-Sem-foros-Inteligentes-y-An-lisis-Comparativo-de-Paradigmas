#|-------------------------------------------------------------------------------------------------------------------
Funcion: Transicion
NATURALEZA: Pura
ESTRATEGIA: estructura condicional (implementada con COND)
IMPACTO: No destructiva
-------------------------------------------------------------------------------------------------------------------|#
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

(transicion 'en-verde 'CAMBIAR-A-VERDE-INTERMITENTE)
(EN-VERDE "CAMBIAR-A-VERDE-INTERMITENTE")

(transicion 'en-rojo 'CAMBIAR-A-ROJO-INTERMITENTE)
(EN-ROJO "CAMBIAR-A-ROJO-INTERMITENTE")

(transicion 'en-amarillo 'CAMBIAR-A-AMARILLO-INTERMITENTE)
(EN-AMARILLO "CAMBIAR-A-AMARILLO-INTERMITENTE")

(transicion 'en-verde-intermitente 'CAMBIAR-A-ROJO)
(EN-VERDE-INTERMITENTE "CAMBIAR-A-ROJO")

(transicion 'en-rojo-intermitente 'CAMBIAR-A-VERDE)
(EN-ROJO-INTERMITENTE "CAMBIAR-A-VERDE")

(transicion 'en-amarillo-intermitente 'CAMBIAR-A-ROJO)
(EN-AMARILLO-INTERMITENTE "CAMBIAR-A-ROJO")

(transicion 'EN-AMARILLO 'ACCION-POR-DEFECTO)
(EN-AMARILLO ACCION-POR-DEFECTO)

#|-------------------------------------------------------------------------------------------------------------------
Funcion: timer
Naturaleza: Pura
Estrategia: Simple (implementada con LET, MOD y COND)
Impacto En Memoria: No Destructiva, no realiza cambios
-------------------------------------------------------------------------------------------------------------------|#

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

(timer 0);EN-ROJO

(timer 40);EN-ROJO

(timer 89);EN-ROJO

(timer 90);EN-ROJO-INTERMITENTE

(timer 91);EN-ROJO-INTERMITENTE

(timer 92);EN-ROJO-INTERMITENTE

(timer 93);EN-VERDE

(timer 185);EN-VERDE

(timer 212);EN-VERDE

(timer 213);EN-VERDE-INTERMITENTE

(timer 214);EN-VERDE-INTERMITENTE

(timer 215);EN-VERDE-INTERMITENTE

(timer 216);EN-AMARILLO

(timer 220);EN-AMARILLO

(timer 221);EN-AMARILLO

(timer 222);EN-AMARILLO-INTERMITENTE

(timer 223);EN-AMARILLO-INTERMITENTE

(timer 224);EN-AMARILLO-INTERMITENTE

;EN OTRO CICLO
(timer 225);EN-ROJO

(timer 316);EN-ROJO INTERMITENTE

(timer 370);EN-VERDE

(timer 440);EN-VERDE-INTERMITENTE

(timer 446);EN-AMARILLO

(timer 449);EN-AMARILLO-INTERMITENTE

;CASO ALEATORIO
(timer 1234);EN-VERDE

#|-------------------------------------------------------------------------------------------------------------------
Funcion: LogginLights
Naturaleza: Impura (por FORMAT T y GET-UNIVERSAL-TIME, escribe en pantalla segun los datos de entrada)
Estrategia: Simple (implementada con FORMAT y GET-UNIVERSAL-TIME)
Impacto En Memoria: No Destructiva, no realiza cambios
-------------------------------------------------------------------------------------------------------------------|#
;correcion: se automatizo el tiempo para que sea calculado directamente dentro de esta funcion, en vez de un tiempo que puede estar
;desactualizado, restamos al tiempo el cual esta dado desde 1970 unos 70 años para que se sicronicen correctamente. 

(defun LogginLights (color-actual cambio-color)
					(format t "Tiempo ~D: la luz ah cambiado de color ~S a ~S" (- (get-universal-time) 2208988800) color-actual cambio-color)
)

(logginLights 'en-rojo 'en-rojo-intermitente)
;Tiempo 1781220382: la luz ah cambiado de color EN-ROJO a EN-ROJO-INTERMITENTE

(logginLights 'en-rojo-intermitente 'en-verde)
;Tiempo 1781220197: la luz ah cambiado de color EN-ROJO-INTERMITENTE a EN-VERDE

(logginLights 'en-verde 'en-verde-intermitente)
;Tiempo 1781220439: la luz ah cambiado de color EN-VERDE a EN-VERDE-INTERMITENTE

(logginLights 'en-verde-intermitente 'en-amarillo)
;Tiempo 1781220476: la luz ah cambiado de color EN-VERDE-INTERMITENTE a EN-AMARILLO

(logginLights 'en-amarillo 'en-amarillo-intermitente)
;Tiempo 1781220500: la luz ah cambiado de color EN-AMARILLO a EN-AMARILLO-INTERMITENTE

(logginLights 'en-amarillo-intermitente 'en-rojo)
;Tiempo 1781220532: la luz ah cambiado de color EN-AMARILLO-INTERMITENTE a EN-ROJO

#|-------------------------------------------------------------------------------------------------------------------
FUNCIÓN: duracion-ciclo
NATURALEZA: Pura
ESTRATEGIA: Simple (no recursiva, no predicado, no utiliza funciones de orden superior)
IMPACTO: No destructiva
-------------------------------------------------------------------------------------------------------------------|#
; requerimiento 4.a 

(defun duracion-ciclo (rojo verde amarillo intermitente )
    (+ rojo verde amarillo (* intermitente 3))
)

;ciclo pedido (con iteracion 2 extension 1)
(duracion-ciclo 90 120 6 3)
;225

;otro ciclo
(duracion-ciclo 40 60 5 2)
;111

#|-------------------------------------------------------------------------------------------------------------------
FUNCIÓN: recomendacion-ciclo
NATURALEZA: Pura
ESTRATEGIA: alternativa doble (condicional IF)
IMPACTO: No destructiva
-------------------------------------------------------------------------------------------------------------------|#
; requerimiento 4.b

(defun recomendacion-ciclo (duracion)
    (if (and (>= duracion 35) (<= duracion 150)); preferi no ser tan especifico y cambiarlo a un if
		"ciclo optimo"
		"ciclo no optimo"
    )
)

(recomendacion-ciclo (duracion-ciclo 90 120 6 3))
;"ciclo no optimo"

(recomendacion-ciclo (duracion ciclo 40 60 6 3))
;"ciclo optimo"

(recomendacion-ciclo (duracion-ciclo 7 12 5 3))
;"ciclo no potimo"

#|-------------------------------------------------------------------------------------------------------------------
FUNCIÓN: ciclos-por-tiempo
NATURALEZA: Impura (escribe en pantalla un resultado dependiendo de los minutos de entrada)
ESTRATEGIA: estructura secuencial (no presenta recursion en su implementacion)
IMPACTO: No destructiva
-------------------------------------------------------------------------------------------------------------------#|
(defun ciclos-por-tiempo(minutos)
    (print "La cantidad de ciclos es de:")
    (print (truncate (/ (* minutos 60) 225)));truncate toma el resultado de una operacion y elimina el decimal, si el resultado es 28.9, quedaria 28
)

;caso pedido
(ciclos-por-tiempo 15)
;4

;otro caso
(ciclos-por-timepo 70)
;18

#|-------------------------------------------------------------------------------------------------------------------   
FUNCION AUXILIAR: CalcularRestoIni
NATURALEZA: pura (dependiendo del resto que recibe, retorna un resultado)
ESTRATEGIA: alternativa Multiple (cond)
IMPACTO: No destrutiva
-------------------------------------------------------------------------------------------------------------------|#

(defun calcularRestoIni (restoIni)
	(cond 
        ((<= 0 restoIni 89) (list (- 89 restoIni) 3 120 3 6 3)) ;(- 90 restoIni) --> indica lo consumido por rojo
        ((<= 90 restoIni 92) (list 0 (- 92 restoIni) 120 3 6 3)) ;(- 92 restoIni) --> indica lo consumido por el rojo-intermitente
		((<= 93 restoIni 212) (list 0 0 (- 212 restoIni) 3 6 3)) ;(- 212 restoIni) --> indica lo consumido por verde
        ((<= 213 restoIni 215) (list 0 0 0 (- 215 restoIni) 6 3)) ;(- 215 restoIni) --> indica lo consumido por verde-intermitente
		((<= 216 restoIni 221) (list 0 0 0 0 (- 221 restoIni) 3)) ;(- 221 restoIni) --> indica lo consumido por amarillo
        ((<= 222 restoIni 224) (list 0 0 0 0 0 (- 224 restoIni))) ;(-224 restoIni) --> indica lo consumido por amarillo-intermitente
    )
)

#|-------------------------------------------------------------------------------------------------------------------
FUNCION AUXILIAR: CalcularRestoFin
NATURALEZA: pura (dependiendo del resto que recibe, retorna un resultado)
ESTRATEGIA: alternativa Multiple (cond)
IMPACTO: No destrutiva
-------------------------------------------------------------------------------------------------------------------|#

(defun calcularRestoFin (restoFin)
	(cond  
        ((<= 0 restoFin 89) (list (- 89 restoFin) 3 120 3 6 3)) ;(- 90 restoFin) --> indica lo consumido por rojo
        ((<= 90 restoFin 92) (list 0 (- 92 restoFin) 120 3 6 3)) ;(- 92 restoFin) --> indica lo consumido por el rojo-intermitente
		((<= 93 restoFin 212) (list 0 0 (- 212 restoFin) 3 6 3)) ;(- 212 restoFin) --> indica lo consumido por verde
        ((<= 213 restoFin 215) (list 0 0 0 (- 215 restoFin) 6 3)) ;(- 215 restoFin) --> indica lo consumido por verde-intermitente
		((<= 216 restoFin 221) (list 0 0 0 0 (- 221 restoFin) 3)) ;(- 221 restoFin) --> indica lo consumido por amarillo
        ((<= 222 restoFin 224) (list 0 0 0 0 0 (- 224 restoFin))) ;(- 224 restoFin) --> indica lo consumido por amarillo-intermitente
    )
)

#|-------------------------------------------------------------------------------------------------------------------
FUNCION AUXILIAR: calcularPorcentajes
NATURALEZA: Pura (devuelve una lista con los porcentajes de cada estado del semáforo)
ESTRATEGIA: Secuencial (implementamos mediante operaciones aritméticas y la construcción de listas)
IMPACTO: No destructiva
-------------------------------------------------------------------------------------------------------------------|#

(defun calcularPorcentajes (ListaIni ListaFin)
	(list 
        (float (/ (* (+ 1440 (car ListaFin) (car ListaIni)) 100) 3600)) ;rojo
	    (float (/ (* (+ 48 (cadr ListaFin) (cadr ListaIni)) 100) 3600)) ;rojo-intermitente
	    (float (/ (* (+ 1920 (caddr ListaFin) (caddr ListaIni)) 100)3600)) ;verde
        (float (/ (* (+ 48 (cadddr ListaFin) (cadddr ListaIni)) 100) 3600)) ;verde-intermitente
        (float (/ (* (+ 96 (cadddr (cdr ListaFin)) (cadddr (cdr ListaIni))) 100)3600)) ;amarillo
        (float (/ (* (+ 48 (car (last ListaFin)) (car (last ListaIni))) 100) 3600)) ;amarillo-intermitente
    )
)


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
(defun distribucionTemp (unix)
	(if (zerop (mod unix 225)) "| 40,0% rojo| 1,3% rojo-intermitente | 53,3% verde| 1,3% verde-intermitente| 2,6% amarillo| 1,3% amarillo intermitente|"
	    (format t "~%| ~A% rojo| ~A% rojo-intermitente| ~A% verde | ~A% verde-intermitente| ~A% amarillo| ~A% amarillo-intermitente|"
	    (car (calcularPorcentajes (calcularRestoIni (mod unix 225)) (calcularRestoFin (mod (- 3600 (- 225 (mod unix 225))) 225))))
		(cadr (calcularPorcentajes (calcularRestoIni (mod unix 225)) (calcularRestoFin (mod (- 3600 (- 225 (mod unix 225))) 225))))
	    (caddr (calcularPorcentajes (calcularRestoIni (mod unix 225)) (calcularRestoFin (mod (- 3600 (- 225 (mod unix 225))) 225))))
        (cadddr (calcularPorcentajes (calcularRestoIni (mod unix 225)) (calcularRestoFin (mod (- 3600 (- 225 (mod unix 225))) 225))))
        (cadddr (cdr (calcularPorcentajes (calcularRestoIni (mod unix 225)) (calcularRestoFin (mod (- 3600 (- 225 (mod unix 225))) 225)))))
        (car (last (calcularPorcentajes (calcularRestoIni (mod unix 225)) (calcularRestoFin (mod (- 3600 (- 225 (mod unix 225))) 225))))))
    )					
)



