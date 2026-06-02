( defun transicion (color-actual cambiar-a)
    (cond
      ((and (eq color-actual "rojo" ) (eq cambiar-a "verde")) (list color-actual "cambiar-a-verde"));nuestra primera condicion, comparamos variables(color-actual y cambiar-a)
                                                                                                    ;y si cumple creamos una lista con el color actual y el color a cambiar
      ((and (eq color-actual "verde" ) (eq cambiar-a "amarillo")) (list color-actual "cambiar-a-amarillo"))
      ((and (eq color-actual "amarillo" ) (eq cambiar-a "rojo")) (list color-actual "cambiar-a-rojo"))
      (t (list color-actual "color-por-defecto"))
    )
)
