//decidi sacar la libreria string porque se puede declarar String sin necesidad de la libreria
/* funcion: transicion */
def transicion(colorActual: String) = { // declare la funcio y como parametro la variable string
  var colorACambiar =
  colorActual match { // funciona como un cond
    case "rojo" => "verde" 
    case "verde" => "amarillo"
    case "amarillo" => "rojo"
  }
}
