/* funcion: transicion */
def transicion(colorActual: String, colorACambiar: String) = { // declare la funcion y como parametros las variables strings
  val colores = List(colorActual, colorACambiar)
  colorActual match { // funciona como un cond, por ahora es simple luego agrego las intermitentes
    case "rojo"     => (colorACambiar == "verde") // caso1
    case "verde"    => (colorACambiar == "amarillo") // caso2, no creo que este termina o bien todavia
    case "amarillo" => (colorACambiar == "rojo") // caso3
  }
  println(colores) // imprimiria en pantalla la lista
}
