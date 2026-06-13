import scala.compiletime.ops.string //para poder utilizar las variabales string

/* funcion: transicion */
def transicion(colorActual: string.type, colorACambiar: string.type) = { // declare la funcion, como parametro las variables string
  colorActual match { // funciona como un cond
    case "rojo" => "verde"
  }
}
