#Mini Compilador LL(1)
Proyecto de compiladores
Entrega: Mini entorno de compilación para un lenguaje didáctico LL(1)

Este proyecto implementa un mini compilador LL(1) que incluye:

Analizador léxico (tokenizador):

Reconoce palabras reservadas, identificadores, literales (NUM, STRING), operadores y símbolos.

Ignora comentarios de la forma //texto// y también //... en línea.

Valida errores léxicos indicando línea y columna.

Analizador sintáctico (parser recursive-descent):

Implementa la gramática didáctica LL(1) dada.

Genera el árbol de derivación del código fuente.

Detecta errores sintácticos e indica el token y tokens esperados.

Interfaz gráfica (GUI con Tkinter):

Permite escribir, abrir, editar y guardar código fuente.

Muestra tabla de tokens en forma de tabla.

Muestra errores léxicos y sintácticos en pestañas separadas.

Muestra el árbol de derivación en texto indentado.

Mensajes emergentes para indicar éxito o errores.
