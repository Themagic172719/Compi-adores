# Mini Compilador LL(1)

## Proyecto para la clase de Diseño de Compiladores
### Facultad de Sistemas - Universidad Autónoma de Coahuila

**Desarrollado por:**
- Marcelo Bazaldua Morales
- Luis Eduardo Hernández Sanchez

## Descripción

Este proyecto implementa un mini compilador LL(1) para un lenguaje de programación educativo, diseñado como herramienta de aprendizaje para comprender las etapas fundamentales del proceso de compilación. Desarrollado como parte del curso de Diseño de Compiladores, este proyecto demuestra la aplicación práctica de los conceptos teóricos de construcción de compiladores.

## Características Principales

### Analizador Léxico (Tokenizador)
- Reconocimiento de palabras reservadas (`module`, `if`, `while`, etc.)
- Procesamiento de identificadores, literales numéricos y cadenas
- Identificación de operadores (aritméticos, lógicos, relacionales)
- Manejo de comentarios en formato `//texto//` y `//...`
- Detección de errores léxicos con información precisa de línea y columna

### Analizador Sintáctico (Parser Recursive-Descent)
- Implementación completa de la gramática LL(1) del lenguaje
- Generación de un árbol de derivación detallado y navegable
- Detección de errores sintácticos con sugerencias de tokens esperados
- Soporte para estructuras de control, definición de tipos y funciones

### Interfaz Gráfica Moderna (PyQt6)
- Editor de código con resaltado de sintaxis avanzado
- Visualización estructurada de tokens generados
- Representación gráfica del árbol de derivación
- Mensajes de error detallados con información de contexto
- Funcionalidades para abrir, editar y guardar archivos de código fuente

## Requisitos del Sistema

- Python 3.6 o superior
- PyQt6 (`pip install PyQt6`)

## Ejecución del Proyecto

El proyecto ofrece dos interfaces:

### Versión con PyQt6 (Recomendada)
```bash
python compiler_gui_pyqt.py
```

### Versión Original con Tkinter
```bash
python compiler_gui.py
```

## Estructura del Lenguaje Implementado

El mini-compilador implementa un lenguaje con la siguiente estructura:

### Declaración de Módulos e Importaciones
```
module miModulo;
import otroModulo;
import math as m;
```

### Definición de Tipos y Estructuras
```
type Entero = int;
struct Persona {
    id: int.
    nombre: string.
    edad: int
};
```

### Variables y Constantes
```
const PI : int = 3;
let contador : int = 0;
```

### Funciones con Parámetros y Retorno
```
fn suma(a:int.b:int) -> int {
    return a + b;
}
```

### Estructuras de Control
```
if (x > 5 && y < 10) {
    // código
} else {
    // código alternativo
}

while (contador < 5) {
    contador = contador + 1;
}
```

## Gramática LL(1) Implementada

El lenguaje implementa una gramática LL(1) completa que incluye:
- Declaraciones de módulo e importación
- Definición de tipos y estructuras
- Declaración de variables y constantes
- Definición de funciones con parámetros y tipos de retorno
- Expresiones con precedencia y asociatividad correcta
- Estructuras de control (if-else, while)

La gramática completa se encuentra documentada en el notebook `compiler_EjemploOctubre.ipynb`.

## Recursos y Documentación

- `compiler_gui_pyqt.py` - Aplicación principal con GUI en PyQt6
- `compiler_gui.py` - Versión original con GUI en Tkinter
- `compiler_EjemploOctubre.ipynb` - Notebook con documentación detallada
- `test/` - Directorio con ejemplos de código fuente

## Trabajo Futuro

- Implementación del análisis semántico
- Generación de código intermedio
- Optimización de código
- Soporte para más tipos de datos y estructuras

## Licencia

Este proyecto es de uso educativo, desarrollado como parte del curso de Diseño de Compiladores de la Facultad de Sistemas UAdeC.

