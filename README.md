# PicUtils 0.1

Librería en Lazarus, con utilidades para la programación de microcontroladores PIC.

## Descripción

Unidad con utilidades para la programación de microcontroladores PIC de rango medio con instrucciones de 14 bits. Incluye a la mayoría de la serie PIC16FXXXX.

Se define un objeto que representa a un PIC de esta serie, que está dimensionado para poder representar al dispositivo más complejo.

El objetivo de esta unidad es poder servir como base para la implementación de ensambladores, compiladores o hasta simuladores.

## Funcionalidades

* Modela la arquitectura de hardware de un PIC.
* Permite codificar y decodificar las instrucciones del microcontrolador.
* Incluye rutinas para el reconocimiento de las instrucciones en ensamblador del PIC.
* Permite generar el archivo de salida HEX, para la grabación del PIC.

## Notas

Actualmente solo se incluyen las rutinas para el manejo de microcontroladores PIC de rango medio, con instrucciones de 14 bits.

El proyecto está aún en una versión básica y no ha sido probado extensivamente.
