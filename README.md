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

## Modo de trabajo

Se puede trabajar de diversas formas con la libreria, dependiendo de la función que se desee implementar. Pero en general, casi siempre se reequerirá primero crear un objeto de la clase TPIC16:

```
uses  ... , pic16utils;

    pic: TPIC16;

	pic := TPIC16.Create;
	
	//hacer algo
	
	pic.Destroy;

```

La mayoría de las funcionalidades de la librería se acceden mediante métodos del objeto TPIC16. Así si por ejemplo se desea limpiar la memoria flash, se debe llamar al método:

```
	pic.ClearMemFlash;
```

## Generación del archivo *.hex

Para obtener el archivo de salida, primero se deben introducir las instrucciones en la memoria flash del objeto TPIC16.

Una forma es usar los métodos codAsm(), que trabaja introduciendo las instrucciones de forma secuencial:

```
  pic.codAsm(CLRWDT);
  pic.codAsm(MOVLW,$01);
  pic.codAsm(MOVWF,$21,toF);  //el parámetro "toF" es irrelevante
```

Luego se debe llamar al método: GenHex(): 

```
  pic.GenHex('salida.hex');
```

## Notas

Actualmente solo se incluyen las rutinas para el manejo de microcontroladores PIC de rango medio, con instrucciones de 14 bits.

