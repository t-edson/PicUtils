{PIC16 16Utils 0.2
Cambios
=======
* Se corrige un error en la codificación y decodificación de las instrucciones BSF, BTFSC
Y BTFSS
* Se corrige un error de decodificación de algunas instrucciones.
* Se crea el tipo TPIC16FlashCell, para representar a las celdas de memoria Flash, y
se elimina TPIC16FlashBool, pues ya no es necesario. Se adapta el código a la nueva.
* Se agrega a TPIC16FlashCell, un campo para incuir comentarios en el código.
estructura.
* Se crea el tipo TPIC16RamCell, para representar a las celdas de memoria RAM.
* Se crea el método TPIC16.putCommAsm((), para permitir agregar un comentario al
código de la memoria flash.
* Se modifica TPIC16.ShowCode(), para que muestre los coemnatrios al volcar el código.
* Se agrega el campo "frequen", para poder disponer de la frecuencia de reloj.
* Se corrige HaveConsecGPR().
* Se agrega el método FindOpcode().
* Se cambia los nombres de los métodos CleanMemRAM y CleanMemFlash;

 Descripción
 ===========
 Unidad con utilidades para la programación de microcontroladores PIC de rango
 medio con instrucciones de 14 bits. Incluye a la mayoría de la serie
 PIC16FXXXX.
 Se define un objeto que representa a un PIC de esta serie, que está dimensionado
 para poder representar al dispositivo más complejo.
 El objetivo de esta unidad es poder servir como base para la implementación de
 ensambladores, compiladores o hasta simuladores.

                                         Creado por Tito Hinostroza   26/07/2015
}

unit Pic16Utils;
{$mode objfpc}{$H+}
interface
uses
  Classes, SysUtils;
const
  PIC_MAX_RAM = 512;
  PIC_MAX_FLASH = 8192;
  PIC_PAGE_SIZE = 2048;

type  //tipos para instrucciones
  //Instrucciones para la serie 16
  TPIC16Inst = (
    //BYTE-ORIENTED FILE REGISTER OPERATIONS
    ADDWF,
    ANDWF,
    CLRF,
    CLRW,
    COMF ,
    DECF ,
    DECFSZ,
    INCF,
    INCFSZ,
    IORWF,
    MOVF,
    MOVWF,
    NOP,
    RLF,
    RRF,
    SUBWF,
    SWAPF,
    XORWF,
    //BIT-ORIENTED FILE REGISTER OPERATIONS
    BCF,
    BSF,
    BTFSC,
    BTFSS,
    //LITERAL AND CONTROL OPERATIONS
    ADDLW,
    ANDLW,
    CALL,
    CLRWDT,
    GOTO_,
    IORLW,
    MOVLW,
    RETFIE,
    RETLW,
    RETURN,
    SLEEP,
    SUBLW,
    XORLW,
    _Inval
  );
  //Indica el destino de la instrucción
  TPIC16destin = (
    toW = %00000000,    //al acumulador
    toF = %10000000     //a memoria
  );


type //Modelo de la memoria RAM
  TPIC16RamCell = record
    value  : byte;     //value of the memory
    used   : boolean;  //indicate if have been written
    name   : string;   //nombre del registro
  end;
  TPIC16Ram = array[0..PIC_MAX_RAM-1] of TPIC16RamCell;
  ptrPIC16Ram = ^TPIC16Ram;

  {Representa a un banco de memoria del PIC. En un banco las direcciones de memoria
   se mapean siempre desde $00 hasta $7F. No almacenan datos, solo usan referencias.}
  ptrRAMBank = ^TRAMBank;
  { TRAMBank }
  TRAMBank = object
     ram    : ptrPIC16Ram;     //puntero a memoria RAM
     AddrStart: word;          //dirección de inicio en la memoria RAM total
     LastMapped: boolean;      //indica si los últimos bytes están mapeados
     BankMapped: ptrRAMBank;   //banco al que están mapeados los últimos bytes
  private
    function Getmem(i : byte): TPIC16RamCell;
    procedure Setmem(i : byte; AValue: TPIC16RamCell);
  public
    procedure Init(AddrStart0: word; BankMapped0: ptrRAMBank; ram0:ptrPIC16Ram);  //inicia objeto
    property mem[i : byte] : TPIC16RamCell read Getmem write Setmem;
    //funciones para administración de la memoria
    function HaveConsecGPR(const i, n: byte): boolean; //Indica si hay "n" bytes libres
    procedure UseConsecGPR(const i, n: byte);  //Ocupa "n" bytes en la posición "i"
    function GetMemRAM(const size: integer; var addr: word): boolean;  //obtiene una dirección libre
    function TotalGPR: byte; //total de bytes que contiene para el usuario
    function UsedGPR: byte;  //total de bytes usados por el usuario
  end;

type  //Modelos de la memoria Flash
  TPIC16FlashCell = record
    value  : word;     //value of the memory
    used   : boolean;  //indicate if have been written
    comment: string;   //comment to code
    {tener cuidado con el tamaño de este registro, pues se va a multiplicar por 8192}
  end;
  TPIC16Flash = array[0..PIC_MAX_FLASH-1] of TPIC16FlashCell;
  ptrPIC16Flash = ^TPIC16Flash;

  {Representa a una página de memoria del PIC. En una página las direcciones de memoria
   se mapean siempre desde $000 hasta $800. No almacenan datos, solo usan referencias.}
  ptrFlashPage = ^TFlashPage;
  { TFlashPage }
  TFlashPage = object
  private
    flash    : ptrPIC16Flash;  //puntero a memoria Flash
    AddrStart: word;           //dirección de inicio en la memoria flash total
  private
    iHex : word;  //índice para exploración de memoria
    nUsed: word;  //número de celdas usdas
    function Getmem(i : word): TPIC16FlashCell;
    procedure Setmem(i : word; AValue: TPIC16FlashCell);
  public
    minUsed, maxUsed: word;  //información útil, por eso se publica
    procedure Init(AddrStart0: word; flash0: ptrPIC16Flash);  //inicia objeto
    property mem[i : word] : TPIC16FlashCell read Getmem write Setmem;
    //funciones para administración de la memoria
    function Total: word; //total de bytes que contiene
    function Used: word;  //total de bytes usados por el usuario
    //funciones para generación de archivo hex
    procedure StartHex;  //inicia la extracción de líneas
    function ExtractHex(var Addr: word): string;  //devuelve una línea de texto del código en hexadecimal
  end;

type
  {Objeto que representa al hardware de un PIC de la serie 16}
  { TPIC16 }
  TPIC16 = class
  private
    hexLines : TStringList; //usado para crear archivo *.hex
    //memorias
    flash    : TPIC16Flash;     //memoria Flash
    ram      : TPIC16Ram;      //memoria RAM
    bank0, bank1, bank2, bank3: TRAMBank;  //bancos de memoria RAM
    page0, page1, page2, page3: TFlashPage;  //páginas de memoria Flash
    procedure GenHexComm(comment: string);
    procedure GenHexData(Address: word; Data: string);
    procedure GenHexData(var pg: TFlashPage);
    procedure GenHexEOF;
    procedure GenHexExAdd(Data: word);
    function HexChecksum(const lin: string): string;
    procedure ShowCode(lOut: TStrings; pag: TFlashPage);
  private
    FCommonRAM: boolean;
    procedure SetCommonRAM(AValue: boolean);
    function StrHexFlash(i1, i2: integer): string;
  private //campos para procesar instrucciones
    idIns: TPIC16Inst;    //ID de Instrucción.
    d_   : TPIC16destin;  //Destino de operación. Válido solo en algunas instrucciones.
    f_   : byte;          //Registro destino. Válido solo en algunas instrucciones.
    b_   : byte;          //Bit destino. Válido solo en algunas instrucciones.
    k_   : word;          //Parámetro Literal. Válido solo en algunas instrucciones.
    procedure Decode(const opCode: word);  //decodifica instrucción
    function Disassembler: string;      //Desensambla la instrucción actual
  public
    iFlash   : integer;   //puntero a la memoria Flash, para escribir
    frequen  : integer;   //frecuencia del reloj
    //Propiedades que definen la arquitectura del PIC destino.
    NumBanks: byte;      //Número de bancos de RAM.
    NumPages: byte;      //Número de páginas de memoria Flash.
    GPRStart: integer;   //dirección de inicio de los registros de usuario
    property CommonRAM: boolean read FCommonRAM write SetCommonRAM;  //indica si tiene mapeada la RAM de otros bancos en el banco 0
    //funciones para la memoria RAM
    function GetMemRAM(const size: integer; var addr: word; var bnk: byte): boolean;  //obtiene una dirección libre
    function FreeMemRAM(const size: integer; var addr: word): boolean;  //libera una dirección usada
    function TotalMemRAM: word;  //devuelve el total de memoria RAM
    function UsedMemRAM: word;  //devuelve el total de memoria RAM usada
    procedure ClearMemRAM;
    //funciones para la memoria Flash
    function TotalMemFlash: word;  //devuelve el total de memoria Flash
    function UsedMemFlash: word;  //devuelve el total de memoria Flash usada
    procedure ClearMemFlash;
    //métodos para codificar programas
    procedure codAsm(const inst: TPIC16Inst; const f: byte; d: TPIC16destin);  //codifica una instrucción ASM simplificada
    procedure codAsm(const inst: TPIC16Inst; const f: byte; b: byte);
    procedure codAsm(const inst: TPIC16Inst; const k: word);
    procedure codAsm(const inst: TPIC16Inst);
    //métodos adicionales
    function FindOpcode(Op: string; var syntax: string): TPIC16Inst;  //busca Opcode
    procedure addCommAsm(comm: string);  //Add a comment to the ASM code
    procedure addCommAsm1(comm: string); //Add lateral comment to the ASM code
    procedure GenHex(hexFile: string);  //genera un archivo hex
    procedure DumpCode(l: TStrings);  //vuelva en código que contiene
  public
     constructor Create;
     destructor Destroy; override;
  end;

var  //variables globales
  //mnemónico de las instrucciones
  PIC16InstName: array[low(TPIC16Inst)..high(TPIC16Inst)] of string[7];
  //sintaxis en ensamblador de las instrucciones
  PIC16InstSyntax: array[low(TPIC16Inst)..high(TPIC16Inst)] of string[5];

implementation
{ TRAMBank }
function TRAMBank.Getmem(i: byte): TPIC16RamCell;
begin
  //Se asume que i debe ser menor que $7F
  if (i>=$70) and LastMapped then begin
    //estas direcciones están mapeadas en otro banco
    Result := BankMapped^.mem[i];
  end else begin
    Result := ram^[i+AddrStart];
  end;
end;
procedure TRAMBank.Setmem(i: byte; AValue: TPIC16RamCell);
begin
  if (i>=$70) and LastMapped then begin
    //estas direcciones están mapeadas en otro banco
    BankMapped^.mem[i] := AValue;
  end else begin
    ram^[i+AddrStart] := AValue;
  end;
end;
procedure TRAMBank.Init(AddrStart0: word; BankMapped0: ptrRAMBank;
  ram0: ptrPIC16Ram);
begin
  AddrStart :=AddrStart0;
  BankMapped:=BankMapped0;
  ram       :=ram0;
end;
function TRAMBank.HaveConsecGPR(const i, n: byte): boolean;
{Indica si hay "n" bytes consecutivos libres en la posicióm "i", en este banco de la RAM}
var
  c: Integer;
  j: Byte;
begin
  Result := false;
  c := 0;
  j := i;
  while (j<=$7F) and (c<n) do begin
    if mem[j].used then exit;  //ya está ocupado
    inc(c);      //verifica siguiente
    inc(j);
  end;
  if j>$7F then exit;  //no hay más espacio
  //si llega aquí es porque estaban libres los bloques
  Result := true;
end;
procedure TRAMBank.UseConsecGPR(const i, n: byte);
{Marca "n" bytes como usados en la posición de memori "i", en este banco.
 Debe haberse verifiacdo previamente que los parámetros son válidos, porque asuí no
 se hará ninguan verificación.}
var j: byte;
begin
  for j:=i to i+n-1 do begin
    ram^[i+AddrStart].used:=true;
    //    mem[j].used := true;   //no se puede
  end;
end;
function TRAMBank.GetMemRAM(const size: integer; var addr: word): boolean;
{Busca un bloque de bytes consecutivs de memoria RAM en este banco.}
var
  i: byte;
begin
  Result := false;  //valor por defecto
  if size=0 then exit;
  for i:=$20 to $7F do begin  //verifica 1 a 1, pro seguridad
    if HaveConsecGPR(i, size) then begin
      //encontró del tamaño buscado
      UseConsecGPR(i, size);  //marca como usado
      addr := i;  //devuelve dirfección
      Result := true;  //indica que encontró espacio
      exit;
    end;
  end;
end;
function TRAMBank.TotalGPR: byte;
{Total de memoria disponible para el usuario}
begin
   if LastMapped then  //últimos bytes maperados
     Result:=96-16  //asume que no son de este banco
   else
     Result:=96;
end;
function TRAMBank.UsedGPR: byte;
var
  i: Integer;
begin
  Result := 0;
  if LastMapped then begin //últimos bytes maperados
    for i:=$20 to $6F do begin
      if mem[i].used then inc(Result);
    end;
  end else begin //bancos independientes
    for i:=$20 to $7F do begin
      if mem[i].used then inc(Result);
    end;
  end;
end;
{ TFlashPage }
function TFlashPage.Getmem(i: word): TPIC16FlashCell;
begin
  //Se asume que i debe ser menor que $800
  Result := flash^[i+AddrStart];
end;
procedure TFlashPage.Setmem(i: word; AValue: TPIC16FlashCell);
begin
  flash^[i+AddrStart] := AValue;
end;
procedure TFlashPage.Init(AddrStart0: word; flash0: ptrPIC16Flash);
begin
  AddrStart :=AddrStart0;
  flash     :=flash0;
end;
function TFlashPage.Total: word;
begin
  Result := PIC_PAGE_SIZE;  //tamaño fijo
end;
function TFlashPage.Used: word;
var
  i: Integer;
begin
  Result := 0;
  for i:=$0000 to PIC_PAGE_SIZE-1 do begin
    if mem[i].used then inc(Result);
  end;
end;
procedure TFlashPage.StartHex;
{Prepara para una exploración del código con ExtractHex().  Actualiza las
variables: iHex, nUsed, minUsed y maxUsed.
Notar que la extracción de instrucciones, se hace de forma dencilla usando un
solo bloque por página. Una extracción de código más precisa podría manejar
diversos blqoues de código en una página.
 }
var
  i: Integer;
begin
  iHex := 0;  //inicia índice
  //Busca la mínima y máxima posición de memoria usada
  minUsed := PIC_PAGE_SIZE;  //valor máximo
  maxUsed := $0000;  //valor máximo
  nUsed := 0;  //aprovecha para calcular elementos usados
  for i:=$0000 to PIC_PAGE_SIZE-1 do begin
    if mem[i].used then begin
      if i<minUsed then minUsed := i;
      if i>maxUsed then maxUsed := i;
      inc(nUsed);
    end;
  end;
  iHex := minUsed;   //inicia índice
end;
function TFlashPage.ExtractHex(var Addr: word): string;
{Devuelve una cadena (de longitud variable) con la lista del código binario que contiene,
en forma de caracteres en hexadecimal, de la misma forma a como se usa en un archivo
*.hex. En "Addr" devuelve la dirección absoluta de inicio desde donde lee.
Debe llamarse, después de llamar a StartHex(). Con cada llamada, devuelve los bloques
consecutivos de datos. Si no hay más datos devuelve cadena vacía.}
const MAX_INS_HEX = 8;  //Número máximo de instrucciones que devuelve por pasada
var
  tmp: String;
  nInst: Integer;
begin
  if nUsed = 0 then begin  //no hay datos
    Result := '';
    exit;
  end;
  //Hay datos y los límites están en minUsed y maxUsed
  if iHex > maxUsed then begin  //llegó al final
    Result := '';
    exit;
  end;
  //extrae bloques de instrucciones
  Result := '';
  Addr := iHex + AddrStart;
  nInst := 0;
  while (iHex<=maxUsed) and (nInst<MAX_INS_HEX) do begin
    tmp := IntToHex(mem[iHex].value,4);
    Result +=copy(tmp,3,2) + copy(tmp,1,2);  //se graba con los bytes invertidos
    Inc(iHex);  //pasa al siguiente
    Inc(nInst);
  end;
end;

{ TPIC16 }
procedure TPIC16.codAsm(const inst: TPIC16Inst; const f: byte; d: TPIC16destin);
//Codifica las instrucciones orientadas a registro.
begin
  case inst of
  ADDWF : flash[iFlash].value := %00011100000000 + ord(d) + f;
  ANDWF : flash[iFlash].value := %00011100000000 + ord(d) + f;
  CLRF  : flash[iFlash].value := %00000110000000 + f;
  COMF  : flash[iFlash].value := %00100100000000 + ord(d) + f;
  DECF  : flash[iFlash].value := %00001100000000 + ord(d) + f;
  DECFSZ: flash[iFlash].value := %00101100000000 + ord(d) + f;
  INCF  : flash[iFlash].value := %00101000000000 + ord(d) + f;
  INCFSZ: flash[iFlash].value := %00111100000000 + ord(d) + f;
  IORWF : flash[iFlash].value := %00010000000000 + ord(d) + f;
  MOVF  : flash[iFlash].value := %00100000000000 + ord(d) + f;
  MOVWF : flash[iFlash].value := %00000010000000 + f;
  RLF   : flash[iFlash].value := %00110100000000 + ord(d) + f;
  RRF   : flash[iFlash].value := %00110000000000 + ord(d) + f;
  SUBWF : flash[iFlash].value := %00001000000000 + ord(d) + f;
  SWAPF : flash[iFlash].value := %00111000000000 + ord(d) + f;
  XORWF : flash[iFlash].value := %00011000000000 + ord(d) + f;
  else
    raise Exception.Create('Error de implementación.');
  end;
  flash[iFlash].used := true;  //marca como usado
  inc(iFlash);
end;
procedure TPIC16.codAsm(const inst: TPIC16Inst; const f: byte; b: byte);
//Codifica las instrucciones orientadas a bit.
begin
  case inst of
  BCF  : flash[iFlash].value := %01000000000000 + (b<<7) + f;
  BSF  : flash[iFlash].value := %01010000000000 + (b<<7) + f;
  BTFSC: flash[iFlash].value := %01100000000000 + (b<<7) + f;
  BTFSS: flash[iFlash].value := %01110000000000 + (b<<7) + f;
  else
    raise Exception.Create('Error de implementación.');
  end;
  flash[iFlash].used := true;  //marca como usado
  inc(iFlash);
end;
procedure TPIC16.codAsm(const inst: TPIC16Inst; const k: word);
{Codifica las instrucciones con constantes y de control.
 "k" debe ser word, porque en la instrucción GOTO, requiere 11 bits.}
begin
  case inst of
  ADDLW : flash[iFlash].value := %11111000000000 + k;
  ANDLW : flash[iFlash].value := %11100100000000 + k;
  CALL  : flash[iFlash].value := %10000000000000 + k;
  GOTO_ : flash[iFlash].value := %10100000000000 + k;
  IORLW : flash[iFlash].value := %11100000000000 + k;
  MOVLW : flash[iFlash].value := %11000000000000 + k;
  RETLW : flash[iFlash].value := %11010000000000 + k;
  SUBLW : flash[iFlash].value := %11110000000000 + k;
  XORLW : flash[iFlash].value := %11101000000000 + k;
  else
    raise Exception.Create('Error de implementación.');
  end;
  flash[iFlash].used := true;  //marca como usado
  inc(iFlash);
end;
procedure TPIC16.codAsm(const inst: TPIC16Inst);
//Codifica las instrucciones de control.
begin
  case inst of
  CLRW  : flash[iFlash].value := %00000110000000;
  NOP   : flash[iFlash].value := %00000000000000;
  CLRWDT: flash[iFlash].value := %00000001100100;
  RETFIE: flash[iFlash].value := %00000000001001;
  RETURN: flash[iFlash].value := %00000000001000;
  SLEEP : flash[iFlash].value := %00000001100011;
  else
    raise Exception.Create('Error de implementación.');
  end;
  flash[iFlash].used := true;  //marca como usado
  inc(iFlash);
end;

function TPIC16.FindOpcode(Op: string; var syntax: string): TPIC16Inst;
{Busca una cádena que represente a una instrucción (Opcode). Si encuentra devuelve
 el identificador de instrucción y una cadena que representa a la sintaxis en "syntax".
 Si no encuentra devuelve "_Inval". }
var
  idInst: TPIC16Inst;
  tmp: String;
  found: Boolean;
begin
  found := false;
  tmp := UpperCase(Op);
  for idInst := low(TPIC16Inst) to high(TPIC16Inst) do begin
    if PIC16InstName[idInst] = tmp then begin
      found := true;
      break;
    end;
  end;
  if found then begin
    Result := idInst;
    syntax := PIC16InstSyntax[idInst];
  end else  begin
    Result := _Inval;
  end;
end;

procedure TPIC16.addCommAsm(comm: string);
{Agrega un comentario de línea al código en la posición de memoria actual}
begin
  flash[iFlash].comment:=comm;
end;
procedure TPIC16.addCommAsm1(comm: string);
{Agrega un comentario al código en la posición de memoria anterior}
begin
  if iFlash= 0 then exit;
  flash[iFlash-1].comment+=comm;   //se agrega al que pudiera haber
end;

function TPIC16.HexChecksum(const lin:string): string;
//Devuelve los caracteres en hexadecimal del Checksum, para el archivo *.hex
var
  i: Integer;
  chk: Integer;
  part: String;
begin
   i:=1;
   chk := 0;
   while i<length(lin) do begin
     part := copy(lin,i,2);
     chk := chk + StrToInt('$'+part);
     inc(i,2);
   end;
   chk := not chk;  //complemento a 1
   inc(chk);        //complemento a 2
   part := IntToHex(chk,4);  //a hexadecimal
   Result := copy(part, length(part)-1,2);  //recorta
end;
procedure TPIC16.SetCommonRAM(AValue: boolean);
begin
//  if FCommonRAM=AValue then Exit;
  bank0.LastMapped:=false;  //siempre
  if FCommonRAM then begin
    bank1.LastMapped:=true;
    bank2.LastMapped:=true;
    bank3.LastMapped:=true;
  end else begin
    bank1.LastMapped:=false;
    bank2.LastMapped:=false;
    bank3.LastMapped:=false;
  end;
end;
procedure TPIC16.GenHexExAdd(Data: word);
//Agrega una línea de Extended Address al archivo *.hex
const RecordType = '04';
var
  ByteCount: Integer;
  lin: String;
begin
  ByteCount := 2;
  lin:= IntToHex(ByteCount,2) + '0000' + RecordType +  IntToHex(Data,4);
  hexLines.Add(':' + lin + HexChecksum(lin));
end;
procedure TPIC16.GenHexData(Address: word; Data: string);
//Agrega una línea de datos al archivo *.hex
const RecordType = '00';
var
  ByteCount: Integer;
  lin: String;
begin
  ByteCount := length(data) div 2;
  lin:= IntToHex(ByteCount,2) + IntToHex(Address,4) + RecordType +  Data;
  hexLines.Add(':'+lin + HexChecksum(lin));
end;
procedure TPIC16.GenHexData(var pg: TFlashPage);
//Genera líneas de datos en hexLines, usando una página
var
  dat: String;
  addr: word;
begin
  pg.StartHex;  //prepara extracción de datos
  dat := pg.ExtractHex(addr);
  while dat <>'' do begin
     GenHexData(addr, dat);
     dat := pg.ExtractHex(addr);
  end;
end;
procedure TPIC16.GenHexEOF;
//Agrega una línea de Extended Address al archivo *.hex
begin
  hexLines.Add(':00000001FF');
end;
procedure TPIC16.GenHexComm(comment: string);
//Agrega una línea de comentario al archivo *.hex
begin
  hexLines.Add(';'+comment);
end;
function  TPIC16.StrHexFlash(i1, i2: integer): string;
{Devuelve la cadena, de bytes hexadecimales de la memoria Flash, desde la posición
 i1 hasta i2. No se espera usar función porque se puede obteenr esta infromación
 pidiéndosela a los objetos de página}
var
  i: Integer;
  tmp: String;
begin
  Result:='';
  for i:=i1 to i2 do begin
    tmp := IntToHex(flash[i].value,4);
    Result+=copy(tmp,3,2) + copy(tmp,1,2);  //se graba con los bytes invertidos
  end;
end;
procedure TPIC16.Decode(const opCode: word);
{Decodifica la instrucción indicada. Actualiza siempre la variable "idIns", y
dependiendo de la instrucción, puede actualizar: d_, f_, b_ y k_}
var
  codH : byte;  //6 bits altos de la instrucción
  codL : byte;  //byte bajo de la instrucción
begin
  codH := (opCode and $3F00) >> 8;  //se debería optimizar
  codL := opCode and $00FF;
  case codH of
  %000111: begin
    idIns := ADDWF;
    d_ := TPIC16destin(codL and %10000000);
    f_ := codL and %01111111;
  end;
  %000101: begin
    idIns := ANDWF;
    d_ := TPIC16destin(codL and %10000000);
    f_ := codL and %01111111;
  end;
  %000001: begin
    if (codL and %10000000) = %10000000 then begin
      idIns := CLRF;
      f_ := codL and %01111111;
    end else begin
      idIns := CLRW;
    end;
  end;
  %001001: begin
    idIns := COMF;
    d_ := TPIC16destin(codL and %10000000);
    f_ := codL and %01111111;
  end;
  %000011: begin
    idIns := DECF;
    d_ := TPIC16destin(codL and %10000000);
    f_ := codL and %01111111;
  end;
  %001011: begin
    idIns := DECFSZ;
    d_ := TPIC16destin(codL and %10000000);
    f_ := codL and %01111111;
  end;
  %001010: begin
    idIns := INCF;
    d_ := TPIC16destin(codL and %10000000);
    f_ := codL and %01111111;
  end;
  %001111: begin
    idIns := INCFSZ;
    d_ := TPIC16destin(codL and %10000000);
    f_ := codL and %01111111;
  end;
  %000100: begin
    idIns := IORWF;
    d_ := TPIC16destin(codL and %10000000);
    f_ := codL and %01111111;
  end;
  %001000: begin
    idIns := MOVF;
    d_ := TPIC16destin(codL and %10000000);
    f_ := codL and %01111111;
  end;
  %000000: begin
    if (codL and %10000000) = %10000000 then begin
      idIns := MOVWF;
      f_ := codL and %01111111;
    end else begin
      //bit7 a cero, hay varias opciones
      case codL of
      %00000000,
      %00100000,
      %01000000,
      %01100000: begin
        idIns := NOP;
      end;
      %01100100: begin
        idIns := CLRWDT;
      end;
      %00001001: begin
        idIns := RETFIE;
      end;
      %00001000: begin
        idIns := RETURN;
      end;
      %01100011: begin
        idIns := SLEEP;
      end;
      else
        idIns := _Inval;
      end;
    end;
  end;
  %001101: begin
    idIns := RLF;
    d_ := TPIC16destin(codL and %10000000);
    f_ := codL and %01111111;
  end;
  %001100: begin
    idIns := RRF;
    d_ := TPIC16destin(codL and %10000000);
    f_ := codL and %01111111;
  end;
  %000010: begin
    idIns := SUBWF;
    d_ := TPIC16destin(codL and %10000000);
    f_ := codL and %01111111;
  end;
  %001110: begin
    idIns := SWAPF;
    d_ := TPIC16destin(codL and %10000000);
    f_ := codL and %01111111;
  end;
  %000110: begin
    idIns := XORWF;
    d_ := TPIC16destin(codL and %10000000);
    f_ := codL and %01111111;
  end;
  %111110,
  %111111: begin
    idIns := ADDLW;
    k_ := codL;
  end;
  %111001: begin
    idIns := ANDLW;
    k_ := codL;
  end;
  %111000: begin
    idIns := IORLW;
    k_ := codL;
  end;
  %110000,
  %110001,
  %110010,
  %110011: begin
    idIns := MOVLW;
    k_ := codL;
  end;
  %110100,
  %110101,
  %110110,
  %110111: begin
    idIns := RETLW;
    k_ := codL;
  end;
  %111100,
  %111101: begin
    idIns := SUBLW;
    k_ := codL;
  end;
  %111010: begin
    idIns := XORLW;
    k_ := codL;
  end;
  else
    if (codH and %110000) = %010000 then begin
      case codH and %001100 of
      %0000: begin
        idIns := BCF;
        b_ := (opCode and %1110000000) >> 7;
        f_ := codL and %01111111;
      end;
      %0100: begin
        idIns := BSF;
        b_ := (opCode and %1110000000) >> 7;
        f_ := codL and %01111111;
      end;
      %1000: begin
        idIns := BTFSC;
        b_ := (opCode and %1110000000) >> 7;
        f_ := codL and %01111111;
      end;
      %1100: begin
        idIns := BTFSS;
        b_ := (opCode and %1110000000) >> 7;
        f_ := codL and %01111111;
      end;
      else
        idIns := _Inval;
      end;
    end else if (codH and %111000) = %100000 then begin
      idIns := CALL;
      k_ := opCode and %11111111111;
    end else if (codH and %111000) = %101000 then begin
      idIns := GOTO_;
      k_ := opCode and %11111111111;
    end else begin
      idIns := _Inval;
    end;
  end;
end;
function TPIC16.Disassembler: string;
{Desensambla la instrucción, actual. No se reciben parámetros sino que se usan los
campos globales, para mejorar la velocidad. Se debe llamar después de llamar a Decode()
para que se actualicen las variables que usa.}
var
  nemo: String;
begin
  nemo := lowerCase(trim(PIC16InstName[idIns])) + ' ';
  case idIns of
  ADDWF,
  ANDWF,
  COMF ,
  DECF ,
  DECFSZ,
  INCF,
  INCFSZ,
  IORWF,
  MOVF,
  RLF,
  RRF,
  SUBWF,
  SWAPF,
  XORWF: begin
       if d_ = toF then
         Result := nemo + IntToHex(f_,3) + ',f'
       else
         Result := nemo + IntToHex(f_,3) + ',w';
     end;
  CLRF,
  MOVWF: begin
         Result := nemo + IntToHex(f_,3);
     end;
  BCF,
  BSF,
  BTFSC,
  BTFSS: begin
       Result := nemo + IntToHex(f_,3) + ', ' + IntToStr(b_);
     end;
  ADDLW,
  ANDLW,
  CALL,
  GOTO_,
  IORLW,
  MOVLW,
  RETLW,
  SUBLW,
  XORLW: begin
       Result := nemo + IntToHex(k_,3);
     end;
  CLRW,
  NOP,
  CLRWDT,
  RETFIE,
  RETURN,
  SLEEP: begin
       Result := nemo ;
     end;
  else
    Result := 'Invalid'
  end;
end;

//funciones para la memoria RAM
function TPIC16.GetMemRAM(const size: integer; var addr: word; var bnk: byte): boolean;
{Devuelve una dirección libre de la memoria flash (y el banco) para ubicar un bloque
 del tamaño indicado. Si encuentra espacio, devuelve TRUE}
begin
  Result := false;   //valor inicial
  if NumBanks = 2 then begin
    //solo 2 bancos
    if bank0.GetMemRAM(size, addr) then begin
      bnk := 0;      //encontró en este banco
      Result := true;
      exit;
    end else if bank1.GetMemRAM(size, addr) then begin
      bnk := 1;      //encontró en este banco
      Result := true;
      exit;
    end;
  end else if NumBanks = 3 then begin
    //3 bancos
    if bank0.GetMemRAM(size, addr) then begin
      bnk := 0;      //encontró en este banco
      Result := true;
      exit;
    end else if bank1.GetMemRAM(size, addr) then begin
      bnk := 1;      //encontró en este banco
      Result := true;
      exit;
    end else if bank2.GetMemRAM(size, addr) then begin
      bnk := 2;      //encontró en este banco
      Result := true;
      exit;
    end;
  end else begin
    //se asume 4 bancos
    if bank0.GetMemRAM(size, addr) then begin
      bnk := 0;      //encontró en este banco
      Result := true;
      exit;
    end else if bank1.GetMemRAM(size, addr) then begin
      bnk := 1;      //encontró en este banco
      Result := true;
      exit;
    end else if bank2.GetMemRAM(size, addr) then begin
      bnk := 2;      //encontró en este banco
      Result := true;
      exit;
    end else if bank3.GetMemRAM(size, addr) then begin
      bnk := 3;      //encontró en este banco
      Result := true;
      exit;
    end;
  end;
  {si llegó aquí es porque no encontró la memoria solicitada,
  al menos de ese tamaño}
end;
function TPIC16.FreeMemRAM(const size: integer; var addr: word): boolean;
begin

end;
function TPIC16.TotalMemRAM: word;
{Devuelve el total de memoria RAM disponible}
begin
  case NumBanks of
  2: Result := bank0.TotalGPR + bank1.TotalGPR;
  3: Result := bank0.TotalGPR + bank1.TotalGPR + bank2.TotalGPR;
  4: Result := bank0.TotalGPR + bank1.TotalGPR + bank2.TotalGPR + bank3.TotalGPR;
  end;
end;
function TPIC16.UsedMemRAM: word;
{Devuelve el total de memoria RAM usada}
begin
  case NumBanks of
  2: Result := bank0.UsedGPR + bank1.UsedGPR;
  3: Result := bank0.UsedGPR + bank1.UsedGPR + bank2.UsedGPR;
  4: Result := bank0.UsedGPR + bank1.UsedGPR + bank2.UsedGPR + bank3.UsedGPR;
  end;
end;
procedure TPIC16.ClearMemRAM;
var
  i: Integer;
begin
  for i:=0 to high(ram) do begin
    ram[i].value := $00;
    ram[i].used := false;
  end;
end;
//funciones para la memoria Flash
function TPIC16.TotalMemFlash: word;
begin
  Result := NumPages * PIC_PAGE_SIZE;
end;
function TPIC16.UsedMemFlash: word;
begin
  case NumPages of
  1: Result := page0.Used;
  2: Result := page0.Used + page1.Used;
  3: Result := page0.Used + page1.Used + page2.Used;
  4: Result := page0.Used + page1.Used + page2.Used + page3.Used;
  end;
end;
procedure TPIC16.ClearMemFlash;
var
  i: Integer;
begin
  for i:=0 to high(flash) do begin
    flash[i].value := $3FFF;
    flash[i].used := false;
    flash[i].comment:='';
  end;
end;
procedure TPIC16.GenHex(hexFile: string);
begin
  hexLines.Clear;
  GenHexExAdd($0000);
  //escribe datos
  case NumPages of
  1: begin
      GenHexData(page0);
  end;
  2:begin
      GenHexData(page0);
      GenHexData(page1);
  end;
  3:begin
      GenHexData(page0);
      GenHexData(page1);
      GenHexData(page2);
  end;
  4:begin
      GenHexData(page0);
      GenHexData(page1);
      GenHexData(page2);
      GenHexData(page3);
  end;
  end;
  GenHexEOF;  //fin de archivo
  GenHexComm('PIC16FXXXX');   //comentario
  hexLines.SaveToFile(hexFile);  //genera archivo
end;
procedure TPIC16.ShowCode(lOut: TStrings; pag: TFlashPage);
{Muestra el código desensamblado de una página}
var
  i, il: Word;
  val: Word;
  comLin: string;   //comentario de línea
  comLat: string;   //comentario lateral
begin
  if pag.nUsed = 0 then exit; //no hay datos
  for i:=pag.minUsed to pag.maxUsed do begin
    if pag.mem[i].comment<>'' then begin  //pone comentario si exste
      comLin := pag.mem[i].comment;
      il := pos('|',comLin);
      if il<>0 then begin
        //hay comentario lateral
        comLat := copy(comLin, il +1, 100);
        comLin := copy(comLin, 1, il-1);
      end else begin
        comLat := '';
      end;
    end else begin
      comLin := '';
      comLat := '';
    end;
    val := pag.mem[i].value;
    Decode(val);   //decodifica instrucción
//    lOut.Add('    $'+IntToHex(i,4) + ':' +IntToHex(val,4)+ ' ' + Disassembler);
    if comLin<>'' then    //escribe comentario de línea
      lOut.Add(comLin);
    lOut.Add('    $'+IntToHex(i,4) + ': ' + Disassembler + ' ' + comLat);
  end;
end;
procedure TPIC16.DumpCode(l: TStrings);
{Desensambla las instrucciones grabadas en el PIC.
 Se debe llamar despues de llamar a GenHex(), para que se actualicen las variables}
begin
  case NumPages of
  1: begin
      ShowCode(l, page0);
  end;
  2:begin
      ShowCode(l, page0);
      ShowCode(l, page1);
  end;
  3:begin
      ShowCode(l, page0);
      ShowCode(l, page1);
      ShowCode(l, page2);
  end;
  4:begin
      ShowCode(l, page0);
      ShowCode(l, page1);
      ShowCode(l, page2);
      ShowCode(l, page3);
  end;
  end;
end;
constructor TPIC16.Create;
begin
  inherited Create;
  hexLines := TStringList.Create;
  //configuración de hardware por defecto
  frequen := 4000000;    //4MHz
  NumBanks:=2;     //Número de bancos de RAM. Por defecto se asume 2
  NumPages:=1;     //Número de páginas de memoria Flash. Por defecto 1
  GPRStart:=$20;   //dirección de inicio de los registros de usuario

  bank0.Init($000, nil   , @ram);
  bank1.Init($080, @bank0, @ram);
  bank2.Init($100, @bank0, @ram);
  bank3.Init($180, @bank0, @ram);

  page0.Init($0000          , @flash);
  page1.Init(1*PIC_PAGE_SIZE, @flash);
  page2.Init(2*PIC_PAGE_SIZE, @flash);
  page3.Init(3*PIC_PAGE_SIZE, @flash);

  CommonRAM:=true; //los últimos 16 bytes están mapeados en el banco 0
  //estado inicial
  iFlash := 0;   //posición de inicio
  ClearMemRAM;
  ClearMemFlash;
end;
destructor TPIC16.Destroy;
begin
  hexLines.Destroy;
  inherited Destroy;
end;

initialization
  //Inicializa Mnemónico de instrucciones
  PIC16InstName[ADDWF ] := 'ADDWF';
  PIC16InstName[ANDWF ] := 'ANDWF';
  PIC16InstName[CLRF  ] := 'CLRF';
  PIC16InstName[CLRW  ] := 'CLRW';
  PIC16InstName[COMF  ] := 'COMF';
  PIC16InstName[DECF  ] := 'DECF';
  PIC16InstName[DECFSZ] := 'DECFSZ';
  PIC16InstName[INCF  ] := 'INCF';
  PIC16InstName[INCFSZ] := 'INCFSZ';
  PIC16InstName[IORWF ] := 'IORWF';
  PIC16InstName[MOVF  ] := 'MOVF';
  PIC16InstName[MOVWF ] := 'MOVWF';
  PIC16InstName[NOP   ] := 'NOP';
  PIC16InstName[RLF   ] := 'RLF';
  PIC16InstName[RRF   ] := 'RRF';
  PIC16InstName[SUBWF ] := 'SUBWF';
  PIC16InstName[SWAPF ] := 'SWAPF';
  PIC16InstName[XORWF ] := 'XORWF';
  PIC16InstName[BCF   ] := 'BCF';
  PIC16InstName[BSF   ] := 'BSF';
  PIC16InstName[BTFSC ] := 'BTFSC';
  PIC16InstName[BTFSS ] := 'BTFSS';
  PIC16InstName[ADDLW ] := 'ADDLW';
  PIC16InstName[ANDLW ] := 'ANDLW';
  PIC16InstName[CALL  ] := 'CALL';
  PIC16InstName[CLRWDT] := 'CLRWDT';
  PIC16InstName[GOTO_ ] := 'GOTO';
  PIC16InstName[IORLW ] := 'IORLW';
  PIC16InstName[MOVLW ] := 'MOVLW';
  PIC16InstName[RETFIE] := 'RETFIE';
  PIC16InstName[RETLW ] := 'RETLW';
  PIC16InstName[RETURN] := 'RETURN';
  PIC16InstName[SLEEP ] := 'SLEEP';
  PIC16InstName[SUBLW ] := 'SUBLW';
  PIC16InstName[XORLW ] := 'XORLW';
  PIC16InstName[_Inval] := '<Inval>';

  //Inicializa Sintaxis de las instrucciones
  PIC16InstSyntax[ADDWF ] := 'fd';
  PIC16InstSyntax[ANDWF ] := 'fd';
  PIC16InstSyntax[CLRF  ] := 'f';
  PIC16InstSyntax[CLRW  ] := '';
  PIC16InstSyntax[COMF  ] := 'fd';
  PIC16InstSyntax[DECF  ] := 'fd';
  PIC16InstSyntax[DECFSZ] := 'fd';
  PIC16InstSyntax[INCF  ] := 'fd';
  PIC16InstSyntax[INCFSZ] := 'fd';
  PIC16InstSyntax[IORWF ] := 'fd';
  PIC16InstSyntax[MOVF  ] := 'fd';
  PIC16InstSyntax[MOVWF ] := 'f';
  PIC16InstSyntax[NOP   ] := '';
  PIC16InstSyntax[RLF   ] := 'fd';
  PIC16InstSyntax[RRF   ] := 'fd';
  PIC16InstSyntax[SUBWF ] := 'fd';
  PIC16InstSyntax[SWAPF ] := 'fd';
  PIC16InstSyntax[XORWF ] := 'fd';
  PIC16InstSyntax[BCF   ] := 'fb';
  PIC16InstSyntax[BSF   ] := 'fb';
  PIC16InstSyntax[BTFSC ] := 'fb';
  PIC16InstSyntax[BTFSS ] := 'fb';
  PIC16InstSyntax[ADDLW ] := 'k';
  PIC16InstSyntax[ANDLW ] := 'k';
  PIC16InstSyntax[CALL  ] := 'k';
  PIC16InstSyntax[CLRWDT] := '';
  PIC16InstSyntax[GOTO_ ] := 'k';
  PIC16InstSyntax[IORLW ] := 'k';
  PIC16InstSyntax[MOVLW ] := 'k';
  PIC16InstSyntax[RETFIE] := '';
  PIC16InstSyntax[RETLW ] := 'k';
  PIC16InstSyntax[RETURN] := '';
  PIC16InstSyntax[SLEEP ] := '';
  PIC16InstSyntax[SUBLW ] := 'k';
  PIC16InstSyntax[XORLW ] := 'k';
  PIC16InstSyntax[_Inval] := '<???>';
end.
