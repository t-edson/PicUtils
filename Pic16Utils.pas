{PIC16Utils

Descripción
===========
Unidad con utilidades para la programación de microcontroladores PIC de rango
medio con instrucciones de 14 bits. Incluye a la mayoría de la serie
PIC16FXXXX.
Esta unidad trabaja con tamaños de página de 2K y tamaños de bancos de 128 bytes.
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
  Classes, SysUtils, LCLProc;
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
  TPIC16CellState = (
     cs_impleSFR,   //Registros de funciones especiales. Habilitado para uso.
     cs_impleGPR,   //Registros de uso general. Habilitado para uso.
     cs_unimplem   //No implementado
  );

  { TPIC16RamCell }
  {Modela a una dirección lógica de la memoria RAM. Se ha taratdo de hacer una
  definición eficiente de esta estructura para facilitar la implementación de
  simuladores en tiempo real. Podemos usar un tamaño mediano para este registro,
  porque no esperamos tener muchas celdas de RAM (<1K).}
  TPIC16RamCellPtr = ^TPIC16RamCell;
  TPIC16RamCell = object
  private
    Fvalue  : byte;     //value of the memory
    Fused   : byte;     //Bitmap. Indicates the used bits ($00->all free; $ff->all bits used.)
    function Getused: byte;
    function Getvalue: byte;
    procedure Setused(AValue: byte);
    procedure Setvalue(AValue: byte);
  public
    addr   : word;     //dirección física de memoria, en donde está la celda.
    name   : string;   //name of the record
    bitname: array[0..7] of string;  //name of the bits.
    state  : TPIC16CellState;  //status of the cell
    mappedTo: TPIC16RamCellPtr;  //Indica que está mapeado a otra celda, de otra dirección
    property value: byte read Getvalue write Setvalue;
    property used: byte read Getused write Setused;
  end;
  TPIC16Ram = array[0..PIC_MAX_RAM-1] of TPIC16RamCell;
  PIC16RamPtr = ^TPIC16Ram;
  TRutExplorRAM = procedure(offs, bnk: byte; regPtr: TPIC16RamCellPtr) of object;
  {Representa a un banco de memoria del PIC. En un banco las direcciones de memoria
   se mapean siempre desde $00 hasta $7F. No almacenan datos, solo usan referencias.}
  ptrRAMBank = ^TRAMBank;
  { TRAMBank }
  TRAMBank = object
  public
    numBank   : integer;       //Número de banco
    ramPtr    : PIC16RamPtr;  //Puntero a memoria RAM
    AddrStart : word;          //dirección de inicio en la memoria RAM total
  private
    function AvailBit(const i: byte): boolean;
    function AvailByte(const i: byte): boolean;
    function UsedByte(const i: byte): boolean;
  public
    procedure Init(num: byte; AddrStart0: word; ram0: PIC16RamPtr);  //inicia objeto
//    property mem[i : byte] : TPIC16RamCellPtr read Getmem;
    //Funciones para administración de la memoria ramPtr
    function HaveConsecGPR(const i, n: byte): boolean; //Indica si hay "n" bytes libres
    procedure UseConsecGPR(const i, n: byte);  //Ocupa "n" bytes en la posición "i"
    function GetFreeByte(out offs: byte): boolean;     //obtiene una dirección libre
    function GetFreeBytes(const size: integer; var offs: byte): boolean;  //obtiene una dirección libre
    procedure ExploreUsed(rutExplorRAM: TRutExplorRAM);  //Explora uno a uno los bytes usados
    function TotalGPR: byte; //total de bytes que contiene para el usuario
  end;

type  //Models for Flash memory
  TPIC16FlashCell = record
    value    : word;     //value of the memory
    used     : boolean;  //indicate if have been written
    {Estos campos de cadena ocupan bastante espacio, aún cuado están en NULL. Si se
    quisiera optimizar el uso de RAM, se podría pensar en codificar, variso campos en
    una sola cadena.}
    topLabel : string;   //label on the top of the cell.
    topComment: string;  //comment on the top of the cell.
    sideComment: string; //right comment to code

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
    function ExtractHex(out Addr: word): string;  //devuelve una línea de texto del código en hexadecimal
  end;

type
  {Objeto que representa al hardware de un PIC de la serie 16}
  { TPIC16 }
  TPIC16 = class
  private
    hexLines : TStringList;   //usado para crear archivo *.hex
    procedure GenHexComm(comment: string);
    procedure GenHexData(Address: word; Data: string);
    procedure GenHexData(var pg: TFlashPage);
    procedure GenHexEOF;
    procedure GenHexExAdd(Data: word);
    function HexChecksum(const lin: string): string;
    procedure ShowCode(lOut: TStrings; pag: TFlashPage; incAdrr, incCom,
      incVarNam: boolean);
  private
//    FCommonRAM: boolean;
    function StrHexFlash(i1, i2: integer): string;
  private //campos para procesar instrucciones
    FMaxFlash: integer;
    idIns: TPIC16Inst;    //ID de Instrucción.
    d_   : TPIC16destin;  //Destino de operación. Válido solo en algunas instrucciones.
    f_   : byte;          //Registro destino. Válido solo en algunas instrucciones.
    b_   : byte;          //Bit destino. Válido solo en algunas instrucciones.
    k_   : word;          //Parámetro Literal. Válido solo en algunas instrucciones.
    function GetBank(i : Longint): TRAMBank;
    function GetINTCON: byte;
    function GetINTCON_GIE: boolean;
    function GetPage(i : Longint): TFlashPage;
    function GetSTATUS: byte;
    function GetSTATUS_C: boolean;
    function GetSTATUS_DC: boolean;
    function GetSTATUS_Z: boolean;
    procedure SetINTCON_GIE(AValue: boolean);
    procedure SetSTATUS_C(AValue: boolean);
    procedure SetSTATUS_DC(AValue: boolean);
    procedure SetSTATUS_Z(AValue: boolean);
    procedure SetMaxFlash(AValue: integer);
  public   //Campos que modelan a los registros internos
    W        : byte;   //Registro de trabajo
    PCL      : byte;   //Contador de Programa L
    PCH      : byte;   //Contador de Programa H
    PCLATH   : byte;   //Contador de Programa H
    STKPTR   : 0..7;   //Puntero de pila
    STACK    : array[0..7] of word;
    property STATUS: byte read GetSTATUS;
    property STATUS_Z: boolean read GetSTATUS_Z write SetSTATUS_Z;
    property STATUS_C: boolean read GetSTATUS_C write SetSTATUS_C;
    property STATUS_DC: boolean read GetSTATUS_DC write SetSTATUS_DC;
    property INTCON: byte read GetINTCON;
    property INTCON_GIE: boolean read GetINTCON_GIE write SetINTCON_GIE;

    procedure Exec();  //Ejecuta instrucción actual
    procedure Reset;
  public
    //memorias
    flash    : TPIC16Flash;   //memoria Flash
    ram      : TPIC16Ram;     //memoria RAM
    Model    : string;    //modelo de PIC
    Npins    : byte;      //número de pines
    frequen  : integer;   //frecuencia del reloj
    MaxFreq  : integer;   //máxima frecuencia del reloj
    //Propiedades que definen la arquitectura del PIC destino.
    NumBanks: byte;      //Número de bancos de RAM.
    NumPages: byte;      //Número de páginas de memoria Flash.
    bank0, bank1, bank2, bank3: TRAMBank;  //bancos de memoria RAM
    page0, page1, page2, page3: TFlashPage;  //páginas de memoria Flash
    iFlash: integer;   //puntero a la memoria Flash, para escribir
    MsjError: string;
    procedure Decode(const opCode: word);  //decodifica instrucción
    function Disassembler(useVarName: boolean=false): string;  //Desensambla la instrucción actual
    property banks[i : Longint]: TRAMBank Read GetBank;
    property pages[i : Longint]: TFlashPage Read GetPage;
    property MaxFlash: integer read FMaxFlash write SetMaxFlash;   {Máximo número de celdas de flash implementadas (solo en los casos de
                         implementación parcial de la Flash). Solo es aplicable cuando es mayor que 0}
    //funciones para la memoria RAM
    function GetFreeBit(var offs, bnk, bit: byte): boolean;
    function GetFreeByte(out offs, bnk: byte): boolean;
    function GetFreeBytes(const size: integer; var offs, bnk: byte): boolean;  //obtiene una dirección libre
    function TotalMemRAM: word; //devuelve el total de memoria RAM
    function UsedMemRAM: word;  //devuelve el total de memoria RAM usada
    procedure ExploreUsed(rutExplorRAM: TRutExplorRAM);    //devuelve un reporte del uso de la RAM
    function ValidRAMaddr(addr: word): boolean;  //indica si una posición de memoria es válida
    procedure ClearMemRAM;
    procedure DisableAllRAM;
    procedure SetStatRAM(i1, i2: word; status0: TPIC16CellState);
    procedure SetMappRAM(i1, i2: word; MappedTo: word);
    function SetStatRAMCom(strDef: string): boolean;
    function SetMappRAMCom(strDef: string): boolean;
    function BankToAbsRAM(const offset, bank: byte): word; //devuelve dirección absoluta
    procedure AbsToBankRAM(const AbsAddr: word; var offset, bank: byte); //convierte dirección absoluta
    //funciones para manejo de nombres
    function NameRAM(const addr: word; const bnk: byte): string;
    function NameRAMbit(const addr: word; const bnk,bit: byte): string;
    procedure SetNameRAM(const addr: word; const bnk: byte; const nam: string);  //Fija nombre a una celda de RAM
    procedure AddNameRAM(const addr: word; const bnk: byte; const nam: string);  //Agrega nombre a una celda de RAM
    procedure SetNameRAMbit(const addr: word; const bnk, bit: byte; const nam: string);  //Fija nombre a un bitde RAM
    //funciones para la memoria Flash
    function UsedMemFlash: word;  //devuelve el total de memoria Flash usada
    procedure ClearMemFlash;
    //Métodos para codificar instrucciones de acuerdo a la sintaxis
    procedure useFlash;
    procedure codAsmFD(const inst: TPIC16Inst; const f: byte; d: TPIC16destin);
    procedure codAsmF(const inst: TPIC16Inst; const f: byte);
    procedure codAsmFB(const inst: TPIC16Inst; const f: byte; b: byte);
    procedure codAsmK(const inst: TPIC16Inst; const k: byte);
    procedure codAsmA(const inst: TPIC16Inst; const a: word);
    procedure codAsm(const inst: TPIC16Inst);
    procedure codGotoAt(iflash0: integer; const k: word);
    procedure codCallAt(iflash0: integer; const k: word);
    //Métodos adicionales
    function FindOpcode(Op: string; var syntax: string): TPIC16Inst;  //busca Opcode
    procedure addTopLabel(lbl: string);  //Add a comment to the ASM code
    procedure addTopComm(comm: string; replace: boolean = true);  //Add a comment to the ASM code
    procedure addSideComm(comm: string; before: boolean); //Add lateral comment to the ASM code
    procedure GenHex(hexFile: string; ConfigWord: integer = - 1);  //genera un archivo hex
    procedure DumpCode(l: TStrings; incAdrr, incCom, incVarNam: boolean);  //vuelva en código que contiene
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

{ TPIC16RamCell }
function TPIC16RamCell.Getused: byte;
begin
  if mappedTo = nil then begin
    //Celda independiente
    Result := Fused;
  end else begin
    //Celda reflejada. Leemos la disponibilidad, de la celda origen.
    Result := mappedTo^.used;
  end;
end;
procedure TPIC16RamCell.Setused(AValue: byte);
begin
  if mappedTo = nil then begin
    //Celda independiente
    Fused := AValue;
  end else begin
    //Celda reflejada. Escribimos la disponibilidad, en la celda origen.
    mappedTo^.used := AValue;
  end;
end;
function TPIC16RamCell.Getvalue: byte;
begin
  if mappedTo = nil then begin
    //Celda independiente
    Result := Fvalue;
  end else begin
    //Celda reflejada. Leemos la disponibilidad, de la celda origen.
    Result := mappedTo^.Fvalue;
  end;
end;
procedure TPIC16RamCell.Setvalue(AValue: byte);
begin
  if mappedTo = nil then begin
    //Celda independiente
    Fvalue := AValue;
  end else begin
    //Celda reflejada. Escribimos la disponibilidad, en la celda origen.
    mappedTo^.Fvalue:= AValue;
  end;
end;

{ TRAMBank }
//procedure TRAMBank.Setmem(i: byte; AValue: TPIC16RamCellPtr);
////Escribe en un banco de memoria
//begin
//  //Se asume que i debe ser menor que $7F
//  if ram^[i+AddrStart].state = cs_mapToBnk then begin
//    //estas direcciones están mapeadas en otro banco
//    BankMapped^.mem[i] := AValue;
//  end else begin  //caso normal
//    ram^[i+AddrStart] := AValue;
//  end;
//end;
procedure TRAMBank.Init(num: byte; AddrStart0: word;
  ram0: PIC16RamPtr);
begin
  numBank := num;
  AddrStart :=AddrStart0;
  ramPtr       :=ram0;
end;
function TRAMBank.AvailBit(const i: byte): boolean; inline;
{Indica si hay al menos un bit disponible, en la posición de memoria indicada}
begin
  Result := (ramPtr^[i+AddrStart].state = cs_impleGPR) and
            (ramPtr^[i+AddrStart].used <> 255);
end;
function TRAMBank.AvailByte(const i: byte): boolean; inline;
{Indica si hay un byte disponible en la posición de memoria indicada}
begin
  Result := (ramPtr^[i+AddrStart].state = cs_impleGPR) and
            (ramPtr^[i+AddrStart].used = 0);
end;
function TRAMBank.UsedByte(const i: byte): boolean; inline;
{Indica si se usa al menos un bit, del byte en la posición de memoria indicada.}
begin
  Result := (ramPtr^[i+AddrStart].state = cs_impleGPR) and
            (ramPtr^[i+AddrStart].used <> 0);
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
    if not AvailByte(i) then exit;  //no se puede usar
    inc(c);      //verifica siguiente
    inc(j);
  end;
  if j>$7F then exit;  //no hay más espacio
  //si llega aquí es porque estaban libres los bloques
  Result := true;
end;
procedure TRAMBank.UseConsecGPR(const i, n: byte);
{Marca "n" bytes como usados en la posición de memoria "i", en este banco.
 Debe haberse verificado previamente que los parámetros son válidos, porque asuí no
 se hará ninguan verificación.}
var j: byte;
begin
  for j:=i to byte(i+n)-1 do begin
    ramPtr^[j+AddrStart].used:=255;  //todos los bits
    //    mem[j].used := true;   //no se puede
  end;
end;
function TRAMBank.GetFreeByte(out offs: byte): boolean;
{Busca un byte de memoria RAM libre, en este banco. }
var
  i: byte;
begin
  Result := false;  //valor por defecto
  for i:=0 to $7F do begin  //verifica 1 a 1, por seguridad
    if AvailByte(i)  then begin
      //encontró
//      mem[i].used:=true;  //marca como usado
      ramPtr^[i+AddrStart].used:=255;   //marca como usado
      offs := i;  //devuelve dirección
      Result := true;  //indica que encontró espacio
      exit;
    end;
  end;
end;
function TRAMBank.GetFreeBytes(const size: integer; var offs: byte): boolean;
{Busca un bloque de bytes consecutivs de memoria RAM en este banco. }
var
  i: byte;
begin
  Result := false;  //valor por defecto
  if size=0 then exit;
  for i:=0 to $7F do begin  //verifica 1 a 1, por seguridad
    if HaveConsecGPR(i, size) then begin
      //encontró del tamaño buscado
      UseConsecGPR(i, size);  //marca como usado
      offs := i;  //devuelve dirección
      Result := true;  //indica que encontró espacio
      exit;
    end;
  end;
end;
procedure TRAMBank.ExploreUsed(rutExplorRAM: TRutExplorRAM);
{Realiza una exploración de la memoria RAM usada y llama a rutExplorRAM(), para cada
byte encontrado.}
var
  i: Byte;
begin
  for i:=0 to $7F do begin  //verifica 1 a 1, por seguridad
    if UsedByte(i) then rutExplorRAM(i, 0, @ramPtr^[i+AddrStart]);
  end;
end;
function TRAMBank.TotalGPR: byte;
{Total de memoria disponible para el usuario}
var
  i: Byte;
begin
  Result := 0;
  for i:=0 to $7F do begin  //verifica 1 a 1, por seguridad
    if ramPtr^[i+AddrStart].state = cs_impleGPR then
      inc(Result);
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
function TFlashPage.ExtractHex(out Addr: word): string;
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
procedure TPIC16.useFlash;
{Marca la posición actual, como usada, e incrementa el puntero iFlash. S ihay error,
actualiza el campo "MsjError"}
begin
  //Protección de desborde
  if iFlash > MaxFlash then begin
    MsjError := 'FLASH Memory limit exceeded.';
    exit;
  end;
  flash[iFlash].used := true;  //marca como usado
  inc(iFlash);
end;
procedure TPIC16.codAsmFD(const inst: TPIC16Inst; const f: byte; d: TPIC16destin);
{Codifica las instrucciones orientadas a registro, con sinatxis: NEMÓNICO f,d}
begin
  case inst of
  ADDWF : flash[iFlash].value := %00011100000000 + ord(d) + (f and %1111111);
  ANDWF : flash[iFlash].value := %00010100000000 + ord(d) + (f and %1111111);
  COMF  : flash[iFlash].value := %00100100000000 + ord(d) + (f and %1111111);
  DECF  : flash[iFlash].value := %00001100000000 + ord(d) + (f and %1111111);
  DECFSZ: flash[iFlash].value := %00101100000000 + ord(d) + (f and %1111111);
  INCF  : flash[iFlash].value := %00101000000000 + ord(d) + (f and %1111111);
  INCFSZ: flash[iFlash].value := %00111100000000 + ord(d) + (f and %1111111);
  IORWF : flash[iFlash].value := %00010000000000 + ord(d) + (f and %1111111);
  MOVF  : flash[iFlash].value := %00100000000000 + ord(d) + (f and %1111111);
  RLF   : flash[iFlash].value := %00110100000000 + ord(d) + (f and %1111111);
  RRF   : flash[iFlash].value := %00110000000000 + ord(d) + (f and %1111111);
  SUBWF : flash[iFlash].value := %00001000000000 + ord(d) + (f and %1111111);
  SWAPF : flash[iFlash].value := %00111000000000 + ord(d) + (f and %1111111);
  XORWF : flash[iFlash].value := %00011000000000 + ord(d) + (f and %1111111);
  else
    raise Exception.Create('Implementation Error.');
  end;
  useFlash;  //marca como usado e incrementa puntero.
end;
procedure TPIC16.codAsmF(const inst: TPIC16Inst; const f: byte);
{Codifica las instrucciones orientadas a registro, con sinatxis: NEMÓNICO f}
begin
  case inst of
  CLRF  : flash[iFlash].value := %00000110000000 + (f and %1111111);
  MOVWF : flash[iFlash].value := %00000010000000 + (f and %1111111);
  else
    raise Exception.Create('Implementation Error.');
  end;
  useFlash;  //marca como usado e incrementa puntero.
end;
procedure TPIC16.codAsmFB(const inst: TPIC16Inst; const f: byte; b: byte);
//Codifica las instrucciones orientadas a bit.
begin
  case inst of
  BCF  : flash[iFlash].value := %01000000000000 + word(b<<7) + (f and %1111111);
  BSF  : flash[iFlash].value := %01010000000000 + word(b<<7) + (f and %1111111);
  BTFSC: flash[iFlash].value := %01100000000000 + word(b<<7) + (f and %1111111);
  BTFSS: flash[iFlash].value := %01110000000000 + word(b<<7) + (f and %1111111);
  else
    raise Exception.Create('Implementation Error.');
  end;
  useFlash;  //marca como usado e incrementa puntero.
end;
procedure TPIC16.codAsmK(const inst: TPIC16Inst; const k: byte);
{Codifica las instrucciones con constantes.}
begin
  case inst of
  ADDLW : flash[iFlash].value := %11111000000000 + k;
  ANDLW : flash[iFlash].value := %11100100000000 + k;
  IORLW : flash[iFlash].value := %11100000000000 + k;
  MOVLW : flash[iFlash].value := %11000000000000 + k;
  RETLW : flash[iFlash].value := %11010000000000 + k;
  SUBLW : flash[iFlash].value := %11110000000000 + k;
  XORLW : flash[iFlash].value := %11101000000000 + k;
  else
    raise Exception.Create('Implementation Error.');
  end;
  useFlash;  //marca como usado e incrementa puntero.
end;
procedure TPIC16.codAsmA(const inst: TPIC16Inst; const a: word);
{Codifica las instrucciones de control.
 "a" debe ser word, porque la dirección destino, requiere 11 bits.}
begin
  case inst of
  CALL  : flash[iFlash].value := %10000000000000 + (a and %11111111111);
  GOTO_ : flash[iFlash].value := %10100000000000 + (a and %11111111111);
  else
    raise Exception.Create('Implementation Error.');
  end;
  useFlash;  //marca como usado e incrementa puntero.
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
    raise Exception.Create('Implementation Error.');
  end;
  useFlash;  //marca como usado e incrementa puntero.
end;
procedure TPIC16.codGotoAt(iflash0: integer; const k: word);
{Codifica una instrucción GOTO, en una posición específica y sin alterar el puntero "iFlash"
actual. Se usa para completar saltos indefinidos}
begin
  flash[iFlash0].value := %10100000000000 + (k and %11111111111);
end;
procedure TPIC16.codCallAt(iflash0: integer; const k: word);
{Codifica una instrucción CALL, en una posición específica y sin alterar el puntero "iFlash"
actual. Se usa para completar llamadas indefinidas}
begin
  flash[iFlash0].value := %10000000000000 + (k and %11111111111);
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
procedure TPIC16.addTopLabel(lbl: string);
begin
  flash[iFlash].topLabel := lbl;
end;
procedure TPIC16.addTopComm(comm: string; replace: boolean);
{Agrega un comentario de línea al código en la posición de memoria actual}
begin
  if replace then begin
    flash[iFlash].topComment := comm;
  end else begin
    flash[iFlash].topComment := flash[iFlash].topComment + comm;
  end;
end;
procedure TPIC16.addSideComm(comm: string; before: boolean);
{Agrega un comentario para que apareza al lado de la instrucción.
 "before" = TRUE -> Se debe llamar después de codificar la instrucción
 "before" = FALSE -> Se debe llamar antes de codificar la instrucción
 }
begin
  if before then begin
    if iFlash= 0 then exit;
    flash[iFlash-1].sideComment+=comm;   //se agrega al que pudiera haber
  end else begin
    if iFlash= 0 then exit;
    flash[iFlash].sideComment+=comm;   //se agrega al que pudiera haber
  end;
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
  lin:= IntToHex(ByteCount,2) + IntToHex(Address*2,4) + RecordType +  Data;
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
function TPIC16.Disassembler(useVarName: boolean = false): string;
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
      if useVarName and (ram[f_].name<>'') then begin
        if d_ = toF then
          Result := nemo + ram[f_].name + ',f'
        else
          Result := nemo + ram[f_].name + ',w';
      end else begin
        if d_ = toF then
          Result := nemo + '0x'+IntToHex(f_,3) + ',f'
        else
          Result := nemo + '0x'+IntToHex(f_,3) + ',w';
      end;
     end;
  CLRF,
  MOVWF: begin
        if useVarName and (ram[f_].name<>'') then begin
          Result := nemo + ram[f_].name;
        end else begin
          Result := nemo + '0x'+IntToHex(f_,3);
        end;
     end;
  BCF,
  BSF,
  BTFSC,
  BTFSS: begin    //Instrucciones de bit
      if useVarName and (ram[f_].bitname[b_]<>'') then begin
        //Hay nombre de bit
        Result := nemo + ram[f_].bitname[b_];
      end else if useVarName and (ram[f_].name<>'') then begin
        //Hay nombre de byte
        Result := nemo + ram[f_].name + ', ' + IntToStr(b_);
      end else begin
        Result := nemo + '0x'+IntToHex(f_,3) + ', ' + IntToStr(b_);
      end;
     end;
  ADDLW,
  ANDLW,
  IORLW,
  MOVLW,
  RETLW,
  SUBLW,
  XORLW: begin
       Result := nemo + '0x'+IntToHex(k_,2);
     end;
  CALL,
  GOTO_: begin   //Faltaría decodificar la dirección
    Result := nemo + '0x'+IntToHex(k_,3);
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
function TPIC16.GetBank(i : Longint): TRAMBank;
begin
  case i of
  0: Result := bank0;
  1: Result := bank1;
  2: Result := bank2;
  3: Result := bank3;
  else
    Result := bank0;
  end;
end;
function TPIC16.GetPage(i: Longint): TFlashPage;
begin
  case i of
  0: Result := page0;
  1: Result := page1;
  2: Result := page2;
  3: Result := page3;
  else
    Result := page0;
  end;
end;
function TPIC16.GetSTATUS: byte;
begin
  Result := ram[$03].Fvalue;
end;
function TPIC16.GetSTATUS_Z: boolean;
begin
  Result := (ram[$03].Fvalue and %00000100) <> 0;
end;
procedure TPIC16.SetSTATUS_Z(AValue: boolean);
begin
  if AVAlue then ram[$03].Fvalue := ram[$03].Fvalue or  %00000100
            else ram[$03].Fvalue := ram[$03].Fvalue and %11111011;
end;
function TPIC16.GetSTATUS_C: boolean;
begin
  Result := (ram[$03].Fvalue and %00000001) <> 0;
end;
procedure TPIC16.SetSTATUS_C(AValue: boolean);
begin
  if AVAlue then ram[$03].Fvalue := ram[$03].Fvalue or  %00000001
            else ram[$03].Fvalue := ram[$03].Fvalue and %11111110;
end;
function TPIC16.GetSTATUS_DC: boolean;
begin
  Result := (ram[$03].Fvalue and %00000010) <> 0;
end;
procedure TPIC16.SetSTATUS_DC(AValue: boolean);
begin
  if AVAlue then ram[$03].Fvalue := ram[$03].Fvalue or  %00000010
            else ram[$03].Fvalue := ram[$03].Fvalue and %11111101;
end;
function TPIC16.GetINTCON: byte;
begin
  Result := ram[$0B].Fvalue;
end;
function TPIC16.GetINTCON_GIE: boolean;
begin
  Result := (ram[$0B].Fvalue and %10000000) <> 0;
end;
procedure TPIC16.SetINTCON_GIE(AValue: boolean);
begin
  if AVAlue then ram[$0B].Fvalue := ram[$0B].Fvalue or  %10000000
            else ram[$0B].Fvalue := ram[$0B].Fvalue and %01111111;
end;
procedure TPIC16.SetMaxFlash(AValue: integer);
begin
  if FMaxFlash = AValue then Exit;
  FMaxFlash := AValue;
end;
procedure TPIC16.Exec;
{Ejecuta la instrución actual.
Falta implementar las opraciones, cuando acceden al registro INDF, falta el SLEEP, el
Watchdog timer, los ocntadores, las interrupciones}
var
  val: Word;
  fullAdd: word;
  msk, resNib: byte;
  resByte, bit7, bit0: byte;
  resWord: word;
  resInt : integer;
begin
  //Decodifica instrucción
  val := page0.mem[PCL].value;
  Decode(val);   //decodifica instrucción
  case idIns of
  ADDWF: begin
    fullAdd := (STATUS and %01100000) << 2 + f_ ;//  f_
    resWord := W + ram[fullAdd].value;
    resNib := (W and $0F) + (ram[fullAdd].value and $0F);
    if d_ = toF then begin
      ram[fullAdd].value := resWord and $FF;
    end else begin  //toW
      w := resWord and $FF;
    end;
    STATUS_Z := (resWord and $ff) = 0;
    STATUS_C := (resWord > 255);
    STATUS_DC := (resNib > 15);
  end;
  ANDWF: begin
    fullAdd := (STATUS and %01100000) << 2 + f_ ;//  f_
    resByte := W and ram[fullAdd].value;
    if d_ = toF then begin
      ram[fullAdd].value := resByte;
    end else begin  //toW
      w := resByte;
    end;
    STATUS_Z := resByte = 0;
  end;
  CLRF: begin
    fullAdd := (STATUS and %01100000) << 2 + f_ ;//  f_
    ram[fullAdd].value := 0;
    STATUS_Z := true;
  end;
  CLRW: begin
    W := 0;
    STATUS_Z := true;
  end;
  COMF : begin
    fullAdd := (STATUS and %01100000) << 2 + f_ ;//  f_
    resByte := not ram[fullAdd].value;
    if d_ = toF then begin
      ram[fullAdd].value := resByte;
    end else begin  //toW
      w := resByte;
    end;
    STATUS_Z := resByte = 0;
  end;
  DECF : begin
    fullAdd := (STATUS and %01100000) << 2 + f_ ;//  f_
    if ram[fullAdd].value = 0 then resByte := $FF
    else resByte := ram[fullAdd].value-1;
    if d_ = toF then begin
      ram[fullAdd].value := resByte;
    end else begin  //toW
      w := resByte;
    end;
    STATUS_Z := resByte = 0;
  end;
  DECFSZ: begin
    fullAdd := (STATUS and %01100000) << 2 + f_ ;//  f_
    if ram[fullAdd].value = 0 then resByte := $FF
    else resByte := ram[fullAdd].value-1;
    if d_ = toF then begin
      ram[fullAdd].value := resByte;
    end else begin  //toW
      w := resByte;
    end;
    STATUS_Z := resByte = 0;
    if STATUS_Z then begin
      //salta una instrucción
      if PCL = 255 then begin
        PCL := 0;
        inc(PCH);
      end else begin
        inc(PCL);
      end;
    end;
  end;
  INCF: begin
    fullAdd := (STATUS and %01100000) << 2 + f_ ;//  f_
    if ram[fullAdd].value = 255 then resByte := 0
    else resByte := ram[fullAdd].value+1;
    if d_ = toF then begin
      ram[fullAdd].value := resByte;
    end else begin  //toW
      w := resByte;
    end;
    STATUS_Z := resByte = 0;
  end;
  INCFSZ: begin
    fullAdd := (STATUS and %01100000) << 2 + f_ ;//  f_
    if ram[fullAdd].value = 255 then resByte := 0
    else resByte := ram[fullAdd].value+1;
    if d_ = toF then begin
      ram[fullAdd].value := resByte;
    end else begin  //toW
      w := resByte;
    end;
    STATUS_Z := resByte = 0;
    if STATUS_Z then begin
      //salta una instrucción
      if PCL = 255 then begin
        PCL := 0;
        inc(PCH);
      end else begin
        inc(PCL);
      end;
    end;
  end;
  IORWF: begin
    fullAdd := (STATUS and %01100000) << 2 + f_ ;//  f_
    resByte := W or ram[fullAdd].value;
    if d_ = toF then begin
      ram[fullAdd].value := resByte;
    end else begin  //toW
      w := resByte;
    end;
    STATUS_Z := resByte <> 0;
  end;
  MOVF: begin
    fullAdd := (STATUS and %01100000) << 2 + f_ ;//  f_
    if d_ = toF then begin
      //no mueve, solo verifica
      STATUS_Z := (ram[fullAdd].value = 0);
    end else begin  //toW
      w := ram[fullAdd].value;  //lee mapeado, si lo está
      STATUS_Z := (W = 0);
    end;
  end;
  MOVWF: begin
    fullAdd := (STATUS and %01100000) << 2 + f_ ;//  f_
    ram[fullAdd].value := W;   //escribe a donde esté mapeado, (si está mapeado)
    if (fullAdd and $7F) = $02 then begin
      //Es el PCL
      PCH := PCLATH;
    end;
  end;
  NOP: begin
  end;
  RLF: begin
    fullAdd := (STATUS and %01100000) << 2 + f_ ;//  f_
    resByte := ram[fullAdd].value;
    bit7 := resByte and $80; //guarda bit 7
    resByte := (resByte << 1) and $ff;  //desplaza
    //pone C en bit bajo
    if STATUS_C then begin  //C era 1
      resByte := resByte or $01;  //pone a 1 el bit 0
    end else begin          //C era 0
      //no es necesario agregarlo, porque por defecto se agrega 0
    end;
    //Actualiza C
    if bit7 = 0 then STATUS_C := false
                else STATUS_C := true;
    ram[fullAdd].value := resByte;
  end;
  RRF: begin
    fullAdd := (STATUS and %01100000) << 2 + f_ ;//  f_
    resByte := ram[fullAdd].value;
    bit0 := resByte and $01; //guarda bit 0
    resByte := resByte >> 1;  //desplaza
    //pone C en bit alto
    if STATUS_C then begin  //C era 1
      resByte := resByte or $80;  //pone a 1 el bit 0
    end else begin          //C era 0
      //no es necesario agregarlo, porque por defecto se agrega 0
    end;
    //Actualiza C
    if bit0 = 0 then STATUS_C := false
                else STATUS_C := true;
    ram[fullAdd].value := resByte;
  end;
  SUBWF: begin
    fullAdd := (STATUS and %01100000) << 2 + f_ ;//  f_
    resInt := ram[fullAdd].value - W;
    if d_ = toF then begin
      ram[fullAdd].value :=  resInt and $FF;
    end else begin  //toW
      w := resInt and $FF;
    end;
    STATUS_Z := (resInt = 0);
    if resInt < 0 then STATUS_C := false   //negativo
    else STATUS_C := true;
    resInt := (ram[fullAdd].value and $0F) - (W and $0F);
    if resInt < 0 then STATUS_DC := false   //negativo
    else STATUS_DC := true;
  end;
  SWAPF: begin
    fullAdd := (STATUS and %01100000) << 2 + f_ ;//  f_
    resNib := (ram[fullAdd].value and $0F) << 4;
    resByte := (ram[fullAdd].value and $F0) >> 4;
    ram[fullAdd].value := resNib or resByte;
  end;
  XORWF: begin
    fullAdd := (STATUS and %01100000) << 2 + f_ ;//  f_
    resByte := W xor ram[fullAdd].value;
    if d_ = toF then begin
      ram[fullAdd].value := resByte;
    end else begin  //toW
      w := resByte;
    end;
    STATUS_Z := resByte <> 0;
  end;
  //BIT-ORIENTED FILE REGISTER OPERATIONS
  BCF: begin
    msk := $1 << b_;
    msk := not msk;
    fullAdd := (STATUS and %01100000) << 2 + f_ ;//  f_
    ram[fullAdd].value := ram[fullAdd].value and msk;
  end;
  BSF: begin
    msk := $1 << b_;
    fullAdd := (STATUS and %01100000) << 2 + f_ ;//  f_
    ram[fullAdd].value := ram[fullAdd].value or msk;// b_
  end;
  BTFSC: begin
    msk := $1 << b_;
    fullAdd := (STATUS and %01100000) << 2 + f_ ;//  f_
    if (ram[fullAdd].value and msk) = 0 then begin
      //salta una instrucción
      if PCL = 255 then begin
        PCL := 0;
        inc(PCH);
      end else begin
        inc(PCL);
      end;
    end;
  end;
  BTFSS: begin
    msk := $1 << b_;
    fullAdd := (STATUS and %01100000) << 2 + f_ ;//  f_
    if (ram[fullAdd].value and msk) <> 0 then begin
      //salta una instrucción
      if PCL = 255 then begin
        PCL := 0;
        inc(PCH);
      end else begin
        inc(PCL);
      end;
    end;
  end;
  //LITERAL AND CONTROL OPERATIONS
  ADDLW: begin
    resWord := W + k_;
    resNib := (W and $0F) + (k_ and $0F);
    w := resWord and $FF;
    STATUS_Z := (resWord and $ff) = 0;
    STATUS_C := (resWord > 255);
    STATUS_DC := (resNib > 15);
  end;
  ANDLW: begin
    resByte := W and K_;
    w := resByte;
    STATUS_Z := resByte = 0;
  end;
  CALL: begin
    //Guarda dirección en Pila
    STACK[STKPTR] := PCH * 256 + PCL;
    if STKPTR = 7 then STKPTR := 0 else STKPTR := STKPTR +1;
    //En k, deben haber 11 bits
    PCL := k_ and $FF;
    PCH := word(k_ >> 8) or   //toma los 3 bits restantes de k
           (PCLATH and %00011000);  //y completa con los bits 3 y 4 de PCLATH
    exit;
  end;
  CLRWDT: begin
  end;
  GOTO_: begin
      //En k, deben haber 11 bits
      PCL := k_ and $FF;
      PCH := byte(k_ >> 8) or   //toma los 3 bits restantes de k
             (PCLATH and %00011000);  //y completa con los bits 3 y 4 de PCLATH
      exit;
  end;
  IORLW: begin
    resByte := W or k_;
    w := resByte;
    STATUS_Z := resByte <> 0;
  end;
  MOVLW: begin
      W := k_;
  end;
  RETFIE: begin
    //Saca dirección en Pila
    if STKPTR = 0 then STKPTR := 7 else STKPTR := STKPTR - 1;
    PCH := hi(STACK[STKPTR]);  //solo debería haber 5 bits
    PCL := lo(STACK[STKPTR]);
    //Activa GIE
    INTCON_GIE := true;
  end;
  RETLW: begin
    //Saca dirección en Pila
    if STKPTR = 0 then STKPTR := 7 else STKPTR := STKPTR - 1;
    PCH := hi(STACK[STKPTR]);  //solo debería haber 5 bits
    PCL := lo(STACK[STKPTR]);
    //Fija valor en W
    W := k_;
  end;
  RETURN: begin
    //Saca dirección en Pila
    if STKPTR = 0 then STKPTR := 7 else STKPTR := STKPTR - 1;
    PCH := hi(STACK[STKPTR]);  //solo debería haber 5 bits
    PCL := lo(STACK[STKPTR]);
  end;
  SLEEP: begin
  end;
  SUBLW: begin
    resInt := k_ - W;
    w := resInt and $FF;
    STATUS_Z := (resInt = 0);
    if resInt < 0 then STATUS_C := false   //negativo
    else STATUS_C := true;
    resInt := (k_ and $0F) - (W and $0F);
    if resInt < 0 then STATUS_DC := false   //negativo
    else STATUS_DC := true;
  end;
  XORLW: begin
    resByte := W xor k_;
    w := resByte;
    STATUS_Z := resByte <> 0;
  end;
  _Inval: begin
    MsjError := 'Invalid Opcode';
  end;
  end;
  //Incrementa contador
  PCL := PCL + 1;
end;
procedure TPIC16.Reset;
//Reinicia el dipsoitivo
var
  i: Integer;
begin
  PCL := 0;
  PCLATH := 0;
  PCH := 0;
  W := 0;
  ram[$03].Fvalue := %00011000;  //STATUS
  STKPTR := 0;   //Posición inicial del puntero de pila
  //Limpia solamente el valro inicial, no toca los otros campos
  for i:=0 to high(ram) do begin
    ram[i].Fvalue := $00;
  end;
end;

//funciones para la memoria RAM
function TPIC16.GetFreeBit(var offs, bnk, bit: byte): boolean;
{Devuelve una dirección libre de la memoria RAM (y el banco). Si encuentra espacio,
 devuelve TRUE.}
var
  maxRam: word;
  i: Integer;
begin
  Result := false;   //valor inicial

  maxRam := NumBanks * $80;  //posición máxima
  //realement debería explorar solo hasta la dirección impleemntada
  for i:=0 to maxRam-1 do begin
    if (ram[i].state = cs_impleGPR) and (ram[i].used <> 255) then begin
      //Esta dirección tiene al menos un bit libre
      offs := i;  //devuelve dirección
      //busca el bit libre
      if          (ram[i].used and %00000001) = 0 then begin
        bit:=0;
      end else if (ram[i].used and %00000010) = 0 then begin
        bit:=1
      end else if (ram[i].used and %00000100) = 0 then begin
        bit:=2
      end else if (ram[i].used and %00001000) = 0 then begin
        bit:=3
      end else if (ram[i].used and %00010000) = 0 then begin
        bit:=4
      end else if (ram[i].used and %00100000) = 0 then begin
        bit:=5
      end else if (ram[i].used and %01000000) = 0 then begin
        bit:=6
      end else if (ram[i].used and %10000000) = 0 then begin
        bit:=7
      end;
      ram[i].used := ram[i].used or (byte(1)<<bit); //marca bit usado
      //Notar que la posición de memoria puede estar mapeada a otro banco.
      Result := true;  //indica que encontró espacio
      exit;
    end;
  end;
end;
function TPIC16.GetFreeByte(out offs, bnk: byte): boolean;
{Devuelve una dirección libre de la memoria flash (y el banco). Si encuentra espacio,
 devuelve TRUE.}
begin
  Result := false;   //valor inicial
  if NumBanks = 1 then begin
    //solo 1 banco
    if bank0.GetFreeByte(offs) then begin
      bnk := 0;      //encontró en este banco
      Result := true;
      exit;
    end;
  end else if NumBanks = 2 then begin
    //solo 2 bancos
    if bank0.GetFreeByte(offs) then begin
      bnk := 0;      //encontró en este banco
      Result := true;
      exit;
    end else if bank1.GetFreeByte(offs) then begin
      bnk := 1;      //encontró en este banco
      Result := true;
      exit;
    end;
  end else if NumBanks = 3 then begin
    //3 bancos
    if bank0.GetFreeByte(offs) then begin
      bnk := 0;      //encontró en este banco
      Result := true;
      exit;
    end else if bank1.GetFreeByte(offs) then begin
      bnk := 1;      //encontró en este banco
      Result := true;
      exit;
    end else if bank2.GetFreeByte(offs) then begin
      bnk := 2;      //encontró en este banco
      Result := true;
      exit;
    end;
  end else begin
    //se asume 4 bancos
    if bank0.GetFreeByte(offs) then begin
      bnk := 0;      //encontró en este banco
      Result := true;
      exit;
    end else if bank1.GetFreeByte(offs) then begin
      bnk := 1;      //encontró en este banco
      Result := true;
      exit;
    end else if bank2.GetFreeByte(offs) then begin
      bnk := 2;      //encontró en este banco
      Result := true;
      exit;
    end else if bank3.GetFreeByte(offs) then begin
      bnk := 3;      //encontró en este banco
      Result := true;
      exit;
    end;
  end;
  {si llegó aquí es porque no encontró la memoria solicitada,
  al menos de ese tamaño}
end;
function TPIC16.GetFreeBytes(const size: integer; var offs, bnk: byte): boolean;
{Devuelve una dirección libre de la memoria flash (y el banco) para ubicar un bloque
 del tamaño indicado. Si encuentra espacio, devuelve TRUE.
 El tamaño se da en bytes, pero si el valor es negativo, se entiende que es en bits.}
begin
  //Busca en lso 4 bancos
  if (NumBanks>0) and bank0.GetFreeBytes(size, offs) then begin
    bnk := 0;      //encontró en este banco
    Result := true;
    exit;
  end;
  if (NumBanks>1) and bank1.GetFreeBytes(size, offs) then begin
    bnk := 1;      //encontró en este banco
    Result := true;
    exit;
  end;
  if (NumBanks>2) and bank2.GetFreeBytes(size, offs) then begin
    bnk := 2;      //encontró en este banco
    Result := true;
    exit;
  end;
  if (NumBanks>3) and bank3.GetFreeBytes(size, offs) then begin
    bnk := 3;      //encontró en este banco
    Result := true;
    exit;
  end;
  Result := false;   //valor inicial
  {si llegó aquí es porque no encontró la memoria solicitada,
  al menos de ese tamaño, o no hay bancos.}
end;
function TPIC16.TotalMemRAM: word;
{Devuelve el total de memoria RAM disponible}
begin
  Result := 0;
  case NumBanks of
  1: Result := bank0.TotalGPR;
  2: Result := bank0.TotalGPR + bank1.TotalGPR;
  3: Result := bank0.TotalGPR + bank1.TotalGPR + bank2.TotalGPR;
  4: Result := bank0.TotalGPR + bank1.TotalGPR + bank2.TotalGPR + bank3.TotalGPR;
  end;
end;
function TPIC16.UsedMemRAM: word;
{Devuelve el total de memoria RAM usada}
var
  i: Integer;
begin
  Result := 0;
  for i := 0 to word(NumBanks * $80) - 1 do begin
    if (ram[i].state = cs_impleGPR) and (ram[i].used <> 0) and (ram[i].mappedTo = nil) then begin
      Result := Result + 1;
    end;
  end;
end;
procedure TPIC16.ExploreUsed(rutExplorRAM: TRutExplorRAM);
{Genera un reporte de uso de RAM}
begin
  case NumBanks of
  1: begin
      bank0.ExploreUsed(rutExplorRAM);
     end;
  2: begin
      bank0.ExploreUsed(rutExplorRAM);
      bank1.ExploreUsed(rutExplorRAM);
    end;
  3: begin
      bank0.ExploreUsed(rutExplorRAM);
      bank1.ExploreUsed(rutExplorRAM);
      bank2.ExploreUsed(rutExplorRAM);
    end;
  4: begin
      bank0.ExploreUsed(rutExplorRAM);
      bank1.ExploreUsed(rutExplorRAM);
      bank2.ExploreUsed(rutExplorRAM);
      bank3.ExploreUsed(rutExplorRAM);
    end;
  end;
end;
function TPIC16.ValidRAMaddr(addr: word): boolean;
{Indica si la dirercción indicada es váldia dentro del hardware del PIC}
begin
  case NumBanks of
  1: begin
      if addr > $80 then exit(false);   //excede límite
  end;
  2: begin
      if addr > $100 then exit(false);   //excede límite
  end;
  3: begin
      if addr > $180 then exit(false);   //excede límite
  end;
  4: begin
      if addr > $200 then exit(false);   //excede límite
  end;
  end;
  exit(true);
end;
procedure TPIC16.ClearMemRAM;
{Limpia el contenido de la memoria}
var
  i: Integer;
begin
  for i:=0 to high(ram) do begin
    ram[i].Fvalue := $00;
    ram[i].used := 0;
    ram[i].name:='';
//    ram[i].state := cs_unimplem;  //por defecto se considera no implementado
    ram[i].bitname[0] := '';
    ram[i].bitname[1] := '';
    ram[i].bitname[2] := '';
    ram[i].bitname[3] := '';
    ram[i].bitname[4] := '';
    ram[i].bitname[5] := '';
    ram[i].bitname[6] := '';
    ram[i].bitname[7] := '';
  end;
end;
procedure TPIC16.DisableAllRAM;
{Inicia el estado de toda la memoria RAM física definida em el Modelo.
Solo debería usarse, para cuando se va a definir el hardware del dispositivo.}
var
  i: word;
begin
  for i:=0 to high(ram) do begin
    ram[i].addr := i;
    ram[i].state := cs_unimplem;
    ram[i].mappedTo := nil;
  end;
end;
procedure TPIC16.SetStatRAM(i1, i2: word; status0: TPIC16CellState);
{Inicia el campo State, de la memoria. Permite definir el estado real de la memoria RAM.
}
var
  i: Integer;
begin
  for i:=i1 to i2 do begin  //verifica 1 a 1, por seguridad
    ram[i].state := status0;
  end;
end;
procedure TPIC16.SetMappRAM(i1, i2: word; MappedTo: word);
{Inicia el campo State, de la memoria. Permite definir el estado real de la memoria RAM.
"MappedTo", indica el número de banco al cual está mapeada la sección de memoria indicada,
cuando se pone "status0" en "cs_mapToBnk". En los otrso estados no es útil.}
var
  i: Integer;
begin
  for i:= i1 to i2 do begin  //verifica 1 a 1, por seguridad
    ram[i].mappedTo := @ram[MappedTo];
    inc(MappedTo)
  end;
end;
function TPIC16.SetStatRAMCom(strDef: string): boolean;
{Define el estado de la memoria RAM, usando una cadena de definición.
La cadena de definición, tiene el formato:
<comando 1>, <comando 2>, ...
Cada comando, tiene el formato:
<dirIni>-<dirFin>:<estado de memoria>
Un ejemplo de cadena de definición, es:
   '000-01F:SFR, 020-07F:GPR'
Si hay error, devuelve FALSE, y el mensaje de error en MsjError.
}
var
  coms: TStringList;
  add1, add2: longint;
  state: TPIC16CellState;
  staMem, com, str: String;
begin
  Result := true;
  coms:= TStringList.Create;
  try
    coms.Delimiter := ',';
    coms.DelimitedText := strDef;
    for str in coms do begin
      com := UpCase(trim(str));
      if com='' then continue;
      if length(com)<>11 then begin
        MsjError := 'Memory definition syntax error: Bad string size.';
        exit(false);
      end;
      if com[4] <> '-' then begin
        MsjError := 'Memory definition syntax error: Expected "-".';
        exit(false);
      end;
      if com[8] <> ':' then begin
        MsjError := 'Memory definition syntax error: Expected ":".';
        exit(false);
      end;
      //Debe tener el formato pedido
      if not TryStrToInt('$'+copy(com,1,3), add1) then begin
        MsjError := 'Memory definition syntax error: Wrong address.';
        exit(false);
      end;
      if not TryStrToInt('$'+copy(com,5,3), add2) then begin
        MsjError := 'Memory definition syntax error: Wrong address.';
        exit(false);
      end;
      staMem := copy(com, 9, 3);
      case staMem of
      'SFR': state := cs_impleSFR;
      'GPR': state := cs_impleGPR;
      'NIM': state := cs_unimplem;
      else
        MsjError := 'Memory definition syntax error: Expected SFR or GPR';
        exit(false);
      end;
      //Ya se tienen los parámetros, para definir la memoria
      SetStatRAM(add1, add2, state);
    end;
  finally
    coms.Destroy;
  end;
end;
function TPIC16.SetMappRAMCom(strDef: string): boolean;
{Define memoria RAM mapeeada, en otra dirección.
La cadena de definición, tiene el formato:
<comando 1>, <comando 2>, ...
Cada comando, tiene el formato:
<dirIni>-<dirFin>:<banco al que está mapeado>
Un ejemplo de cadena de definición, es:
   '000-01F:bnk0, 020-07F:bnk1'
Si hay error, devuelve FALSE, y el mensaje de error en MsjError.
}
var
  coms: TStringList;
  add1, add2, addTar: longint;
  bnkTarStr, com, str: String;
  bnkTar: byte;
begin
  Result := true;
  coms:= TStringList.Create;
  try
    coms.Delimiter := ',';
    coms.DelimitedText := strDef;
    for str in coms do begin
      com := UpCase(trim(str));
      if com='' then continue;
      if length(com)<>12 then begin
        MsjError := 'Memory mapping syntax error: Bad string size.';
        exit(false);
      end;
      if com[4] <> '-' then begin
        MsjError := 'Memory mapping syntax error: Expected "-".';
        exit(false);
      end;
      if com[8] <> ':' then begin
        MsjError := 'Memory mapping syntax error: Expected ":".';
        exit(false);
      end;
      //Debe tener el formato pedido
//      debugln(com);
      if not TryStrToInt('$'+copy(com,1,3), add1) then begin
        MsjError := 'Memory mapping syntax error: Wrong address.';
        exit(false);
      end;
      if not TryStrToInt('$'+copy(com,5,3), add2) then begin
        MsjError := 'Memory mapping syntax error: Wrong address.';
        exit(false);
      end;
      bnkTarStr := copy(com, 9, 4);
      if copy(bnkTarStr,1,3)<>'BNK' then begin
        MsjError := 'Memory mapping syntax error: Expected "bnk0", ...';
        exit(false);
      end;
      if not (bnkTarStr[4] in ['0'..'3']) then begin
        MsjError := 'Memory mapping syntax error: Expected "bnk0", ...';
        exit(false);
      end;
      bnkTar := ord(bnkTarStr[4])-48;  //convierte a número
      //Ya se tienen los parámetros, para definir el mapeo
      case bnkTar of
      0: addTar := (add1 and $7F);
      1: addTar := (add1 and $7F) or $080;
      2: addTar := (add1 and $7F) or $100;
      3: addTar := (add1 and $7F) or $180;
      end;
      SetMappRAM(add1, add2, addTar);
    end;
  finally
    coms.Destroy;
  end;
end;

function TPIC16.BankToAbsRAM(const offset, bank: byte): word;
{Convierte una dirección y banco a una dirección absoluta}
begin
  Result := 0;
  case bank of
  0: Result := offset;
  1: Result := $80 +offset;
  2: Result := $100+offset;
  3: Result := $180+offset;
  end;
end;
procedure TPIC16.AbsToBankRAM(const AbsAddr: word; var offset, bank: byte);
{Convierte dirección absoluta a dirección en bancos}
begin
   offset := AbsAddr and %01111111;
   bank :=  AbsAddr >> 7;
end;
function TPIC16.NameRAM(const addr: word; const bnk: byte): string;
{Devuelve el nombre de una celda de la memoria RAM.}
begin
  Result := ram[BankToAbsRAM(addr, bnk)].name;
end;
function TPIC16.NameRAMbit(const addr: word; const bnk, bit: byte): string;
begin
  Result := ram[BankToAbsRAM(addr, bnk)].bitname[bit];
end;
procedure TPIC16.SetNameRAM(const addr: word; const bnk: byte; const nam: string
  );
{Escribe en el campo "name" de la RAM en la psoición indicada}
begin
   ram[BankToAbsRAM(addr, bnk)].name:=nam;
end;
procedure TPIC16.AddNameRAM(const addr: word; const bnk: byte; const nam: string
  );
{Escribe en el campo "name" de la RAM en la psoición indicada. Si ya existía un nombre,
lo argega después de una coma.}
begin
  if ram[BankToAbsRAM(addr, bnk)].name = '' then begin
    ram[BankToAbsRAM(addr, bnk)].name:=nam;
  end else begin
    ram[BankToAbsRAM(addr, bnk)].name+=','+nam;
  end;
end;
procedure TPIC16.SetNameRAMbit(const addr: word; const bnk, bit: byte;
  const nam: string);
begin
  if (bit>7) then exit;
  ram[BankToAbsRAM(addr, bnk)].bitname[bit] := nam;
end;

//funciones para la memoria Flash
function TPIC16.UsedMemFlash: word;
begin
  Result := 0;
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
    flash[i].topLabel   := '';
    flash[i].sideComment:= '';
    flash[i].topComment := '';
  end;
end;
procedure TPIC16.GenHex(hexFile: string; ConfigWord: integer = -1);
var
  cfg, tmp: String;
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
  3: begin
      GenHexData(page0);
      GenHexData(page1);
      GenHexData(page2);
  end;
  4: begin
      GenHexData(page0);
      GenHexData(page1);
      GenHexData(page2);
      GenHexData(page3);
  end;
  end;
  if ConfigWord<>-1 then begin
    //Se pide generar bits de configuración
    {Los bits de configuración para la serie 16F, se almacenan en:
Config: 0x2007 (0x400E in the HEX file)
EEPROM: 0x2100 (0x4200 in the HEX file) }
    cfg := IntToHex(ConfigWord and $FFFF, 4);
    tmp +=copy(cfg,3,2) + copy(cfg,1,2);  //se graba con los bytes invertidos
    GenHexData($2007, tmp);
  end;
  GenHexEOF;  //fin de archivo
  GenHexComm(self.Model);   //comentario
  hexLines.SaveToFile(hexFile);  //genera archivo
end;
procedure TPIC16.ShowCode(lOut: TStrings; pag: TFlashPage; incAdrr, incCom, incVarNam: boolean);
{Muestra el código desensamblado de una página}
var
  i: Word;
  val: Word;
  comLin: string;   //comentario de línea
  comLat: string;   //comentario lateral
  lin , lblLin: string;
begin
  if pag.nUsed = 0 then exit; //no hay datos
  for i:=pag.minUsed to pag.maxUsed do begin
    //Lee comentarios y etiqueta
    lblLin := pag.mem[i].topLabel;
    comLat := pag.mem[i].sideComment;
    comLin := pag.mem[i].topComment;
    //Decodifica instrucción
    val := pag.mem[i].value;
    Decode(val);   //decodifica instrucción
    //Escribe etiqueta al inicio de línea
    if lblLin<>'' then lOut.Add(lblLin+':');
    //Escribe comentario al inicio de línea
    if incCom and (comLin<>'') then  begin
      lOut.Add(comLin);
    end;
    //Escribe línea
    lin := Disassembler(incVarNam);
    if incAdrr then  begin //Incluye dirección física
      lin := '0x'+IntToHex(i,3) + ' ' + lin;
    end;
    if incCom then begin  //Incluye comentario lateral
      lin := lin  + ' ' + comLat;
    end;
    lOut.Add('    ' + lin);
  end;
end;
procedure TPIC16.DumpCode(l: TStrings; incAdrr, incCom, incVarNam: boolean);
{Desensambla las instrucciones grabadas en el PIC.
 Se debe llamar despues de llamar a GenHex(), para que se actualicen las variables}
begin
  case NumPages of
  1: begin
      ShowCode(l, page0, incAdrr, incCom, incVarNam);
  end;
  2:begin
      ShowCode(l, page0, incAdrr, incCom, incVarNam);
      ShowCode(l, page1, incAdrr, incCom, incVarNam);
  end;
  3:begin
      ShowCode(l, page0, incAdrr, incCom, incVarNam);
      ShowCode(l, page1, incAdrr, incCom, incVarNam);
      ShowCode(l, page2, incAdrr, incCom, incVarNam);
  end;
  4:begin
      ShowCode(l, page0, incAdrr, incCom, incVarNam);
      ShowCode(l, page1, incAdrr, incCom, incVarNam);
      ShowCode(l, page2, incAdrr, incCom, incVarNam);
      ShowCode(l, page3, incAdrr, incCom, incVarNam);
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
  MaxFlash := PIC_PAGE_SIZE;  //En algunos casos, puede ser menor al tamaño de una página
  bank0.Init(0, $000, @ram);
  bank1.Init(1, $080, @ram);
  bank2.Init(2, $100, @ram);
  bank3.Init(3, $180, @ram);
  //inicia una configuración común
  ClearMemRAM;
  SetStatRAM($020, $04F, cs_impleGPR);

  page0.Init($0000          , @flash);
  page1.Init(1*PIC_PAGE_SIZE, @flash);
  page2.Init(2*PIC_PAGE_SIZE, @flash);
  page3.Init(3*PIC_PAGE_SIZE, @flash);

  //estado inicial
  iFlash := 0;   //posición de inicio
  ClearMemFlash;
end;
destructor TPIC16.Destroy;
begin
  hexLines.Destroy;
  inherited Destroy;
end;

procedure InitTables;
begin
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
  {Los valorees para la sintaxis significan:
  f->dirección de un registro en RAM (0..127)
  d->destino (W o F)
  b->número de bit (0..7)
  a->dirección destino (0..$7FF)
  k->literal byte (0..255)
  }
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
  PIC16InstSyntax[CALL  ] := 'a';
  PIC16InstSyntax[CLRWDT] := '';
  PIC16InstSyntax[GOTO_ ] := 'a';
  PIC16InstSyntax[IORLW ] := 'k';
  PIC16InstSyntax[MOVLW ] := 'k';
  PIC16InstSyntax[RETFIE] := '';
  PIC16InstSyntax[RETLW ] := 'k';
  PIC16InstSyntax[RETURN] := '';
  PIC16InstSyntax[SLEEP ] := '';
  PIC16InstSyntax[SUBLW ] := 'k';
  PIC16InstSyntax[XORLW ] := 'k';
  PIC16InstSyntax[_Inval] := '<???>';
end;
initialization
  InitTables;
end.

