{PICCore

Contains basic definitions applicable to all PIC microcontroller Cores
                                         Created by Tito Hinostroza   28/04/2018
}
unit PicCore;
{$mode objfpc}{$H+}
interface
uses
  Classes, SysUtils, LCLProc;
type
  TPICCellState = (
     cs_impleSFR,   //Special function Registers. Can be used.
     cs_impleGPR,   //General Purpose Registers. Can be used.
     cs_unimplem    //Not implemented.
  );

  { TPICRAMBank }
  {Represent a RAM memory bank of the PIC. }
  TPICRAMBank = object
    AddrStart : word;     //Address start of RAM bank
    AddrEnd   : word;     //Address end of RAM bank
  end;

  { TPICRamCell }
  {Modela a una dirección lógica de la memoria RAM. Se ha taratdo de hacer una
  definición eficiente de esta estructura para facilitar la implementación de
  simuladores en tiempo real. Podemos usar un tamaño mediano para este registro,
  porque no esperamos tener muchas celdas de RAM (<1K).}
  TPICRamCellPtr = ^TPICRamCell;
  TPICRamCell = object
  private
    Fvalue  : byte;     //value of the memory
    Fused   : byte;     //Bitmap. Indicates the used bits ($00->all free; $ff->all bits used.)
    Fimplem : byte;     //Bitmap. Indicates the implemented bits
    function Getused: byte;
    function Getvalue: byte;
    procedure Setused(AValue: byte);
    procedure Setvalue(AValue: byte);
  public
    addr   : word;     //dirección física de memoria, en donde está la celda.
    name   : string;   //Name of the register (or variable)
    bitname: array[0..7] of string;  //Name of the bits.
    shared : byte;     //Used to share this register
    state  : TPICCellState;  //status of the cell
    mappedTo: TPICRamCellPtr;  //Indica que está mapeado a otra celda, de otra dirección
    property value: byte read Getvalue write Setvalue;
    property dvalue: byte read Fvalue write Fvalue;   //Direct access to "Fvalue".
    property dimplem: byte read Fimplem write Fimplem;  //Direct access to "Fimplem".
    property used: byte read Getused write Setused;
    function AvailGPR: boolean;
  end;

  TPICFlashCell = record
    value     : word;     //Value of the memory (OpCode)
    used      : boolean;  //Indicates if have been written
    curBnk    : byte;     {Current RAM bank where it's supposed this Opcode works.
                           The current bank can be different after executing this OpCode.}
    //Information of position in source code. Used for debug
    rowSrc    : word;     //Row number
    colSrc    : word;     //Column number
    idFile    : SmallInt; //Index to a file. No load the name to save space.
    {Estos campos de cadena ocupan bastante espacio, aún cuado están en NULL. Si se
    quisiera optimizar el uso de RAM, se podría pensar en codificar, varios campos en
    una sola cadena.}
    topLabel   : string;  //Label on the top of the cell.
    topComment : string;  //Comment on the top of the cell.
    sideComment: string;  //Right comment to code
    //Campos para deputación
    breakPnt  : boolean;  //Indicates if this cell have a Breakpoint
    {Be careful on the size of this record, because it's going to be multiplied by 8192}
  end;

  { TPICFlashPage }
  {Represent a PIC memory page.}
  TPICFlashPage = object
    AddrStart : word;     //Address start of FLASH bank
    AddrEnd   : word;     //Address end of FLASH bank
  end;

  //Modelo para un pin físico del PIC
  TPICPinType = (
    pptVcc,    //Alimentación
    pptGND,    //Tierra
    pptControl,//Pin de control
    pptPort,   //Puerto Entrada/Salida
    pptUnused  //Pin no usado
  );

  { TPICPin }
  //Modela a un pin del PIC
  TPICPin = object
    nam: string;      //Eqtiueta o nombre
    typ: TPICPinType; //Tipo de pin
    add: word;        //Dirección en RAM
    bit: byte;        //Bit en RAM
    function GetLabel: string;
  end;

  { TPicCore }
  {Abcestor of all 8 bits PIC cores}
  TPicCore = class
  public   //General fields
    Model    : string;    //modelo de PIC
    Npins    : byte;      //número de pines
    frequen  : integer;   //frecuencia del reloj
    MaxFreq  : integer;   //máxima frecuencia del reloj
    //Propiedades que definen la arquitectura del PIC destino.
    NumBanks: byte;      //Número de bancos de RAM.
    NumPages: byte;      //Número de páginas de memoria Flash.
  protected  //Generation of HEX files
    hexLines : TStringList;  //Uusado para crear archivo *.hex
    function HexChecksum(const lin: string): string;
    procedure GenHexComm(comment: string);
    procedure GenHexExAdd(Data: word);
    procedure GenHexData(Address: word; Data: string);
    procedure GenHexEOF;
  public  //Initialization
    constructor Create; virtual;
    destructor Destroy; override;
  end;

implementation

{ TPicCore }

constructor TPicCore.Create;
begin
  hexLines := TStringList.Create;
  frequen := 4000000;    //4MHz
end;

destructor TPicCore.Destroy;
begin
  hexLines.Destroy;
  inherited Destroy;
end;

{ TPICRamCell }
function TPICRamCell.Getused: byte;
begin
  if mappedTo = nil then begin
    //Celda independiente
    Result := Fused;
  end else begin
    //Celda reflejada. Leemos la disponibilidad, de la celda origen.
    Result := mappedTo^.used;
  end;
end;
procedure TPICRamCell.Setused(AValue: byte);
begin
  if mappedTo = nil then begin
    //Celda independiente
    Fused := AValue;
  end else begin
    //Celda reflejada. Escribimos la disponibilidad, en la celda origen.
    mappedTo^.used := AValue;
  end;
end;
function TPICRamCell.Getvalue: byte;
begin
  if mappedTo = nil then begin
    //Celda independiente
    Result := Fvalue;
  end else begin
    //Celda reflejada. Leemos la disponibilidad, de la celda origen.
    Result := mappedTo^.Fvalue;
  end;
end;
procedure TPICRamCell.Setvalue(AValue: byte);
begin
  if mappedTo = nil then begin
    //Celda independiente
    Fvalue := AValue;
  end else begin
    //Celda reflejada. Escribimos la disponibilidad, en la celda origen.
    mappedTo^.Fvalue:= AValue;
  end;
end;
function TPICRamCell.AvailGPR: boolean;
{Indica si el registro es una dirección disponible en la memoria RAM.}
begin
  Result := (state = cs_impleGPR) and (mappedTo = nil);
end;

{ TPICPin }
function TPICPin.GetLabel: string;
{Devuelve una etiqueta para el pin}
begin
  case typ of
  pptUnused: Result := 'NC';
  else
    Result := nam;
  end;
end;

//Creación de archivo *.hex
function TPicCore.HexChecksum(const lin:string): string;
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
procedure TPicCore.GenHexComm(comment: string);
//Agrega una línea de comentario al archivo *.hex
begin
  hexLines.Add(';'+comment);
end;
procedure TPicCore.GenHexExAdd(Data: word);
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
procedure TPicCore.GenHexData(Address: word; Data: string);
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
procedure TPicCore.GenHexEOF;
//Agrega una línea de Extended Address al archivo *.hex
begin
  hexLines.Add(':00000001FF');
end;

initialization
end.

