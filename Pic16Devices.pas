unit Pic16Devices;
{$mode objfpc}{$H+}
interface
uses
  Classes, SysUtils , Pic16Utils;

function GetHardwareInfo(pic: TPIC16; model: string): boolean;

implementation

procedure SetRAM16F70_71(pic: TPIC16);
begin
   pic.bank0.InitStateMem($00, $7F, cs_enabled);
   pic.bank1.InitStateMem($00, $3F, cs_enabled);
   pic.bank1.InitStateMem($60, $6F, cs_disabled);
   pic.bank1.InitStateMem($70, $7F, cs_mapToBnk);
   pic.bank1.BankMapped:=@pic.bank0;

   pic.bank2.InitStateMem($00, $0F, cs_enabled);
   pic.bank2.InitStateMem($10, $1F, cs_disabled);
   pic.bank2.InitStateMem($20, $6F, cs_enabled);
   pic.bank2.InitStateMem($70, $7F, cs_mapToBnk);
   pic.bank2.BankMapped:=@pic.bank0;

   pic.bank2.InitStateMem($00, $0F, cs_enabled);
   pic.bank2.InitStateMem($10, $1F, cs_disabled);
   pic.bank2.InitStateMem($20, $3F, cs_enabled);
   pic.bank2.InitStateMem($40, $6F, cs_disabled);
   pic.bank2.InitStateMem($70, $7F, cs_mapToBnk);
   pic.bank2.BankMapped:=@pic.bank0;
end;
procedure SetRAM16F73_74(pic: TPIC16);
begin
   pic.bank0.InitStateMem($00, $7F, cs_enabled);
   pic.bank1.InitStateMem($00, $7F, cs_enabled);
   pic.bank2.InitStateMem($00, $0F, cs_enabled);
   pic.bank2.InitStateMem($10, $1F, cs_disabled);
   pic.bank2.InitStateMem($20, $7F, cs_mapToBnk);
   pic.bank2.BankMapped:=@pic.bank0;
   pic.bank3.InitStateMem($00, $0F, cs_enabled);
   pic.bank3.InitStateMem($10, $1F, cs_disabled);
   pic.bank3.InitStateMem($20, $7F, cs_mapToBnk);
   pic.bank3.BankMapped:=@pic.bank1;
end;
procedure SetRAM16F76_77(pic: TPIC16);
begin
   pic.bank0.InitStateMem($00, $7F, cs_enabled);
   pic.bank1.InitStateMem($00, $6F, cs_enabled);
   pic.bank1.InitStateMem($70, $7F, cs_mapToBnk);
   pic.bank1.BankMapped:=@pic.bank0;
   pic.bank1.InitStateMem($00, $6F, cs_enabled);
   pic.bank1.InitStateMem($70, $7F, cs_mapToBnk);
   pic.bank1.BankMapped:=@pic.bank0;
   pic.bank2.InitStateMem($00, $6F, cs_enabled);
   pic.bank2.InitStateMem($70, $7F, cs_mapToBnk);
   pic.bank2.BankMapped:=@pic.bank0;
   pic.bank2.GPRStart:=$10;
   pic.bank3.InitStateMem($00, $6F, cs_enabled);
   pic.bank3.InitStateMem($70, $7F, cs_mapToBnk);
   pic.bank3.BankMapped:=@pic.bank0;
   pic.bank3.GPRStart:=$10;
end;

function GetHardwareInfo(pic: TPIC16; model: string): boolean;
{Obtiene información parra un modelo de PIC en especial. Si no lo encuentra, devuelve
 FALSE}
begin
   Result := true;
   case Upcase(model) of
   'PIC16F83',
   'PIC16CR83': begin
     pic.Npins := 18;
     pic.NumBanks:=2;    //los bancos 2 y 3 están reflejados
     pic.NumPages:=2;
     pic.GPRStart:=$0C;
     pic.bank0.InitStateMem($0C, $2F, cs_enabled);
     pic.bank0.InitStateMem($30, $7F, cs_disabled);
     pic.bank1.InitStateMem($00, $0B, cs_enabled);
     pic.bank1.InitStateMem($0C, $2F, cs_mapToBnk);
     pic.bank1.InitStateMem($30, $7F, cs_disabled);
     pic.bank1.BankMapped:=@pic.bank0;
   end;
   'PIC16F84',
   'PIC16CR84',
   'PIC16F84A': begin
     pic.Npins := 18;
     pic.NumBanks:=2;    //los bancos 2 y 3 están reflejados
     pic.NumPages:=2;
     pic.GPRStart:=$0C;
     pic.bank0.InitStateMem($00, $4F, cs_enabled);
     pic.bank0.InitStateMem($50, $7F, cs_disabled);
     pic.bank1.InitStateMem($00, $0B, cs_enabled);
     pic.bank1.InitStateMem($0C, $4F, cs_mapToBnk);
     pic.bank1.InitStateMem($50, $7F, cs_disabled);
     pic.bank1.BankMapped:=@pic.bank0;
   end;
   'PIC16F870': begin
     pic.Npins := 28;
     pic.NumBanks := 4;  //tiene un bloque sin usar en el banco 1 y reflejado los bancos 2 y 3
     pic.NumPages:=1;
     SetRAM16F70_71(pic);
   end;
   'PIC16F871': begin
     pic.Npins := 40;
     pic.NumBanks := 4;  //tiene un bloque sin usar en el banco 1 y reflejado los bancos 2 y 3
     pic.NumPages:=1;
     SetRAM16F70_71(pic);
   end;
   'PIC16F872': begin
     pic.Npins := 28;
     pic.NumBanks := 2;  //tiene un bloque sin usar en el banco 1 y reflejado los bancos 2 y 3
     pic.NumPages:=1;
     SetRAM16F70_71(pic);
   end;
   'PIC16F873',
   'PIC16F873A': begin
     pic.Npins := 28;
     pic.NumBanks:=4;    //los bancos 2 y 3 están reflejados
     pic.NumPages:=2;
     pic.GPRStart:=$20;
     SetRAM16F73_74(pic);
   end;
   'PIC16F874',
   'PIC16F874A': begin
     pic.Npins := 40;
     pic.NumBanks:=4;    //los bancos 2 y 3 están reflejados
     pic.NumPages:=2;
     pic.GPRStart:=$20;
     SetRAM16F73_74(pic);
   end;
   'PIC16F876',
   'PIC16F876A': begin
     pic.Npins := 28;
     pic.NumBanks:=4;
     pic.NumPages:=4;
     pic.GPRStart:=$20;   //es solo el valor de los bancoa 0 y 1
     SetRAM16F76_77(pic);
   end;
   'PIC16F877',
   'PIC16F877A',
   'PIC16F887': begin
     pic.Npins := 40;
     pic.NumBanks:=4;
     pic.NumPages:=4;
     pic.GPRStart:=$20;   //es solo el valor de los bancoa 0 y 1
     SetRAM16F76_77(pic);
   end;
   else
     exit(false);
   end;
end;

end.

