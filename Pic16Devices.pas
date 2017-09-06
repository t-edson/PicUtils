unit Pic16Devices;
{$mode objfpc}{$H+}
interface
uses
  Classes, SysUtils , Pic16Utils;

procedure GetSupportedDevices(list: TStrings);
function GetHardwareInfo(pic: TPIC16; model: string): boolean;

implementation

procedure SetRAM16F870_1_2(pic: TPIC16);
begin
   pic.SetStatRAMCom('');
   pic.SetStatRAM($000, $01F, cs_impleSFR);
   pic.SetStatRAM($020, $07F, cs_impleGPR);

   pic.SetStatRAM($080, $09F, cs_impleSFR);
   pic.SetStatRAM($0A0, $0BF, cs_impleGPR);

   pic.SetStatRAM($100, $10F, cs_impleSFR);
   pic.SetStatRAM($120, $16F, cs_impleGPR);

   pic.SetStatRAM($180, $18F, cs_impleSFR);
   pic.SetStatRAM($1A0, $1BF, cs_impleGPR);
   //Direcciones mapeadas
   pic.SetMappRAMCom('0F0-1FF:bnk0');
   pic.SetMappRAMCom('120-17F:bnk0');
   pic.SetMappRAMCom('1A0-1BF:bnk1, 1F0-1FF:bnk0');
end;

procedure GetSupportedDevices(list: TStrings);
{Devuelve la lista de dispositivos soportados. Esta lista debe ser la misma que hay en
 GetHardwareInfo. }
begin
   list.Add('PIC12F629');
   list.Add('PIC12F675');

   list.Add('PIC16C63');
   list.Add('PIC16CR63');
   list.Add('PIC16C65');
   list.Add('PIC16C65A');
   list.Add('PIC16CR65');

   list.Add('PIC16F72');
   list.Add('PIC16F83');
   list.Add('PIC16CR83');
   list.Add('PIC16F84');
   list.Add('PIC16CR84');
   list.Add('PIC16F84A');
   list.Add('PIC16F870');
   list.Add('PIC16F871');
   list.Add('PIC16F872');
   list.Add('PIC16F873');
   list.Add('PIC16F873A');
   list.Add('PIC16F874');
   list.Add('PIC16F874A');
   list.Add('PIC16F876');
   list.Add('PIC16F876A');
   list.Add('PIC16F877');
   list.Add('PIC16F877A');
   list.Add('PIC16F887');
   list.Add('PIC16F627A');
   list.Add('PIC16F628A');
   list.Add('PIC16F648A');
end;

function GetHardwareInfo(pic: TPIC16; model: string): boolean;
{Obtiene información parra un modelo de PIC en especial. Si no lo encuentra, devuelve
 FALSE}
begin
   Result := true;
   pic.MaxFlash := 0;  //inicia valor
   pic.MaxFreq := 0;
   pic.DisableAllRAM;
   pic.Model := model;
   case Upcase(model) of
   'PIC12F629',
   'PIC12F675': begin
     pic.MaxFreq:=20000000;
     pic.Npins := 8;
     pic.NumBanks:=2;
     pic.NumPages:=1; pic.MaxFlash:=1024;  //banco 0 implementado parcialmente
     pic.GPRStart:=$20;
     pic.SetStatRAMCom('000-01F:SFR, 020-05F:GPR');
     pic.SetStatRAMCom('080-09F:SFR, 0A0-0DF:GPR');
     pic.SetMappRAMCom('0A0-0DF:bnk0');
   end;
   'PIC16C63',
   'PIC16CR63':begin
     pic.MaxFreq:=20000000;
     pic.Npins := 28;
     pic.NumBanks:=2;
     pic.NumPages:=2; pic.MaxFlash:=4096;
     pic.GPRStart:=$20;
     pic.SetStatRAMCom('000-01F:SFR, 020-07F:GPR');
     pic.SetStatRAMCom('080-09F:SFR, 0A0-0FF:GPR');
   end;
   'PIC16C65',
   'PIC16C65A',
   'PIC16CR65': begin
     pic.MaxFreq:=20000000;
     pic.Npins := 40;
     pic.NumBanks:=2;
     pic.NumPages:=2; pic.MaxFlash:=4096;
     pic.GPRStart:=$20;
     pic.SetStatRAMCom('000-01F:SFR, 020-07F:GPR');
     pic.SetStatRAMCom('080-09F:SFR, 0A0-0FF:GPR');
   end;
   'PIC16F72': begin
     pic.MaxFreq:=20000000;
     pic.Npins := 28;
     pic.NumBanks:=4;    //los bancos 2 y 3 están reflejados
     pic.NumPages:=1; pic.MaxFlash:=2048;
     pic.GPRStart:=$20;
     pic.SetStatRAM($020, $07F, cs_impleGPR);
     pic.SetStatRAM($0A0, $0BF, cs_impleGPR);
     pic.SetMappRAMCom('0C0-0FF:bnk0');
     pic.SetMappRAMCom('120-17F:bnk0');
     pic.SetMappRAMCom('1A0-1BF:bnk1, 1C0-1FF:bnk0');
   end;
   'PIC16F83',
   'PIC16CR83': begin
     pic.MaxFreq:=10000000;
     pic.Npins := 18;
     pic.NumBanks:=2;    //los bancos 2 y 3 están reflejados
     pic.NumPages:=1; pic.MaxFlash:=512;  //banco 0 implementado parcialmente
     pic.GPRStart:=$0C;
     pic.SetStatRAM($0C, $2F, cs_impleGPR);
     pic.SetMappRAMCom('08C-0AF:bnk0');
   end;
   'PIC16F84',
   'PIC16CR84': begin
     pic.MaxFreq:=10000000;
     pic.Npins := 18;
     pic.NumBanks:=2;
     pic.NumPages:=1; pic.MaxFlash:=1024;  //banco 0 implementado parcialmente
     pic.GPRStart:=$0C;
     pic.SetStatRAMCom('000-00B:SFR, 00C-04F:GPR');
     pic.SetStatRAMCom('080-08B:SFR, 08C-0CF:GPR');
     pic.SetMappRAMCom('08C-0CF:bnk0');
   end;
   'PIC16F84A': begin
     pic.MaxFreq:=20000000;
     pic.Npins := 18;
     pic.NumBanks:=2;    //los bancos 2 y 3 están reflejados
     pic.NumPages:=1; pic.MaxFlash:=1024;  //banco 0 implementado parcialmente
     pic.GPRStart:=$0C;
     pic.SetMappRAMCom('080-080:bnk0');
     pic.SetMappRAMCom('082-084:bnk0');
     pic.SetMappRAMCom('08A-08B:bnk0');
     pic.SetStatRAMCom('000-00B:SFR, 00C-04F:GPR');
     pic.SetStatRAMCom('080-08B:SFR, 08C-0CF:GPR');
     pic.SetMappRAMCom('08C-0CF:bnk0');
   end;
   'PIC16F870': begin
     pic.MaxFreq:=20000000;
     pic.Npins := 28;
     pic.NumBanks := 4;  //tiene un bloque sin usar en el banco 1 y reflejado los bancos 2 y 3
     pic.NumPages:=1; pic.MaxFlash:=2048;
     SetRAM16F870_1_2(pic);
   end;
   'PIC16F871': begin
     pic.MaxFreq:=20000000;
     pic.Npins := 40;
     pic.NumBanks := 4;  //tiene un bloque sin usar en el banco 1 y reflejado los bancos 2 y 3
     pic.NumPages:=1; pic.MaxFlash:=2048;
     SetRAM16F870_1_2(pic);
   end;
   'PIC16F872': begin
     pic.MaxFreq:=20000000;
     pic.Npins := 28;
     pic.NumBanks := 2;  //tiene un bloque sin usar en el banco 1 y reflejado los bancos 2 y 3
     pic.NumPages:=1; pic.MaxFlash:=2048;
     SetRAM16F870_1_2(pic);
   end;
   'PIC16F873',
   'PIC16F873A': begin
     pic.MaxFreq:=20000000;
     pic.Npins := 28;
     pic.NumBanks:=4;    //los bancos 2 y 3 están reflejados
     pic.NumPages:=2; pic.MaxFlash:=4096;
     pic.GPRStart:=$20;
     pic.SetStatRAMCom('000-01F:SFR, 020-07F:GPR');
     pic.SetStatRAMCom('080-09F:SFR, 0A0-0FF:GPR');
     pic.SetStatRAMCom('100-10F:SFR, 120-17F:GPR');
     pic.SetStatRAMCom('180-18F:SFR, 1A0-1FF:GPR');
     pic.SetMappRAMCom('120-17F:bnk0, 1A0-1FF:bnk1');
   end;
   'PIC16F874',
   'PIC16F874A': begin
     pic.MaxFreq:=20000000;
     pic.Npins := 40;
     pic.NumBanks:=4;    //los bancos 2 y 3 están reflejados
     pic.NumPages:=2; pic.MaxFlash:=4096;
     pic.GPRStart:=$20;
     pic.SetStatRAMCom('000-01F:SFR, 020-07F:GPR');
     pic.SetStatRAMCom('080-09F:SFR, 0A0-0FF:GPR');
     pic.SetStatRAMCom('100-10F:SFR, 120-17F:GPR');
     pic.SetStatRAMCom('180-18F:SFR, 1A0-1FF:GPR');
     pic.SetMappRAMCom('120-17F:bnk0, 1A0-1FF:bnk1');
   end;
   'PIC16F876',
   'PIC16F876A': begin
     pic.MaxFreq:=20000000;
     pic.Npins := 28;
     pic.NumBanks:=4;
     pic.NumPages:=4; pic.MaxFlash:=8192;
     pic.GPRStart:=$20;   //es solo el valor de los bancoa 0 y 1
     pic.bank2.GPRStart:=$10;
     pic.bank3.GPRStart:=$10;
     pic.SetStatRAMCom('000-01F:SFR, 020-07F:GPR');
     pic.SetStatRAMCom('080-09F:SFR, 0A0-0FF:GPR');
     pic.SetStatRAMCom('100-10F:SFR, 110-17F:GPR');
     pic.SetStatRAMCom('180-18F:SFR, 190-1FF:GPR');
     pic.SetMappRAMCom('0F0-0FF:bnk0, 170-17F:bnk0, 1F0-1FF:bnk0');
   end;
   'PIC16F877',
   'PIC16F877A',
   'PIC16F887': begin
     pic.MaxFreq:=20000000;
     pic.Npins := 40;
     pic.NumBanks:=4;
     pic.NumPages:=4; pic.MaxFlash:=8192;
     pic.GPRStart:=$20;   //es solo el valor de los bancoa 0 y 1
     pic.bank2.GPRStart:=$10;
     pic.bank3.GPRStart:=$10;
     pic.SetStatRAMCom('000-01F:SFR, 020-07F:GPR');
     pic.SetStatRAMCom('080-09F:SFR, 0A0-0FF:GPR');
     pic.SetStatRAMCom('100-10F:SFR, 110-17F:GPR');
     pic.SetStatRAMCom('180-18F:SFR, 190-1FF:GPR');
     pic.SetMappRAMCom('0F0-0FF:bnk0, 170-17F:bnk0, 1F0-1FF:bnk0');
   end;
   'PIC16F627A': begin
     pic.MaxFreq:=20000000;
     pic.Npins := 16;
     pic.NumBanks:=4;
     pic.NumPages:=1; pic.MaxFlash:=1024;  //banco 0 implementado parcialmente
     pic.GPRStart:=$20;

     pic.SetStatRAM($020, $07F, cs_impleGPR);
     pic.SetStatRAM($0A0, $0EF, cs_impleGPR);
     pic.SetStatRAM($120, $14F, cs_impleGPR);
     pic.SetMappRAMCom('0F0-0FF:bnk0, 170-17F:bnk0, 1F0-1FF:bnk0');
   end;
   'PIC16F628A': begin
     pic.MaxFreq:=20000000;
     pic.Npins := 16;
     pic.NumBanks:=4;
     pic.NumPages:=1; pic.MaxFlash:=2048;
     pic.GPRStart:=$20;   //es solo el valor de los bancoa 0 y 1

     pic.SetStatRAM($020, $07F, cs_impleGPR);
     pic.SetStatRAM($0A0, $0EF, cs_impleGPR);
     pic.SetStatRAM($120, $14F, cs_impleGPR);
     pic.SetMappRAMCom('0F0-0FF:bnk0, 170-17F:bnk0, 1F0-1FF:bnk0');
   end;
   'PIC16F648A': begin
     pic.MaxFreq:=20000000;
     pic.Npins := 16;
     pic.NumBanks:=4;
     pic.NumPages:=2; pic.MaxFlash:=4096;
     pic.GPRStart:=$20;   //es solo el valor de los bancoa 0 y 1
     pic.SetStatRAM($020, $07F, cs_impleGPR);
     pic.SetStatRAM($0A0, $0EF, cs_impleGPR);
     pic.SetStatRAM($120, $16F, cs_impleGPR);
     pic.SetMappRAMCom('0F0-0FF:bnk0, 170-17F:bnk0, 1F0-1FF:bnk0');
   end
   else
     exit(false);
   end;
end;

end.

