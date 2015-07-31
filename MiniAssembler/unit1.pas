{Sample of how to create a very basic assembler tool, using the unit pic16utils.}
unit Unit1;
{$mode objfpc}{$H+}

interface
uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,
  pic16utils;

type
  { TForm1 }

  TForm1 = class(TForm)
    Button1: TButton;
    Memo1: TMemo;
    Memo2: TMemo;
    procedure Button1Click(Sender: TObject);
  public
    function ExtractStr(var lin: string; delim: string): string;
  end;

var
  Form1: TForm1;

implementation
{$R *.lfm}
{ TForm1 }

function TForm1.ExtractStr(var lin: string; delim: string): string;
var
  p: SizeInt;
begin
  Result := '';
  p := Pos(delim,lin);
  if p=0 then begin
    exit;
  end;
  Result := copy(lin,1,p-1);  //get string
  lin := trim(copy(lin,p+1,100));    //trim
end;

procedure TForm1.Button1Click(Sender: TObject);
var
  l: String;
  idInst: TPIC16Inst;
  Inst: String;
  found: Boolean;
  stx: String;
  f, k, d, b: String;
begin
  for l in Memo1.Lines do begin
    Inst := ExtractStr(l, ' '); //extract mnemonic
    if Inst = '' then begin
      Application.MessageBox('Syntax Error','');
      exit;
    end;
    //find mnemonic
    found := false;
    for idInst := low(TPIC16Inst) to high(TPIC16Inst) do begin
      if PIC16InstName[idInst] = UpperCase(Inst) then begin
        found := true;
        break;
      end;
    end;
    if not found then begin
      Application.MessageBox(PChar('Invalid Opcode: '+ Inst),'');
      exit;
    end;
    //Found. Load syntax
    stx := PIC16InstSyntax[idInst];
    case stx of
    'fd': begin
       f:=ExtractStr(l,',');
       d:=l;
    end;
    'f':begin
      f:=l;
    end;
    'fb':begin
      f:=ExtractStr(l,',');
      b:=l;
    end;
    'k': begin
      k:=ExtractStr(l,',');
    end;
    '': begin
      if l <> '' then begin
        Application.MessageBox('Syntax Error','');
        exit;
      end;
    end;
    end;

    Memo2.Lines.Add(Inst + ' '+ stx);
  end;
end;

end.

