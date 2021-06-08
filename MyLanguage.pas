program MyLanguage;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Classes, SysUtils, CustApp, Basic, Interpreter
  { you can add units after this };

type

  { Console }

  Console = class(TCustomApplication)
  protected
    procedure DoRun; override;
  public
    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;
    procedure WriteHelp; virtual;
  end;

{ Console }

procedure Console.DoRun;
var
  ErrorMsg: String;
  text: String;
  result: TNumberResult;
begin
  // quick check parameters
  ErrorMsg:=CheckOptions('h', 'help');
  if ErrorMsg<>'' then begin
    ShowException(Exception.Create(ErrorMsg));
    Terminate;
    Exit;
  end;

  // parse parameters
  if HasOption('h', 'help') then begin
    WriteHelp;
    Terminate;
    Exit;
  end;

  while true do begin
    write('basic> '); readln(text);
    if text = 'exit' then break;
    result := RunCode(text);
    if result.error <> nil then
       WriteLn(result.error.AsStr())
    else begin
      WriteLn(result.num.Repr());
    end;
  end;

  {result := RunCode('1 + 1.');
  if result.error <> nil then
    WriteLn(result.error.AsStr())
  else
    WriteLn(result.num.Repr());}

  // stop program loop
  Terminate;
end;

constructor Console.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);
  StopOnException:=True;
end;

destructor Console.Destroy;
begin
  inherited Destroy;
end;

procedure Console.WriteHelp;
begin
  { add your help code here }
  writeln('Usage: ', ExeName, ' -h');
end;

var
  Application: Console;
begin
  Application:=Console.Create(nil);
  Application.Title:='My Language';
  Application.Run;
  Application.Free;
end.

