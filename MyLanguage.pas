program MyLanguage;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Classes, SysUtils, CustApp, Basic, Parser
  { you can add units after this };

type

  { Interpreter }

  Interpreter = class(TCustomApplication)
  protected
    procedure DoRun; override;
  public
    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;
    procedure WriteHelp; virtual;
  end;

{ Console }

procedure Interpreter.DoRun;
var
  ErrorMsg: String;
  text: String;
  result: TParseResult;
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
    result := runcode(text);
    if result.error <> nil then
       WriteLn(result.error.AsStr())
    else begin
      WriteLn(result.node.Repr());
    end;
  end;

  // stop program loop
  Terminate;
end;

constructor Interpreter.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);
  StopOnException:=True;
end;

destructor Interpreter.Destroy;
begin
  inherited Destroy;
end;

procedure Interpreter.WriteHelp;
begin
  { add your help code here }
  writeln('Usage: ', ExeName, ' -h');
end;

var
  Application: Interpreter;
begin
  Application:=Interpreter.Create(nil);
  Application.Title:='My Language';
  Application.Run;
  Application.Free;
end.

