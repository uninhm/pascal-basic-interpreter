program MyLanguage(stdin);

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Classes, SysUtils, CustApp, Basic, Interpreter, TermIO
  { you can add units after this };

type

  { Console }

  Console = class(TCustomApplication)
	private
		stdin: Text;
  protected
    procedure DoRun; override;
  public
    constructor Create2(var inp: Text; TheOwner: TComponent);
    destructor Destroy; override;
    procedure WriteHelp; virtual;
  end;

{ Console }

procedure Console.DoRun;
var
  ErrorMsg: String;
  text: String;
  res: TValueResult;
  intptr: TInterpreter;
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

  intptr := TInterpreter.Create();
  while true do begin
		if IsATTY(stdin) = 1 then
			Write('basic> ');
		ReadLn(text);
    if text = 'exit' then break;
    res := RunCode(text, intptr);
    if res.error <> nil then
       WriteLn(res.error.AsStr())
    else begin
      WriteLn(res.value.Repr());
    end;
  end;

  {res := RunCode('var hola = 5');
  if res.error <> nil then
    WriteLn(res.error.AsStr())
  else
    WriteLn(res.value.Repr());}

  // stop program loop
  Terminate;
end;

constructor Console.Create2(var inp: Text; TheOwner: TComponent);
begin
  Create(TheOwner);
	stdin := inp;
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
	stdin: Text;
begin
  Application:=Console.Create2(stdin, nil);
  Application.Title:='My Language';
  Application.Run;
  Application.Free;
end.

