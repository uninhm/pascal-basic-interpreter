unit Basic;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Token, Parser, Lexer, Interpreter;

function RunCode(text: string; intptr: TInterpreter): TValueResult;

implementation

function RunCode(text: string; intptr: TInterpreter): TValueResult;
var
  i: Integer;
  lxr: TLexer;
  lxr_rslt: TTokensResult;
  prsr: TParser;
  prsr_rslt: TParseResult;
begin
  RunCode := Default(TValueResult);
  lxr := TLexer.Create(text, '<console>');
  lxr_rslt := lxr.makeTokens();

  if lxr_rslt.error <> nil then begin
    RunCode.error := lxr_rslt.error;
    Exit;
  end;

  {
  i := 1;
  while lxr_rslt.tokens[i] <> nil do begin
    WriteLn(lxr_rslt.tokens[i].Repr());
    inc(i);
  end;
  }

  prsr := TParser.Create(lxr_rslt.tokens);
  prsr_rslt := prsr.Parse();

  if prsr_rslt.error <> nil then begin
    RunCode.error := prsr_rslt.error;
    Exit;
  end;

  // WriteLn(prsr_rslt.node.Repr());

  RunCode := intptr.Visit(prsr_rslt.node);
end;

end.
