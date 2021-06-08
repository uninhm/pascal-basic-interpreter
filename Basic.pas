unit Basic;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Token, Parser, Lexer, Interpreter;

function RunCode(text: string): TNumberResult;

implementation

function RunCode(text: string): TNumberResult;
var
  lxr: TLexer;
  lxr_rslt: TTokensResult;
  prsr: TParser;
  prsr_rslt: TParseResult;
  intptr: TInterpreter;
begin
  RunCode := Default(TNumberResult);
  lxr := TLexer.Create(text, '<console>');
  lxr_rslt := lxr.makeTokens();

  if lxr_rslt.error <> nil then begin
    RunCode.error := lxr_rslt.error;
    Exit;
  end;

  prsr := TParser.Create(lxr_rslt.tokens);
  prsr_rslt := prsr.Parse();

  if prsr_rslt.error <> nil then begin
    RunCode.error := prsr_rslt.error;
    Exit;
  end;

  intptr := Default(TInterpreter);
  RunCode := intptr.Visit(prsr_rslt.node);
end;

end.
