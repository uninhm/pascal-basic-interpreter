unit Basic;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LangErrors, Position, Token, Parser, Lexer;

function RunCode(text: string): TParseResult;

implementation

function RunCode(text: string): TParseResult;
var
  lxr: TLexer;
  rslt: TTokensResult;
  prsr: TParser;
  ast: TNode;
begin
  RunCode := Default(TParseResult);
  lxr := TLexer.Create(text, '<console>');
  rslt := lxr.makeTokens();

  if rslt.error <> nil then begin
    RunCode.error := rslt.error;
    Exit;
  end;

  prsr := TParser.Create(rslt.tokens);
  ast := prsr.Parse();

  RunCode.node := ast;
end;

end.
