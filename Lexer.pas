unit Lexer;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Position, Token, LangErrors;

const
  DIGITS = '1234567890';

type
  TLexer = class
    private
      text: string;
      pos: TPosition;
      current_char: PChar;
      fn: ShortString;

      procedure advance();
      function makeNumber(): TToken;
    public
      constructor create(s: AnsiString; filename: ShortString);
      function makeTokens(): TTokensResult;
  end;

implementation
constructor TLexer.Create(s: AnsiString; filename: ShortString); begin
  current_char := GetMem(1);
  text := s;
  fn := filename;
  pos := TPosition.Create(0, 1, 0, fn, text);
  advance();
end;
procedure TLexer.Advance(); begin
  pos.Advance(current_char^);
  if pos.GetIndex() <= text.length then
     current_char^ := text[pos.GetIndex()]
  else
     current_char := nil;
end;
function Contains(s: string; c: PChar): boolean;
var
  i: integer;
begin
  Contains := false;
  for i := 1 to s.length do
    if s[i] = c^ then begin
       Contains := true;
       break;
    end;
end;
function TLexer.MakeTokens(): TTokensResult;
var
  i: integer;

begin
  makeTokens := Default(TTokensResult);
  SetLength(makeTokens.tokens, 20);
  for i := 0 to 20 do
    makeTokens.tokens[i] := nil;
  i := 1;
  while current_char <> nil do begin
    if Contains(' \t', current_char) then
       advance()
    else if Contains(DIGITS, current_char) then begin
       makeTokens.tokens[i] := makeNumber();
       inc(i);
    end else begin
      case current_char^ of
       '+': makeTokens.tokens[i] := TToken.Create(TT_PLUS);
       '-': makeTokens.tokens[i] := TToken.Create(TT_MINUS);
       '*': makeTokens.tokens[i] := TToken.Create(TT_MUL);
       '/': makeTokens.tokens[i] := TToken.Create(TT_DIV);
       '(': makeTokens.tokens[i] := TToken.Create(TT_LPAREN);
       ')': makeTokens.tokens[i] := TToken.Create(TT_RPAREN);
       else begin
         makeTokens.tokens := nil;
         makeTokens.error := TIllegalCharError.Create(pos.Copy(), Concat('''', current_char^, ''''));
         Exit;
       end;
      end;
      advance();
      inc(i);
    end;
  end;
end;
function TLexer.MakeNumber(): TToken;
var
  num_str: string;
  dot_count: integer;

begin
  num_str := ''; dot_count := 0;
  while (current_char <> nil) and (Contains(DIGITS, current_char) or (current_char^ = '.')) do begin
    if current_char^ = '.' then begin
      inc(dot_count);
      if dot_count > 1 then
         raise Exception.Create('Invalid float, found multiple dots');
    end;
    num_str := Concat(num_str, current_char^);
    advance();
  end;

  if dot_count = 0 then
     makeNumber := TToken.CreateInt(StrToInt(num_str))
  else
     makeNumber := TToken.CreateFloat(StrToFloat(num_str));
end;
end.

