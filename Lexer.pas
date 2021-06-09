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
  SetLength(makeTokens.tokens, 32);
  i := 1;
  while current_char <> nil do begin
    if i >= Length(makeTokens.tokens) then
      SetLength(makeTokens.tokens, Length(makeTokens.tokens)*2);
    case current_char^ of
      ' ', #9: begin // Space or tab
        Advance();
        continue;
      end;
      '0' .. '9': begin
        makeTokens.tokens[i] := makeNumber();
        inc(i);
        continue;
      end;
      '+': makeTokens.tokens[i] := TToken.Create(pos.Copy(), TT_PLUS);
      '-': makeTokens.tokens[i] := TToken.Create(pos.Copy(), TT_MINUS);
      '*': makeTokens.tokens[i] := TToken.Create(pos.Copy(), TT_MUL);
      '/': makeTokens.tokens[i] := TToken.Create(pos.Copy(), TT_DIV);
      '(': makeTokens.tokens[i] := TToken.Create(pos.Copy(), TT_LPAREN);
      ')': makeTokens.tokens[i] := TToken.Create(pos.Copy(), TT_RPAREN);
      else begin
        makeTokens.tokens := nil;
        makeTokens.error := TIllegalCharError.Create(pos.Copy(), Concat('''', current_char^, ''''));
        Exit;
      end;
    end;
    Advance();
    inc(i);
  end;
  makeTokens.tokens[i] := TToken.Create(pos.Copy(), TT_EOF);
end;
function TLexer.MakeNumber(): TToken;
var
  num_str: string;
  dot_count: integer;
  p: TPosition;
begin
  num_str := ''; dot_count := 0;
  p := pos.Copy();
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
     makeNumber := TToken.CreateInt(p, StrToInt(num_str))
  else
     makeNumber := TToken.CreateFloat(p, StrToFloat(num_str));
end;
end.

