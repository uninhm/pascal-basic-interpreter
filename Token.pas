unit Token;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LangErrors, Position;

type
  PReal = ^Real;
  TTokenKind = (TT_INT, TT_BOOL, TT_FLOAT, TT_PLUS, TT_MINUS, TT_DIV, TT_MUL, TT_EQ, TT_LPAREN, TT_RPAREN, TT_IDENT, TT_KEYWORD, TT_EOF);
  TTokenKindArray = array of TTokenKind;

  TToken = class
    private
      kind: TTokenKind;
      value: Pointer;
      pos: TPosition;

    public
      constructor Create(p: TPosition; k: TTokenKind);
      constructor CreateInt(p: TPosition; x: Integer);
      constructor CreateBool(p: TPosition; x: Boolean);
      constructor CreateFloat(p: TPosition; x: Real);
      constructor CreateKeyword(p: TPosition; x: String);
      constructor CreateIdent(p: TPosition; x: String);
      function IsInt(): Boolean;
      function IsBool(): Boolean;
      function IsFloat(): Boolean;
      function IsKeyword(): Boolean;
      function IsIdent(): Boolean;
      function IsPlus(): Boolean;
      function IsMinus(): Boolean;
      function IsMul(): Boolean;
      function IsDiv(): Boolean;
      function IsEq(): Boolean;
      function IsLParen(): Boolean;
      function IsRParen(): Boolean;
      function IsEOF(): Boolean;
      function IsOfKind(kinds: TTokenKindArray): Boolean;
      procedure SetInt(x: Integer);
      procedure SetBool(x: Boolean);
      procedure SetFloat(x: Real);
      procedure SetKeyword(x: AnsiString);
      procedure SetIdent(x: AnsiString);
      function GetInt(): Integer;
      function GetBool(): Boolean;
      function GetFloat(): Real;
      function GetKeyword(): AnsiString;
      function GetIdent(): AnsiString;
      function GetPosition(): TPosition;
      function Repr(): String;
  end;

  TTokenArray = array of TToken;

  TTokensResult = record
    tokens: TTokenArray;
    error: TError;
  end;

implementation
constructor TToken.Create(p: TPosition; k: TTokenKind); begin
  pos := p;
  kind := k;
end;
constructor TToken.CreateInt(p: TPosition; x: integer); begin
  Create(p, TT_INT);
  SetInt(x);
end;
constructor TToken.CreateBool(p: TPosition; x: boolean); begin
  Create(p, TT_BOOL);
  SetBool(x);
end;
constructor TToken.CreateFloat(p: TPosition; x: real); begin
  Create(p, TT_FLOAT);
  SetFloat(x);
end;
constructor TToken.CreateKeyword(p: TPosition; x: AnsiString); begin
  Create(p, TT_KEYWORD);
  SetKeyword(x);
end;
constructor TToken.CreateIdent(p: TPosition; x: AnsiString); begin
  Create(p, TT_IDENT);
  SetIdent(x);
end;

function TToken.IsInt(): boolean; begin
  IsInt := kind = TT_INT;
end;
function TToken.IsBool(): boolean; begin
  IsBool := kind = TT_BOOL;
end;
function TToken.IsFloat(): boolean; begin
  IsFloat := kind = TT_FLOAT;
end;
function TToken.IsKeyword(): boolean; begin
  IsKeyword := kind = TT_KEYWORD;
end;
function TToken.IsIdent(): boolean; begin
  IsIdent := kind = TT_IDENT;
end;
function TToken.IsPlus(): Boolean; begin
  IsPlus := kind = TT_PLUS
end;
function TToken.IsMinus(): Boolean; begin
  IsMinus := kind = TT_MINUS;
end;
function TToken.IsMul(): Boolean; begin
  IsMul := kind = TT_MUL;
end;
function TToken.IsDiv(): Boolean; begin
  IsDiv := kind = TT_DIV;
end;
function TToken.IsEq(): Boolean; begin
  IsEq := kind = TT_EQ;
end;
function TToken.IsLParen(): Boolean; begin
  IsLParen := kind = TT_LPAREN;
end;
function TToken.IsRParen(): Boolean; begin
  IsRParen := kind = TT_RPAREN;
end;
function TToken.IsOfKind(kinds: TTokenKindArray): Boolean;
var
  i: Cardinal;
begin
  IsOfKind := false;
  for i := 0 to (Length(kinds)-1) do
    if kinds[i] = kind then begin
      IsOfKind := true;
      break;
    end;
end;
function TToken.IsEOF(): Boolean; begin
  IsEOF := kind = TT_EOF;
end;

procedure TToken.SetInt(x: integer); begin
  if IsInt() then begin
    if Assigned(value) then
      FreeMem(value);
    value := GetMem(SizeOf(x));
    PInteger(value)^ := x;
  end else
    raise Exception.Create('Tried to assign an int to a non-int token');
end;
procedure TToken.SetBool(x: boolean); begin
  if IsBool() then begin
    if Assigned(value) then
      FreeMem(value);
    value := GetMem(SizeOf(x));
    PBoolean(value)^ := x;
  end else
    raise Exception.Create('Tried to assign a bool to a non-bool token');
end;
procedure TToken.SetFloat(x: real); begin
  if IsFloat() then begin
    if Assigned(value) then
      FreeMem(value);
    value := GetMem(SizeOf(x));
    PReal(value)^ := x;
  end else
    raise Exception.Create('Tried to assign a float to a non-float token');
end;
procedure TToken.SetKeyword(x: AnsiString); begin
  if IsKeyword() then begin
    if Assigned(value) then
      FreeMem(value);
    value := GetMem(Length(x)+1);
    AnsiString(value) := x;
  end else
    raise Exception.Create('Tried to assign a keyword to a non-keyword token');
end;
procedure TToken.SetIdent(x: AnsiString); begin
  if IsIdent() then begin
    if Assigned(value) then
      FreeMem(value);
    value := GetMem(Length(x)+1);
    AnsiString(value) := x
  end else
    raise Exception.Create('Tried to assign an identifier to a non-identifier token');
end;

function TToken.GetInt(): integer; begin
  if kind = TT_INT then
     GetInt := PInteger(value)^
  else
     raise Exception.Create('Tried to get int value of a non-int token')
end;
function TToken.GetBool(): boolean; begin
  if kind = TT_BOOL then
     GetBool := PBoolean(value)^
  else
     raise Exception.Create('Tried to get bool value of a non-bool token')
end;
function TToken.GetFloat(): real; begin
  if kind = TT_FLOAT then
     GetFloat := PReal(value)^
  else
     raise Exception.Create('Tried to get float value of a non-float token')
end;
function TToken.GetKeyword(): AnsiString; begin
  if IsKeyword() then
     GetKeyword := AnsiString(value)
  else
     raise Exception.Create('Tried to get keyword value of a non-keyword token')
end;
function TToken.GetIdent(): AnsiString; begin
  if IsIdent() then
     GetIdent := AnsiString(value)
  else
     raise Exception.Create('Tried to get identifier value of a non-identifier token')
end;
function TToken.GetPosition(): TPosition; begin
  GetPosition := pos.Copy();
end;

function TToken.Repr(): string; begin
  if kind = TT_INT then
     Repr := Concat('INT:', IntToStr(GetInt()))
  else if kind = TT_BOOL then
     Repr := Concat('BOOL:', BoolToStr(GetBool()))
  else if kind = TT_FLOAT then
     Repr := Concat('FLOAT:', FloatToStr(GetFloat()))
  else if kind = TT_PLUS then
     Repr := 'PLUS'
  else if kind = TT_MINUS then
     Repr := 'MINUS'
  else if kind = TT_MUL then
     Repr := 'MUL'
  else if kind = TT_DIV then
     Repr := 'DIV'
  else if kind = TT_LPAREN then
     Repr := 'LPAREN'
  else if kind = TT_RPAREN then
     Repr := 'RPAREN';
end;
end.

