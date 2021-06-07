unit Token;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LangErrors;

type
  TTokenKind = (TT_INT, TT_STR, TT_BOOL, TT_FLOAT, TT_PLUS, TT_MINUS, TT_DIV, TT_MUL, TT_LPAREN, TT_RPAREN);
  TTokenKindArray = array of TTokenKind;

  TToken = class
    private
      kind: TTokenKind;
      intv: Integer;
      // strv: string;
      boolv: Boolean;
      floatv: Real;

    public
      constructor Create(k: TTokenKind);
      constructor CreateInt(x: Integer);
      constructor CreateBool(x: Boolean);
      constructor CreateFloat(x: Real);
      function IsInt(): Boolean;
      // function isStr(): boolean;
      function IsBool(): Boolean;
      function IsFloat(): Boolean;
      function IsPlus(): Boolean;
      function IsMinus(): Boolean;
      function IsMul(): Boolean;
      function IsDiv(): Boolean;
      function IsLParen(): Boolean;
      function IsRParen(): Boolean;
      function IsOfKind(kinds: TTokenKindArray): Boolean;
      procedure SetInt(x: Integer);
      // procedure setStr(x: string);
      procedure SetBool(x: Boolean);
      procedure SetFloat(x: Real);
      function GetInt(): Integer;
      // function getStr(): integer;
      function GetBool(): Boolean;
      function GetFloat(): Real;
      function Repr(): String;
  end;

  TTokenArray = array of TToken;

  TTokensResult = record
    tokens: TTokenArray;
    error: TError;
  end;

implementation
constructor TToken.Create(k: TTokenKind); begin
  kind := k;
end;
constructor TToken.CreateInt(x: integer); begin
  kind := TT_INT;
  intv := x;
end;
constructor TToken.CreateBool(x: boolean); begin
  kind := TT_BOOL;
  boolv := x;
end;
constructor TToken.CreateFloat(x: real); begin
  kind := TT_FLOAT;
  floatv := x;
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

procedure TToken.SetInt(x: integer); begin
  if IsInt() then
     intv := x
  else
     raise Exception.Create('Tried to assign an int to a non-int token');
end;
procedure TToken.SetBool(x: boolean); begin
  if IsBool() then
     boolv := x
  else
     raise Exception.Create('Tried to assign a bool to a non-bool token');
end;
procedure TToken.SetFloat(x: real); begin
  if IsFloat() then
     floatv := x
  else
     raise Exception.Create('Tried to assign a float to a non-float token');
end;

function TToken.GetInt(): integer; begin
  if kind = TT_INT then
     GetInt := intv
  else
     raise Exception.Create('Tried to get int value of a non-int token')
end;
function TToken.GetBool(): boolean; begin
  if kind = TT_BOOL then
     GetBool := boolv
  else
     raise Exception.Create('Tried to get bool value of a non-bool token')
end;
function TToken.GetFloat(): real; begin
  if kind = TT_FLOAT then
     GetFloat := floatv
  else
     raise Exception.Create('Tried to get float value of a non-float token')
end;

function TToken.Repr(): string; begin
  if kind = TT_INT then
     Repr := Concat('INT:', IntToStr(intv))
  else if kind = TT_BOOL then
     Repr := Concat('BOOL:', BoolToStr(boolv))
  else if kind = TT_FLOAT then
     Repr := Concat('FLOAT:', FloatToStr(floatv))
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

