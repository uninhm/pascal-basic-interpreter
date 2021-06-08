unit Interpreter;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Parser, LangErrors, Position;

type
  TNumberKind = (NK_INT, NK_FLOAT);


  TNumber = class
    private
      kind: TNumberKind;
      pos: TPosition;
      intv: Integer;
      floatv: Real;
    public
      constructor Create(val: Integer);
      constructor Create(val: Real);
      function GetPosition(): TPosition;
      function GetKind(): TNumberKind;
      function GetFloat(): Real;
      function GetInt(): Integer;
      function Repr(): String;
  end;

  TNumberResult = record
    num: TNumber;
    error: TError;
  end;

  TInterpreter = class
    public
      function Visit(nd: TNode): TNumberResult;
      function Visit(nd: TNumberNode): TNumberResult;
      function Visit(nd: TBinOpNode): TNumberResult;
      function Visit(nd: TUnaryOpNode): TNumberResult;
  end;

implementation

constructor TNumber.Create(val: Integer); begin
  kind := NK_INT;
  intv := val;
end;
constructor TNumber.Create(val: Real); begin
  kind := NK_FLOAT;
  floatv := val;
end;

function TNumber.GetPosition(): TPosition; begin
  GetPosition := pos.Copy();
end;
function TNumber.GetKind(): TNumberKind; begin
  Result := kind;
end;
function TNumber.GetFloat(): Real; begin
  if kind = NK_FLOAT then
     GetFloat := floatv
  else
     raise Exception.Create('Tried to get float value of a non-float token');
end;
function TNumber.GetInt(): Integer; begin
  if kind = NK_INT then
     GetInt := intv
  else
     raise Exception.Create('Tried to get int value of a non-int token')
end;

function AddedTo(num, other: TNumber): TNumberResult; begin
  Result := Default(TNumberResult);
  if num.GetKind() <> other.GetKind() then begin
    Result.error := TInvalidOperationError.Create(other.GetPosition(), 'Can''t operate different types');
    Exit;
  end;

  if num.GetKind() = NK_INT then
    Result.num := TNumber.Create(num.GetInt() + other.GetInt())
  else if num.GetKind() = NK_FLOAT then
    Result.num := TNumber.Create(num.GetFloat() + other.GetFloat())
end;
function SubbedBy(num, other: TNumber): TNumberResult; begin
  Result := Default(TNumberResult);
  if num.GetKind() <> other.GetKind() then begin
    Result.error := TInvalidOperationError.Create(other.GetPosition(), 'Can''t operate different types');
    Exit;
  end;

  if num.GetKind() = NK_INT then
    Result.num := TNumber.Create(num.GetInt() - other.GetInt())
  else if num.GetKind() = NK_FLOAT then
    Result.num := TNumber.Create(num.GetFloat() - other.GetFloat())
end;
function MultedBy(num, other: TNumber): TNumberResult; begin
  Result := Default(TNumberResult);
  if num.GetKind() <> other.GetKind() then begin
    Result.error := TInvalidOperationError.Create(other.GetPosition(), 'Can''t operate different types');
    Exit;
  end;

  if num.GetKind() = NK_INT then
    Result.num := TNumber.Create(num.GetInt() * other.GetInt())
  else if num.GetKind() = NK_FLOAT then
    Result.num := TNumber.Create(num.GetFloat() * other.GetFloat())
end;
function DivedBy(num, other: TNumber): TNumberResult; begin
  Result := Default(TNumberResult);
  if num.GetKind() <> other.GetKind() then begin
    Result.error := TInvalidOperationError.Create(other.GetPosition(), 'Can''t operate different types');
    Exit;
  end;

  if num.GetKind() = NK_INT then begin
    if other.GetInt() = 0 then
      Result.error := TInvalidOperationError.Create(other.GetPosition(), 'Can''t divide by zero')
    else
      Result.num := TNumber.Create(num.GetInt() / other.GetInt());
  end else if num.GetKind() = NK_FLOAT then begin
    if other.GetFloat() = 0.0 then
      Result.error := TInvalidOperationError.Create(other.GetPosition(), 'Can''t divide by zero')
    else
      Result.num := TNumber.Create(Num.GetFloat() / other.GetFloat())
  end;
end;

function TNumber.Repr(): String; begin
  if kind = NK_INT then
    Repr := IntToStr(intv)
  else if kind = NK_FLOAT then
    Repr := FloatToStr(floatv);
end;

function TInterpreter.Visit(nd: TNode): TNumberResult; begin
  if nd.ClassType.InheritsFrom(TNumberNode) then
    Result := Visit(TNumberNode(nd))
  else if nd.ClassType.InheritsFrom(TBinOpNode) then
    Result := Visit(TBinOpNode(nd))
  else if nd.ClassType.InheritsFrom(TUnaryOpNode) then
    Result := Visit(TUnaryOpNode(nd));
end;

function TInterpreter.Visit(nd: TNumberNode): TNumberResult; begin
  Result := Default(TNumberResult);
  if nd.IsInt() then
     Result.num := TNumber.Create(nd.GetInt())
  else if nd.IsFloat() then
     Result.num := TNumber.create(nd.GetFloat());
  Result.num.pos := nd.GetPosition();
end;
function TInterpreter.Visit(nd: TBinOpNode): TNumberResult;
var
  left, right: TNumberResult;
begin
  left := Visit(nd.left);
  if left.error <> nil then begin
    Result := left; Exit;
  end;
  right := Visit(nd.right);
  if right.error <> nil then begin
    Result := right; Exit;
  end;
  if nd.op.IsPlus() then
    Result := AddedTo(left.num, right.num)
  else if nd.op.IsMinus() then
    Result := SubbedBy(left.num, right.num)
  else if nd.op.IsMul() then
    Result := MultedBy(left.num, right.num)
  else if nd.op.IsDiv() then
    Result := DivedBy(left.num, right.num);
  if Result.error = nil then
    Result.num.pos := nd.op.GetPosition();
end;
function TInterpreter.Visit(nd: TUnaryOpNode): TNumberResult; begin
  Result := Visit(nd.nd);
  if Result.error <> nil then Exit;

  if nd.op.IsMinus() then begin
    if Result.num.GetKind() = NK_INT then
       Result := MultedBy(Result.num, TNumber.Create(-1))
    else if Result.num.GetKind() = NK_FLOAT then
       Result := MultedBy(Result.num, TNumber.Create(-1.0));
  end;
  if Result.error = nil then
    Result.num.pos := nd.op.GetPosition();
end;
end.

