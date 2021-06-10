unit Interpreter;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Parser, LangErrors, Position, fgl;

type
  PReal = ^Real;
  TValueKind = (VK_INT, VK_FLOAT, VK_IDENT, VK_NIL);

  TValue = class
    private
      kind: TValueKind;
      pos: TPosition;
      value: Pointer;
    public
      constructor Create(val: Integer);
      constructor Create(val: Real);
      constructor Create(val: AnsiString);
      constructor CreateNil();
      function GetPosition(): TPosition;
      function GetKind(): TValueKind;
      function GetFloat(): Real;
      function GetInt(): Integer;
      function GetIdent(): AnsiString;
      procedure SetInt(val: Integer);
      procedure SetFloat(val: Real);
      procedure SetIdent(val: AnsiString);
      function Repr(): String;
  end;

  TVariables = specialize TFPGMap<String, TValue>;

  TValueResult = record
    value: TValue;
    error: TError;
  end;

  TInterpreter = class
    private
      variables: TVariables;
    public
      constructor Create();
      function Visit(nd: TNode): TValueResult;
      function Visit(nd: TValueNode): TValueResult;
      function Visit(nd: TBinOpNode): TValueResult;
      function Visit(nd: TAssignationNode): TValueResult;
      function Visit(nd: TUnaryOpNode): TValueResult;
  end;

implementation

constructor TValue.Create(val: Integer); begin
  kind := VK_INT;
  SetInt(val);
end;
constructor TValue.Create(val: Real); begin
  kind := VK_FLOAT;
  SetFloat(val);
end;
constructor TValue.Create(val: AnsiString); begin
  kind := VK_IDENT;
  SetIdent(val);
end;
constructor TValue.CreateNil(); begin
  kind := VK_NIL;
end;

procedure TValue.SetInt(val: Integer); begin
  if Assigned(value) then
    FreeMem(value);
  value := GetMem(SizeOf(val));
  PInteger(value)^ := val;
end;
procedure TValue.SetFloat(val: Real); begin
  if Assigned(value) then
    FreeMem(value);
  value := GetMem(SizeOf(val));
  PReal(value)^ := val;
end;
procedure TValue.SetIdent(val: AnsiString); begin
  if Assigned(value) then
    FreeMem(value);
  value := GetMem(Length(val)+1);
  AnsiString(value) := val;
end;

function TValue.GetPosition(): TPosition; begin
  GetPosition := pos.Copy();
end;
function TValue.GetKind(): TValueKind; begin
  Result := kind;
end;
function TValue.GetFloat(): Real; begin
  if kind = VK_FLOAT then
     GetFloat := PReal(value)^
  else
     raise Exception.Create('Tried to get float of a non-float value');
end;
function TValue.GetInt(): Integer; begin
  if kind = VK_INT then
     GetInt := PInteger(value)^
  else
     raise Exception.Create('Tried to get int of a non-int value')
end;
function TValue.GetIdent(): AnsiString; begin
  if kind = VK_IDENT then
     GetIdent := AnsiString(value)
  else
     raise Exception.Create('Tried to get ident of a non-ident value')
end;

function TValue.Repr(): String; begin
  if kind = VK_INT then
    Repr := IntToStr(GetInt())
  else if kind = VK_FLOAT then
    Repr := FloatToStr(GetFloat())
  else if kind = VK_NIL then
    Repr := 'NIL';
end;

function AddedTo(num, other: TValue): TValueResult; begin
  Result := Default(TValueResult);
  if num.GetKind() <> other.GetKind() then begin
    Result.error := TInvalidOperationError.Create(other.GetPosition(), 'Can''t operate different types');
    Exit;
  end;

  if num.GetKind() = VK_INT then
    Result.value := TValue.Create(num.GetInt() + other.GetInt())
  else if num.GetKind() = VK_FLOAT then
    Result.value := TValue.Create(num.GetFloat() + other.GetFloat())
end;
function SubbedBy(num, other: TValue): TValueResult; begin
  Result := Default(TValueResult);
  if num.GetKind() <> other.GetKind() then begin
    Result.error := TInvalidOperationError.Create(other.GetPosition(), 'Can''t operate different types');
    Exit;
  end;

  if num.GetKind() = VK_INT then
    Result.value := TValue.Create(num.GetInt() - other.GetInt())
  else if num.GetKind() = VK_FLOAT then
    Result.value := TValue.Create(num.GetFloat() - other.GetFloat())
end;
function MultedBy(num, other: TValue): TValueResult; begin
  Result := Default(TValueResult);
  if num.GetKind() <> other.GetKind() then begin
    Result.error := TInvalidOperationError.Create(other.GetPosition(), 'Can''t operate different types');
    Exit;
  end;

  if num.GetKind() = VK_INT then
    Result.value := TValue.Create(num.GetInt() * other.GetInt())
  else if num.GetKind() = VK_FLOAT then
    Result.value := TValue.Create(num.GetFloat() * other.GetFloat())
end;
function DivedBy(num, other: TValue): TValueResult; begin
  Result := Default(TValueResult);
  if num.GetKind() <> other.GetKind() then begin
    Result.error := TInvalidOperationError.Create(other.GetPosition(), 'Can''t operate different types');
    Exit;
  end;

  if num.GetKind() = VK_INT then begin
    if other.GetInt() = 0 then
      Result.error := TInvalidOperationError.Create(other.GetPosition(), 'Can''t divide by zero')
    else
      Result.value := TValue.Create(num.GetInt() / other.GetInt());
  end else if num.GetKind() = VK_FLOAT then begin
    if other.GetFloat() = 0.0 then
      Result.error := TInvalidOperationError.Create(other.GetPosition(), 'Can''t divide by zero')
    else
      Result.value := TValue.Create(Num.GetFloat() / other.GetFloat())
  end;
end;

constructor TInterpreter.Create(); begin
  variables := TVariables.Create();
end;

function TInterpreter.Visit(nd: TNode): TValueResult; begin
  if nd.ClassType.InheritsFrom(TValueNode) then
    Result := Visit(TValueNode(nd))
  else if nd.ClassType.InheritsFrom(TBinOpNode) then
    Result := Visit(TBinOpNode(nd))
  else if nd.ClassType.InheritsFrom(TAssignationNode) then
    Result := Visit(TAssignationNode(nd))
  else if nd.ClassType.InheritsFrom(TUnaryOpNode) then
    Result := Visit(TUnaryOpNode(nd));
end;

function TInterpreter.Visit(nd: TValueNode): TValueResult;
var
  i: Integer;
begin
  Result := Default(TValueResult);
  if nd.IsInt() then
     Result.value := TValue.Create(nd.GetInt())
  else if nd.IsFloat() then
     Result.value := TValue.Create(nd.GetFloat())
  else if nd.IsIdent() then begin
    i := variables.IndexOf(nd.GetIdent());
    if i = (-1) then begin
      Result.error := TRuntimeError.Create(nd.GetPosition(), Concat('Undefined variable ''', nd.GetIdent(), ''''));
      Exit;
    end;
    Result.value := variables.Data[i];
  end;
  Result.value.pos := nd.GetPosition();
end;

function TInterpreter.Visit(nd: TBinOpNode): TValueResult;
var
  left, right: TValueResult;
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
    Result := AddedTo(left.value, right.value)
  else if nd.op.IsMinus() then
    Result := SubbedBy(left.value, right.value)
  else if nd.op.IsMul() then
    Result := MultedBy(left.value, right.value)
  else if nd.op.IsDiv() then
    Result := DivedBy(left.value, right.value);
  if Result.error = nil then
    Result.value.pos := nd.op.GetPosition();
end;
function TInterpreter.Visit(nd: TAssignationNode): TValueResult; begin
  Result := Visit(nd.value);
  if Result.error <> nil then Exit;

  variables.Add(nd.ident.GetIdent(), Result.value);
end;
function TInterpreter.Visit(nd: TUnaryOpNode): TValueResult; begin
  Result := Visit(nd.nd);
  if Result.error <> nil then Exit;

  if nd.op.IsMinus() then begin
    if Result.value.GetKind() = VK_INT then
       Result := MultedBy(Result.value, TValue.Create(-1))
    else if Result.value.GetKind() = VK_FLOAT then
       Result := MultedBy(Result.value, TValue.Create(-1.0));
  end;
  if Result.error = nil then
    Result.value.pos := nd.op.GetPosition();
end;
end.

