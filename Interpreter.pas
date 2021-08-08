unit Interpreter;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Parser, LangErrors, Position, fgl;

type
  PReal = ^Real;
  TValueKind = (VK_INT, VK_FLOAT, VK_BOOL, VK_IDENT, VK_NIL);

  TValue = class
    private
      kind: TValueKind;
      pos: TPosition;
      value: Pointer;
    public
      constructor Create(k: TValueKind);
      constructor CreateInt(val: Integer);
      constructor CreateFloat(val: Real);
      constructor CreateBool(val: Boolean);
      constructor CreateIdent(val: AnsiString);
      constructor CreateNil();
      function IsInt(): Boolean;
      function IsFloat(): Boolean;
      function IsBool(): Boolean;
      function IsIdent(): Boolean;
      function IsNil(): Boolean;
      function GetPosition(): TPosition;
      function GetKind(): TValueKind;
      function GetFloat(): Real;
      function GetInt(): Integer;
      function GetBool(): Boolean;
      function GetIdent(): AnsiString;
      procedure SetInt(val: Integer);
      procedure SetFloat(val: Real);
      procedure SetBool(val: Boolean);
      procedure SetIdent(val: AnsiString);
      function Repr(): String;
  end;

  TContent = specialize TFPGMap<String, TValue>;
  TEnvironment = class
    private
      parent: TEnvironment;
      content: TContent;
    public
      constructor Create(p: TEnvironment);
      function Get(key: String): TValue;
      procedure AddOrSet(key: String; data: TValue);
  end;

  TValueResult = record
    value: TValue;
    error: TError;
  end;

  TInterpreter = class
    private
      env: TEnvironment;
    public
      constructor Create();
      function Visit(nd: TNode): TValueResult;
      function Visit(nd: TValueNode): TValueResult;
      function Visit(nd: TBinOpNode): TValueResult;
      function Visit(nd: TAssignationNode): TValueResult;
      function Visit(nd: TUnaryOpNode): TValueResult;
  end;

implementation
constructor TValue.Create(k: TValueKind); begin
  kind := k;
end;

constructor TValue.CreateInt(val: Integer); begin
  Create(VK_INT);
  SetInt(val);
end;
constructor TValue.CreateFloat(val: Real); begin
  Create(VK_FLOAT);
  SetFloat(val);
end;
constructor TValue.CreateBool(val: Boolean); begin
  Create(VK_BOOL);
  SetBool(val);
end;
constructor TValue.CreateIdent(val: AnsiString); begin
  Create(VK_IDENT);
  SetIdent(val);
end;
constructor TValue.CreateNil(); begin
  Create(VK_NIL);
end;

function TValue.IsInt(): Boolean; begin
  Result := kind = VK_INT;
end;
function TValue.IsFloat(): Boolean; begin
  Result := kind = VK_FLOAT;
end;
function TValue.IsBool(): Boolean; begin
  Result := kind = VK_BOOL;
end;
function TValue.IsIdent(): Boolean; begin
  Result := kind = VK_IDENT;
end;
function TValue.IsNil(): Boolean; begin
  Result := kind = VK_NIL;
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
procedure TValue.SetBool(val: Boolean); begin
  if Assigned(value) then
    FreeMem(value);
  value := GetMem(SizeOf(val));
  PBoolean(value)^ := val;
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
  if IsFloat() then
     GetFloat := PReal(value)^
  else
     raise Exception.Create('Tried to get float of a non-float value');
end;
function TValue.GetInt(): Integer; begin
  if IsInt() then
     GetInt := PInteger(value)^
  else
     raise Exception.Create('Tried to get int of a non-int value')
end;
function TValue.GetBool(): Boolean; begin
  if IsBool() then
     Result := PBoolean(value)^
  else
     raise Exception.Create('Tried to get bool of a non-bool value')
end;
function TValue.GetIdent(): AnsiString; begin
  if IsIdent() then
     GetIdent := AnsiString(value)
  else
     raise Exception.Create('Tried to get ident of a non-ident value')
end;

function TValue.Repr(): String; begin
  Repr := '';
  if IsInt() then
    Repr := 'Int: ' + IntToStr(GetInt())
  else if IsFloat() then
    Repr := 'Float: ' + FloatToStr(GetFloat())
  else if IsBool() then
    Repr := 'Bool: ' + BoolToStr(GetBool(), 'true', 'false')
  else if IsIdent() then
    Repr := 'Ident: ' + AnsiString(value)
  else if IsNil() then
    Repr := 'NIL';
end;

constructor TEnvironment.Create(p: TEnvironment); begin
  parent := p;
  content := TContent.Create();
end;

function TEnvironment.Get(key: String): TValue;
var
  p: TEnvironment;
  i: Integer;
begin
  Result := nil;
  p := Self;
  while p <> nil do begin
    i := p.content.IndexOf(key);
    if i <> (-1) then begin
      Result := p.content.data[i];
      break;
    end;
    p := p.parent;
  end;
end;

procedure TEnvironment.AddOrSet(key: String; data: TValue);
var
  p: TEnvironment;
  i: Integer;
  found: Boolean;
begin
  p := Self;
  found := false;
  while p <> nil do begin
    i := p.content.IndexOf(key);
    if i <> (-1) then begin
      found := true;
      p.content.AddOrSetData(key, data);
      break;
    end;
    p := p.parent;
  end;
  if not found then
    content.AddOrSetData(key, data);
end;

function AddedTo(num, other: TValue): TValueResult; begin
  Result := Default(TValueResult);
  if num.GetKind() <> other.GetKind() then begin
    Result.error := TInvalidOperationError.Create(other.GetPosition(), 'Can''t operate different types');
    Exit;
  end;

  if num.IsInt() then
    Result.value := TValue.CreateInt(num.GetInt() + other.GetInt())
  else if num.IsFloat() then
    Result.value := TValue.CreateFloat(num.GetFloat() + other.GetFloat())
  else
    Result.error := TInvalidOperationError.Create(num.GetPosition(), 'Operation not defined for type');
end;
function SubbedBy(num, other: TValue): TValueResult; begin
  Result := Default(TValueResult);
  if num.GetKind() <> other.GetKind() then begin
    Result.error := TInvalidOperationError.Create(other.GetPosition(), 'Can''t operate different types');
    Exit;
  end;

  if num.IsInt() then
    Result.value := TValue.CreateInt(num.GetInt() - other.GetInt())
  else if num.IsFloat() then
    Result.value := TValue.CreateFloat(num.GetFloat() - other.GetFloat())
  else
    Result.error := TInvalidOperationError.Create(num.GetPosition(), 'Operation not defined for type');
end;
function MultedBy(num, other: TValue): TValueResult; begin
  Result := Default(TValueResult);
  if num.GetKind() <> other.GetKind() then begin
    Result.error := TInvalidOperationError.Create(other.GetPosition(), 'Can''t operate different types');
    Exit;
  end;

  if num.IsInt() then
    Result.value := TValue.CreateInt(num.GetInt() * other.GetInt())
  else if num.IsFloat() then
    Result.value := TValue.CreateFloat(num.GetFloat() * other.GetFloat())
  else
    Result.error := TInvalidOperationError.Create(num.GetPosition(), 'Operation not defined for type');
end;
function DivedBy(num, other: TValue): TValueResult; begin
  Result := Default(TValueResult);
  if num.GetKind() <> other.GetKind() then begin
    Result.error := TInvalidOperationError.Create(other.GetPosition(), 'Can''t operate different types');
    Exit;
  end;

  if num.IsInt() then begin
    if other.GetInt() = 0 then
      Result.error := TInvalidOperationError.Create(other.GetPosition(), 'Can''t divide by zero')
    else
      Result.value := TValue.CreateInt(num.GetInt() div other.GetInt());
  end else if num.IsFloat() then begin
    if other.GetFloat() = 0.0 then
      Result.error := TInvalidOperationError.Create(other.GetPosition(), 'Can''t divide by zero')
    else
      Result.value := TValue.CreateFloat(Num.GetFloat() / other.GetFloat());
  end else
    Result.error := TInvalidOperationError.Create(num.GetPosition(), 'Operation not defined for type');
end;

constructor TInterpreter.Create(); begin
  env := TEnvironment.Create(nil);
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
begin
  Result := Default(TValueResult);
  if nd.IsInt() then
    Result.value := TValue.CreateInt(nd.GetInt())
  else if nd.IsFloat() then
    Result.value := TValue.CreateFloat(nd.GetFloat())
  else if nd.IsBool() then
    Result.value := TValue.CreateBool(nd.GetBool())
  else if nd.IsIdent() then begin
    Result.value := env.Get(nd.GetIdent());
    if Result.value = nil then begin
      Result.error := TRuntimeError.Create(nd.GetPosition(), Concat('Undefined variable ''', nd.GetIdent(), ''''));
      Result.value := nil;
      Exit;
    end;
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
function TInterpreter.Visit(nd: TAssignationNode): TValueResult;
begin
  Result := Visit(nd.value);
  if Result.error <> nil then Exit;

  env.AddOrSet(nd.ident.GetIdent(), Result.value);
end;
function TInterpreter.Visit(nd: TUnaryOpNode): TValueResult; begin
  Result := Visit(nd.nd);
  if Result.error <> nil then Exit;

  if nd.op.IsMinus() then begin
    if Result.value.IsInt() then
       Result := MultedBy(Result.value, TValue.CreateInt(-1))
    else if Result.value.IsFloat() then
       Result := MultedBy(Result.value, TValue.CreateFloat(-1));
  end;
  if Result.error = nil then
    Result.value.pos := nd.op.GetPosition();
end;
end.

