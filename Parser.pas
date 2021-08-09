unit Parser;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Token, LangErrors, Position;

type
  TNode = class
    public
      function Repr(): string; virtual;
  end;

  TNodeArray = Array of TNode;

  TParseResult = record
    node: TNode;
    error: TError;
  end;

  TValueNode = class(TNode)
    private
      tok: TToken;
    public
      constructor Create(t: TToken);
      function Repr(): String; override;
      function IsInt(): Boolean;
      function IsFloat(): Boolean;
      function IsBool(): Boolean;
      function IsIdent(): Boolean;
      function GetInt(): Integer;
      function GetFloat(): Real;
      function GetBool(): Boolean;
      function GetIdent(): AnsiString;
      function GetPosition(): TPosition;
  end;

  TAssignationNode = class(TNode)
    public
      ident: TToken;
      value: TNode;
      constructor Create(ident_token: TToken; value_node: TNode);
      //function Repr(): string; override;
  end;

  TOpNode = class(TNode)
    public
      op: TNode;
      operands: TNodeArray;
      constructor Create(operator_node: TNode; operands_list: TNodeArray);
      function Repr(): string; override;
  end;

  TNodeFunction = function:TParseResult of object;

  TParser = class
    private
      tokens: TTokenArray;
      tok_idx: Cardinal;
      current_tok: TToken;

      procedure Advance();
      function Expr(): TParseResult;
    public
      constructor Create(tkns: TTokenArray);
      function Parse(): TParseResult;
  end;

implementation
constructor TValueNode.Create(t: TToken); begin
  tok := t;
end;
constructor TAssignationNode.Create(ident_token: TToken; value_node: TNode); begin
  ident := ident_token;
  value := value_node;
end;
constructor TOpNode.Create(operator_node: TNode; operands_list: TNodeArray); begin
  op := operator_node;
  operands := operands_list;
end;

function TValueNode.IsInt(): Boolean; begin
  Result := tok.IsInt();
end;
function TValueNode.IsFloat(): Boolean; begin
  Result := tok.IsFloat();
end;
function TValueNode.IsBool(): Boolean; begin
  Result := tok.IsBool();
end;
function TValueNode.IsIdent(): Boolean; begin
  Result := tok.IsIdent();
end;

function TValueNode.GetInt(): Integer; begin
  Result := tok.GetInt();
end;
function TValueNode.GetFloat(): Real; begin
  Result := tok.GetFloat();
end;
function TValueNode.GetBool(): Boolean; begin
  Result := tok.GetBool();
end;
function TValueNode.GetIdent(): AnsiString; begin
  Result := tok.GetIdent();
end;
function TValueNode.GetPosition(): TPosition; begin
  Result := tok.GetPosition();
end;

function TNode.Repr(): string; begin
  Repr := 'NULL';
end;
function TValueNode.Repr(): string; begin
  Repr := tok.Repr();
end;
function TOpNode.Repr(): string;
var
  i: Integer;
begin
  Repr := Concat('(', op.Repr());
  i := 0;
  while operands[i] <> nil do begin
    Repr := Concat(Repr, ' ', operands[i].Repr());
    inc(i);
  end;
  Repr := Concat(Repr, ')');
end;

constructor TParser.Create(tkns: TTokenArray); begin
  tokens := tkns;
  tok_idx := 0;
  Advance();
end;
procedure TParser.Advance(); begin
  inc(tok_idx);
  current_tok := tokens[tok_idx]
end;
function TParser.Expr(): TParseResult;
var
  op: TNode;
  operands: TNodeArray;
  i: Integer;
begin
  Result := Default(TParseResult);

  if current_tok.IsEOF() then begin
    Result.error := TInvalidSyntaxError.Create(current_tok.GetPosition(), 'Expression expected but EOF found');
    Exit;
  end;

  if current_tok.IsInt() or current_tok.IsFloat() or current_tok.IsBool() then begin
    Result.node := TValueNode.Create(current_tok);
    Advance();
  { end else if current_tok.IsPlus() or current_tok.IsMinus() then begin
    tkn := current_tok;
    Advance();
    Result := Atom();
    if Result.error <> nil then Exit;
    Result.node := TUnaryOpNode.Create(tkn, Result.node); }
  end else if current_tok.IsLParen() then begin
    Advance();
    Result := Expr();
    if Result.error <> nil then Exit;
    op := Result.node;
    operands := TNodeArray.Create(nil);
    SetLength(operands, 2);
    i := 0;
    while not current_tok.IsRParen() do begin
      if i = Length(operands) then
        SetLength(operands, Length(operands)+5);
      Result := Expr();
      if Result.error <> nil then Exit;
      operands[i] := Result.node;
      Inc(i);
    end;
    Result.node := TOpNode.Create(op, operands);
    Advance();
  end else if current_tok.IsIdent() then begin
    Result.node := TValueNode.Create(current_tok);
    Advance();
  end else
    Result.error := TInvalidSyntaxError.Create(current_tok.GetPosition(), 'Expression expected');
end;

function TParser.Parse(): TParseResult; begin
  Parse := Expr();
  if (Parse.error = nil) and (not current_tok.IsEOF()) then begin
    Parse.node := nil;
    Parse.error := TInvalidSyntaxError.Create(current_tok.GetPosition(), 'Operator expected');
  end;
end;

end.

