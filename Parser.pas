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

  TParseResult = record
    node: TNode;
    error: TError;
  end;

  TNumberNode = class(TNode)
    private
      tok: TToken;
    public
      constructor Create(t: TToken);
      function Repr(): String; override;
      function IsInt(): Boolean;
      function IsFloat(): Boolean;
      function GetInt(): Integer;
      function GetFloat(): Real;
      function GetPosition(): TPosition;
  end;

  TBinOpNode = class(TNode)
    public
      left, right: TNode;
      op: TToken;
      constructor Create(left_node: TNode; operator_token: TToken; right_node: TNode);
      function Repr(): string; override;
  end;

  TUnaryOpNode = class(TNode)
    public
      nd: TNode;
      op: TToken;
      constructor Create(operator_token: TToken; node: TNode);
      function Repr(): string; override;
  end;

  TNodeFunction = function:TParseResult of object;

  TParser = class
    private
      tokens: TTokenArray;
      tok_idx: Cardinal;
      current_tok: TToken;

      procedure Advance();
      function Factor(): TParseResult;
      function BinOp(func: TNodeFunction; ops: TTokenKindArray): TParseResult;
      function Term(): TParseResult;
      function Expr(): TParseResult;
    public
      constructor Create(tkns: TTokenArray);
      function Parse(): TParseResult;
  end;

implementation
constructor TNumberNode.Create(t: TToken); begin
  tok := t;
end;
constructor TBinOpNode.Create(left_node: TNode; operator_token: TToken; right_node: TNode); begin
  left := left_node;
  right := right_node;
  op := operator_token;
end;
constructor TUnaryOpNode.Create(operator_token: TToken; node: TNode); begin
  nd := node;
  op := operator_token;
end;

function TNumberNode.IsInt(): Boolean; begin
  Result := tok.IsInt();
end;
function TNumberNode.IsFloat(): Boolean; begin
  Result := tok.IsFloat();
end;

function TNumberNode.GetInt(): Integer; begin
  Result := tok.GetInt();
end;
function TNumberNode.GetFloat(): Real; begin
  Result := tok.GetFloat();
end;
function TNumberNode.GetPosition(): TPosition; begin
  Result := tok.GetPosition();
end;

function TNode.Repr(): string; begin
  Repr := 'NULL';
end;
function TNumberNode.Repr(): string; begin
  Repr := tok.Repr();
end;
function TBinOpNode.Repr(): string; begin
  Repr := Concat('(', left.Repr(), ', ', op.Repr(), ', ', right.Repr(), ')');
end;
function TUnaryOpNode.Repr(): string; begin
  Repr := Concat('(', op.Repr(), ', ', nd.Repr(), ')');
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
function TParser.Factor(): TParseResult;
var
  tkn: TToken;
begin
  Factor := Default(TParseResult);

  if current_tok.IsEOF() then begin
    Factor.error := TInvalidSyntaxError.Create(current_tok.GetPosition(), 'Expression expected');
    Exit;
  end;

  if current_tok.IsInt() or current_tok.IsFloat() then begin
    Factor.node := TNumberNode.Create(current_tok);
    Advance();
  end else if current_tok.IsPlus() or current_tok.IsMinus() then begin
    tkn := current_tok;
    Advance();
    Factor := Factor();
    if Factor.error <> nil then Exit;
    Factor.node := TUnaryOpNode.Create(tkn, Factor.node);
  end else if current_tok.IsLParen() then begin
    Advance();
    Factor := Expr();
    if not current_tok.IsRParen() then begin
      Factor.node := nil;
      Factor.error := TInvalidSyntaxError.Create(current_tok.GetPosition(), 'Expected '')''');
    end;
    Advance();
  end else
    Factor.error := TInvalidSyntaxError.Create(current_tok.GetPosition(), 'Expression expected');
end;

function TParser.BinOp(func: TNodeFunction; ops: TTokenKindArray): TParseResult;
var
  right: TParseResult;
  op: TToken;
begin
  BinOp := func();
  while (BinOp.error = nil) and (current_tok <> nil) and current_tok.IsOfKind(ops) do begin
    op := current_tok;
    Advance();
    right := func();
    BinOp.node := TBinOpNode.Create(BinOp.node, op, right.node);
    if right.error <> nil then
      BinOp := right;
  end;
end;
function TParser.Term(): TParseResult;
var
  ops: TTokenKindArray = (TT_MUL, TT_DIV);
begin
  Term := BinOp(@Factor, ops);
end;
function TParser.Expr(): TParseResult;
var
  ops: TTokenKindArray = (TT_PLUS, TT_MINUS);
begin
  Expr := BinOp(@Term, ops);
end;
function TParser.Parse(): TParseResult; begin
  Parse := Expr();
  if (Parse.error = nil) and (not current_tok.IsEOF()) then begin
    Parse.node := nil;
    Parse.error := TInvalidSyntaxError.Create(current_tok.GetPosition(), 'Operator expected');
  end;
end;

end.

