unit Parser;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Token, LangErrors;

type
  TNode = class
    public
      function Repr(): string; virtual;
  end;

  TNumberNode = class(TNode)
    private
      tok: TToken;
    public
      constructor Create(t: TToken);
      function Repr(): string; override;
  end;

  TBinOpNode = class(TNode)
    private
      left, right: TNode;
      op: TToken;
    public
      constructor Create(left_node: TNode; operator_token: TToken; right_node: TNode);
      function Repr(): string; override;
  end;

  TParseResult = record
    node: TNode;
    error: TError;
  end;

  TNodeFunction = function:TNode of object;

  TParser = class
    private
      tokens: TTokenArray;
      tok_idx: Cardinal;
      current_tok: TToken;
    public
      constructor Create(tkns: TTokenArray);
      procedure Advance();
      function Factor(): TNode;
      function BinOp(func: TNodeFunction; ops: TTokenKindArray): TNode;
      function Term(): TNode;
      function Expr(): TNode;
      function Parse(): TNode;
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
function TNode.Repr(): string; begin
  Repr := 'NULL';
end;
function TNumberNode.Repr(): string; begin
  Repr := tok.Repr();
end;
function TBinOpNode.Repr(): string; begin
  Repr := Concat('(', left.Repr(), ', ', op.Repr(), ', ', right.Repr(), ')');
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
function TParser.Factor(): TNode; begin
  if (current_tok <> nil) and (current_tok.IsInt() or current_tok.IsFloat()) then begin
    Factor := TNumberNode.Create(current_tok);
    Advance();
  end;
end;

function TParser.BinOp(func: TNodeFunction; ops: TTokenKindArray): TNode;
var
  left, right: TNode;
  op: TToken;
begin
  left := func();
  while (current_tok <> nil) and current_tok.IsOfKind(ops) do begin
    op := current_tok;
    Advance();
    right := func();
    left := TBinOpNode.Create(left, op, right);
  end;
  BinOp := left;
end;
function TParser.Term(): TNode;
var
  ops: TTokenKindArray = (TT_MUL, TT_DIV);
begin
  Term := BinOp(@Factor, ops);
end;
function TParser.Expr(): TNode;
var
  ops: TTokenKindArray = (TT_PLUS, TT_MINUS);
begin
  Expr := BinOp(@Term, ops);
end;
function TParser.Parse(): TNode; begin
  Parse := Expr();
end;

end.

