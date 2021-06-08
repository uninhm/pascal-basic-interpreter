unit LangErrors;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Position;

type
  TError = class
    private
      error_name, details: string;
      pos: TPosition;
    public
      constructor Create(p: TPosition; n: string; d: string);
      function AsStr(): string;
  end;

  TIllegalCharError = class(TError)
    public
      constructor Create(p: TPosition; d: string);
  end;

  TInvalidSyntaxError = class(TError)
    public
      constructor Create(p: TPosition; d: string);
  end;

  TInvalidOperationError = class(TError)
    public
      constructor Create(p: TPosition; d: string);
  end;

implementation
constructor TError.Create(p: TPosition; n: string; d: string); begin
  pos := p;
  error_name := n;
  details := d;
end;
function TError.AsStr(): string; begin
  AsStr := 'Error';
  AsStr := Concat('File ', pos.GetFilename(), ', line ', IntToStr(pos.GetLineNumber()), ', column ', IntToStr(pos.GetColumn()), ':');
  AsStr := Concat(AsStr, chr(10), error_name, ': ', details);
end;

constructor TIllegalCharError.Create(p: TPosition; d: string); begin
  inherited Create(p, 'Illegal Character', d);
end;

constructor TInvalidSyntaxError.Create(p: TPosition; d: string); begin
  inherited Create(p, 'Invalid Syntax', d);
end;

constructor TInvalidOperationError.Create(p: TPosition; d: string); begin
  inherited Create(p, 'Invalid Operation', d);
end;

end.

