unit Position;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;

type
  TPosition = class
    private
      idx: Cardinal;
      ln: Cardinal;
      col: Cardinal;
      fn: ShortString;
      ftxt: AnsiString;
    public
      constructor Create(index, line, column: Cardinal; filename: ShortString; file_text: AnsiString);
      procedure Advance(current_char: Char);
      function Copy(): TPosition;
      function GetIndex(): Cardinal;
      function GetFilename(): ShortString;
      function GetLineNumber(): Cardinal;
  end;

implementation
constructor TPosition.Create(index, line, column: Cardinal; filename: ShortString; file_text: AnsiString); begin
  idx := index;
  ln := line;
  col := column;
  fn := filename;
  ftxt := file_text;
end;
procedure TPosition.Advance(current_char: Char); begin
  inc(idx);
  inc(col);

  if current_char = chr(10) then begin
     inc(ln);
     col := 0;
  end;
end;
function TPosition.Copy(): TPosition; begin
  Copy := TPosition.Create(idx, ln, col, fn, ftxt);
end;
function TPosition.GetIndex(): Cardinal; begin
  GetIndex := idx;
end;
function TPosition.GetFilename(): ShortString; begin
  GetFilename := fn;
end;
function TPosition.GetLineNumber(): Cardinal; begin
  GetLineNumber := ln;
end;
end.

