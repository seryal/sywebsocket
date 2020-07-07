unit sycommon;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, synautil;

function IsWebSocketConnect(AHeader: TStringList): boolean;
function IsValidUTF8(AValue: PChar; ALen: integer): boolean;


implementation

function IsWebSocketConnect(AHeader: TStringList): boolean;
var
  s: string;
  headerKey, headerValue: string;
begin
  Result := False;
  for s in AHeader do
  begin
    headerValue := s;
    headerKey := Fetch(headerValue, ':');
    if (LowerCase(headerKey) = 'upgrade') and (LowerCase(headerValue) = 'websocket') then
    begin
      Result := True;
      Exit;
    end;
  end;
end;

///////////////////////////////////////////////////
{
 +----------+----------+----------+----------+
 | $00..$7F |          |          |          |
 | $C2..$DF | $80..$BF |          |          |
 | $E0      | $A0..$BF | $80..$BF |          |
 | $E1..$EC | $80..$BF | $80..$BF |          |
 | $ED      | $80..$9F | $80..$BF |          |
 | $EE..$EF | $80..$BF | $80..$BF |          |
 | $F0      | $90..$BF | $80..$BF | $80..$BF |
 | $F1..$F3 | $80..$BF | $80..$BF | $80..$BF |
 | $F4      | $80..$8F | $80..$BF | $80..$BF |
 +----------+----------+----------+----------+
}
///////////////////////////////////////////////////
function IsValidUTF8(AValue: PChar; ALen: integer): boolean;
var
  i, len, n, j: integer;
  c: ^byte;
begin
  Result := False;
  len := ALen;
  i := 0;
  c := @AValue[0];
  while i < len do
  begin
    if (c^ >= $00) and (c^ <= $7f) then
      n := 0
    else if (c^ >= $c2) and (c^ <= $df) then
      n := 1
    else if (c^ = $e0) then
      n := 2
    else if (c^ >= $e1) and (c^ <= $ec) then
      n := 2
    else if (c^ = $ed) then
      n := 2
    else if (c^ >= $ee) and (c^ <= $ef) then
      n := 2
    else if (c^ = $f0) then
      n := 3
    else if (c^ >= $f1) and (c^ <= $f3) then
      n := 3
    else if (c^ = $f4) then
      n := 3
    else
      exit;

    j := 0;
    Inc(i);

    while j < n do
    begin
      if i >= len then
        exit;
      case c^ of
        $c2..$df, $e1..$ec, $ee..$ef, $f1..$f3:
          if not (((c + 1)^ >= $80) and ((c + 1)^ <= $bf)) then
            exit;
        $e0:
          if not (((c + 1)^ >= $a0) and ((c + 1)^ <= $bf)) then
            exit;
        $ed:
          if not (((c + 1)^ >= $80) and ((c + 1)^ <= $9f)) then
            exit;
        $f0:
          if not (((c + 1)^ >= $90) and ((c + 1)^ <= $bf)) then
            exit;
        $f4:
          if not (((c + 1)^ >= $80) and ((c + 1)^ <= $8f)) then
            exit;
        $80..$bf:
          if not (((c + 1)^ >= $80) and ((c + 1)^ <= $bf)) then
            exit;
      end;
      Inc(c);
      Inc(i);
      Inc(j);
    end;
    Inc(c);
  end;
  Result := True;
end;

end.

