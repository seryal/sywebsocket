unit sycommon;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, synautil;

function IsWebSocketConnect(AHeader: TStringList): boolean;


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

end.

