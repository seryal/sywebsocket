unit httpheader;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, synautil;

type

  { THTTPRecord }

  THTTPRecord = object
    Method: string;
    Uri: string;
    Protocol: string;
    procedure Parse(AValue: string);
  end;

implementation

{ THTTPRecord }

procedure THTTPRecord.Parse(AValue: string);
var
  s: string;
begin
  s := AValue;
  if s = '' then
    exit;
  method := fetch(s, ' ');
  if method = '' then
    Exit;
  uri := fetch(s, ' ');
  if uri = '' then
    Exit;
  protocol := fetch(s, ' ');
end;

end.
