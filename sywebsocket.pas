{ This file was automatically created by Lazarus. Do not edit!
  This source is only used to compile and install the package.
 }

unit syWebSocket;

{$warn 5023 off : no warning about unused units}
interface

uses
  httpheader, syconnectedclient, syWebSocketServer, websocketframe, 
  websocketmessage, websocpackmanager, LazarusPackageIntf;

implementation

procedure Register;
begin
end;

initialization
  RegisterPackage('syWebSocket', @Register);
end.
