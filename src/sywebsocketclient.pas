unit sywebsocketclient;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, blcksock, base64, sycommon;

type

  { TsyWebsocketClient }

  TsyWebsocketClient = class(TThread)
  private
    FHost: string;
    FPort: word;
    FCritSection: TRTLCriticalSection;
    FTerminateEvent: PRTLEvent;
    FSock: TTCPBlockSocket;
    FSecKey: string;
    FWebSocket: boolean;
    procedure Execute; override;
    procedure OnStatus(Sender: TObject; Reason: THookSocketReason; const Value: string);
    procedure SendHandshake;
  public
    constructor Create(AHost: string; APort: word);
    destructor Destroy; override;
    procedure TerminateThread;
  end;

implementation

{ TsyWebsocketClient }

procedure TsyWebsocketClient.Execute;
var
  str: string;
  Header: TStringList;

begin
  // connect to server
  FSock.Connect(FHost, IntToStr(FPort));
  FSock.OnStatus := @OnStatus;
  // send HTTP handshake - i'm websocket client
  SendHandshake;
  Header := TStringList.Create;
  try
    // get answer from server
    repeat
      str := FSock.RecvString(5000);
      Header.Add(str);
    until str = '';
    str := Header.Text;
    // websocket server or not?
    FWebSocket := IsWebSocketConnect(Header);
    // if not websoket server then exit
    if not FWebSocket then
      exit;

    // if websocket server then check Secure-Key
//    if not CheckSecureKey(Header) then
//      exit;



  finally
    FreeAndNil(Header);
  end;
  str := '';
  while not Terminated do
  begin
    RTLeventWaitFor(FTerminateEvent, 1000);
  end;
  TerminateThread;
end;

procedure TsyWebsocketClient.OnStatus(Sender: TObject; Reason: THookSocketReason; const Value: string);
begin
  case Reason of
    HR_Error:
      TerminateThread;
  end;
end;

procedure TsyWebsocketClient.SendHandshake;
var
  str: string;
  key: string;
begin
  Randomize;
  str := 'GET / HTTP/1.1' + CRLF;
  str := str + 'Host: ' + FHost + CRLF;
  str := str + 'Upgrade: websocket' + CRLF;
  str := str + 'Connection: Upgrade' + CRLF;
  FSecKey := EncodeStringBase64(IntToHex(Random($7FFFFFFFFFFFFFFF), 16));
  str := str + 'Sec-WebSocket-Key: ' + FSecKey + CRLF;
  str := str + 'Origin: ' + FHost + CRLF;
  str := str + 'Sec-WebSocket-Protocol: chat, superchat' + CRLF;
  str := str + 'Sec-WebSocket-Version: 13' + CRLF;
  FSock.SendString(str + CRLF);
end;

constructor TsyWebsocketClient.Create(AHost: string; APort: word);
begin
  FHost := AHost;
  FPort := APort;
  InitCriticalSection(FCritSection);
  FSock := TTCPBlockSocket.Create;
  FTerminateEvent := RTLEventCreate;
  FreeOnTerminate := True;
  inherited Create(True);
end;

destructor TsyWebsocketClient.Destroy;
begin
  RTLeventdestroy(FTerminateEvent);
  FreeAndNil(FSock);
  DoneCriticalsection(FCritSection);
  inherited Destroy;
end;

procedure TsyWebsocketClient.TerminateThread;
begin
  if Terminated then
    exit;
  Terminate;
  FSock.CloseSocket;
  RTLeventSetEvent(FTerminateEvent);
end;

end.





