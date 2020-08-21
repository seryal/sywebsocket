unit sywebsocketclient;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, blcksock, base64, sywebsocketcommon, sywebsocketpackmanager, sywebsocketframe;

type

  { TsyWebsocketClient }

  TsyWebsocketClient = class(TThread)
  private
    FHost: string;
    FOnConnected: TNotifyEvent;
    FOnMessage: TNotifyEvent;
    FPort: word;
    FCritSection: TRTLCriticalSection;
    FTerminateEvent: PRTLEvent;
    FSock: TTCPBlockSocket;
    FSecKey: string;
    FWebSocket: boolean;
    FWebsocketFrame: TsyWebsockPackManager;
    FMessageQueue: TMessageQueue;
    procedure Execute; override;
    procedure OnStatus(Sender: TObject; Reason: THookSocketReason; const Value: string);
    procedure SendHandshake;
    procedure MessageNotify;
    procedure DoConnected;
  public
    constructor Create(AHost: string; APort: word);
    destructor Destroy; override;
    property OnMessage: TNotifyEvent read FOnMessage write FOnMessage;
    property OnConnected: TNotifyEvent read FOnConnected write FOnConnected;
    property MessageQueue: TMessageQueue read FMessageQueue;
    procedure SendMessage(AValue: string);
    procedure TerminateThread;
  end;

implementation

{ TsyWebsocketClient }

procedure TsyWebsocketClient.Execute;
var
  str: string;
  Header: TStringList;
  DataLen: integer;
  DataBuffer: TBytes;
  RcvLen: integer;
  RcvFrame: TMemoryStream;
  wsFrame: TsyBaseWebsocketFrame;
  MsgRec: TMessageRecord;
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
  // start websocket protocol
  try
    Queue(@DoConnected);

    FWebsocketFrame := TsyWebsockPackManager.Create;
    while not Terminated do
    begin
      if FSock.CanRead(1000) then
      begin
        DataLen := FSock.WaitingData;
        if DataLen = 0 then
          exit;
        SetLength(DataBuffer, DataLen);
        RcvLen := FSock.RecvBuffer(@DataBuffer[0], DataLen);
        if RcvLen <> DataLen then // need raise exception
          Exit;
        FWebsocketFrame.InsertData(DataBuffer, RcvLen);

        while FWebsocketFrame.Count > 0 do
        begin
          RcvFrame := FWebsocketFrame.Pop;
          wsFrame := TsyBaseWebsocketFrame.Create;
          try
            wsFrame.Frame := RcvFrame;
            MsgRec.Opcode := wsFrame.OpCode;
            MsgRec.Reason := wsFrame.Reason;
            MsgRec.Sender := self;
            MsgRec.BinaryData := wsFrame.Binary;
            MsgRec.Message := wsFrame.MessageStr;
            FMessageQueue.PushItem(MsgRec);
            Synchronize(@MessageNotify);
          finally
            FreeAndNil(wsFrame);
          end;

        end;

      end;
      //RTLeventWaitFor(FTerminateEvent, 1000);
    end;

  finally
    FreeAndNil(FWebsocketFrame);
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

procedure TsyWebsocketClient.MessageNotify;
begin
  if not Terminated then
    if Assigned(OnMessage) then
      OnMessage(Self);
end;

procedure TsyWebsocketClient.DoConnected;
begin
  if Assigned(OnConnected) then
    OnConnected(Self);
end;

constructor TsyWebsocketClient.Create(AHost: string; APort: word);
begin
  FHost := AHost;
  FPort := APort;
  InitCriticalSection(FCritSection);
  FMessageQueue := TMessageQueue.Create;
  FSock := TTCPBlockSocket.Create;
  FTerminateEvent := RTLEventCreate;
  FreeOnTerminate := True;
  inherited Create(True);
end;

destructor TsyWebsocketClient.Destroy;
begin
  RTLeventdestroy(FTerminateEvent);
  FreeAndNil(FMessageQueue);
  FreeAndNil(FSock);
  DoneCriticalsection(FCritSection);
  inherited Destroy;
end;

procedure TsyWebsocketClient.SendMessage(AValue: string);
var
  WFrame: TsyBaseWebsocketFrame;
begin
  EnterCriticalsection(FCritSection);
  try
    WFrame := TsyBaseWebsocketFrame.Create;
    try
      WFrame.Opcode := optText;
      WFrame.Mask := True;
      WFrame.MessageStr := AValue;
      if FSock.CanWrite(1000) then
        FSock.SendBuffer(WFrame.Frame.Memory, WFrame.Frame.Size);
    finally
      FreeAndNil(WFrame);
    end;
  finally
    LeaveCriticalsection(FCritSection);
  end;
end;

procedure TsyWebsocketClient.TerminateThread;
begin
  if Terminated then
    exit;
  FSock.AbortSocket;
  Terminate;
  RTLeventSetEvent(FTerminateEvent);
end;

end.






