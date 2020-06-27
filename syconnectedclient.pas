unit syconnectedclient;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, blcksock, synautil, synsock, ssl_openssl, sylogevent, sha1, base64, websocketframe, httpheader;

const
  TIMEOUT = 10000;
  ANSWER_STRING = 'It''s sy Websocket Server';

type

  TOnClientTextMessage = procedure(Sender: TObject; Message: string) of object;
  TOnClientCloseConnect = procedure(Sender: TObject; Reason: integer; Message: string) of object;
  //  TOnClientBinaryMessage = procedure(Sender: TObject; BinData: TDynamicByteArray) of object;


  { TsyConnectedClient }

  TsyConnectedClient = class(TThread)
  private
    FCritSection: TRTLCriticalSection;
    FOnClientPing: TOnClientTextMessage;
    FTerminateEvent: PRTLEvent;
    FSock: TTCPBlockSocket;
    FWebSocket: boolean;
    FHandShake: boolean;
    FOnClientTextMessage: TOnClientTextMessage;
    //    FOnClientBinaryData: TOnClientBinaryMessage;
    FOnClientClose: TOnClientCloseConnect;
    FTag: integer;
    FCookie: string;
    //FBaseFrame: TBaseFrame;
    FWebsocketFrame: TWebsocketFrame;
    function MyEncodeBase64(sha1: TSHA1Digest): string;
    procedure Execute; override;
    procedure OnMonitor(Sender: TObject; Writing: boolean; const Buffer: TMemory; Len: integer);
    procedure OnStatus(Sender: TObject; Reason: THookSocketReason; const Value: string);
    function IsWebSocketConnect(AHeader: TStringList): boolean;
    procedure SendHTTPAnswer;
    procedure SendHandShake(ASecWebSocketKey: string);
    function GetWebSocketKey(AHeader: TStringList): string;
    procedure DataProcessing(AWebsocketFrame: TWebsocketFrame);

  public
    constructor Create(hSock: TSocket);
    destructor Destroy; override;
    procedure TerminateThread;

    // комманды отправки
    procedure SendCloseFrame(AReason: integer; AMessage: string);
    procedure SendMessageFrame(AMessage: string);
    //    procedure SendBinaryFrame(ABinData: TDynamicByteArray);
    procedure SendPong(AMessage: string);

    property OnClientTextMessage: TOnClientTextMessage read FOnClientTextMessage write FOnClientTextMessage;
    //  property OnClientBinaryData: TOnClientBinaryMessage read FOnClientBinaryData write FOnClientBinaryData;
    property OnClientClose: TOnClientCloseConnect read FOnClientClose write FOnClientClose;
    property OnClientPing: TOnClientTextMessage read FOnClientPing write FOnClientPing;
    property Tag: integer read FTag write FTag;
  end;

implementation


{ TsyConnectedClient }


function TsyConnectedClient.MyEncodeBase64(sha1: TSHA1Digest): string;
var
  i: integer;
  DecodedStream: TStringStream;
  EncodedStream: TStringStream;
  Encoder: TBase64EncodingStream;
  Output: string;
begin
  EncodedStream := TStringStream.Create('');
  try
    Encoder := TBase64EncodingStream.Create(EncodedStream);
    try
      encoder.WriteBuffer(sha1, length(sha1));
      Encoder.Flush;
      Output := EncodedStream.DataString;
    finally
      FreeAndNil(Encoder);
    end;
  finally
    FreeAndNil(EncodedStream);
  end;
  Result := Output;
end;

procedure TsyConnectedClient.Execute;
var
  s: string;
  HTTPRec: THTTPRecord;
  Header: TStringList;
  //  HeaderBuf: array [0..1000] of byte;

begin
  FSock.OnMonitor := @OnMonitor;
  FSock.OnStatus := @OnStatus;
  FWebSocket := False;
  FHandShake := False;
  s := FSock.RecvString(TIMEOUT);
  sylog.Info(FSock.LastErrorDesc);

  if FSock.LastError <> 0 then
  begin
    syLog.Info(FSock.LastErrorDesc);
    exit;
  end;

  HTTPRec.Parse(s);
  // if not HTTP request then close connection
  if HTTPRec.Protocol <> 'HTTP/1.1' then
    exit;

  // read header
  Header := TStringList.Create;
  try
    repeat
      s := FSock.RecvString(TIMEOUT);
      Header.Add(s);
    until s = '';
    FWebSocket := IsWebSocketConnect(Header);

    if FWebSocket then
    begin
      if not FHandShake then
      begin
        // Handshake with client
        SendHandShake(GetWebSocketKey(Header));
        FHandShake := True;
        syLog.Info('WebSocket Connection');
        // loop for read buffer
      end;
    end
    else
    begin
      // Send Answer to browser
      Sylog.Info('HTTP Connection');
      SendHTTPAnswer;
      exit;
    end;
  finally
    FreeAndNil(Header);
  end;
  if FHandShake then
  begin
    FWebsocketFrame := TWebsocketFrame.Create(FSock);
    try
      // Websocket Loop
      while not Terminated do
      begin
        // get data from websocket
        FWebsocketFrame.Start;
        if FSock.LastError <> 0 then
          exit;
        // manipulate with Data
        DataProcessing(FWebsocketFrame);
      end;
    finally
      FreeAndNil(FWebsocketFrame);
    end;
  end;
  TerminateThread;
end;

procedure TsyConnectedClient.OnMonitor(Sender: TObject; Writing: boolean; const Buffer: TMemory; Len: integer);
begin
  if FHandShake then
  begin
    if Terminated then
      exit;
    if Writing then
      exit;

    // Необходимо вычислить полную длину приходящего пакета
    syLog.Info('OnMonitor: ' + IntToStr(FSock.LastError));
   {
    try
      FBaseFrame.Parse(Buffer, Len);
      if FBaseFrame.Ready then
        case FBaseFrame.Opcode of
          optText: // if Text then send OnClientTextMessage event to parent Thread about new Text message;
            if Assigned(OnClientTextMessage) then
              OnClientTextMessage(Self, FBaseFrame.MessageStr);
          optCloseConnect: // if Close send OnClientClose to parent Thread about Close connection
          begin
            if Assigned(OnClientClose) then
              OnClientClose(Self, FBaseFrame.Reason, FBaseFrame.MessageStr);
          end;
          optBinary:
          begin
            if assigned(OnClientBinaryData) then
              OnClientBinaryData(Self, FBaseFrame.BinaryData);
            syLog.Warning('OnMonitor Binary: ');
          end;
          optPing:
          begin
            if length(FBaseFrame.MessageStr) > 125 then
            begin
              SendCloseFrame(1002, '');
              TerminateThread;
              Exit;
            end;
            if Assigned(OnClientPing) then
              OnClientPing(Self, FBaseFrame.MessageStr);
            syLog.Warning('OnMonitor Ping: ');
            SendPong(FBaseFrame.MessageStr);
          end;
          optPong:
          begin
            SendCloseFrame(1000, '');
          end;
        end;
    except
    end;
       }
  end;
end;


function TsyConnectedClient.IsWebSocketConnect(AHeader: TStringList): boolean;
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

procedure TsyConnectedClient.SendHTTPAnswer;
var
  AnswerStr: string;
begin
  AnswerStr := ANSWER_STRING;
  FSock.SendString('HTTP/1.0 200' + CRLF);
  FSock.SendString('Content-type: text/html');
  FSock.SendString('Content-length: ' + IntToStr(Length(AnswerStr)) + CRLF);
  FSock.SendString('Date: ' + Rfc822DateTime(now) + CRLF);
  FSock.SendString('Connection: close' + CRLF);
  FSock.SendString('Server: syWebsocket Server' + CRLF);
  FSock.SendString('' + CRLF);
  FSock.SendString(AnswerStr);
end;

procedure TsyConnectedClient.SendHandShake(ASecWebSocketKey: string);
var
  wsValue: string;
  sendStr: string;
begin

  sendstr := 'HTTP/1.1 101 Web Socket Protocol Handshake' + CRLF;
  sendstr := sendstr + 'Server: syWebsocket Server' + CRLF;
  sendstr := sendstr + 'X-Powered-By: syWebSocket Server' + CRLF;
  sendstr := sendstr + 'Connection: Upgrade' + CRLF;



  wsValue := ASecWebSocketKey + '258EAFA5-E914-47DA-95CA-C5AB0DC85B11';
  wsvalue := MyEncodeBase64(SHA1String(wsValue));

  sendstr := sendstr + 'Sec-WebSocket-Accept: ' + wsValue + CRLF;
  sendstr := sendstr + 'Upgrade: websocket' + CRLF;
  FSock.SendString(sendStr + CRLF);
end;


function TsyConnectedClient.GetWebSocketKey(AHeader: TStringList): string;
var
  s: string;
  headerKey, headerValue: string;
begin
  Result := '';
  for s in AHeader do
  begin
    headerValue := s;
    headerKey := Fetch(headerValue, ':');
    if headerKey = 'Sec-WebSocket-Key' then
    begin
      Result := headerValue;
      Exit;
    end;
  end;
end;

procedure TsyConnectedClient.DataProcessing(AWebsocketFrame: TWebsocketFrame);
begin
  case AWebsocketFrame.Opcode of
    optText:
    begin
      if Assigned(OnClientTextMessage) then
        OnClientTextMessage(Self, AWebsocketFrame.MessageStr);
    end;
  end;
end;

constructor TsyConnectedClient.Create(hSock: TSocket);
begin
  syLog.Info('Start Client Thread');
  InitCriticalSection(FCritSection);
  FTerminateEvent := RTLEventCreate;
  FSock := TTCPBlockSocket.Create;
  FSock.Socket := hSock;
  FreeOnTerminate := True;
  inherited Create(True);
end;

destructor TsyConnectedClient.Destroy;
begin
  FreeAndNil(FSock);
  RTLeventdestroy(FTerminateEvent);
  DoneCriticalsection(FCritSection);
  inherited Destroy;
end;

procedure TsyConnectedClient.TerminateThread;
begin
  if Terminated then
    exit;
  FSock.CloseSocket;
  Terminate;
  RTLeventSetEvent(FTerminateEvent);
end;

procedure TsyConnectedClient.SendCloseFrame(AReason: integer; AMessage: string);
var
  WFrame: TWebsocketFrame;
  len: integer;
begin
  syLog.Info('Send Close Frame');
  ;
  EnterCriticalsection(FCritSection);
  try
    len := Length(AMessage);
    WFrame := TWebsocketFrame.Create;
    try
      WFrame.Opcode := optCloseConnect;
      WFrame.Mask := False;
      WFrame.Reason := AReason;
      WFrame.MessageStr := AMessage;
      if FSock.CanWrite(100) then
        FSock.SendBuffer(WFrame.SendData, WFrame.FrameSize);
    finally
      FreeAndNil(WFrame);
    end;
  finally
    LeaveCriticalsection(FCritSection);
  end;
  TerminateThread;
end;

procedure TsyConnectedClient.SendMessageFrame(AMessage: string);
var
  WFrame: TWebsocketFrame;
  len: integer;
begin
  syLog.Info('Send Message');
  EnterCriticalsection(FCritSection);
  try
    len := Length(AMessage);
    WFrame := TWebsocketFrame.Create;
    try
      WFrame.Opcode := optText;
      WFrame.Mask := False;
      WFrame.MessageStr := AMessage;
      if FSock.CanWrite(100) then
        len := FSock.SendBuffer(WFrame.SendData, WFrame.FrameSize);
      len := 0;
    finally
      FreeAndNil(WFrame);
    end;
  finally
    LeaveCriticalsection(FCritSection);
  end;
end;

{procedure TsyConnectedClient.SendBinaryFrame(ABinData: TDynamicByteArray);
var
  BaseFrame: TBaseFrame;
  len: integer;
  dt: TDynamicByteArray;
begin
  EnterCriticalsection(FCritSection);
  try
    len := Length(ABinData);
    BaseFrame := TBaseFrame.Create;
    try
      BaseFrame.Fin := True;
      BaseFrame.Opcode := optBinary;
      BaseFrame.Mask := False;
      BaseFrame.BinaryData := ABinData;
      dt := BaseFrame.SendData;
      len := Length(dt);
      FSock.SendBuffer(@dt[0], len - 1);
    finally
      FreeAndNil(BaseFrame);
    end;
  finally
    LeaveCriticalsection(FCritSection);
  end;
end;
}

procedure TsyConnectedClient.SendPong(AMessage: string);
///var
//  BaseFrame: TBaseFrame;
//  len: integer;
//  dt: TDynamicByteArray;
begin
{  EnterCriticalsection(FCritSection);
  try
    len := Length(AMessage);
    BaseFrame := TBaseFrame.Create;
    try
      BaseFrame.Fin := True;
      BaseFrame.Opcode := optPong;
      BaseFrame.Mask := False;
      BaseFrame.MessageStr := AMessage;
      dt := BaseFrame.SendData;
      len := Length(dt);
      FSock.SendBuffer(@dt[0], len - 1);
    finally
      FreeAndNil(BaseFrame);
    end;
  finally
    LeaveCriticalsection(FCritSection);
  end;   }
end;

procedure TsyConnectedClient.OnStatus(Sender: TObject; Reason: THookSocketReason; const Value: string);
begin
  case Reason of
    HR_ResolvingBegin:
      syLog.Info('HR_ResolvingBegin');
    HR_ResolvingEnd:
      syLog.Info('HR_ResolvingEnd');
    HR_SocketCreate:
      syLog.Info('HR_SocketCreate');
    HR_SocketClose:
      syLog.Info('HR_SocketClose');
    HR_Bind:
      syLog.Info('HR_Bind');
    HR_Connect:
      syLog.Info('HR_Connect');
    HR_CanRead:
      syLog.Info('HR_CanRead');
    HR_CanWrite:
      syLog.Info('HR_CanWrite');
    HR_Listen:
      syLog.Info('HR_Listen');
    HR_Accept:
      syLog.Info('HR_Accept');
    HR_ReadCount:
      syLog.Info('HR_ReadCount ' + Value);
    HR_WriteCount:
      syLog.Info('HR_WriteCount');
    HR_Wait:
      syLog.Info('HR_Wait');
    HR_Error:
      syLog.Info('HR_Error: ' + Value);
  end;
end;


end.





