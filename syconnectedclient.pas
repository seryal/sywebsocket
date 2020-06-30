unit syconnectedclient;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, blcksock, synautil, synsock, ssl_openssl, sylogevent, sha1, base64, websocpackmanager, httpheader, websocketframe;

const
  TIMEOUT = 10000;
  ANSWER_STRING = 'It''s sy Websocket Server';

type

  TOnClientTextMessage = procedure(Sender: TObject; Message: string) of object;
  TOnClientCloseConnect = procedure(Sender: TObject; Reason: integer; Message: string) of object;
  TOnClientBinaryMessage = procedure(Sender: TObject; BinData: TBytes) of object;


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
    FOnClientBinaryData: TOnClientBinaryMessage;
    FOnClientClose: TOnClientCloseConnect;
    FTag: integer;
    FCookie: string;
    FWebsocketFrame: TWebsockPackManager;
    function MyEncodeBase64(sha1: TSHA1Digest): string;
    procedure Execute; override;
    procedure OnMonitor(Sender: TObject; Writing: boolean; const Buffer: TMemory; Len: integer);
    procedure OnStatus(Sender: TObject; Reason: THookSocketReason; const Value: string);
    function IsWebSocketConnect(AHeader: TStringList): boolean;
    procedure SendHTTPAnswer;
    procedure SendHandShake(ASecWebSocketKey: string);
    function GetWebSocketKey(AHeader: TStringList): string;
  public
    constructor Create(hSock: TSocket);
    destructor Destroy; override;
    procedure TerminateThread;

    // комманды отправки
    procedure SendCloseFrame(AReason: integer; AMessage: string);
    procedure SendMessageFrame(AMessage: string);
    procedure SendBinaryFrame(ABinData: TBytes);
    procedure SendPong(AMessage: string);

    property OnClientTextMessage: TOnClientTextMessage read FOnClientTextMessage write FOnClientTextMessage;
    property OnClientBinaryData: TOnClientBinaryMessage read FOnClientBinaryData write FOnClientBinaryData;
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

  DataBuffer: TBytes;
  DataLen: integer;
  RcvLen: integer;
  RcvFrame: TMemoryStream;
  wsMessage: TBaseWebsocketMessage;
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
    FWebsocketFrame := TWebsockPackManager.Create();
    try
      // Websocket Loop
      while not Terminated do
      begin
        // get data from socket
        if FSock.CanRead(1000) then
        begin
          syLog.Info('Read Data');
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
            RcvFrame := FWebsocketFrame.pop;
            wsMessage := TBaseWebsocketMessage.Create;
            wsMessage.Frame := RcvFrame;
            //DataBuffer := wsMessage.Binary;
            case wsMessage.OpCode of
              optText: // if Text then send OnClientTextMessage event to parent Thread about new Text message;
                if Assigned(OnClientTextMessage) then
                  OnClientTextMessage(Self, wsMessage.MessageStr);
              optBinary: // if Text then send OnClientTextMessage event to parent Thread about new Text message;
                if Assigned(OnClientBinaryData) then
                  OnClientBinaryData(Self, wsMessage.Binary);
            end;

            wsMessage.Free;
          end;


          SetLength(DataBuffer, 0);
        end;
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
  WFrame: TBaseWebsocketMessage;
begin
  EnterCriticalsection(FCritSection);
  try
    syLog.Info('Send Close');
    WFrame := TBaseWebsocketMessage.Create;
    try
      WFrame.Opcode := optCloseConnect;
      WFrame.Mask := False;
      WFrame.Reason := AReason;
      WFrame.MessageStr := AMessage;
      if FSock.CanWrite(1000) then
        FSock.SendBuffer(WFrame.Frame.Memory, WFrame.Frame.Size);
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
  WFrame: TBaseWebsocketMessage;
begin
  EnterCriticalsection(FCritSection);
  try
    syLog.Info('Send Message');
    WFrame := TBaseWebsocketMessage.Create;
    try
      WFrame.Opcode := optText;
      WFrame.Mask := False;
      WFrame.MessageStr := AMessage;
      if FSock.CanWrite(1000) then
        FSock.SendBuffer(WFrame.Frame.Memory, WFrame.Frame.Size);
    finally
      FreeAndNil(WFrame);
    end;
  finally
    LeaveCriticalsection(FCritSection);
  end;
end;

procedure TsyConnectedClient.SendBinaryFrame(ABinData: TBytes);
var
  WFrame: TBaseWebsocketMessage;
  len: integer;
  dt: TBytes;
begin
  EnterCriticalsection(FCritSection);
  try
    len := Length(ABinData);
    WFrame := TBaseWebsocketMessage.Create;
    try
      WFrame.Fin := True;
      WFrame.Opcode := optBinary;
      WFrame.Mask := False;

      WFrame.Binary := ABinData;
      if FSock.CanWrite(1000) then
        FSock.SendBuffer(WFrame.Frame.Memory, WFrame.Frame.Size);
    finally
      FreeAndNil(WFrame);
    end;
  finally
    LeaveCriticalsection(FCritSection);
  end;
end;

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
      syLog.Info('HR_CanRead ' + Value);
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
    begin
      syLog.Info('HR_Error: ' + Value);
      TerminateThread;
    end;
  end;
end;


end.




