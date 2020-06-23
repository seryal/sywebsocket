unit syconnectedclient;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, blcksock, synautil, synsock, ssl_openssl, sylogevent, sha1, base64, baseframe;

const
  TIMEOUT = 10000;
  ANSWER_STRING = 'It''s Sy Websocket Server';

type

  TOnTextMessage = procedure(Sender: TObject; Message: string) of object;
  TOnCloseConnect = procedure(Sender: TObject; Reason: integer; Message: string) of object;


  { TsyConnectedClient }

  TsyConnectedClient = class(TThread)
  private
    FCritSection: TRTLCriticalSection;
    FTerminateEvent: PRTLEvent;
    FSock: TTCPBlockSocket;
    FWebSocket: boolean;
    FHandShake: boolean;
    FOnTextMessage: TOnTextMessage;
    FOnClose: TOnCloseConnect;
    FTag: integer;
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

    property OnTextMessage: TOnTextMessage read FOnTextMessage write FOnTextMessage;
    property OnClose: TOnCloseConnect read FOnClose write FOnClose;
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
begin
  FSock.OnMonitor := @OnMonitor;
  FSock.OnStatus := @OnStatus;
  FWebSocket := False;
  FHandShake := False;
  while not Terminated do
  begin
    s := FSock.RecvString(TIMEOUT);
    // get http header
    sylog.Info(FSock.LastErrorDesc);

    if (FSock.LastError = WSAETIMEDOUT) or (FSock.LastError = 0) then
    begin
      syLog.Info(FSock.LastErrorDesc);
    end
    else
    begin
      TerminateThread;
      exit;

    end;
    if s = '' then
      Continue;
    HTTPRec.Parse(s);

    // if not HTTP request then close connection
    if HTTPRec.Protocol <> 'HTTP/1.1' then
      exit;

    // read header
    Header := TStringList.Create;
    try
      if not FWebSocket then
      begin
        repeat
          s := FSock.RecvString(TIMEOUT);
          Header.Add(s);
        until s = '';
        FWebSocket := IsWebSocketConnect(Header);
      end;
      if FWebSocket then
      begin
        if not FHandShake then
        begin
          SendHandShake(GetWebSocketKey(Header));
          FHandShake := True;
        end;
        // Handshake with client
        syLog.Info('WebSocket Connection');
      end
      else
      begin
        // Send Answer to browser
        Sylog.Info('HTTP Connection');
        SendHTTPAnswer;
        TerminateThread;
      end;
      syLog.Log(Header.Text);
    finally
      FreeAndNil(Header);
    end;
  end;
  TerminateThread;
end;

procedure TsyConnectedClient.OnMonitor(Sender: TObject; Writing: boolean; const Buffer: TMemory; Len: integer);
var
  BaseFrame: TBaseFrame;
begin
  if FHandShake then
  begin
    if Writing then
      exit;
    BaseFrame := TBaseFrame.Create;
    try
      BaseFrame.Parse(Buffer, Len);
      case BaseFrame.Opcode of
        optText: // if Text then send OnTextMessage event to parent Thread about new Text message;
          if Assigned(OnTextMessage) then
            OnTextMessage(Self, BaseFrame.MessageStr);
        optCloseConnect: // if Close send OnCloseMessage to parent Thread about Close message
        begin
          if Assigned(OnClose) then
            OnClose(Self, BaseFrame.Reason, BaseFrame.MessageStr);
        end;
      end;
    finally
      FreeAndNil(BaseFrame);
    end;
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
    if (headerKey = 'Upgrade') and (headerValue = 'websocket') then
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
  FSock.SendString('Server: SyWebsocket Server' + CRLF);
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
  BaseFrame: TBaseFrame;
  len: integer;
  dt: TDynamicByteArray;
begin
  EnterCriticalsection(FCritSection);
  try
    BaseFrame := TBaseFrame.Create;
    try
      BaseFrame.Fin := True;
      BaseFrame.Opcode := optCloseConnect;
      BaseFrame.Mask := False;
      BaseFrame.MessageStr := AMessage;
      BaseFrame.Reason := AReason;
      dt := BaseFrame.SendData;
      len := Length(dt);
      //FSock.SendBuffer(@dt[0], len);
    finally
      FreeAndNil(BaseFrame);
    end
  finally
    LeaveCriticalsection(FCritSection);
  end;
end;

procedure TsyConnectedClient.SendMessageFrame(AMessage: string);
var
  BaseFrame: TBaseFrame;
  len: integer;
  dt: TDynamicByteArray;
begin
  EnterCriticalsection(FCritSection);
  try

    BaseFrame := TBaseFrame.Create;
    try
      BaseFrame.Fin := True;
      BaseFrame.Opcode := optText;
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
  end;
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
      syLog.Info('HR_ReadCount');
    HR_WriteCount:
      syLog.Info('HR_WriteCount');
    HR_Wait:
      syLog.Info('HR_Wait');
    HR_Error:
      syLog.Info('HR_Error: ' + Value);
  end;
end;


end.





