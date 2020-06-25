unit syWebSocketServer;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, blcksock, synsock, syconnectedclient, Generics.Collections, sylogevent, lazCollections, baseframe;

type

  TLockedClientList = class(specialize TThreadList<TsyConnectedClient>);
  TClientList = specialize TList<TsyConnectedClient>;

  TMessageRecord = record
    Opcode: TOpcodeType;
    Reason: integer;
    Message: string;
    BinaryData: TDynamicByteArray;
    Sender: TsyConnectedClient;
  end;

  TMessageQueue = specialize TLazThreadedQueue<TMessageRecord>;


  { TsyWebSocketServer }
  TsyWebSocketServer = class(TThread)
  private
    FClientCount: integer;
    FSock: TTCPBlockSocket;
    FPort: integer;
    FLockedClientList: TLockedClientList;
    // Messages from client
    FMessageQueue: TMessageQueue;
    FOnTextMessage: TNotifyEvent;
    FOnBinData: TNotifyEvent;
    FOnCloseConnection: TNotifyEvent;
    procedure OnClientBinaryData(Sender: TObject; BinData: TDynamicByteArray);
    procedure OnClientClose(Sender: TObject; Reason: integer; Message: string);
    procedure OnClientTextMessage(Sender: TObject; Message: string);
    procedure OnTerminate(Sender: TObject);

    procedure TextMessageNotify;
    procedure CloseConnectionNotify;
    procedure BinDataNotify;
  public
    constructor Create(APort: integer);
    destructor Destroy; override;
    procedure Execute; override;
    property OnTextMessage: TNotifyEvent read FOnTextMessage write FOnTextMessage;
    property OnCloseConnection: TNotifyEvent read FOnCloseConnection write FOnCloseConnection;
    property OnBinData: TNotifyEvent read FOnBinData write FOnBinData;
    property MessageQueue: TMessageQueue read FMessageQueue;
    property LockedClientList: TLockedClientList read FLockedClientList;
  end;

implementation

{ TsyWebSocketServer }

procedure TsyWebSocketServer.OnTerminate(Sender: TObject);
var
  List: TClientList;
begin
  syLog.Info('Client Thread Terminated');
  if Terminated then
    Exit;
  if not Assigned(FLockedClientList) then
    exit;
  list := FLockedClientList.LockList;
  try
    list.Remove(TsyConnectedClient(Sender));
  finally
    FLockedClientList.UnlockList;
  end;
end;

procedure TsyWebSocketServer.TextMessageNotify;
begin
  if Terminated then
    exit;
  if Assigned(OnTextMessage) then
    OnTextMessage(self);
end;

procedure TsyWebSocketServer.CloseConnectionNotify;
begin
  if Terminated then
    exit;
  if Assigned(OnCloseConnection) then
    OnCloseConnection(self);
end;

procedure TsyWebSocketServer.BinDataNotify;
begin
  if Terminated then
    exit;
  if Assigned(OnBinData) then
    OnBinData(self);

end;

procedure TsyWebSocketServer.OnClientTextMessage(Sender: TObject; Message: string);
var
  MsgRec: TMessageRecord;
begin
  // add message to Queue
  if not (Sender is TsyConnectedClient) then
    exit;
  MsgRec.Message := Message;
  MsgRec.Sender := TsyConnectedClient(Sender);
  MsgRec.Opcode := optText;
  MsgRec.Reason := 0;
  FMessageQueue.PushItem(MsgRec);

  // send event to MainProgram about new Text Message
  // The client must read the data from the queue FMessageQueue;
  Queue(@TextMessageNotify);
end;

procedure TsyWebSocketServer.OnClientClose(Sender: TObject; Reason: integer; Message: string);
var
  MsgRec: TMessageRecord;
begin
  syLog.Info('Mesage About Close: ' + Message);
  // befor Close connect we CAN send message to CLient;
  if not (Sender is TsyConnectedClient) then
    exit;
  MsgRec.Message := Message;
  MsgRec.Sender := TsyConnectedClient(Sender);
  MsgRec.Opcode := optCloseConnect;
  MsgRec.Reason := Reason;
  FMessageQueue.PushItem(MsgRec);
  TsyConnectedClient(Sender).SendCloseFrame(Reason, Message);
  TsyConnectedClient(Sender).TerminateThread;
  Queue(@CloseConnectionNotify);
end;

procedure TsyWebSocketServer.OnClientBinaryData(Sender: TObject; BinData: TDynamicByteArray);
var
  MsgRec: TMessageRecord;
begin
  syLog.Info('Mesage Binary: ');
  // befor Close connect we CAN send message to CLient;
  if not (Sender is TsyConnectedClient) then
    exit;
  MsgRec.Message := '';
  MsgRec.BinaryData := BinData;
  MsgRec.Sender := TsyConnectedClient(Sender);
  MsgRec.Opcode := optBinary;
  MsgRec.Reason := 0;
  FMessageQueue.PushItem(MsgRec);
  Queue(@BinDataNotify);
end;

constructor TsyWebSocketServer.Create(APort: integer);
begin
  FreeOnTerminate := False;
  FPort := APort;
  FSock := TTCPBlockSocket.Create;
  FMessageQueue := TMessageQueue.Create();
  FLockedClientList := TLockedClientList.Create;
  FClientCount := 1;
  inherited Create(True);
end;

destructor TsyWebSocketServer.Destroy;
var
  List: specialize TList<TsyConnectedClient>;
  Cl: TsyConnectedClient;
begin
  syLog.Info('Main Thread is Terminated');
  list := FLockedClientList.LockList;
  try
    for cl in List do
    begin
      cl.TerminateThread;
    end;
  finally
    FLockedClientList.UnlockList;
  end;
  //  FLockedClientList.Clear;
  FreeAndNil(FMessageQueue);
  FreeAndNil(FLockedClientList);
  FreeAndNil(FSock);
  inherited Destroy;
end;

procedure TsyWebSocketServer.Execute;
var
  ClientSock: TSocket;
  Client: TsyConnectedClient;
begin
  FSock.CreateSocket;
  FSock.SetLinger(True, 5000);
  FSock.Bind('0.0.0.0', IntToStr(FPort));
  FSock.Listen;
  repeat
    if terminated then
      break;
    if FSock.CanRead(1000) then
    begin
      if Terminated then
        break;
      ClientSock := FSock.accept;

      if FSock.lastError = 0 then
      begin
        // create client thread
        Client := TsyConnectedClient.Create(ClientSock);
        Client.OnTerminate := @OnTerminate;
        Client.OnClientTextMessage := @OnClientTextMessage;
        Client.OnClientClose := @OnClientClose;
        Client.OnClientBinaryData := @OnClientBinaryData;
        Client.Tag := FClientCount;
        Inc(FClientCount);
        FLockedClientList.Add(Client);
        Client.Start;
      end;
    end;
  until False;
  Terminate;
end;

end.
