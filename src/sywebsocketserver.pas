{==============================================================================|
| Project : sy WebSocket Server                                                |
|==============================================================================|
| Copyright (c)2020, Yuri Serebrennikov                                        |
| All rights reserved.                                                         |
|                                                                              |
| Redistribution and use in source and binary forms, with or without           |
| modification, are permitted provided that the following conditions are met:  |
|                                                                              |
| Redistributions of source code must retain the above copyright notice, this  |
| list of conditions and the following disclaimer.                             |
|                                                                              |
| Redistributions in binary form must reproduce the above copyright notice,    |
| this list of conditions and the following disclaimer in the documentation    |
| and/or other materials provided with the distribution.                       |
|                                                                              |
| Neither the name of Yuri serebrennikov nor the names of its contributors may |
| be used to endorse or promote products derived from this software without    |
| specific prior written permission.                                           |
|                                                                              |
| THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS"  |
| AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE    |
| IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE   |
| ARE DISCLAIMED. IN NO EVENT SHALL THE REGENTS OR CONTRIBUTORS BE LIABLE FOR  |
| ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL       |
| DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR   |
| SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER   |
| CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT           |
| LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY    |
| OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH  |
| DAMAGE.                                                                      |
|==============================================================================|
| The Initial Developer of the Original Code is Yuri Serebrennikov             |
| All Rights Reserved.                                                         |
|==============================================================================|
|          (Found at URL: https://github.com/seryal/sywebsocket/)              |
|==============================================================================}
unit syWebSocketServer;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, blcksock, synsock, syconnectedclient, Generics.Collections, sywebsocketcommon;

type

  TLockedClientList = class(specialize TThreadList<TsyConnectedClient>);
  TClientList = specialize TList<TsyConnectedClient>;




  { TsyWebSocketServer }
  TsyWebSocketServer = class(TThread)
  private
    FClientCount: integer;
    FOnClientConnected: TNotifyEvent;
    FOnClientDisconnected: TNotifyEvent;
    FOnPing: TNotifyEvent;
    FSock: TTCPBlockSocket;
    FPort: integer;
    FLockedClientList: TLockedClientList;
    // Messages from client
    FMessageQueue: TMessageQueue;
    FOnTextMessage: TNotifyEvent;
    FOnBinData: TNotifyEvent;
    FOnCloseConnection: TNotifyEvent;
    procedure DoClientConnected(Sender: TObject);
    procedure OnClientBinaryData(Sender: TObject; BinData: TBytes);
    procedure OnClientClose(Sender: TObject; Reason: integer; Message: string);
    procedure OnClientPing(Sender: TObject; Message: string);
    procedure OnClientTextMessage(Sender: TObject; Message: string);
    procedure OnClientTerminate(Sender: TObject);

    procedure TextMessageNotify;
    procedure CloseConnectionNotify;
    procedure BinDataNotify;
    procedure PingMessageNotify;
  public
    constructor Create(APort: integer);
    destructor Destroy; override;
    procedure Execute; override;
    property OnMessage: TNotifyEvent read FOnTextMessage write FOnTextMessage;
    property OnClientConnected: TNotifyEvent read FOnClientConnected write FOnClientConnected;
    property OnClientDisconnected: TNotifyEvent read FOnClientDisconnected write FOnClientDisconnected;
    property MessageQueue: TMessageQueue read FMessageQueue;
    property LockedClientList: TLockedClientList read FLockedClientList;
    procedure TerminateThread;
  end;

implementation

{ TsyWebSocketServer }

procedure TsyWebSocketServer.OnClientTerminate(Sender: TObject);
var
  List: TClientList;
begin

  if Terminated then
    Exit;
  if Assigned(OnClientDisconnected) then
    OnClientDisconnected(Sender);
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
  if Assigned(OnMessage) then
    OnMessage(self);
end;

procedure TsyWebSocketServer.CloseConnectionNotify;
begin
  if Terminated then
    exit;
  if Assigned(OnMessage) then
    OnMessage(self);
end;

procedure TsyWebSocketServer.BinDataNotify;
begin
  if Terminated then
    exit;
  if Assigned(OnMessage) then
    OnMessage(self);

end;

procedure TsyWebSocketServer.PingMessageNotify;
begin
  if Terminated then
    exit;
  if Assigned(OnMessage) then
    OnMessage(self);
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

procedure TsyWebSocketServer.OnClientPing(Sender: TObject; Message: string);
var
  MsgRec: TMessageRecord;
begin
  // add message to Queue
  if not (Sender is TsyConnectedClient) then
    exit;
  MsgRec.Message := Message;
  MsgRec.Sender := TsyConnectedClient(Sender);
  MsgRec.Opcode := optPing;
  MsgRec.Reason := 0;
  FMessageQueue.PushItem(MsgRec);

  // send event to MainProgram about new Text Message
  // The client must read the data from the queue FMessageQueue;
  Queue(@PingMessageNotify);
end;

procedure TsyWebSocketServer.OnClientBinaryData(Sender: TObject; BinData: TBytes);

var
  MsgRec: TMessageRecord;
begin
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

procedure TsyWebSocketServer.DoClientConnected(Sender: TObject);
begin
  if Assigned(OnClientConnected) then
    OnClientConnected(Sender);

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
    try
      if FSock.CanRead(5000) then
      begin
        if Terminated then
          break;
        ClientSock := FSock.accept;

        if FSock.lastError = 0 then
        begin
          // create client thread
          if Terminated then
            exit;
          Client := TsyConnectedClient.Create(ClientSock);
          Client.OnTerminate := @OnClientTerminate;
          Client.OnClientTextMessage := @OnClientTextMessage;
          Client.OnClientClose := @OnClientClose;
          Client.OnClientBinaryData := @OnClientBinaryData;
          Client.OnClientPing := @OnClientPing;
          Client.OnCLientConnected := @DoClientConnected;
          Client.Tag := FClientCount;
          Inc(FClientCount);
          FLockedClientList.Add(Client);
          //          if Assigned(OnClientConnected) then
          //            OnClientConnected(Client);
          Client.Start;
        end;
      end;
    except

    end;
  until False;
  Terminate;
end;

procedure TsyWebSocketServer.TerminateThread;
begin
  FSock.AbortSocket;
  Terminate;
end;

end.
