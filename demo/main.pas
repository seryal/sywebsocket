unit main;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, ExtCtrls,
  syWebSocketServer, syconnectedclient, sywebsocketframe, sywebsocketclient, lclintf, sywebsocketcommon;

type

  { TForm1 }

  TForm1 = class(TForm)
    btnStart: TButton;
    btnStop: TButton;
    Button1: TButton;
    btnClientStart: TButton;
    btnClientStop: TButton;
    Button2: TButton;
    Edit1: TEdit;
    Edit2: TEdit;
    Edit3: TEdit;
    Edit4: TEdit;
    Edit5: TEdit;
    GroupBox1: TGroupBox;
    GroupBox2: TGroupBox;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Memo1: TMemo;
    Memo2: TMemo;
    procedure btnClientStopClick(Sender: TObject);
    procedure btnStartClick(Sender: TObject);
    procedure btnStopClick(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure btnClientStartClick(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure Edit2Change(Sender: TObject);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure Label1Click(Sender: TObject);
  private
    FWebSocket: TsyWebSocketServer;
    FwsClient: TsyWebsocketClient;
    procedure OnClientConected(Sender: TObject);
    procedure OnClientDisconnected(Sender: TObject);
    procedure OnClientMessage(Sender: TObject);
    procedure OnClientTerminate(Sender: TObject);
    procedure OnMessage(Sender: TObject);
  public

  end;

var
  Form1: TForm1;

implementation

{$R *.lfm}

{ TForm1 }

procedure TForm1.btnStartClick(Sender: TObject);
begin
  FWebSocket := TsyWebSocketServer.Create(StrToInt(Edit2.Text));
  // Event notifying that there are messages in the queue
  FWebSocket.OnMessage := @OnMessage;
  FWebSocket.OnClientConnected := @OnClientConected;
  FWebSocket.OnClientDisconnected := @OnClientDisconnected;
  FWebSocket.Start;
  btnStart.Enabled := False;
  btnStop.Enabled := True;
end;

procedure TForm1.btnClientStopClick(Sender: TObject);
begin
  if assigned(FwsClient) then
    FwsClient.TerminateThread;
  FwsClient := nil;
  btnClientStop.Enabled := False;
  btnClientStart.Enabled := True;

end;

procedure TForm1.btnStopClick(Sender: TObject);
begin
  if Assigned(FWebSocket) then
  begin
    FWebSocket.TerminateThread;
    FreeAndNil(FWebSocket);
  end;
  btnStop.Enabled := False;
  btnStart.Enabled := True;
end;

procedure TForm1.Button1Click(Sender: TObject);
var
  ClientList: TClientList;
  Client: TsyConnectedClient;
begin
  if not Assigned(FWebSocket) then
    exit;
  ClientList := FWebSocket.LockedClientList.LockList;

  try
    for client in ClientList do
    begin
      Client.SendMessageFrame(Edit1.Text);
    end;

  finally
    FWebSocket.LockedClientList.UnlockList;
  end;
end;

procedure TForm1.btnClientStartClick(Sender: TObject);
begin
  FwsClient := TsyWebsocketClient.Create(edit3.Text, StrToInt64Def(Edit4.Text, 8080));
  FwsClient.OnMessage := @OnClientMessage;
  FwsClient.OnTerminate := @OnClientTerminate;
  FwsClient.Start;
  btnClientStart.Enabled := False;
  btnClientStop.Enabled := True;
end;

procedure TForm1.Button2Click(Sender: TObject);
begin
  FwsClient.SendMessage(Edit5.Text);
end;



procedure TForm1.Edit2Change(Sender: TObject);
begin
  Label1.Caption := 'Url for test ws://localhost:' + Edit2.Text;
end;

procedure TForm1.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin
  btnClientStopClick(Sender);
  btnStopClick(Sender);
end;

procedure TForm1.Label1Click(Sender: TObject);
begin
  OpenURL('https://www.websocket.org/echo.html');
end;


procedure TForm1.OnMessage(Sender: TObject);
var
  val: TMessageRecord;
begin

  if not Assigned(FWebSocket) then
    exit;
  if FWebSocket.MessageQueue.TotalItemsPushed = FWebSocket.MessageQueue.TotalItemsPopped then
    exit;
  while FWebSocket.MessageQueue.TotalItemsPushed <> FWebSocket.MessageQueue.TotalItemsPopped do
  begin
    FWebSocket.MessageQueue.PopItemTimeout(val, 100);

    case val.Opcode of
      optText:
      begin
        TsyConnectedClient(val.Sender).SendMessageFrame(val.Message);
        //        Memo1.Lines.Add(IntToStr(TsyConnectedClient(val.Sender).Tag) + ': Message Len ' + IntToStr(length(val.Message)));
        Memo1.Lines.Add(IntToStr(TsyConnectedClient(val.Sender).Tag) + ': ' + val.Message);
      end;
      optCloseConnect:
      begin
        Memo1.Lines.Add(IntToStr(TsyConnectedClient(val.Sender).Tag) + ': Close Len ' + IntToStr(length(val.Message)));
      end;
      optPing:
      begin
        Memo1.Lines.Add(IntToStr(TsyConnectedClient(val.Sender).Tag) + ': Ping Len ' + IntToStr(length(val.Message)));
      end;
      optBinary:
      begin
        TsyConnectedClient(val.Sender).SendBinaryFrame(val.BinaryData);
        Memo1.Lines.Add(IntToStr(TsyConnectedClient(val.Sender).Tag) + ': Bin Length ' +
          IntToStr(length(val.BinaryData)));
      end;
    end;
  end;
end;

procedure TForm1.OnClientDisconnected(Sender: TObject);
begin
  Memo1.Lines.Add('Client Disconnected: ' + IntToStr(TsyConnectedClient(Sender).Tag));
end;

procedure TForm1.OnClientMessage(Sender: TObject);
var
  val: TMessageRecord;

begin
  if not Assigned(FWebSocket) then
    exit;
  while FwsClient.MessageQueue.TotalItemsPushed <> FwsClient.MessageQueue.TotalItemsPopped do
  begin
    FwsClient.MessageQueue.PopItem(val);
    Memo2.Lines.Add(val.Message);

  end;
end;

procedure TForm1.OnClientTerminate(Sender: TObject);
begin
  Memo2.Lines.Add('Terminated');
  btnClientStopClick(self);
end;

procedure TForm1.OnClientConected(Sender: TObject);
begin
  Memo1.Lines.Add('Client Connected: ' + IntToStr(TsyConnectedClient(Sender).Tag));
  TsyConnectedClient(Sender).SendMessageFrame('Hello');
end;




end.
