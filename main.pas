unit main;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, ExtCtrls,
  syWebSocketServer, syconnectedclient, baseframe, lclintf;

type

  { TForm1 }

  TForm1 = class(TForm)
    btnStart: TButton;
    btnStop: TButton;
    Button1: TButton;
    Edit1: TEdit;
    Edit2: TEdit;
    Label1: TLabel;
    Memo1: TMemo;
    procedure btnStartClick(Sender: TObject);
    procedure btnStopClick(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure Edit2Change(Sender: TObject);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure Label1Click(Sender: TObject);
  private
    FWebSocket: TsyWebSocketServer;
    procedure OnBinData(Sender: TObject);
    procedure OnCloseConnection(Sender: TObject);
    procedure OnTextMessage(Sender: TObject);
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
  FWebSocket.OnTextMessage := @OnTextMessage;
  FWebSocket.OnBinData := @OnBinData;
  FWebSocket.OnCloseConnection := @OnCloseConnection;
  FWebSocket.Start;
  btnStart.Enabled := False;
  btnStop.Enabled := True;
end;

procedure TForm1.btnStopClick(Sender: TObject);
begin
  if Assigned(FWebSocket) then
  begin
    FWebSocket.Terminate;
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

procedure TForm1.Edit2Change(Sender: TObject);
begin
  Label1.Caption := 'Url for test ws://localhost:' + Edit2.Text;
end;

procedure TForm1.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin
  btnStopClick(Sender);
end;

procedure TForm1.Label1Click(Sender: TObject);
begin
  OpenURL('https://www.websocket.org/echo.html');
end;


procedure TForm1.OnTextMessage(Sender: TObject);
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
    if val.Opcode = optText then
    begin
      TsyConnectedClient(val.Sender).SendMessageFrame(val.Message);
      Memo1.Lines.Add(IntToStr(TsyConnectedClient(val.Sender).Tag) + ': Message Len ' + IntToStr(length(val.Message)));
      TsyConnectedClient(val.Sender).SendCloseFrame(1000, '');
    end;
  end;
  // Verifying that the main thread does not stop the worker thread
  // sleep(5000);
end;

procedure TForm1.OnCloseConnection(Sender: TObject);
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
    if val.Opcode = optCloseConnect then
    begin
      Memo1.Lines.Add(IntToStr(TsyConnectedClient(val.Sender).Tag) + ': Close Len ' + IntToStr(length(val.Message)));
    end;
  end;

end;

procedure TForm1.OnBinData(Sender: TObject);
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
    if val.Opcode = optBinary then
    begin
      TsyConnectedClient(val.Sender).SendBinaryFrame(val.BinaryData);
      Memo1.Lines.Add(IntToStr(TsyConnectedClient(val.Sender).Tag) + ': Bin Length ' + IntToStr(length(val.BinaryData)));
      TsyConnectedClient(val.Sender).SendCloseFrame(1000, '');

    end;
  end;
end;

end.
