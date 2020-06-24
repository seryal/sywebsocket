# sywebsocket
Web Socket Server for Lazarus Applications.

## Requirements:
http://www.ararat.cz/synapse/doku.php/download


## For Windows:
Installed openssl, and have also copied the two dll's (libeay32.dll and libssl32.dll) to project directory.

## For Ubuntu:
sudo apt-get install openssl libssl-dev

## Example

var
  FWebSocket := TsyWebSocketServer;  

begin
  FWebSocket := TsyWebSocketServer.Create(8081);
  FWebSocket.OnTextMessage := @OnTextMessage;
  FWebSocket.Start;
end;            

procedure OnTextMessage(Sender: TObject);
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
      Memo1.Lines.Add(IntToStr(TsyConnectedClient(val.Sender).Tag) + ': ' + val.Message);
    end;
  end;
end;   

Page for test https://www.websocket.org/echo.html

RFC 6455 not fully supported.

In progres...
