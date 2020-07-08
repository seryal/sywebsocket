# sywebsocket
Web Socket Server and Client for Lazarus Applications.

## Requirements:
https://github.com/svn2github/Ararat-Synapse/tree/master/trunk



## For Windows:
**TESTED**

## For Ubuntu:
**TESTED**

## Example
```pascal
var
  FWebSocket: TsyWebSocketServer;  

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
```

Page for test https://www.websocket.org/echo.html


## RFC 6455
[Server test result](http://syware.ru/html_result/)

Autobahn WebSocket Testsuite v0.8.0/v0.10.9. results. (https://github.com/crossbario/autobahn-testsuite)


