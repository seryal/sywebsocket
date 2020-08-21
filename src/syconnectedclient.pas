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
unit syconnectedclient;

{ TODO : Exceptions for errors }
{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, blcksock, synautil, synsock, sha1, base64, sywebsocketpackmanager,
  syhttpheader, sywebsocketframe, sywebsocketmessage, sywebsocketcommon;

const
  TIMEOUT = 5000;
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
    FWebsocketFrame: TsyWebsockPackManager;
    function MyEncodeBase64(sha1: TSHA1Digest): string;
    procedure Execute; override;
    procedure OnStatus(Sender: TObject; Reason: THookSocketReason; const Value: string);
    procedure SendHTTPAnswer;
    procedure SendHandShake(ASecWebSocketKey: string);
    function GetWebSocketKey(AHeader: TStringList): string;
    procedure ProcessData;
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
  FSock.OnStatus := @OnStatus;
  FWebSocket := False;
  FHandShake := False;
  s := FSock.RecvString(TIMEOUT);

  if FSock.LastError <> 0 then
    exit;

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
      end;
    end
    else
    begin
      // Send Answer to browser
      SendHTTPAnswer;
      exit;
    end;
  finally
    FreeAndNil(Header);
  end;
  if FHandShake then
  begin
    ProcessData;
  end;

  TerminateThread;
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

procedure TsyConnectedClient.ProcessData;
var
  DataLen: integer;
  DataBuffer: TBytes;
  RcvLen: integer;
  RcvFrame: TMemoryStream;
  wsMessage: TsyWebSocketMessage;
  wsFrame: TsyBaseWebsocketFrame;
  ////
  tmp_s: string;
begin
  // Websocket Loop
  FWebsocketFrame := TsyWebsockPackManager.Create();
  wsMessage := TsyWebSocketMessage.Create;
  try
    while not Terminated do
    begin
      // get data from socket
      if FSock.CanRead(1000) then
      begin
        if Terminated then
          break;
        DataLen := FSock.WaitingData;
        if DataLen = 0 then
          exit;
        SetLength(DataBuffer, DataLen);
        if Terminated then
          break;

        RcvLen := FSock.RecvBuffer(@DataBuffer[0], DataLen);
        if RcvLen <> DataLen then // need raise exception
          Exit;
        FWebsocketFrame.InsertData(DataBuffer, RcvLen);

        while FWebsocketFrame.Count > 0 do
        begin
          RcvFrame := FWebsocketFrame.pop;

          wsFrame := TsyBaseWebsocketFrame.Create;
          try
            wsFrame.Frame := RcvFrame;

            // if set RSV1-RSV3 bit disconnect with error code 1002
            if wsFrame.Rsv1 or wsFrame.Rsv2 or wsFrame.Rsv3 then
            begin
              SendCloseFrame(1002, '');
              Exit;
            end;

            if ((wsFrame.OpCode > optBinary) and (wsFrame.OpCode < optCloseConnect)) or (wsFrame.OpCode > optPong) then
            begin
              SendCloseFrame(1002, '');
              Exit;
            end;


            case wsFrame.Opcode of
              optPing, optPong, optCloseConnect: // not fragmented frame
              begin
                case wsFrame.OpCode of
                  optPing:
                  begin
                    if wsFrame.PayloadLen > 125 then // payload <=125
                    begin
                      SendCloseFrame(CLOSE_PROTOCOL_ERROR, '');
                      exit;
                    end;
                    if not wsFrame.Fin then   // no fragmentation
                    begin
                      SendCloseFrame(CLOSE_PROTOCOL_ERROR, '');
                      exit;
                    end;
                    if Assigned(OnClientPing) then
                    begin
                      SendPong(wsFrame.MessageStr);
                      OnClientPing(self, wsFrame.MessageStr);
                    end;
                  end;
                  optPong: // we can not send ping. And if we get Pong we disconnect
                  begin
                    if not wsFrame.Fin then   // no fragmentation
                    begin
                      SendCloseFrame(CLOSE_PROTOCOL_ERROR, '');
                      exit;
                    end;
                  end;
                  optCloseConnect:
                  begin
                    if wsFrame.PayloadLen > 123 then
                    begin
                      SendCloseFrame(CLOSE_PROTOCOL_ERROR, '');
                      Exit;
                    end;
                    if wsFrame.PayloadLen > 2 then
                      if not IsValidUTF8(@wsFrame.Binary[2], wsFrame.PayloadLen - 2) then
                      begin
                        SendCloseFrame(CLOSE_INVALID_FRAME_PAYLOAD_DATA, '');
                        exit;
                      end;
                    case wsFrame.Reason of
                      0..999, CLOSE_RESERVER, CLOSE_NO_STATUS_RCVD, CLOSE_ABNORMAL_CLOSURE, 1012..1014, CLOSE_TLS_HANDSHAKE, 1016..2999:
                        SendCloseFrame(CLOSE_PROTOCOL_ERROR, '');
                      else
                      begin
                        SendCloseFrame(wsFrame.Reason, wsFrame.MessageStr);
                      end;

                    end;
                    Exit;
                  end;

                end;

              end;
              optText, optBinary, optContinue: // fragmented frame
              begin
                // Copy Payload data to TsyWebsocketMessage

                if not wsMessage.AddData(wsFrame) then
                  exit;  // critical error - Disconnect;
                //                tmp_s := wsMessage.MessageStr;
                if wsMessage.IsReady then
                begin
                  case wsMessage.MessageType of
                    optText: // if Text then send OnClientTextMessage event to parent Thread about new Text message;
                    begin
                      if wsMessage.PayloadLen > 0 then // check valid UTF-8 string
                        if not IsValidUTF8(@wsMessage.BinData[0], wsMessage.PayloadLen) then
                        begin
                          SendCloseFrame(CLOSE_INVALID_FRAME_PAYLOAD_DATA, '');
                          exit;
                        end;
                      if Assigned(OnClientTextMessage) then
                        OnClientTextMessage(Self, wsMessage.MessageStr);

                    end;
                    optBinary: // if Text then send OnClientTextMessage event to parent Thread about new Binary message;
                      if Assigned(OnClientBinaryData) then
                        OnClientBinaryData(Self, wsMessage.BinData);
                  end;
                end;

              end;
            end;
          finally
            FreeAndNil(wsFrame);
          end;
        end;

        SetLength(DataBuffer, 0);
      end;
    end;
  finally
    FreeAndNil(wsMessage);
    FreeAndNil(FWebsocketFrame);
  end;

end;

constructor TsyConnectedClient.Create(hSock: TSocket);
begin
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
  if Assigned(FSock) then
    FSock.CloseSocket;
  Terminate;

  RTLeventSetEvent(FTerminateEvent);

end;

procedure TsyConnectedClient.SendCloseFrame(AReason: integer; AMessage: string);
var
  WFrame: TsyBaseWebsocketFrame;
begin
  EnterCriticalsection(FCritSection);
  try
    if Terminated then
      exit;
    WFrame := TsyBaseWebsocketFrame.Create;
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
  WFrame: TsyBaseWebsocketFrame;
begin
  EnterCriticalsection(FCritSection);
  try
    if Terminated then
      Exit;
    WFrame := TsyBaseWebsocketFrame.Create;
    try
      WFrame.Opcode := optText;
      WFrame.Mask := False;
      WFrame.MessageStr := AMessage;
      try
        if FSock.CanWrite(1000) then
          FSock.SendBuffer(WFrame.Frame.Memory, WFrame.Frame.Size);
      except
      end;
    finally
      FreeAndNil(WFrame);
    end;
  finally
    LeaveCriticalsection(FCritSection);
  end;
end;

procedure TsyConnectedClient.SendBinaryFrame(ABinData: TBytes);
var
  WFrame: TsyBaseWebsocketFrame;
begin
  EnterCriticalsection(FCritSection);
  try
    if Terminated then
      Exit;
    WFrame := TsyBaseWebsocketFrame.Create;
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
var
  WFrame: TsyBaseWebsocketFrame;
begin
  EnterCriticalsection(FCritSection);
  try
    if Terminated then
      exit;
    WFrame := TsyBaseWebsocketFrame.Create;
    try
      WFrame.Opcode := optPong;
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

procedure TsyConnectedClient.OnStatus(Sender: TObject; Reason: THookSocketReason; const Value: string);
begin
  if Terminated then
    exit;

  case Reason of
    HR_Error:
      Terminate;
  end;
end;


end.



