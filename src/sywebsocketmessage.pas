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
unit sywebsocketmessage;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, sywebsocketframe, sywebsocketcommon;

type

  { TsyWebSocketMessage }

  TsyWebSocketMessage = class
  private
    FData: TMemoryStream;
    FReason: word;
    FIsReady: boolean;
    FMessageType: TOpcodeType;
    function GetBinData: TBytes;
    function GetMessageStr: string;
    function GetPayloadLen: integer;
  public
    function AddData(AFrame: TsyBaseWebsocketFrame): boolean;
    constructor Create;
    destructor Destroy; override;
    property IsReady: boolean read FIsReady write FisReady;
    property Opcode: TOpcodeType read FMessageType write FMessageType;
    property MessageStr: string read GetMessageStr;
    property BinData: TBytes read GetBinData;
    property MessageType: TOpcodeType read FMessageType;
    property PayloadLen: integer read GetPayloadLen;
  end;


implementation

{ TsyWebSocketMessage }

function TsyWebSocketMessage.GetBinData: TBytes;
var
  len: integer;
begin
  len := FData.Size;
  SetLength(Result, len);
  if len > 0 then
  begin
    FData.Position := 0;
    FData.ReadBuffer(Result[0], len);
    //    SetLength(Result, len);
  end;
end;

function TsyWebSocketMessage.GetMessageStr: string;
var
  ustr: UTF8String;
  len: integer;
begin
  Result := '';
  len := FData.Size;
  if len > 0 then
  begin
    SetLength(ustr, len);
    FData.Position := 0;
    FData.ReadBuffer(ustr[1], len);
    Result := ustr;
  end;
end;

function TsyWebSocketMessage.GetPayloadLen: integer;
begin
  Result := FData.Size;
end;

function TsyWebSocketMessage.AddData(AFrame: TsyBaseWebsocketFrame): boolean;
begin
  Result := True;
  FReason := AFrame.Reason;

{  if (not FIsReady) and (AFrame.OpCode <> optContinue) then // only optContinue if we wait next frame;
  begin
    Result := False;
    exit;
  end;}

  case AFrame.OpCode of
    optContinue:
    begin
      if FIsReady then   // if first frame continue then HALT
      begin
        Result := False;
        exit;
      end;
      FIsReady := AFrame.Fin;
      if AFrame.PayloadLen > 0 then
        FData.Write(AFrame.Binary[0], AFrame.PayloadLen);
    end;
    optText, optBinary:
    begin
      if not FIsReady then
      begin
        Result := False;
        exit;
      end;
      FIsReady := AFrame.Fin;
      FMessageType := AFrame.OpCode;
      FData.Clear;
      if AFrame.PayloadLen > 0 then
        FData.Write(AFrame.Binary[0], AFrame.PayloadLen);
    end;
  end;
end;

constructor TsyWebSocketMessage.Create;
begin
  FIsReady := True;
  FData := TMemoryStream.Create;
end;

destructor TsyWebSocketMessage.Destroy;
begin
  FreeAndNil(FData);
  inherited Destroy;
end;

end.
