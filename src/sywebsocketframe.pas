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
unit sywebsocketframe;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, sywebsocketcommon;

const
  CLOSE_NORMAL_CLOSURE = 1000;
  CLOSE_GOING_AWAY = 1001;
  CLOSE_PROTOCOL_ERROR = 1002;
  CLOSE_UNSUPORTED_DATA = 1003;
  CLOSE_RESERVER = 1004;
  CLOSE_NO_STATUS_RCVD = 1005;
  CLOSE_ABNORMAL_CLOSURE = 1006;
  CLOSE_INVALID_FRAME_PAYLOAD_DATA = 1007;
  CLOSE_POLICY_VIOLATION = 1008;
  CLOSE_MESSAGE_TOO_BIG = 1009;
  CLOSE_MANDRATORY_EXT = 1010;
  CLOSE_INTERNAL_SERVER_ERROR = 1011;
  CLOSE_TLS_HANDSHAKE = 1015;

type
  { TsyBaseWebsocketFrame }

  TsyBaseWebsocketFrame = class
  private
    FFin: boolean;
    FMessageStr: string;
    FRsv1: boolean;
    FRsv2: boolean;
    FRsv3: boolean;
    FOpCode: TOpcodeType;
    FMask: boolean;
    FPayloadLen: QWord;
    FHeaderLen: integer;
    FMaskValue: DWord;
    FReason: word;
    FFrame: TMemoryStream;
    FBinary: TBytes;
    function GetBinary: TBytes;
    function GetFrame: TMemoryStream;
    function GetMessageStr: string;
    procedure SetBinary(AValue: TBytes);
    procedure SetFin(AValue: boolean);
    procedure SetFrame(AValue: TMemoryStream);
    procedure SetMask(AValue: boolean);
    procedure SetMaskValue(AValue: DWord);
    procedure SetMessageStr(AValue: string);
    procedure SetOpcode(AValue: TOpcodeType);
    procedure SetPayloadLen(AValue: QWord);
  public
    // full received frame or for send
    property Frame: TMemoryStream read GetFrame write SetFrame;
    property Fin: boolean read FFin write SetFin;
    property OpCode: TOpcodeType read FOpCode write SetOpcode;
    property Mask: boolean read FMask write SetMask;
    property PayloadLen: QWord read FPayloadLen write SetPayloadLen;
    property MaskValue: DWord read FMaskValue write SetMaskValue;
    property MessageStr: string read GetMessageStr write SetMessageStr;
    property Reason: word read FReason write FReason;
    property Binary: TBytes read GetBinary write SetBinary;
    property Rsv1: boolean read FRsv1;
    property Rsv2: boolean read FRsv2;
    property Rsv3: boolean read FRsv3;
    constructor Create;
    destructor Destroy; override;
  end;

implementation

{ TsyBaseWebsocketFrame }


procedure TsyBaseWebsocketFrame.SetFrame(AValue: TMemoryStream);
var
  Arr: TBytes;
  i: integer;
  _Payload7: byte;
  _Payload16: word;
  _Payload64: QWord;
  ustr: UTF8String;
  pos: integer;
  b: ^byte;
begin
  if FFrame = AValue then
    Exit;
  if assigned(FFrame) then
    FreeAndNil(FFrame);
  FFrame := AValue;
  setlength(Arr, FFrame.Size);
  FFrame.Position := 0;
  FFrame.ReadBuffer(arr[0], FFrame.Size);
  FFin := (Arr[0] and 128) = 128;
  FRsv1 := (Arr[0] and 64) = 64;
  FRsv2 := (Arr[0] and 32) = 32;
  FRsv3 := (Arr[0] and 16) = 16;
  FOpCode := TOpcodeType(Arr[0] and 15);
  FMask := (Arr[1] and 128) = 128;
  _Payload7 := Arr[1] and 127;
  FPayloadLen := _Payload7;
  pos := 2;
  case _PayLoad7 of
    126:
    begin
      Move(Arr[2], _Payload16, 2);
      _Payload16 := SwapEndian(_Payload16);
      FPayloadLen := _Payload16;
      pos := 4;
    end;
    127:
    begin
      Move(Arr[2], _Payload64, 8);
      _Payload64 := SwapEndian(_Payload64);
      FPayloadLen := _Payload64;
      pos := 10;
    end;
  end;

  { TODO :   // correct? need check mask -> pos+4 -> check payloadlen }
  if (FMask) and (FPayloadLen > 0) then
  begin
    move(arr[pos], FMaskValue, 4);
    pos := pos + 4;
    b := FFrame.Memory + pos;
    for i := 0 to FPayloadLen - 1 do
    begin
      b^ := b^ xor ((FMaskValue shr ((i mod 4) * 8)) and $FF);
      Inc(b);
    end;
  end;
  FHeaderLen := pos;
  case OpCode of
    optCloseConnect:
    begin
      if PayloadLen = 0 then
        FReason := CLOSE_NORMAL_CLOSURE;
      if PayloadLen = 1 then
        FReason := CLOSE_PROTOCOL_ERROR;
      if PayloadLen >= 2 then
      begin
        FPayloadLen := FPayloadLen - 2;
        FFrame.Position := FHeaderLen;
        FFrame.ReadBuffer(Freason, 2);
        FReason := SwapEndian(FReason);
        FHeaderLen := FHeaderLen + 2;
      end;
    end;
  end;
end;

procedure TsyBaseWebsocketFrame.SetMask(AValue: boolean);
begin
  if FMask = AValue then
    Exit;
  FMask := AValue;
end;

procedure TsyBaseWebsocketFrame.SetMaskValue(AValue: DWord);
begin
  if FMaskValue = AValue then
    Exit;
  FMaskValue := AValue;
end;

procedure TsyBaseWebsocketFrame.SetMessageStr(AValue: string);
type
  THeadBuffer = array[0..13] of byte;
var
  ustr: UTF8String;
  Buffer: TBytes;
  len: integer;
begin
  ///
  ustr := AValue;
  len := length(ustr);
  FPayloadLen := len;
  if opcode = optCloseConnect then
  begin
    SetLength(ustr, len + 2);
    if FPayloadLen > 0 then
      move(ustr[1], ustr[3], len);
    ustr[2] := chr(FReason and $FF);
    ustr[1] := chr((FReason and $FF00) shr 8);
    len := len + 2;
    FPayloadLen := FPayloadLen + 2;
  end;

  setlength(Buffer, len);
  if len > 0 then
    move(ustr[1], Buffer[0], len);
  SetBinary(Buffer);
end;

procedure TsyBaseWebsocketFrame.SetOpcode(AValue: TOpcodeType);
begin
  if FOpCode = AValue then
    Exit;
  FOpCode := AValue;
end;

procedure TsyBaseWebsocketFrame.SetPayloadLen(AValue: QWord);
begin
  if FPayloadLen = AValue then
    Exit;
  FPayloadLen := AValue;
end;

constructor TsyBaseWebsocketFrame.Create;
begin
  FFrame := TMemoryStream.Create;
end;

destructor TsyBaseWebsocketFrame.Destroy;
begin
  if assigned(FFrame) then
    FreeAndNil(FFrame);
  inherited Destroy;
end;

function TsyBaseWebsocketFrame.GetFrame: TMemoryStream;
begin
  Result := FFrame;
end;

function TsyBaseWebsocketFrame.GetMessageStr: string;
var
  ustr: UTF8String;
begin
  Result := '';
  if FPayloadLen > 0 then
  begin
    SetLength(ustr, FPayloadLen);
    FFrame.Position := FHeaderLen;
    FFrame.ReadBuffer(ustr[1], FPayloadLen);
    Result := ustr;
  end;
end;

function TsyBaseWebsocketFrame.GetBinary: TBytes;
begin
  SetLength(Result, FPayloadLen);
  if FPayloadLen > 0 then
  begin
    FFrame.Position := FHeaderLen;
    FFrame.ReadBuffer(Result[0], FPayloadLen);
    SetLength(Result, FPayloadLen);
  end;
end;

procedure TsyBaseWebsocketFrame.SetBinary(AValue: TBytes);
type
  THeadBuffer = array[0..13] of byte;
var
  Data: TBytes;
  len16: word;
  len64: QWord;
  fullsize: integer;
  plType: byte;
  HeadBuffer: ^THeadBuffer;
  tmp: ^THeadBuffer;
  i: integer;
begin
  // forming websocket frame for send
  FPayloadLen := Length(AValue);
  SetLength(Data, FPayloadLen);
  if FPayloadLen > 0 then
    move(AValue[0], Data[0], FPayloadLen);
  //  len := 2;
  fullsize := FPayloadLen + 2;
  pltype := 1;
  if FPayloadLen > 125 then
  begin
    fullsize := fullsize + 2;
    pltype := 2;
  end;
  if FPayloadLen > High(word) then
  begin
    fullsize := fullsize + 6;
    plType := 3;
  end;
  if Fmask then
    fullsize := fullsize + 4;

  FFrame.SetSize(fullsize);

  HeadBuffer := FFrame.Memory;
  HeadBuffer^[0] := 128;
  HeadBuffer^[0] := HeadBuffer^[0] or integer(FOpcode);

  // set mask
  if Fmask then
  begin
    HeadBuffer^[1] := 128;
    FMaskValue := Random($FFFFFFFF);
    FMaskValue := 0;
  end
  else
    HeadBuffer^[1] := 0;


  case plType of
    1:
    begin
      HeadBuffer^[1] := HeadBuffer^[1] or FPayloadLen;
      move(FMaskValue, HeadBuffer^[2], 4);
    end;
    2:
    begin
      len16 := FPayloadLen;
      len16 := SwapEndian(len16);
      HeadBuffer^[1] := HeadBuffer^[1] or 126;
      move(len16, HeadBuffer^[2], 2);
      move(FMaskValue, HeadBuffer^[4], 4);
    end;
    3:
    begin
      len64 := FPayloadLen;
      len64 := SwapEndian(len64);
      HeadBuffer^[1] := HeadBuffer^[1] or 127;
      move(len64, HeadBuffer^[2], 8);
      move(FMaskValue, HeadBuffer^[10], 4);
    end;
  end;
  FHeaderLen := fullsize - FPayloadLen;
  FFrame.Position := fullsize - FPayloadLen;
  if FPayloadLen = 0 then
    exit;
  // encode Payload data
  if mask then
  begin
    //FMaskValue := SwapEndian(FMaskValue);
    for i := 0 to FPayloadLen - 1 do
      Data[i] := Data[i] xor ((FMaskValue shr ((i mod 4) * 8)) and $FF);

  end;

  FPayloadLen := FFrame.Write(Data[0], FPayloadLen);

  //  HeadBuffer := FFrame.Memory;
end;

procedure TsyBaseWebsocketFrame.SetFin(AValue: boolean);
begin
  if FFin = AValue then
    Exit;
  FFin := AValue;
end;


end.




















