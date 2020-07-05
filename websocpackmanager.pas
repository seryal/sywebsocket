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
unit websocpackmanager;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, contnrs, Generics.Collections;

const
  MIN_WEBSOCKETSIZE = 2;

type
  PBytes = ^TBytes;

  TFrameQueue = class(specialize TQueue<TMemoryStream>);
  { TsyWebsockPackManager }

  TsyWebsockPackManager = class
  private
    // размер ожидаемого пакета
    FFrameSize: QWord;
    // Буфер фрейма.
    FWebsocketBuffer: TMemoryStream;
    // Список полученых фреймов
    FFrameQueue: TFrameQueue;

    function GetCount: integer;
    function GetPop: TMemoryStream;
    function GetWebsocketFrameSize(ABuffer: Pointer; ASize: integer): QWord;
    function InsertWebsocketFrame(ABuffer: TBytes; Position: integer; Size: integer): integer;
  public
    constructor Create;
    destructor Destroy; override;
    procedure InsertData(AData: TBytes; ALen: integer);
    property Count: integer read GetCount;
    property Pop: TMemoryStream read GetPop;
  end;

implementation

{ TsyWebsockPackManager }

function TsyWebsockPackManager.GetWebsocketFrameSize(ABuffer: Pointer; ASize: integer): QWord;
var
  HeaderArr: array [0..9] of byte;
  HeaderCount: integer;
  _mask: boolean;
  Payload7: byte;
  PayLoad16: word;
  Payload64: QWord;
  PayloadLen: Qword;
begin
  Result := 0;
  HeaderCount := 2;
  if ASize < MIN_WEBSOCKETSIZE then
    exit;
  move(ABuffer^, HeaderArr[0], ASize);
  _Mask := (HeaderArr[1] and 128) = 128;
  Payload7 := HeaderArr[1] and %1111111;
  PayloadLen := Payload7;
  Payload16 := 0;
  Payload64 := 0;
  case Payload7 of
    126:
    begin
      if ASize < 4 then
        exit;
      PayLoad16 := HeaderArr[2] shl 8;
      PayLoad16 := PayLoad16 or HeaderArr[3];
      PayloadLen := Payload16;
      HeaderCount := 4;
    end;
    127:
    begin
      if ASize < 10 then
        exit;
      Payload64 := HeaderArr[2] shl 56;
      Payload64 := Payload64 or (HeaderArr[3] shl 48);
      Payload64 := Payload64 or (HeaderArr[4] shl 40);
      Payload64 := Payload64 or (HeaderArr[5] shl 32);
      Payload64 := Payload64 or (HeaderArr[6] shl 24);
      Payload64 := Payload64 or (HeaderArr[7] shl 16);
      Payload64 := Payload64 or (HeaderArr[8] shl 8);
      Payload64 := Payload64 or HeaderArr[9];
      PayloadLen := Payload64;
      HeaderCount := 10;
    end;
  end;
  if _mask then
    HeaderCount := HeaderCount + 4;

  Result := PayloadLen + HeaderCount;
end;

function TsyWebsockPackManager.GetCount: integer;
begin
  Result := FFrameQueue.Count;
end;

function TsyWebsockPackManager.GetPop: TMemoryStream;
begin
  Result := FFrameQueue.Dequeue;
end;


function TsyWebsockPackManager.InsertWebsocketFrame(ABuffer: TBytes; Position: integer; Size: integer): integer;
begin
  FWebsocketBuffer.Write(ABuffer[Position], Size);
end;

constructor TsyWebsockPackManager.Create;
begin
  FWebsocketBuffer := TMemoryStream.Create;
  FFrameSize := 0;
  FFrameQueue := TFrameQueue.Create;
end;

destructor TsyWebsockPackManager.Destroy;
var
  mem: TMemoryStream;
  s: integer;
begin
  s := FFrameQueue.Count;
  while FFrameQueue.Count > 0 do
  begin
    mem := FFrameQueue.Dequeue;
    s := mem.Size;
    FreeAndNil(mem);
  end;

  FreeAndNil(FFrameQueue);
  FreeAndNil(FWebsocketBuffer);
  inherited Destroy;
end;


procedure TsyWebsockPackManager.InsertData(AData: TBytes; ALen: integer);
var
  tmp: integer;
  Mem: Pointer;
  // count not used byte
  Amount: integer;
  Position: integer;
begin
  Amount := ALen;
  // offset to next frame
  Position := 0;

  // try frame
  while Amount > 0 do
  begin
    while (FFrameSize = 0) and (Amount > 0) do
    begin
      FWebsocketBuffer.Write(AData[0 + position], 1);
      Amount := Amount - 1;
      Inc(position);
      FFrameSize := GetWebsocketFrameSize(FWebsocketBuffer.Memory, FWebsocketBuffer.Size);
      if FFrameSize > 0 then
        break;
    end;
    while FWebsocketBuffer.Size < FFrameSize do
    begin
      tmp := FFrameSize - FWebsocketBuffer.Size;
      if tmp > Amount then
        tmp := Amount;
      if tmp = 0 then
        exit;
      FWebsocketBuffer.Write(AData[0 + position], tmp);
      Amount := Amount - tmp;
      Position := position + tmp;
    end;

    if FWebsocketBuffer.Size = FFrameSize then
    begin
      // save frame to queue
      FFrameQueue.Enqueue(FWebsocketBuffer);
      // create ne frame
      FWebsocketBuffer := TMemoryStream.Create;
      FFrameSize := 0;
    end;
  end;
end;


end.
























