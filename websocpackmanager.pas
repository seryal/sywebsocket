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
  { TWebsockPackManager }

  TWebsockPackManager = class
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

{ TWebsockPackManager }

function TWebsockPackManager.GetWebsocketFrameSize(ABuffer: Pointer; ASize: integer): QWord;
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

function TWebsockPackManager.GetCount: integer;
begin
  Result := FFrameQueue.Count;
end;

function TWebsockPackManager.GetPop: TMemoryStream;
begin
  Result := FFrameQueue.Dequeue;
end;


function TWebsockPackManager.InsertWebsocketFrame(ABuffer: TBytes; Position: integer; Size: integer): integer;
begin
  FWebsocketBuffer.Write(ABuffer[Position], Size);
end;

constructor TWebsockPackManager.Create;
begin
  FWebsocketBuffer := TMemoryStream.Create;
  FFrameSize := 0;
  FFrameQueue := TFrameQueue.Create;
end;

destructor TWebsockPackManager.Destroy;
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


procedure TWebsockPackManager.InsertData(AData: TBytes; ALen: integer);
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
























