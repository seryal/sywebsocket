unit websocpackmanager;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, contnrs, Generics.Collections;

const
  MIN_WEBSOCKETSIZE = 2;
  MIN_HEAD126 = 4;
  MIN_HEAD127 = 10;

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

    function GetWebsocketFrameSize(ABuffer: Pointer; ASize: integer): QWord;
    function InsertWebsocketFrame(ABuffer: TBytes; Position: integer; Size: integer): integer;
  public
    constructor Create;
    destructor Destroy; override;
    procedure InsertData2(AData: TBytes; ALen: integer);
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


procedure TWebsockPackManager.InsertData2(AData: TBytes; ALen: integer);
var
  tmp: integer;
  Mem: Pointer;
  // количество еще не обработаных байт
  Amount: integer;
  Position: integer;
begin
  Amount := ALen;
  // смещение до следующего фрейма
  Position := 0;

  // формируем заголовок
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
      // Сохраним фрейм в список
      FFrameQueue.Enqueue(FWebsocketBuffer);
      // создаем новый буфер.
      FWebsocketBuffer := TMemoryStream.Create;
      FFrameSize := 0;
    end;
  end;
end;


end.
{
// если ждем новый пакетif FNewPacket then
begin
// ожидаемый размер пакета  FFrameSize := MIN_WEBSOCKETSIZE;
// если пришел только 1 байт то запишем его, потому как не сможем вычислить размер пакета
if Amount < MIN_WEBSOCKETSIZE then
begin
// запишем один пришедший байт в буфер    FWebsocketBuffer.Clear;
FWebsocketBuffer.write(Adata[0 + Position], Amount);
FSavedSize := Amount;
tmp := FWebsocketBuffer.Size;
FNewPacket := False;
exit;
end;
// write code here  FWebsocketBuffer.Clear;
FWebsocketBuffer.write(Adata[0 + Position], MIN_WEBSOCKETSIZE);
end
else
// если ждем продолжение пакетаbegin
// Запишем в буфер недостающий набор данных
// получим сколь-ко же байт еще надо получить  tmp := FFrameSize - FSavedSize;
if Amount < tmp then
tmp := Amount;
FWebsocketBuffer.write(AData[0 + Position], tmp);
AMount := Amount - tmp;
position := tmp;
Mem := FWebsocketBuffer.Memory;
FFrameSize := GetWebsocketFrameSize(Mem, FWebsocketBuffer.Size);
FFrameSize := 0;
end;
}

































