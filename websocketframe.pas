unit websocketframe;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, blcksock;

type

  TOpcodeType = (
    optContinue = 0,
    optText = 1,
    optBinary = 2,
    { 3..7 - reserved }
    optCloseConnect = 8,
    optPing = 9,
    optPong = 10);


  { FWebsocketFrame }

  { TWebsocketFrame }

  TWebsocketFrame = class
  private
    FFrameSize: integer;
    FReason: word;
    FSocket: TTCPBlockSocket;
    FOpCode: TOpcodeType;
    FMask: boolean;
    FPayloadLen: int64;
    FMaskValue: DWord;
    FPayLoadBuffer: TMemoryStream;
    FSendData: TMemoryStream;
    FError: integer;
    function GetMessageStr: string;
    function GetPayloadData: Pointer;
    function getSendData: Pointer;
    procedure SetMessageStr(AValue: string);
  public
    constructor Create(ATCPBlockSocket: TTCPBlockSocket);
    constructor Create;
    destructor Destroy; override;
    procedure Start;
    property PayloadLen: int64 read FPayloadLen;
    property PayloadData: Pointer read GetPayloadData;
    property Opcode: TOpcodeType read FOpCode write FOpCode;
    property MessageStr: string read GetMessageStr write SetMessageStr;
    property Mask: boolean read FMask write FMask;
    property SendData: Pointer read getSendData;
    property FrameSize: integer read FFrameSize;
    property Reason: word read FReason write FReason;
  end;

implementation

{ FWebsocketFrame }

function TWebsocketFrame.GetPayloadData: Pointer;
begin
  if FPayloadLen > 0 then
    Result := FPayLoadBuffer.Memory;
end;

function TWebsocketFrame.getSendData: Pointer;
begin
  Result := FSendData.Memory;
end;

procedure TWebsocketFrame.SetMessageStr(AValue: string);
type
  THeadBuffer = array[0..13] of byte;
var
  len: integer;
  HeadBuffer: ^THeadBuffer;
  len7: byte;
  len16: word;
  len64: QWord;
  fullsize: integer;
  plType: byte;
  str: UTF8String;
begin
  str := AValue;
  len := length(str);
  fullsize := len + 2;
  pltype := 1;
  if len > 125 then
  begin
    fullsize := fullsize + 2;
    pltype := 2;
  end;
  if len > High(word) then
  begin
    fullsize := fullsize + 6;
    plType := 3;
  end;

  if opcode = optCloseConnect then
  begin
    SetLength(str, len + 2);
    str[2] := chr(FReason and $FF);
    str[1] := chr((FReason and $FF00) shr 8);
    if len > 0 then
      move(AValue[1], str[3], len);
    len := len + 2;
    fullsize := fullsize + 2;
  end;


  FSendData.SetSize(fullsize);
  HeadBuffer := FSendData.Memory;
  // set fin
  HeadBuffer^[0] := 128;
  HeadBuffer^[0] := HeadBuffer^[0] or integer(FOpcode);
  // set mask
  HeadBuffer^[1] := 0;



  // set payloda len
  case plType of
    1:
    begin
      HeadBuffer^[1] := HeadBuffer^[1] or len;
    end;
    2:
    begin
      len16 := len;
      len16 := SwapEndian(len16);
      HeadBuffer^[1] := 126;
      move(len16, HeadBuffer^[2], 2);
    end;
    3:
    begin
      len64 := len;
      len64 := SwapEndian(len64);
      HeadBuffer^[1] := 127;
      move(len64, HeadBuffer^[2], 8);
    end;
  end;

  FSendData.Position := fullsize - len;
  FFrameSize := fullsize;
  if len = 0 then
    exit;
  len := FSendData.Write(str[1], len);
  len := FSendData.Size;
  len := 0;
end;

function TWebsocketFrame.GetMessageStr: string;
var
  str: UTF8String;
begin
  Result := '';
  if PayloadLen = 0 then
    exit;
  SetLength(str, PayloadLen);
  Move(FPayLoadBuffer.Memory^, str[1], PayloadLen);
  Result := str;
end;

constructor TWebsocketFrame.Create(ATCPBlockSocket: TTCPBlockSocket);
begin
  inherited Create;
  FSocket := ATCPBlockSocket;
  FPayLoadBuffer := TMemoryStream.Create;
end;

constructor TWebsocketFrame.Create;
begin
  FSendData := TMemoryStream.Create;
end;

destructor TWebsocketFrame.Destroy;
begin
  FreeAndNil(FSendData);
  FreeAndNil(FPayLoadBuffer);
  inherited Destroy;
end;

procedure TWebsocketFrame.Start;
var
  HeadBuffer: array[0..13] of byte;
  PayloadBuffer: Pointer;
  dataLen7: byte;
  dataLen16: word;
  dataLen64: QWord;
  fin: boolean;
  offset: integer;
  mval: DWORD;
  headerSize: byte;
  i, r: integer;
  b: ^byte;
begin
  FError := 0;
  offset := 0;
  // read first 2 byte for get paylodalen and Flags
  FSocket.RecvBuffer(@HeadBuffer[offset], 2);
  // get opcode
  FOpCode := TOpcodeType(HeadBuffer[offset] and %1111);
  Inc(offset);
  // get fin flag
  fin := (HeadBuffer[offset] and 128) = 128;
  dataLen7 := HeadBuffer[offset] and %1111111;
  FMask := (HeadBuffer[offset] and 128) = 128;
  Inc(offset);

  FPayloadLen := dataLen7;
  if datalen7 = 126 then
  begin
    FSocket.RecvBuffer(@HeadBuffer[offset], 2);
    Move(HeadBuffer[offset], dataLen16, 2);
    FPayloadLen := SwapEndian(dataLen16);
    Inc(offset, 2);
  end;
  if datalen7 = 127 then
  begin
    FSocket.RecvBuffer(@HeadBuffer[offset], 8);
    Move(HeadBuffer[offset], dataLen64, 8);
    FPayloadLen := SwapEndian(dataLen64);
    Inc(offset, 8);
  end;

  if Fmask then
  begin
    FSocket.RecvBuffer(@HeadBuffer[offset], 4);
    move(HeadBuffer[offset], mval, 4);
    FMaskValue := mval;//SwapEndian(mval);
    Inc(offset, 4);
  end;

  headerSize := offset;

  if FPayloadLen > 0 then
  begin
    FPayLoadBuffer.SetSize(FPayloadLen);
    FSocket.RecvBuffer(FPayLoadBuffer.Memory, FPayloadLen);
    b := FPayLoadBuffer.Memory;
    for i := 0 to FPayloadLen - 1 do
    begin
      r := (FMaskValue shr ((i mod 4) * 8)) and $FF;
      //      FPayLoadBuffer.ReadByte;
      b^ := b^ xor r;
      Inc(b);
    end;

  end
  else
    FPayLoadBuffer.Clear;
  // not forget update if fin = false
end;

end.























