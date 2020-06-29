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



  { TBaseWebsocketMessage }

  TBaseWebsocketMessage = class
  private
    FFin: boolean;
    FMessageStr: string;
    FRsv1: boolean;
    FRsv2: boolean;
    FRsv3: boolean;
    FOpCode: TOpcodeType;
    FMask: boolean;
    FPayloadLen: QWord;
    FMaskValue: DWord;
    FReason: word;
    FFrame: TMemoryStream;
    function GetFrame: TMemoryStream;
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
    property MessageStr: string read FMessageStr write SetMessageStr;
    property Reason: Word read FReason write FReason;
    constructor Create;
    destructor Destroy; override;
  end;

implementation

{ TBaseWebsocketMessage }


procedure TBaseWebsocketMessage.SetFrame(AValue: TMemoryStream);
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
  if FPayloadLen = 0 then
    exit;

  if FMask then
  begin
    move(arr[pos], FMaskValue, 4);
    pos := pos + 4;
    for i := 0 to FPayloadLen - 1 do
      arr[pos + i] := arr[pos + i] xor ((FMaskValue shr ((i mod 4) * 8)) and $FF);
  end;
  case OpCode of
    optText:
    begin
      SetLength(ustr, FPayloadLen);
      Move(Arr[pos], ustr[1], FPayloadLen);
      FMessageStr := ustr;
    end;
  end;
end;

procedure TBaseWebsocketMessage.SetMask(AValue: boolean);
begin
  if FMask = AValue then
    Exit;
  FMask := AValue;
end;

procedure TBaseWebsocketMessage.SetMaskValue(AValue: DWord);
begin
  if FMaskValue = AValue then
    Exit;
  FMaskValue := AValue;
end;

procedure TBaseWebsocketMessage.SetMessageStr(AValue: string);
type
  THeadBuffer = array[0..13] of byte;
var
  ustr: UTF8String;
  len16: word;
  len64: QWord;
  fullsize: integer;
  plType: byte;
  HeadBuffer: ^THeadBuffer;
  tmp: ^THeadBuffer;
begin
  FMessageStr := AValue;
  // forming websocket frame for send
  ustr := AValue;
  FPayloadLen := Length(ustr);
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

  if opcode = optCloseConnect then
  begin
    SetLength(ustr, FPayloadLen + 2);
    ustr[2] := chr(FReason and $FF);
    ustr[1] := chr((FReason and $FF00) shr 8);
    if FPayloadLen > 0 then
      move(FMessageStr[1], ustr[3], FPayloadLen);
    FPayloadLen := FPayloadLen + 2;
    fullsize := fullsize + 2;
  end;
  FFrame.SetSize(fullsize);
  HeadBuffer := FFrame.Memory;
  HeadBuffer^[0] := 128;
  HeadBuffer^[0] := HeadBuffer^[0] or integer(FOpcode);
  // set mask
  HeadBuffer^[1] := 0;

  case plType of
    1:
    begin
      HeadBuffer^[1] := HeadBuffer^[1] or FPayloadLen;
    end;
    2:
    begin
      len16 := FPayloadLen;
      len16 := SwapEndian(len16);
      HeadBuffer^[1] := 126;
      move(len16, HeadBuffer^[2], 2);
    end;
    3:
    begin
      len64 := FPayloadLen;
      len64 := SwapEndian(len64);
      HeadBuffer^[1] := 127;
      move(len64, HeadBuffer^[2], 8);
    end;
  end;
  FFrame.Position := fullsize - FPayloadLen;
  if FPayloadLen = 0 then
    exit;
  FPayloadLen := FFrame.Write(ustr[1], FPayloadLen);
end;

procedure TBaseWebsocketMessage.SetOpcode(AValue: TOpcodeType);
begin
  if FOpCode = AValue then
    Exit;
  FOpCode := AValue;
end;

procedure TBaseWebsocketMessage.SetPayloadLen(AValue: QWord);
begin
  if FPayloadLen = AValue then
    Exit;
  FPayloadLen := AValue;
end;

constructor TBaseWebsocketMessage.Create;
begin
  FFrame := TMemoryStream.Create;
end;

destructor TBaseWebsocketMessage.Destroy;
begin
  if assigned(FFrame) then
    FreeAndNil(FFrame);
  inherited Destroy;
end;

function TBaseWebsocketMessage.GetFrame: TMemoryStream;
begin
  Result := FFrame;
end;

procedure TBaseWebsocketMessage.SetFin(AValue: boolean);
begin
  if FFin = AValue then
    Exit;
  FFin := AValue;
end;


end.





















