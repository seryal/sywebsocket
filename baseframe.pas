unit baseframe;

{$mode objfpc}{$H+}

interface


uses
  Classes, SysUtils, sylogevent, synautil;

type
  TOpcodeType = (
    optContinue = 0,
    optText = 1,
    optBinary = 2,
    { 3..7 - reserved }
    optCloseConnect = 8,
    optPing = 9,
    optPong = 10);

  TDynamicByteArray = array of byte;


  { TPayloadText }

  TPayloadText = object
  private
    FMessage: string;
  public
    procedure Clear;
    procedure LoadMessage(AData: TDynamicByteArray; AStart: byte; ALen: integer; AMaskKey: longword);
    property MessageStr: string read FMessage;
  end;

  { TPayLoadBinary }

  TPayLoadBinary = object
  private
    FBinaryData: TDynamicByteArray;
    procedure Clear;
    procedure LoadBinary(AData: TDynamicByteArray; AStart: byte; ALen: integer; AMaskKey: longword);
    property BinaryData: TDynamicByteArray read FBinaryData;

  end;


  { TBaseFrame }

  TBaseFrame = class
  private
    // string of message
    FMessageStr: string;
    // binary data
    FBinaryData: TDynamicByteArray;

    FPayloadLen7: byte;
    FPayloadLen16: word;
    FPayloadLen64: int64;
    FReason: integer;
    FSendData: Pointer;

    // флаг о том что это продолжение входного буфера
    FContinueBuffer: boolean;
    FPacketLen: integer;
    FBufferArray: TDynamicByteArray;
    FIncomSize: integer;

    function getReady: boolean;
    function GetSendData: TDynamicByteArray;
    function GetMessage: string;
    function GetPayloadLen: int64;
    procedure SetMessage(AValue: string);

    function GetWebSocketPacketSize(ABuffer: Pointer; Alen: integer): int64;

  public
    Fin: boolean;
    Rsv1: boolean;
    Rsv2: boolean;
    Rsv3: boolean;
    Opcode: TOpcodeType;
    Mask: boolean;
    MaskingKey: longword;
    constructor Create;
    procedure Parse(AMemory: Pointer; Len: integer);
    property PayloadLen: int64 read GetPayloadLen;
    property MessageStr: string read FMessageStr write SetMessage;
    property BinaryData: TDynamicByteArray read FBinaryData write FBinaryData;
    property Reason: integer read FReason write FReason;
    property SendData: TDynamicByteArray read GetSendData;
    property Ready: boolean read getReady;
    //    property PacketLen: Integer read GetPacke
  end;

  { THTTPRecord }

  THTTPRecord = object
    Method: string;
    Uri: string;
    Protocol: string;
    procedure Parse(AValue: string);
  end;


implementation

{ TPayLoadBinary }

procedure TPayLoadBinary.Clear;
begin

end;

procedure TPayLoadBinary.LoadBinary(AData: TDynamicByteArray; AStart: byte; ALen: integer; AMaskKey: longword);
var
  i: integer;
  r: integer;
  Buffer: TDynamicByteArray;
begin
  if ALen > 0 then
  begin
    SetLength(Buffer, Alen);
    for i := 0 to ALen - 1 do
    begin
      r := (AMaskKey shr ((i mod 4) * 8)) and $FF;
      Buffer[i] := AData[AStart + i] xor r;
    end;
    FBinaryData := Buffer;
  end;
end;

{ TPayloadText }

procedure TPayloadText.Clear;
begin

end;

procedure TPayloadText.LoadMessage(AData: TDynamicByteArray; AStart: byte; ALen: integer; AMaskKey: longword);
var
  str: UTF8String;
  i: integer;
  r: integer;
  Buffer: TDynamicByteArray;
  val: string;
begin
  FMessage := '';
  if ALen > 0 then
  begin
    SetLength(Buffer, Alen);
    SetLength(str, ALen);

    for i := 0 to ALen - 1 do
    begin
      r := (AMaskKey shr ((i mod 4) * 8)) and $FF;
      //r := (AMaskKey) and $FF;

      Buffer[i] := AData[AStart + i] xor r;
    end;

    move(Buffer[0], str[1], ALen);
    //str := 'dd';
    FMessage := str;
  end;
  //  syLog.Info(val);
end;

{ TBaseFrame }

function TBaseFrame.GetPayloadLen: int64;
begin
  Result := FPayloadLen7;
  case FPayloadLen7 of
    126:
      Result := FPayloadLen16;
    127:
      Result := FPayloadLen64;
  end;
end;

function TBaseFrame.GetMessage: string;
begin

end;

function TBaseFrame.GetSendData: TDynamicByteArray;
var
  plen: integer;
  DataLen: integer;
  utfstr: UTF8String;
  offset: integer;
  ptype: integer;
begin
  plen := High(word);
  plen := 2;


  case Opcode of
    optCloseConnect:
    begin
      DataLen := Length(utfstr) + 2;
      SetLength(utfstr, DataLen + Length(FMessageStr));
      utfstr[2] := chr(Reason and $FF);
      utfstr[1] := chr((Reason and $FF00) shr 8);
      if Length(FMessageStr) > 0 then
        move(FMessageStr[1], utfstr[3], Length(FMessageStr));
      //      utfstr := FMessageStr;
    end;
    optText:
    begin
      utfstr := FMessageStr;
      DataLen := Length(utfstr);
    end;
    optBinary:
    begin
      DataLen := Length(FBinaryData);
    end;
  end;

  if DataLen > 125 then
    plen := plen + 2;
  if DataLen > High(word) then
    plen := plen + 6;



  case plen of
    2: ptype := 1;
    4: ptype := 2;
    10: ptype := 3;
  end;

  plen := plen + DataLen;


  // Set length of dynamic array
  SetLength(Result, plen + 1);
  offset := 0;

  // Set FIN
  Result[offset] := 128;
  Result[offset] := Result[0] or (integer(Opcode));
  Inc(offset); //1
  Result[offset] := 0; // MASK

  case ptype of
    1:
    begin
      Result[offset] := Result[offset] or DataLen;
      Inc(offset);
    end;
    2:
    begin
      Result[offset] := 126;
      Inc(Offset);
      Result[offset] := (DataLen and $ff00) shr 8;
      Inc(Offset);
      Result[offset] := DataLen and $ff;
      Inc(offset);
    end;
    3:     // ????
    begin
      Result[offset] := 127;
      Inc(offset);
      Result[offset] := (DataLen and $ff00000000000000) shr 56;
      Inc(offset);
      Result[offset] := (DataLen and $ff000000000000) shr 48;
      Inc(offset);
      Result[offset] := (DataLen and $ff0000000000) shr 40;
      Inc(offset);
      Result[offset] := (DataLen and $ff00000000) shr 32;
      Inc(offset);
      Result[offset] := (DataLen and $ff000000) shr 24;
      Inc(offset);
      Result[offset] := (DataLen and $ff0000) shr 16;
      Inc(offset);
      Result[offset] := (DataLen and $ff00) shr 8;
      Inc(offset);
      Result[offset] := DataLen and $ff;
      Inc(offset);
    end;
  end;
  if DataLen > 0 then
  begin
    case Opcode of
      optText, optCloseConnect:
      begin
        move(utfstr[1], Result[offset], DataLen);
      end;
      optBinary:
      begin
        move(FBinaryData[0], Result[offset], DataLen);
      end;
    end;
  end;
  Result[offset + DataLen] := 0;
  utfstr := '';
end;

function TBaseFrame.getReady: boolean;
begin
  Result := not FContinueBuffer;
end;

procedure TBaseFrame.SetMessage(AValue: string);
begin
  FMessageStr := AValue;
end;

function TBaseFrame.GetWebSocketPacketSize(ABuffer: Pointer; Alen: integer): int64;
type
  SmallArray = array[0..9] of byte;
var
  Buffer: ^SmallArray;
  HeaderCount: integer;
  _mask: boolean;
begin
  Result := 0;
  Buffer := @ABuffer^;
  HeaderCount := 2;
  _Mask := (Buffer^[1] and 128) = 128;
  FPayloadLen7 := Buffer^[1] and %1111111;
  FPayloadLen16 := 0;
  FPayloadLen64 := 0;
  case FPayloadLen7 of
    126:
    begin
      FPayloadLen16 := Buffer^[2] shl 8;
      FPayloadLen16 := FPayloadLen16 or Buffer^[3];
      HeaderCount := 4;
    end;
    127:
    begin
      FPayloadLen64 := Buffer^[2] shl 56;
      FPayloadLen64 := FPayloadLen64 or (Buffer^[3] shl 48);
      FPayloadLen64 := FPayloadLen64 or (Buffer^[4] shl 40);
      FPayloadLen64 := FPayloadLen64 or (Buffer^[5] shl 32);
      FPayloadLen64 := FPayloadLen64 or (Buffer^[6] shl 24);
      FPayloadLen64 := FPayloadLen64 or (Buffer^[7] shl 16);
      FPayloadLen64 := FPayloadLen64 or (Buffer^[8] shl 8);
      FPayloadLen64 := FPayloadLen64 or Buffer^[9];
      HeaderCount := 10;
    end;
  end;
  if _mask then
    HeaderCount := HeaderCount + 4;
  Result := PayloadLen + HeaderCount;
end;

constructor TBaseFrame.Create;
begin
  FContinueBuffer := False;
end;

procedure TBaseFrame.Parse(AMemory: Pointer; Len: integer);
var
  Data: ^TDynamicByteArray;
  offset: byte;
  Message: TPayloadText;
  Binary: TPayLoadBinary;
begin

  if not FContinueBuffer then
  begin
    // calculate full size of websocketbuffer.
    FPacketLen := GetWebSocketPacketSize(AMemory, Len);
    FContinueBuffer := True;
    FIncomSize := 0;
    if FPacketLen = Len then
      FContinueBuffer := False;
    Setlength(FBufferArray, FPacketLen);
  end;
  new(Data);
  Data^ := AMemory;
  Move(Data^[0], FBufferArray[FIncomSize], Len);
  dispose(Data);
  FIncomSize := FIncomSize + Len;
  if FIncomSize = FPacketLen then
    FContinueBuffer := False;
  if FContinueBuffer then
    exit;


  FMessageStr := '';
  offset := 0;
  fin := (FBufferArray[offset] and 128) = 128;
  OPCode := TOpcodeType(FBufferArray[offset] and %1111);
  Inc(offset); //1
  Mask := (FBufferArray[offset] and 128) = 128;
  FPayloadLen7 := FBufferArray[offset] and %1111111;
  Inc(offset);    //2

  if FPayloadLen7 = 126 then                  // change to Move
  begin
    Inc(offset, 2);     //3
  end;
  if FPayloadLen7 = 127 then   // change to Move
  begin
    Inc(offset, 8);        //3
  end;

  if Mask then
  begin
    MaskingKey := FBufferArray[offset];
    Inc(offset);
    MaskingKey := MaskingKey or (FBufferArray[offset] shl 8);
    Inc(offset);
    MaskingKey := MaskingKey or (FBufferArray[offset] shl 16);
    Inc(offset);
    MaskingKey := MaskingKey or (FBufferArray[offset] shl 24);
    Inc(offset);
  end;
  case Opcode of
    optContinue: ;
    optText:
    begin
      Message.LoadMessage(FBufferArray, offset, PayloadLen, MaskingKey);
      FMessageStr := Message.MessageStr;
    end;
    optBinary:
    begin
      Binary.LoadBinary(FBufferArray, offset, PayloadLen, MaskingKey);
      FBinaryData := Binary.BinaryData;
      syLog.Warning('Binary Data');

    end;
    optCloseConnect:
    begin
      Message.LoadMessage(FBufferArray, offset, PayloadLen, MaskingKey);
      { TODO : Dirty hack. FIX THIS }
      begin
        if Message.MessageStr <> '' then
          try
            FReason := Ord(Message.MessageStr[2]);
            FReason := FReason or (Ord(Message.MessageStr[1]) shl 8);
            FMessageStr := copy(Message.MessageStr, 3, length(Message.MessageStr));
          except
            FReason := 0;
            FMessageStr := '';
          end;
      end;
    end;
    optPing: ;
    optPong: ;
  end;

end;

{ THTTPRecord }

procedure THTTPRecord.Parse(AValue: string);
var
  s: string;
begin
  syLog.Info(AValue);
  s := AValue;
  if s = '' then
    exit;
  method := fetch(s, ' ');
  if method = '' then
    Exit;
  uri := fetch(s, ' ');
  if uri = '' then
    Exit;
  protocol := fetch(s, ' ');
end;


end.
