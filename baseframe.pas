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
    property MessageStr: string read FMessage write FMessage;
  end;

  { TBaseFrame }

  TBaseFrame = class
  private
    FMessageStr: string;
    FPayloadLen7: byte;
    FPayloadLen16: word;
    FPayloadLen64: int64;
    FReason: integer;
    FPacketLen: integer;
    FSendData: Pointer;
    function GetSendData: TDynamicByteArray;
    function GetMessage: string;
    function GetPayloadLen: int64;
    procedure SetMessage(AValue: string);
  public
    Fin: boolean;
    Rsv1: boolean;
    Rsv2: boolean;
    Rsv3: boolean;
    Opcode: TOpcodeType;
    Mask: boolean;
    MaskingKey: longword;
    procedure Parse(AMemory: Pointer; Len: integer);
    property PayloadLen: int64 read GetPayloadLen;
    property MessageStr: string read FMessageStr write SetMessage;
    property Reason: integer read FReason write FReason;
    property SendData: TDynamicByteArray read GetSendData;
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
  MessageStr := '';
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
    MessageStr := str;
  end;
  //  syLog.Info(val);
end;

{ TBaseFrame }

function TBaseFrame.GetPayloadLen: int64;
begin
  Result := FPayloadLen7;
end;

function TBaseFrame.GetMessage: string;
begin

end;

function TBaseFrame.GetSendData: TDynamicByteArray;
var
  plen: integer;
  strlen: integer;
  utfstr: UTF8String;
  offset: integer;
  ptype: integer;
begin
  plen := High(word);
  plen := 2;
  utfstr := FMessageStr;
  if Length(utfstr) > 125 then
    plen := plen + 2;
  if Length(utfstr) > High(word) then
    plen := plen + 6;

  case plen of
    2: ptype := 1;
    4: ptype := 2;
    8: ptype := 3;
  end;

  plen := plen + Length(utfstr);

  strlen := Length(utfstr);
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
      Result[offset] := Result[offset] or strlen;
      Inc(offset);
    end;
    2:
    begin
      Result[offset] := 126;
      Inc(Offset);
      Result[offset] := strlen and $ff;
      Inc(Offset);
      Result[offset] := (strlen and $ff00) shr 8;
      Inc(offset);
    end;
    3:     // ????
    begin
      Result[1] := 127;
      Result[2] := strlen and $ff;
      Result[3] := (strlen and $ff00) shr 8;
      Result[4] := (strlen and $ff0000) shr 16;
      Result[5] := (strlen and $ff000000) shr 24;
      Result[6] := (strlen and $ff00000000) shr 32;
      Result[7] := (strlen and $ff0000000000) shr 40;
      Result[8] := (strlen and $ff000000000000) shr 48;
      Result[9] := (strlen and $ff00000000000000) shr 56;
    end;
  end;
  move(utfstr[1], Result[offset], strlen);
  Result[offset + strlen] := 0;
  utfstr := '';
  /// move message to array

end;

procedure TBaseFrame.SetMessage(AValue: string);
begin
  // формируем кусок данных для отправки в сервер
  FMessageStr := AValue;
end;

procedure TBaseFrame.Parse(AMemory: Pointer; Len: integer);
var
  Data: ^TDynamicByteArray;
  offset: byte;
  Message: TPayloadText;
begin
  FMessageStr := '';
  New(Data);
  SetLength(Data^, Len);
  Data^ := AMemory;
  offset := 0;
  fin := (Data^[offset] and 128) = 128;
  OPCode := TOpcodeType(Data^[offset] and %1111);
  Inc(offset); //1
  Mask := (Data^[offset] and 128) = 128;
  FPayloadLen7 := Data^[offset] and %1111111;
  Inc(offset);    //2
  if FPayloadLen7 = 126 then
  begin
    FPayloadLen16 := Data^[offset] shl 8;
    Inc(offset);     //3
    FPayloadLen16 := FPayloadLen16 or Data^[offset];
    Inc(offset);     //4
  end;
  if FPayloadLen7 = 127 then
  begin
    FPayloadLen64 := Data^[offset] shl 56;
    Inc(offset);        //3
    FPayloadLen64 := FPayloadLen64 or (Data^[offset] shl 48);
    Inc(offset);           //4
    FPayloadLen64 := FPayloadLen64 or (Data^[offset] shl 40);
    Inc(offset);           //5
    FPayloadLen64 := FPayloadLen64 or (Data^[offset] shl 32);
    Inc(offset);           //6
    FPayloadLen64 := FPayloadLen64 or (Data^[offset] shl 24);
    Inc(offset);           //7
    FPayloadLen64 := FPayloadLen64 or (Data^[offset] shl 16);
    Inc(offset);           //8
    FPayloadLen64 := FPayloadLen64 or (Data^[offset] shl 8);
    Inc(offset);           //9
    FPayloadLen64 := FPayloadLen64 or Data^[offset];
    Inc(offset);
  end;

  if Mask then
  begin
    MaskingKey := Data^[offset];
    Inc(offset);
    MaskingKey := MaskingKey or (Data^[offset] shl 8);
    Inc(offset);
    MaskingKey := MaskingKey or (Data^[offset] shl 16);
    Inc(offset);
    MaskingKey := MaskingKey or Data^[offset] shl 24;
    Inc(offset);
  end;
  case Opcode of
    optContinue: ;
    optText:
    begin
      Message.LoadMessage(Data^, offset, PayloadLen, MaskingKey);
      FMessageStr := Message.MessageStr;
    end;
    optBinary: ;
    optCloseConnect:
    begin
      Message.LoadMessage(Data^, offset, PayloadLen, MaskingKey);
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

  Dispose(Data);
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
