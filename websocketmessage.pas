unit websocketmessage;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, websocketframe, LazUTF8;

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
    SetLength(Result, len);
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
    //    FData.Seek(0, soBeginning);
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
