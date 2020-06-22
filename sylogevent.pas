unit sylogevent;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, eventlog;

type

  { TsyLog }

  TsyLog = class
  private
    FLog: TEventLog;
  public
    constructor Create; overload;
    destructor Destroy; override;
    procedure Log(AValue: string);
    procedure Warning(AValue: string);
    procedure Error(AValue: string);
    procedure Info(AValue: string);
  end;



function syLog: TsyLog;

implementation

var
  FsyLog: TsyLog;

function syLog: TsyLog;
begin
  Result := FsyLog;
end;


{ TsyLog }

constructor TsyLog.Create;
begin
  inherited Create;
  FLog := TEventLog.Create(nil);
  flog.LogType := ltSystem;
  if not DirectoryExists('Log') then
    CreateDir('Log');
  FLog.FileName := 'Log/debug.log';
  FLog.Active := True;
end;

destructor TsyLog.Destroy;
begin
  FreeAndNil(FLog);
  inherited Destroy;
end;

procedure TsyLog.Log(AValue: string);
begin
  {$IFDEF CONSOLE}
  //Writeln(AValue + #13);
  {$ENDIF}

  FLog.Log(AValue);
end;

procedure TsyLog.Warning(AValue: string);
begin
  {$IFDEF CONSOLE}
  //  Writeln(AValue+#13);
  {$ENDIF}
  FLog.Warning(AValue);

end;

procedure TsyLog.Error(AValue: string);
begin
  {$IFDEF CONSOLE}
  //  Writeln(AValue+#13);
  {$ENDIF}
  FLog.Error(AValue);
end;

procedure TsyLog.Info(AValue: string);
begin
  {$IFDEF CONSOLE}
  //  Writeln(AValue+#13);
  {$ENDIF}


  FLog.Info(AValue + #13);
end;


initialization
  FsyLog := TsyLog.Create;

finalization
  FreeAndNil(FsyLog);

end.
