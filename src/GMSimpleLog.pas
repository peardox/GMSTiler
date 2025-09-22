unit GMSimpleLog;

interface

uses
  System.SysUtils;

type
  TGMSLogProc = procedure(const Msg: String) of object;
  TGMSLogFormatProc = procedure(const Format: String; const Args: array of const) of object;

  TGMSLogger = class
  private
    FLog: TGMSLogProc;
    FLogFormat: TGMSLogFormatProc;
  public
    procedure Log(const Msg: String); virtual;
    procedure LogFormat(const Msg: String; const Args: array of const); virtual;
    property LogProc: TGMSLogProc read FLog write FLog;
    property LogFormatProc: TGMSLogformatProc read FLogFormat write FLogFormat;
  end;

  TGMSNullLogger = class(TGMSLogger)
  public
    procedure Log(const Msg: String); override;
    procedure LogFormat(const Format: String; const Args: array of const); override;
  end;

var
  GMS: TGMSLogger;

implementation

{ TGMSLogger }

procedure TGMSLogger.Log(const Msg: String);
begin
  // Base implementation - does nothing
end;

procedure TGMSLogger.LogFormat(const Msg: String; const Args: array of const);
begin
  // Base implementation - does nothing
end;

{ TGMSNullLogger }

procedure TGMSNullLogger.Log(const Msg: String);
begin
  // Null implementation - does nothing
  if(Assigned(FLog)) then
    FLog(Msg);
end;

procedure TGMSNullLogger.LogFormat(const Format: String; const Args: array of const);
begin
  // Null implementation - does nothing
  if(Assigned(FLogFormat)) then
    FLogFormat(Format, Args);
end;

initialization
  GMS := TGMSNullLogger.Create;

finalization
  GMS.Free;

end.
