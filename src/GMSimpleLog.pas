unit GMSimpleLog;

interface

uses
  System.SysUtils;

var
  GMSLog: TProc<String>;

procedure GMSNullLogger(s: String);

implementation

procedure GMSNullLogger(s: String);
begin

end;

initialization
  GMSLog := GMSNullLogger;

end.
