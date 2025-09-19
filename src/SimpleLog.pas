unit SimpleLog;

interface

uses
  System.SysUtils;

var
  Log: TProc<String>;

procedure NullLogger(s: String);

implementation

procedure NullLogger(s: String);
begin

end;

initialization
  Log := NullLogger;

end.
