unit Settings;

interface

uses
  System.SysUtils, System.IOUtils, System.Types, System.UITypes, System.Classes,
  System.Variants, FMX.Forms, FMX.Dialogs;

procedure GlobalInit;

var
  DefaultHome: String;

const
  AppName : String = 'SpriteBuilder';

implementation

uses clJsonSerializer;

procedure GlobalInit;
begin
  {$IF DEFINED(MACOS)}
  // Default Home = /Users/peardox/Library/SpriteBuilder
  DefaultHome := IncludeTrailingPathDelimiter(System.IOUtils.TPath.GetLibraryPath) + appname;
  {$ELSEIF DEFINED(LINUX)}
  // Default Home = /home/peardox/.SpriteBuilder
  DefaultHome := IncludeTrailingPathDelimiter(System.IOUtils.TPath.GetHomePath) + '.' + appname;
  {$ELSE}
  // Default Home = C:\Users\peardox\AppData\Roaming\SpriteBuilder
  DefaultHome := IncludeTrailingPathDelimiter(System.IOUtils.TPath.GetHomePath) + appname;
  {$ENDIF}
end;

procedure GlobalTidyUp;
begin

end;

Initialization

Finalization
  GlobalTidyUp;

end.
