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

procedure GlobalInit;
begin
  {$IF DEFINED(MACOS)}
  // Default Home = /Users/simon/Library/SpriteBuilder
  DefaultHome := IncludeTrailingPathDelimiter(System.IOUtils.TPath.GetLibraryPath) + appname;
  {$ELSEIF DEFINED(LINUX)}
  // Default Home = /home/simon/.SpriteBuilder
  DefaultHome := IncludeTrailingPathDelimiter(System.IOUtils.TPath.GetHomePath) + '.' + appname;
  {$ELSE}
  // Default Home = C:\Users\simon\AppData\Roaming\SpriteBuilder
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
