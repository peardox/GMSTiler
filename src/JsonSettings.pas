unit JsonSettings;

interface

uses
  System.SysUtils, System.IOUtils, System.Types, System.UITypes, System.Classes,
  System.Variants, FMX.Forms, FMX.Dialogs;

type
  TSettings = class
  private
    FAppHome: String;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Save;
    property AppHome: String read FAppHome write FAppHome;
  end;

var
  Settings: TSettings;
  DefaultHome: String;

const
  AppName : String = 'SpriteBuilder';

procedure GlobalInit;

implementation

uses
  System.Generics.Collections,
  JsonSerializer,
  GMSimpleLog,
  LayoutCSV;

procedure GlobalInit;
var
  Layouts: TSheetLayoutDict;
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

  Settings := TSettings.Create;
  Settings.AppHome := DefaultHome;
end;

procedure GlobalTidyUp;
begin
  Settings.Save;
  Settings.Free;
end;

{ TSettings }

constructor TSettings.Create;
begin
  if(Settings <> Nil) then
    Abort;
end;

destructor TSettings.Destroy;
begin
  if(Settings <> Self) then
    Abort;
  inherited;
end;

procedure TSettings.Save;
begin
   SaveObjAsJson(Settings, Settings.AppHome, 'Settings.json');
   SaveObjAsJson(SheetLayouts, Settings.AppHome, 'SheetLayoutsTest.json');
end;

Initialization
  GlobalInit;
  LoadLayouts();

Finalization
  GlobalTidyUp;
  UnLoadLayouts();

end.
