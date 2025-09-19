program GMSTiler;

uses
  System.StartUpCopy,
  FMX.Forms,
  FMX.Skia,
  GMSTilerMain in 'GMSTilerMain.pas' {Form1},
  LayoutCSV in 'LayoutCSV.pas',
  TileUtils in 'TileUtils.pas',
  SpriteAtlas in 'SpriteAtlas.pas',
  GMSimpleLog in 'GMSimpleLog.pas';

{$R *.res}

begin
  GlobalUseSkia := True;
  ReportMemoryLeaksOnShutdown := True;
  Application.Initialize;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
