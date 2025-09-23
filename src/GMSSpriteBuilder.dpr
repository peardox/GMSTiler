program GMSSpriteBuilder;

uses
  System.StartUpCopy,
  FMX.Forms,
  FMX.Skia,
  GMSSpriteBuilderMain in 'GMSSpriteBuilderMain.pas' {Form1},
  LayoutCSV in 'LayoutCSV.pas',
  TileUtils in 'TileUtils.pas',
  SpriteAtlas in 'SpriteAtlas.pas',
  GMSimpleLog in 'GMSimpleLog.pas',
  SkComponents in 'SkComponents.pas',
  JsonSettings in 'JsonSettings.pas';

{$R *.res}

begin
  GlobalUseSkia := True;
  ReportMemoryLeaksOnShutdown := True;
  Application.Initialize;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
