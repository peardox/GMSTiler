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
  Settings in 'Settings.pas',
  clJsonParser in '..\thirdparty\json-serializer\json\clJsonParser.pas',
  clJsonSerializer in '..\thirdparty\json-serializer\json\clJsonSerializer.pas',
  clJsonSerializerBase in '..\thirdparty\json-serializer\json\clJsonSerializerBase.pas';

{$R *.res}

begin
  GlobalUseSkia := True;
  ReportMemoryLeaksOnShutdown := True;
  Application.Initialize;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
