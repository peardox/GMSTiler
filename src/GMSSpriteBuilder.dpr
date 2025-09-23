program GMSSpriteBuilder;

uses
  System.StartUpCopy,
  FMX.Forms,
  FMX.Skia,
  GMSSpriteBuilderMain in 'GMSSpriteBuilderMain.pas' {Form1},
  JsonSettings in 'JsonSettings.pas',
  JsonSerializer in 'JsonSerializer.pas',
  LayoutCSV in 'LayoutCSV.pas',
  TileUtils in 'TileUtils.pas',
  SpriteAtlas in 'SpriteAtlas.pas',
  GMSimpleLog in 'GMSimpleLog.pas',
  SkComponents in 'SkComponents.pas',
  Neon.Core.Utils in '..\thirdparty\json-serializer\Source\Neon.Core.Utils.pas',
  Neon.Core.Types in '..\thirdparty\json-serializer\Source\Neon.Core.Types.pas',
  Neon.Core.Attributes in '..\thirdparty\json-serializer\Source\Neon.Core.Attributes.pas',
  Neon.Core.DynamicTypes in '..\thirdparty\json-serializer\Source\Neon.Core.DynamicTypes.pas',
  Neon.Core.Nullables in '..\thirdparty\json-serializer\Source\Neon.Core.Nullables.pas',
  Neon.Core.Persistence in '..\thirdparty\json-serializer\Source\Neon.Core.Persistence.pas',
  Neon.Core.Persistence.JSON in '..\thirdparty\json-serializer\Source\Neon.Core.Persistence.JSON.pas',
  Neon.Core.Persistence.JSON.Schema in '..\thirdparty\json-serializer\Source\Neon.Core.Persistence.JSON.Schema.pas',
  Neon.Core.Serializers.Nullables in '..\thirdparty\json-serializer\Source\Neon.Core.Serializers.Nullables.pas',
  Neon.Core.Serializers.RTL in '..\thirdparty\json-serializer\Source\Neon.Core.Serializers.RTL.pas',
  Neon.Core.Tags in '..\thirdparty\json-serializer\Source\Neon.Core.Tags.pas',
  Neon.Core.TypeInfo in '..\thirdparty\json-serializer\Source\Neon.Core.TypeInfo.pas';

{$R *.res}

begin
  GlobalUseSkia := True;
  ReportMemoryLeaksOnShutdown := True;
  Application.Initialize;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
