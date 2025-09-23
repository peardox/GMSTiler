unit JsonSerializer;

interface

uses
  System.Classes,
  System.SysUtils,
  System.IOUtils,
  System.JSON,
  System.JSON.Types,
  System.Rtti,
  System.TypInfo,
//  Neon.Core.Utils,
  Neon.Core.Types,
  Neon.Core.DynamicTypes,
//  Neon.Core.Attributes,
  Neon.Core.Persistence,
  Neon.Core.Persistence.JSON,
//  Neon.Core.Serializers.RTL,
  System.Generics.Collections,
  LayoutCSV,
  GMSimpleLog;
{
type
  // Serialization helper class
  TSerializationHelper = class
  public
    class function SerializeNestedObjectList<T: class>(AObject: T): string;
    class function DeserializeNestedObjectList<T: class, constructor>(const AJson: string): T;
  end;
}

function SaveObjAsJson(Obj: TObject; const APathname: String; const AFilename: String): Boolean; overload;
function SaveObjAsJson(Obj: TSheetLayoutDict; const APathname: String; const AFilename: String): Boolean; overload;
function LoadObjAsJson(var Obj: TObject; const APathname: String; const AFilename: String): Boolean; overload;
function LoadObjAsJson(var Obj: TSheetLayoutDict; const APathname: String; const AFilename: String): Boolean; overload;

implementation

function GetJsonConfig: INeonConfiguration;
begin
  Result := TNeonConfiguration.Default
    .SetMemberCase(TNeonCase.CamelCase)     // Case settings
    .SetMembers([TNeonMembers.Properties])    // Member type settings
    .SetIgnoreFieldPrefix(False)             // F Prefix settings
    .SetVisibility([mvPublic]) // Visibility settings
  ;
end;

function SaveObjAsJson(Obj: TObject; const APathname: String; const AFilename: String): Boolean;
var
  LJSON: TJSONValue;
  LConfig: INeonConfiguration;
begin
  Result := False;

  LConfig := GetJsonConfig;
  LJSON := TNeon.ObjectToJSON(Obj, LConfig);
  try
    if not(DirectoryExists(APathname)) then
      ForceDirectories(APathname);
    TFile.WriteAllText(TPath.Combine(APathname, AFilename), LJSON.ToString);
  finally
     LJSON.Free;
  end;

  Result := True;
end;

function SaveObjAsJson(Obj: TSheetLayoutDict; const APathname: String; const AFilename: String): Boolean; overload;
var
  LJSON: TJSONValue;
  LConfig: INeonConfiguration;
begin
  Result := False;

  LConfig := GetJsonConfig;
  LJSON := TNeon.ObjectToJSON(Obj, LConfig);
  try
    if not(DirectoryExists(APathname)) then
      ForceDirectories(APathname);
    TFile.WriteAllText(TPath.Combine(APathname, AFilename), LJSON.ToString);
  finally
     LJSON.Free;
  end;

  Result := True;
end;

function LoadObjAsJson(var Obj: TSheetLayoutDict; const APathname: String; const AFilename: String): Boolean;
var
  LJSON: TJSONValue;
  LReader: TNeonDeserializerJSON;
  LConfig: INeonConfiguration;
begin
  Result := False;
  LJSON := TJSONObject.ParseJSONValue(TFile.ReadAllText(TPath.Combine(APathname, AFilename)));

  if not Assigned(LJSON) then
    raise Exception.Create('Error parsing JSON string');

  LConfig := GetJsonConfig;
  try
    LReader := TNeonDeserializerJSON.Create(LConfig);
    try
      LReader.JSONToObject(Obj, LJSON);
      GMS.Log('===== JSON Errors =====');
      for var I := 0 to LReader.Errors.Count - 1 do
        GMS.Log(LReader.Errors[I]);
      GMS.Log('=======================');
      Result := True;
    finally
      LReader.Free;
    end;
  finally
    LJSON.Free;
  end;
end;

function LoadObjAsJson(var Obj: TObject; const APathname: String; const AFilename: String): Boolean;
var
  LJSON: TJSONValue;
  LReader: TNeonDeserializerJSON;
  LConfig: INeonConfiguration;
begin
  Result := False;
  LJSON := TJSONObject.ParseJSONValue(TFile.ReadAllText(TPath.Combine(APathname, AFilename)));

  if not Assigned(LJSON) then
    raise Exception.Create('Error parsing JSON string');

  LConfig := GetJsonConfig;
  try
    LReader := TNeonDeserializerJSON.Create(LConfig);
    try
      LReader.JSONToObject(Obj, LJSON);
      Result := True;
    finally
      LReader.Free;
    end;
  finally
    LJSON.Free;
  end;
end;

end.
