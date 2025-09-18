unit TileUtils;

interface

uses
  System.SysUtils, System.IOUtils, System.Types, System.Classes, System.Skia,
  System.Generics.Collections, System.Generics.Defaults;

type
  TRectArray = Array of TRect;
  TFitScale = Record
    Scale: Single;
    Offset: TPointF;
  End;
  TDebugProc = procedure(const AMsg: String) of object;

  TFileDirectory = class
  private
    fParentDir: String;
    fSubDir: String;
    fFileName: String;
    fGroupID: Integer;
  public
    constructor Create(const AParentDir: String; ASubDir: String; AFileName: String; AGroupID: Integer);
    destructor Destroy; override;
    function GetFullFileName: String;
    property ParentDir: String read fParentDir write fParentDir;
    property SubDir: String read fSubDir write fSubDir;
    property FileName: String read fFileName write fFileName;
    property GroupID: Integer read fGroupID write fGroupID;
  end;
  TFileDirectoryList = TObjectList<TFileDirectory>;

function MD5File(const AFilename: String): String;
function SHAFile(const AFilename: String): String;
function GetFitScale(const ARect: TRectF; const AContainer: TRectF): TFitScale;
function FitInsideContainer(const ARect: TRectF; const AContainer: TRectF): TRectF; overload;
function FitInsideContainer(const ARect: TRectF; const AContainer: TRectF; FitScale: TFitScale): TRectF; overload;
function GetBoundingRect(Pixels: ISkPixmap; const AAlphaThreshold: Single = 0.0): TRect; overload;
function GetBoundingRect(SubRect: TRect; Pixels: ISkPixmap; const AAlphaThreshold: Single = 0.0): TRect; overload;
function GetLayerRect(NewRect: TRect; BoundRect: TRect): TRect;
function EncloseRect(ARect: TRect; Border: Integer = 0): TRect;
function EncloseRectF(ARect: TRect; Border: Integer = 0): TRectF;
procedure ScanFiles(const ADir: String; const ASubDir: String; const AFileExt: TArray<String>; var AList: TObjectList<TFileDirectory>; var GroupID: Integer; const Debug: TDebugProc = Nil);
function SheetRemap(const SpriteIndex: Integer; const SpriteCount: Integer; const SheetSizeX: Integer; const SheetSizeY: Integer): TRectArray;
procedure GrabSprite(LSurface: ISkSurface; const AFilename: String);
function IsSpriteEmpty(Pixels: ISkSurface; const AAlphaThreshold: Single = 0): Boolean; overload;
function IsSpriteEmpty(Pixels: ISkImage; const AAlphaThreshold: Single = 0): Boolean; overload;
function IsSpriteEmpty(Pixels: ISkPixmap; const AAlphaThreshold: Single = 0): Boolean; overload;

implementation

uses Math, System.Hash, FMX.Graphics;

constructor TFileDirectory.Create(const AParentDir: String; ASubDir,
  AFileName: String; AGroupID: Integer);
begin
  inherited Create;
  fParentDir := AParentDir;
  fSubDir := ASubDir;
  fFileName := AFileName;
  fGroupID := AGroupID;
end;

destructor TFileDirectory.Destroy;
begin
  inherited;
end;

function TFileDirectory.GetFullFileName: String;
begin
  if fSubDir = String.Empty then
    Result := fParentDir + TPath.DirectorySeparatorChar + fFileName
  else
    Result := fParentDir + TPath.DirectorySeparatorChar + fSubDir + TPath.DirectorySeparatorChar + fFileName;
end;

function MD5File(const AFilename: String): String;
begin
  if(FileExists(AFilename)) then
    Result := THashMD5.GetHashStringFromFile(AFilename)
  else
    Result := '';
end;

function SHAFile(const AFilename: String): String;
begin
  if(FileExists(AFilename)) then
    Result := THashSHA2.GetHashStringFromFile(AFilename)
  else
    Result := '';
end;

function GetFitScale(const ARect: TRectF; const AContainer: TRectF): TFitScale;
var
  Left, Top, Scale: Single;
begin
  Scale := Min((AContainer.Width / ARect.Width), (AContainer.Height / ARect.Height));
  Left := (AContainer.Width - (ARect.Width * Scale)) / 2;
  Top := (AContainer.Height - (ARect.Height * Scale)) / 2;
  Result.Scale := Scale;
  Result.Offset := PointF(Left, Top);
end;

function FitInsideContainer(const ARect: TRectF; const AContainer: TRectF; FitScale: TFitScale): TRectF;
begin
  Result := RectF(FitScale.Offset.X, FitScale.Offset.Y, FitScale.Offset.X + (ARect.Width * FitScale.Scale), FitScale.Offset.Y + (ARect.Height * FitScale.Scale));
end;

function FitInsideContainer(const ARect: TRectF; const AContainer: TRectF): TRectF;
var
  FitScale: TFitScale;
begin
  FitScale := GetFitScale(ARect, AContainer);
  Result := RectF(FitScale.Offset.X, FitScale.Offset.Y, FitScale.Offset.X + (ARect.Width * FitScale.Scale), FitScale.Offset.Y + (ARect.Height * FitScale.Scale));
end;

function GetBoundingRect(Pixels: ISkPixmap; const AAlphaThreshold: Single): TRect;
begin
  if Assigned(Pixels) then
    Result := GetBoundingRect(Rect(0, 0, Pixels.Width, Pixels.Height), Pixels, AAlphaThreshold)
  else
    Result := Rect(0,0,0,0);
end;

function IsSpriteEmpty(Pixels: ISkImage; const AAlphaThreshold: Single): Boolean;
begin
  if(Assigned(Pixels.PeekPixels)) then
    Result := IsSpriteEmpty(Pixels.PeekPixels, AAlphaThreshold)
  else
    Raise Exception.Create('IsSpriteEmpty : Null Pixmap');
end;

function IsSpriteEmpty(Pixels: ISkSurface; const AAlphaThreshold: Single): Boolean;
begin
  if(Assigned(Pixels.PeekPixels)) then
    Result := IsSpriteEmpty(Pixels.PeekPixels, AAlphaThreshold)
  else
    Raise Exception.Create('IsSpriteEmpty : Null Pixmap');
end;

function IsSpriteEmpty(Pixels: ISkPixmap; const AAlphaThreshold: Single): Boolean;
var
  X: Integer;
  Y: Integer;
  BoundTop: Integer;
  Found: Boolean;
begin
  BoundTop := 0;

  Found := False;
  For BoundTop := 0 to (Pixels.Height - 1) do
    begin
      For X := 0 to (Pixels.Width - 1) do
        begin
          if(Pixels.GetAlpha(X, BoundTop) > AAlphaThreshold) then
            begin
              Found := True;
              break;
            end;
        end;
      if(Found) then
        break;
    end;

  if(not Found) then
    Result := True
  else
    Result := False;
end;

function GetBoundingRect(SubRect: TRect; Pixels: ISkPixmap; const AAlphaThreshold: Single): TRect;
var
  X: Integer;
  Y: Integer;
  BoundLeft: Integer;
  BoundRight: Integer;
  BoundTop: Integer;
  BoundBottom: Integer;
  Found: Boolean;
begin
  BoundLeft := 0;
  BoundRight := 0;
  BoundTop := 0;
  BoundBottom := 0;

  Found := False;
  For BoundTop := 0 to (SubRect.Height - 1) do
    begin
      For X := 0 to (SubRect.Width - 1) do
        begin
          if(Pixels.GetAlpha(X, BoundTop) > AAlphaThreshold) then
            begin
              Found := True;
              break;
            end;
        end;
      if(Found) then
        break;
    end;
  if(not Found) then
    Exit(Rect(0,0,0,0));

  Found := False;
  For BoundBottom := (SubRect.Height - 1) downto BoundTop do
    begin
      For X := 0 to (SubRect.Width - 1) do
        begin
          if(Pixels.GetAlpha(X, BoundBottom) > AAlphaThreshold) then
            begin
              Found := True;
              break;
            end;
        end;
      if(Found) then
        break;
    end;

  Found := False;
  For BoundLeft := 0 to (SubRect.Width - 1) do
    begin
      For Y := BoundTop to BoundBottom do
        begin
          if(Pixels.GetAlpha(BoundLeft, Y) > AAlphaThreshold) then
            begin
              Found := True;
              break;
            end;
        end;
      if(Found) then
        break;
    end;

  Found := False;
  For BoundRight := (SubRect.Width - 1) downto BoundLeft do
    begin
      For Y := BoundTop to BoundBottom do
        begin
          if(Pixels.GetAlpha(BoundRight, Y) > AAlphaThreshold) then
            begin
              Found := True;
              break;
            end;
        end;
      if(Found) then
        break;
    end;

  Result := Rect(BoundLeft, BoundTop, BoundRight, BoundBottom);

end;

function GetLayerRect(NewRect: TRect; BoundRect: TRect): TRect;
begin
  Result := BoundRect;
  if(Result.IsEmpty) then
    Result := Rect(MaxInt, MaxInt, 0 , 0);

  if(not NewRect.IsEmpty) then
    begin
      if Result.Left > NewRect.Left then
        Result.Left := NewRect.Left;
      if Result.Right < NewRect.Right then
        Result.Right := NewRect.Right;
      if Result.Top > NewRect.Top then
        Result.Top := NewRect.Top;
      if Result.Bottom < NewRect.Bottom then
        Result.Bottom := NewRect.Bottom;
    end;
end;


function EncloseRect(ARect: TRect; Border: Integer = 0): TRect;
begin
  Result := Rect(ARect.Left - Border, ARect.Top - Border, ARect.Right + Border, ARect.Bottom + Border);
end;

function EncloseRectF(ARect: TRect; Border: Integer = 0): TRectF;
begin
  Result := Rect(ARect.Left - Border, ARect.Top - Border, ARect.Right + Border, ARect.Bottom + Border);
end;

procedure ScanFiles(const ADir: String; const ASubDir: String; const AFileExt: TArray<String>; var AList: TObjectList<TFileDirectory>; var GroupID: Integer; const Debug: TDebugProc);
var
  sr: TSearchRec;
  FileAttrs: Integer;
  AFileSpec: String;
  ASrch: String;
  I: Integer;
  DirItem: TFileDirectory;
  first: Boolean;
begin
//  if Assigned(Debug) then
//    Debug('Entering ' + ADir + ' -> ' + ASubDir);
  {$IF DEFINED(MSWINDOWS)}
  AFileSpec := '*.*';
  {$ELSE}
  AFileSpec := '*';
  {$ENDIF}
  First := True;

  FileAttrs := faAnyFile or faDirectory;
  if ASubDir = String.Empty then
    ASrch := ADir + TPath.DirectorySeparatorChar + AFileSpec
  else
    ASrch := ADir + TPath.DirectorySeparatorChar + ASubDir + TPath.DirectorySeparatorChar + AFileSpec;

  if FindFirst(ASrch, FileAttrs, sr) = 0 then
    begin
    repeat
      begin
        if ((sr.Attr and faDirectory) <> faDirectory) then // Not a Directory
          begin
            for I := Low(AFileExt) to High(AFileExt) do
              begin
                if CompareText(TPath.GetExtension(sr.Name), AFileExt[I]) = 0 then
                  begin
                    if First then
                      begin
                        First := False;
                        Inc(GroupID);
                        {$ifdef logit}
                        WriteLnLog('');
                        {$endif}
                      end;
                    if ASubDir = String.Empty then
                      DirItem := TFileDirectory.Create(ADir, '', sr.Name, GroupID)
                    else
                      DirItem := TFileDirectory.Create(ADir, ASubDir, sr.Name, GroupID);
                    AList.Add(DirItem);
                    if Assigned(Debug) then
                      Debug(Format('%5d - %03d - %s - %s', [AList.Count, DirItem.GroupID, DirItem.SubDir, DirItem.FileName]));
                    break;
                  end;
              end;
          end
        else
          begin
            if (sr.Name <> '.') and (sr.Name <> '..')  then
              begin
                if ASubDir = String.Empty then
                  ScanFiles(ADir, sr.Name, AFileExt, AList, GroupID, Debug)
                else
                  ScanFiles(ADir, ASubDir + TPath.DirectorySeparatorChar + sr.Name, AFileExt, AList, GroupID, Debug);
              end;
          end
      end;
    until FindNext(sr) <> 0;
    FindClose(sr);
    end;
end;

function SheetRemap(const SpriteIndex: Integer; const SpriteCount: Integer; const SheetSizeX: Integer; const SheetSizeY: Integer): TRectArray;
var
  Col: Integer;
  Row: Integer;
  procedure SplitRun(var Runs: Integer; const SpriteCol: Integer; const SpritesLeft: Integer);
  begin
    if(SpritesLeft > 0) then
      begin
        Inc(Runs);
        if(SpriteCol + SpritesLeft) > SheetSizeX then
          begin
            SplitRun(Runs, 0, SpritesLeft - (SheetSizeX - SpriteCol));
          end;
     end;
  end;
  procedure MakeRun(var Res: TRectArray; const Runs: Integer; const SpriteRow: Integer;  const SpriteCol: Integer; const SpritesLeft: Integer);
  var
    I: Integer;
    FromCol: Integer;
    ToCol: Integer;
  begin
    if(SpritesLeft > 0) then
      begin
        FromCol := SpriteCol;
        ToCol := SpritesLeft;
        for I := 0 to Runs - 1 do
          begin
            if (FromCol + ToCol) > SheetSizeX then
              begin
                Res[I] := Rect(FromCol,SpriteRow + I,FromCol + (SheetSizeX - FromCol),SpriteRow + I + 1);
                ToCol := ToCol - (SheetSizeX - FromCol)
              end
            else
              Res[I] := Rect(FromCol, SpriteRow + I, FromCol + ToCol, SpriteRow + I + 1);
            FromCol := 0;
          end;
     end;
  end;
begin
  Row := SpriteIndex div SheetSizeX;
  Col := SpriteIndex - (Row * SheetSizeX);
  var Runs := 0;
  SplitRun(Runs, Col, SpriteCount);
  SetLength(Result, Runs);
  MakeRun(Result, Runs, Row, Col, SpriteCount);
end;

procedure GrabSprite(LSurface: ISkSurface; const AFilename: String);
var
  LStream: TMemoryStream;
  LBitMap: TBitMap;
begin
  LStream := TMemoryStream.Create;
  LSurface.MakeImageSnapshot.EncodeToStream(LStream);
  LBitmap := TBitmap.Create;
  LBitmap.LoadFromStream(LStream);
  LBitmap.SaveToFile(AFilename);
  LBitmap.Free;
  LStream.Free;
end;

end.
