unit SpriteAtlas;

{$DEFINE TESTSPRITE}
interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  System.Skia, System.Generics.Defaults, System.Generics.Collections, LayoutCSV;

type
  TImageFormat = (Sheet, Strip);
  TSpriteStorage = (Full, Compact, CompactParts);

  TCompositeSheet = class;

  TSpriteFrame = class
  strict private
    FSprite: ISkImage;
    FOffset: TRect;
    FContainer: TRect;
    FCompactBytes: Integer;
    FSpriteBytes: Integer;
  public
    constructor Create(const ASprite: ISkImage; const ABounds: TRect; const AContainer: TRect);
    destructor Destroy(); override;
    property Sprite: ISkImage read FSprite;
    property Offset: TRect read FOffset;
    property Container: TRect read FContainer;
    property CompactBytes: Integer read FCompactBytes write FCompactBytes;
    property SpriteBytes: Integer read FSpriteBytes write FSpriteBytes;
  end;
  TSpriteFrameArray = TArray<TSpriteFrame>;

  // TActionSprite stores an image with any transparent border removed
  // To reconstruct the original create a FWidth x FHeight empty
  // transparent image and add the FSprite pixels using the FOffset value
  // for placing and sizing. This ensures that the absolute minumum number
  // of pixels is actually stored reducing memory usage.
  TActionSprite = class
  strict private
    FFrames: Integer;
    FSprite: ISkImage;
    FParts: TSpriteFrameArray;
    FFrameBytes: Integer;
    FCompactBytes: Integer;
    FSpriteBytes: Integer;
    FIsEmpty: Boolean;
    FIsCompact: Boolean;
    FOffset: TRect;
    FContainer: TRect;
    function ReConstruct: ISkImage;
  public
    constructor Create(const ASprite: TSpriteFrameArray; const ABounds: TRect; const AContainer: TRect; const AIsCompact: Boolean; const FrameCount: Integer);
    destructor Destroy(); override;
    property Sprite: ISkImage read ReConstruct;
    property IsEmpty: Boolean read FIsEmpty;
    property Offset: TRect read FOffset;
    property Container: TRect read FContainer;
    property Parts: TSpriteFrameArray read FParts write FParts;
    property CompactBytes: Integer read FCompactBytes write FCompactBytes;
    property SpriteBytes: Integer read FSpriteBytes write FSpriteBytes;
  end;

  TSpriteSheet = class
  strict private
    FSprites: TObjectList<TActionSprite>;
    FSizeX: Integer;
    FSizeY: Integer;
    FFrameSizeX: Integer;
    FFrameSizeY: Integer;
    FFrames: Integer;
    FSpareFrames: Integer;
    FSheetFormat: String;
    FFormat: TImageFormat;
    FCompactBytes: Integer;
    FSpriteBytes: Integer;
  private
  public
    constructor Create;
    destructor Destroy(); override;
    function LoadSheet(const ASheetFormat: String; const AFilename: String; const Layer: String; const MakeCompact: Boolean = False): Boolean;
    property SizeX: Integer read FSizeX write FSizeX;
    property SizeY: Integer read FSizeY write FSizeY;
    property FrameSizeX: Integer read FFrameSizeX write FFrameSizeX;
    property FrameSizeY: Integer read FFrameSizeY write FFrameSizeY;
    property Frames: Integer read FFrames write FFrames;
    property Sprites: TObjectList<TActionSprite> read FSprites write FSprites;
    property SpareFrames: Integer read FSpareFrames write FSpareFrames;
    property SheetFormat: String read FSheetFormat write FSheetFormat;
    property Format: TImageFormat read FFormat write FFormat;
    property CompactBytes: Integer read FCompactBytes write FCompactBytes;
    property SpriteBytes: Integer read FSpriteBytes write FSpriteBytes;
  end;

  TCompositeSheet = class
  strict private
    FSheets: TObjectList<TSpriteSheet>;
    FFrameSizeX: Integer;
    FFrameSizeY: Integer;
    FFrames: Integer;
    FFormat: String;
  public
    constructor Create(const AFormat: String; const ASpriteSizeX, ASpriteSizeY, AFrameCount: Integer);
    destructor Destroy(); override;
    procedure AddSheet(const AFilename: String);
    property FrameSizeX: Integer read FFrameSizeX write FFrameSizeX;
    property FrameSizeY: Integer read FFrameSizeY write FFrameSizeY;
    property Format: String read FFormat write FFormat;
    property Frames: Integer read FFrames write FFrames;
  end;

implementation

uses
  FMX.Skia, FMX.Graphics, TileUtils, GMSimpleLog;

function SheetRemap(const SpriteIndex: Integer; const SheetSizeX: Integer; const SheetSizeY: Integer): TRect;
var
  Col: Integer;
  Row: Integer;
begin
  Row := SpriteIndex div SheetSizeX;
  Col := SpriteIndex - (Row * SheetSizeX);
  Result := Rect(Col,Row,Col + 1, Row + 1);
end;

{ TSpriteSheet }

constructor TSpriteSheet.Create;
begin
    FSprites := TObjectList<TActionSprite>.Create;
end;

destructor TSpriteSheet.Destroy;
begin
  if(Assigned(FSprites)) then
    FSprites.Free;
  inherited;
end;

function TSpriteSheet.LoadSheet(const ASheetFormat: String; const AFilename: String; const Layer: String; const MakeCompact: Boolean): Boolean;
var
  LPaint: ISkPaint;
  LSurface: ISkSurface;
  LImage: ISkImage;
  LSprite: ISkImage;
  LCompactPaint: ISkPaint;
  LCompactSprite: ISkImage;
  LCompactSurface: ISkSurface;
  I: Integer;
  Spr: Integer;
  Layout: TSheetLayout;
  SprFrames: Integer;
  SprDir: Integer;
  FrameIndex: Integer;
  SprRect: TRect;
  LParts: TSpriteFrameArray;
  Split: TRect;
  PartRect: TRect;
  PartBounds: TRect;
  LCBytes: Integer;
  LSBytes: Integer;
  DeltaX: Integer;
  Action: TActionSprite;
//  DoneSave: Boolean;
  NilCount, SprCount: Integer;
  Bounds: TRect;
  LStream: TMemoryStream;
procedure ShowImageDraw(const ASurface: ISkSurface; const AImage: ISkImage; const src: TRectF; const dst: TRectF; const APaint: ISkPaint);
begin
  ASurface.Canvas.DrawImageRect(AImage, src, dst, APaint);
end;
begin
  NilCount := 0;
  SprCount := 0;
{
  DoneSave := False;
  if(not Afilename.Contains('Base')) then
    DoneSave := True;
}
  Result := False;
  FSheetFormat := ASheetFormat;
  FCompactBytes := 0;
  FSpriteBytes := 0;

  if(not FileExists(AFilename)) then
    Exit;

  if(Assigned(SheetLayouts)) then
    begin
      if(not SheetLayouts.TryGetValue(FSheetFormat, Layout)) then
        Raise Exception.CreateFmt('Couldn''t find Layout for %s', [AFilename]);
    end
  else
    Raise Exception.CreateFmt('Couldn''t find Layout for %s', [AFilename]);

  LStream := TMemoryStream.Create;
  try
    LStream.LoadFromFile(AFilename);
    LImage := TSkImage.MakeFromEncodedStream(LStream);
  finally
    LStream.Free;
  end;

  FSizeX := LImage.Width;
  FSizeY := LImage.Height;

  if((FSizeX = 0) or (FSizeY = 0)) then
    Raise Exception.CreateFmt('Empty Image : %s', [AFilename]);

  if((FSizeX mod Layout.ColCount) <> 0) then
    Raise Exception.CreateFmt('Fractional FrameSize : %s', [AFilename]);

  if((FSizeY mod Layout.RowCount) <> 0) then
    Raise Exception.CreateFmt('Fractional FrameSize : %s', [AFilename]);

  FFormat := TImageFormat.Sheet;

  FFrameSizeX := FSizeX div Layout.ColCount;
  FFrameSizeY := FSizeX div Layout.RowCount;


  FFrames := Layout.ColCount * Layout.RowCount;
  FSpareFrames := FFrames - Layout.FrameCount;
  if (FSpareFrames < 0) then
    Raise Exception.CreateFmt('Suspect SpareFrames : %s', [AFilename]);
  if (FSpareFrames >= Layout.ColCount) then
    Raise Exception.CreateFmt('Suspect Empty Frame Row : %s', [AFilename]);

{$IFDEF TESTSPRITE}
  if(not DirectoryExists('sprites')) then
    mkdir('sprites');
  if(not DirectoryExists('sprites/'+Layer)) then
    mkdir('sprites/'+Layer);
{$ENDIF}

  // Deconstruct SpriteSheet into SpriteRuns
  LPaint := TSkPaint.Create;
  LCompactPaint := TSkPaint.Create;
  FSprites.Clear;
  FSprites.Capacity := Layout.FrameCount;

  for Spr := 0 to Layout.Items.Count - 1 do
    begin
      SprFrames := Layout.Items[Spr].ActionFrames;
      for SprDir := 0 to Layout.Items[Spr].ActionDirections - 1 do
        begin
          SprRect := Rect(0, 0, SprFrames * FFrameSizeX, FFrameSizeY);
          PartRect := Rect(0, 0, FFrameSizeX, FFrameSizeY);
          LParts := TSpriteFrameArray.Create();
          SetLength(LParts, SprFrames);
          Bounds := Rect(0,0,0,0);
          LCBytes := 0;
          LSBytes := 0;

          for FrameIndex := 0 to SprFrames - 1 do
            begin
              LSurface := TSkSurface.MakeRaster(FFrameSizeX, FFrameSizeY);
              Split := SheetRemap(Layout.Items[Spr].FirstFrame + (SprDir * SprFrames) + FrameIndex, Layout.ColCount, Layout.RowCount);
              LSurface.Canvas.DrawImageRect(
              //ShowImageDraw(LSurface,
                LImage,
                RectF(Split.Left * FFrameSizeX,
                      Split.Top * FFrameSizeY,
                      (Split.Left * FFrameSizeX) + (Split.Width * FFrameSizeX) - 1,
                      (Split.Bottom * FFrameSizeY) - 1),
                RectF(0,
                      0,
                      (Split.Width * FFrameSizeX) - 1,
                      FFrameSizeY - 1),
                LPaint);

              if ((Layer = 'base') and (Layout.Items[Spr].Action = 'Walk') and (SprDir = 0) and (FrameIndex = 2)) then
                PartBounds := GetBoundingRect(LSurface.PeekPixels, 0)
              else
                PartBounds := GetBoundingRect(LSurface.PeekPixels, 0);
              LSprite := LSurface.MakeImageSnapshot;
              Bounds := GetLayerRect(PartBounds, Bounds);
              if(PartBounds.IsEmpty) then
                begin
                  LCompactSprite := Nil;
                  Inc(NilCount);
                end
              else
                begin
//              if ((Layer = 'base') and (Layout.Items[Spr].Action = 'Walk') and (SprDir = 0) and (FrameIndex = 2)) then
                  LCompactSurface := TSkSurface.MakeRaster(PartBounds.Width, PartBounds.Height);
                  LCompactSurface.Canvas.DrawImageRect(LSprite, PartBounds, Rect(0, 0, PartBounds.Width-1, PartBounds.Height-1), LCompactPaint);
                  LCompactSprite := LCompactSurface.MakeImageSnapshot;
                  Inc(SprCount);
                end;

              LParts[FrameIndex] := TSpriteFrame.Create(LCompactSprite, PartBounds, PartRect);

{$IF DEFINED(TESTSPRITE)}
              if ((Layer = 'base') and (Layout.Items[Spr].Action = 'Walk') and (SprDir = 0)) then
                begin
                  GMS.LogFormat('layer = %s - action = %s - dir = %d, Frame = %d',[Layer, Layout.Items[Spr].Action, SprDir, FrameIndex]);
                  GMS.LogFormat('#%d Bounds : Left = %d - Top = %d, Right = %d, Bottom = %d - Width = %d, Height = %d',[FrameIndex, PartBounds.Left, PartBounds.Top, PartBounds.Right, PartBounds.Bottom, PartBounds.Width, PartBounds.Height]);
                  if(not Action.IsEmpty) then
                    GrabSprite(LCompactSurface, 'sprites/'+ Layout.Items[Spr].Action + IntToStr(FrameIndex) + '.png');
                  if(LSurface <> Nil) then
                    GrabSprite(LSurface, 'sprites/surface'+ IntToStr(FrameIndex) + '.png');
                end;
{$ENDIF}

              LCBytes := LCBytes + LParts[FrameIndex].CompactBytes;
              LSBytes := LSBytes + LParts[FrameIndex].SpriteBytes;

            end;

            Action := TActionSprite.Create(LParts, Bounds, SprRect, True, SprFrames);
            FSprites.Add(Action);

            Action.CompactBytes := LCBytes;
            Action.SpriteBytes := LSBytes;

            FCompactBytes := FCompactBytes + Action.CompactBytes;
            FSpriteBytes := FSpriteBytes + Action.SpriteBytes;
{$IF DEFINED(TESTSPRITE)}
          if ((Layer = 'base') and (Layout.Items[Spr].Action = 'Walk') and (SprDir = 0)) then
            begin
              if(not Action.IsEmpty) then
                GrabSprite(Action.Sprite, 'sprites/'+ Layer + '/' + Layout.Items[Spr].Action + '_dir' + IntToStr(SprDir) + '_strip'  + IntToStr(SprFrames) + '.png');
            end;
{$ENDIF}
        end;
    end;

  GMS.LogFormat('NilCount = %d, SprCount = %d',[NilCount, SprCount]);
  Result := True;
end;

{ TCompositeSheet }

procedure TCompositeSheet.AddSheet(const AFilename: String);
begin
  // Placeholder
end;

constructor TCompositeSheet.Create(const AFormat: String;
  const ASpriteSizeX, ASpriteSizeY, AFrameCount: Integer);
begin
    FSheets := TObjectList<TSpriteSheet>.Create;
    FFrameSizeX := ASpriteSizeX;
    FFrameSizeY := ASpriteSizeY;
    FFrames := AFrameCount;
end;

destructor TCompositeSheet.Destroy;
begin
  FSheets.Free();
  inherited;
end;

{ TActionSprite }

// Action := TActionSprite.Create(LParts, SprRect, True, SprFrames)
constructor TActionSprite.Create(const ASprite: TSpriteFrameArray; const ABounds: TRect; const AContainer: TRect; const AIsCompact: Boolean; const FrameCount: Integer);
begin
  FIsCompact := AIsCompact;
  if FIsCompact then
    begin
      FSprite := Nil;
      FParts := ASprite;
    end
  else
  FIsEmpty := ABounds.IsEmpty;
  FOffset := ABounds;
  FContainer := AContainer;
end;

destructor TActionSprite.Destroy;
var
  I: Integer;
begin
    FSprite := Nil;
    For I := 0 to Length(FParts) - 1 do
      FParts[I].Free;
    SetLength(FParts, 0);
  inherited;
end;

function TActionSprite.ReConstruct: ISkImage;
var
  LPaint: TSkPaint;
  LSurface: ISkSurface;
  I: Integer;
begin
  if FSprite = Nil then
    begin
      LPaint := TSkPaint.Create;
      LSurface := TSkSurface.MakeRaster(FContainer.Width, FContainer.Height);
      LSurface.Canvas.Clear(TAlphaColors.Null);
      for I := 0 to Length(Parts) - 1 do
        begin
          if Parts[I].Sprite <> Nil then
            LSurface.Canvas.DrawImageRect(Parts[I].Sprite,
              Rect(0, 0, Parts[I].Offset.Width - 1, Parts[I].Offset.Height - 1),
              Rect((Parts[I].Container.Width * I) + Parts[I].Offset.Left, Parts[I].Offset.Top, (Parts[I].Container.Width * I) + Parts[I].Offset.Right, Parts[I].Offset.Bottom),
              LPaint);
        end;
      FSprite := LSurface.MakeImageSnapshot;
      LPaint.Free;
    end;
Result := FSprite;
end;

{ TSpriteFrame }

constructor TSpriteFrame.Create(const ASprite: ISkImage; const ABounds,
  AContainer: TRect);
begin
  FSprite := ASprite;
  FOffset := ABounds;
  FContainer := AContainer;
  FCompactBytes := FOffset.Width * FOffset.Height * 4;
  FSpriteBytes := FContainer.Width * FContainer.Height * 4;
end;

destructor TSpriteFrame.Destroy;
begin
  FSprite := Nil;
  inherited;
end;

end.
