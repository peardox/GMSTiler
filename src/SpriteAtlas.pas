unit SpriteAtlas;

// {$DEFINE TESTSPRITE}

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
  end;
  TSpriteFrameArray = TArray<TSpriteFrame>;

  // TActionSprite stores an image with any transparent border removed
  // To reconstruct the original create a FWidth x FHeight empty
  // transparent image and add the FSprite pixels using the FOffset value
  // for placing and sizing. This ensures that the absolute minumum number
  // of pixels is actually stored reducing memory usage.
  TActionSprite = class
  strict private
    FSprite: ISkImage;
    FParts: TSpriteFrameArray;
    FFrames: Integer;
    FIsEmpty: Boolean;
    FIsCompact: Boolean;
    FOffset: TRect;
    FContainer: TRect;
    function ReConstruct: ISkImage;
  public
    constructor Create(const ASprite: ISkImage; const ABounds: TRect; const AContainer: TRect);
    property Sprite: ISkImage read ReConstruct;
    property IsEmpty: Boolean read FIsEmpty;
    property Offset: TRect read FOffset;
    property Container: TRect read FContainer;
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
    FSheetFormat: TSheetFormat;
    FFormat: TImageFormat;
  private
  public
    constructor Create;
    destructor Destroy(); override;
    function LoadSheet(const ASheetFormat: TSheetFormat; const AFilename: String; const Layer: String; const MakeCompact: Boolean = False): Boolean;
    property SizeX: Integer read FSizeX write FSizeX;
    property SizeY: Integer read FSizeY write FSizeY;
    property FrameSizeX: Integer read FFrameSizeX write FFrameSizeX;
    property FrameSizeY: Integer read FFrameSizeY write FFrameSizeY;
    property Frames: Integer read FFrames write FFrames;
    property Sprites: TObjectList<TActionSprite> read FSprites write FSprites;
    property SpareFrames: Integer read FSpareFrames write FSpareFrames;
    property SheetFormat: TSheetFormat read FSheetFormat write FSheetFormat;
    property Format: TImageFormat read FFormat write FFormat;
  end;

  TCompositeSheet = class
  strict private
    FSheets: TObjectList<TSpriteSheet>;
    FFrameSizeX: Integer;
    FFrameSizeY: Integer;
    FFrames: Integer;
    FFormat: TSheetFormat;
  public
    constructor Create(const AFormat: TSheetFormat; const ASpriteSizeX, ASpriteSizeY, AFrameCount: Integer);
    destructor Destroy(); override;
    procedure AddSheet(const AFilename: String);
    property FrameSizeX: Integer read FFrameSizeX write FFrameSizeX;
    property FrameSizeY: Integer read FFrameSizeY write FFrameSizeY;
    property Format: TSheetFormat read FFormat write FFormat;
    property Frames: Integer read FFrames write FFrames;
  end;

implementation

uses
  FMX.Skia, FMX.Graphics, TileUtils, GMSimpleLog;

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

function TSpriteSheet.LoadSheet(const ASheetFormat: TSheetFormat; const AFilename: String; const Layer: String; const MakeCompact: Boolean): Boolean;
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
  SprRect: TRect;
  Split: TRectArray;
  DeltaX: Integer;
  Action: TActionSprite;
//  DoneSave: Boolean;
  Bounds: TRect;
  LStream: TMemoryStream;
procedure ShowImageDraw(const ASurface: ISkSurface; const AImage: ISkImage; const src: TRectF; const dst: TRectF; const APaint: ISkPaint);
begin
  ASurface.Canvas.DrawImageRect(AImage, src, dst, APaint);
end;
begin
{
  DoneSave := False;
  if(not Afilename.Contains('Base')) then
    DoneSave := True;
}
  Result := False;
  FSheetFormat := ASheetFormat;

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
          LSurface := TSkSurface.MakeRaster(SprRect.Width, SprRect.Height);
          Split := SheetRemap(Layout.Items[Spr].FirstFrame + (SprDir * SprFrames), SprFrames, Layout.ColCount, Layout.RowCount); // = 46,44
          DeltaX := 0;
          for I := 0 to Length(Split) - 1 do
            begin
              LSurface.Canvas.DrawImageRect(
//              ShowImageDraw(LSurface,
                LImage,                          // 2344
                RectF(Split[I].Left * FFrameSizeX, Split[I].Top * FFrameSizeY,
                     (Split[I].Left * FFrameSizeX) + (Split[I].Width * FFrameSizeX) - 1, (Split[I].Bottom * FFrameSizeY) - 1),
                RectF(DeltaX * FFrameSizeX, 0,
                      (DeltaX * FFrameSizeX) + (Split[I].Width * FFrameSizeX) - 1, FFrameSizeY - 1),
                LPaint);
                DeltaX := DeltaX + Split[I].Width;
            end;
{
          if(not DoneSave) then
            begin
              GrabSprite(LSurface, 'SubBitmap.png');
              DoneSave := True;
            end;
}
          Bounds := GetBoundingRect(LSurface.PeekPixels, 0);
          LSprite := LSurface.MakeImageSnapshot;

          if(Bounds.IsEmpty) then
            begin
              LCompactSurface := TSkSurface.MakeRaster(1, 1);
              LCompactSurface.Canvas.Clear(TAlphaColors.Null);
              LCompactSprite := LCompactSurface.MakeImageSnapshot;
            end
          else
            begin
              LCompactSurface := TSkSurface.MakeRaster(Bounds.Width, Bounds.Height);
              LCompactSurface.Canvas.DrawImageRect(LSprite, Bounds, Rect(0, 0, Bounds.Width, Bounds.Height), LCompactPaint);
              LCompactSprite := LCompactSurface.MakeImageSnapshot;
{$IFDEF TESTSPRITE}
          if(not Action.IsEmpty) then
            GrabSprite(LCompactSurface, 'sprites/'+ Layer + '/' + Layout.Items[Spr].Action + '_dir' + IntToStr(SprDir) + '_strip'  + IntToStr(SprFrames) + '.png');
{$ENDIF}
            end;

          if(MakeCompact) then
            Action := TActionSprite.Create(LCompactSprite, Bounds, SprRect)
          else
            Action := TActionSprite.Create(LSprite, SprRect, SprRect);
          FSprites.Add(Action);


        //  GrabSprite(LSurface, System.SysUtils.Format('TestSprite%0*d.png',[3,Counter]));
        //  Counter := Counter + 1;
        //  ADrawProc(LSurface.Canvas, RectF(0, 0, AWidth, AHeight));
        end;
    end;

  Result := True;
end;

{ TCompositeSheet }

procedure TCompositeSheet.AddSheet(const AFilename: String);
begin
  // Placeholder
end;

constructor TCompositeSheet.Create(const AFormat: TSheetFormat;
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

constructor TActionSprite.Create(const ASprite: ISkImage; const ABounds: TRect; const AContainer: TRect);
begin
  FSprite := ASprite;
  FIsEmpty := ABounds.IsEmpty;
  FOffset := ABounds;
  FContainer := AContainer;
  if((FOffset.Width < FContainer.Width) or (FOffset.Height < FContainer.Height)) then
    FIsCompact := True
  else
    FIsCompact := False;

end;

function TActionSprite.ReConstruct: ISkImage;
var
  LPaint: TSkPaint;
  LSurface: ISkSurface;
begin
  if FIsCompact then
    begin
      LPaint := TSkPaint.Create;
      LSurface := TSkSurface.MakeRaster(FContainer.Width, FContainer.Height);
      LSurface.Canvas.Clear(TAlphaColors.Null);
      if not FIsEmpty then
        LSurface.Canvas.DrawImageRect(FSprite, Rect(0, 0, FOffset.Width, FOffset.Height), FOffset, LPaint);
      Result := LSurface.MakeImageSnapshot;
      LPaint.Free;
    end
  else
    Result := FSprite;
end;

end.
