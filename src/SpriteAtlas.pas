unit SpriteAtlas;

// {$DEFINE TESTSPRITE}

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  System.Skia, System.Generics.Defaults, System.Generics.Collections, LayoutCSV;

type
  TImageFormat = (Sheet, Strip);

  TCompositeSheet = class;

  TActionSprite = class
    FSprite: ISkImage;
    FIsEmpty: Boolean;
  public
    constructor Create(ASprite: ISkImage; ASpriteIsEmpty: Boolean);
    property Sprite: ISkImage read FSprite;
    property IsEmpty: Boolean read FIsEmpty;
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
  public
    constructor Create;
    destructor Destroy(); override;
    function LoadSheet(const ASheetFormat: TSheetFormat; const AFilename: String; const Layer: String): Boolean;
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
  FMX.Skia, FMX.Graphics, TileUtils;

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

function TSpriteSheet.LoadSheet(const ASheetFormat: TSheetFormat; const AFilename: String; const Layer: String): Boolean;
var
  LPaint: ISkPaint;
  LSurface: ISkSurface;
  LImage: ISkImage;
  LSprite: ISkImage;
//  LBitmap: TBitmap;
  I: Integer;
  Spr: Integer;
  Layout: TSheetLayout;
  SprFrames: Integer;
  SprDir: Integer;
  Split: TRectArray;
  DestX: Integer;
  Action: TActionSprite;
//  bound: TRect;
{$IF DEFINED(IMAGELOADUSESTREAM)}
  LStream: TMemoryStream;
{$ENDIF}
procedure ShowImageDraw(const ASurface: ISkSurface; const AImage: ISkImage; const src: TRectF; const dst: TRectF; const APaint: ISkPaint);
begin
  ASurface.Canvas.DrawImageRect(AImage, src, dst, APaint);
end;
begin
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

{$IF DEFINED(IMAGELOADUSESTREAM)}
  LStream := TMemoryStream.Create;
  try
    LStream.LoadFromFile(AFilename);
    LImage := TSkImage.MakeFromEncodedStream(LStream);
  finally
    LStream.Free;
  end;
{$ELSE}
//  LBitmap := TBitmap.Create;
  try
//  LBitmap.LoadFromFile(AFilename);

  LImage := TSkImage.MakeFromEncodedFile(AFilename);
//  LImage := BitmapToSkImage(LBitmap); //.MakeFromEncodedFile(AFilename);
{$ENDIF}
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
  FSprites.Clear;

  for Spr := 0 to Layout.Items.Count - 1 do
    begin
      SprFrames := Layout.Items[Spr].ActionFrames;
      for SprDir := 0 to Layout.Items[Spr].ActionDirections - 1 do
        begin
          LSurface := TSkSurface.MakeRaster(SprFrames * FFrameSizeX, FFrameSizeY);
         // LSurface.Canvas.Clear(TAlphaColors.Null);
          Split := SheetRemap(Layout.Items[Spr].FirstFrame + (SprDir * SprFrames), SprFrames, Layout.ColCount, Layout.RowCount); // = 46,44
          DestX := 0;
          for I := 0 to Length(Split) - 1 do
            begin
//              LSurface.Canvas.DrawImageRect(
              ShowImageDraw(LSurface,
                LImage,                          // 2344
                RectF(Split[I].Left * FFrameSizeX, Split[I].Top * FFrameSizeY,
                     (Split[I].Left * FFrameSizeX) + (Split[I].Width * FFrameSizeX) - 1, (Split[I].Bottom * FFrameSizeY) - 1),
                RectF(DestX * FFrameSizeX, 0,
                      (DestX * FFrameSizeX) + (Split[I].Width * FFrameSizeX) - 1, FFrameSizeY - 1),
                LPaint);
                DestX := DestX + Split[I].Width;
            end;
        {
          bound := GetBoundingRect(LSurface.PeekPixels, 0);
          if(bound.IsEmpty) then
            Raise Exception.CreateFmt('Empty Image for %s', [AFilename]);
        }
          LSprite := LSurface.MakeImageSnapshot;
          Action := TActionSprite.Create(LSprite, IsSpriteEmpty(LSprite));
          FSprites.Add(Action);
{$IFDEF TESTSPRITE}
          if(not Action.IsEmpty) then
            GrabSprite(LSurface, 'sprites/'+ Layer + '/' + Layout.Items[Spr].Action + '_dir' + IntToStr(SprDir) + '_strip'  + IntToStr(SprFrames) + '.png');
{$ENDIF}

        //  GrabSprite(LSurface, System.SysUtils.Format('TestSprite%0*d.png',[3,Counter]));
        //  Counter := Counter + 1;
        //  ADrawProc(LSurface.Canvas, RectF(0, 0, AWidth, AHeight));
        end;
    end;

    finally
  //    LBitmap.Free;
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

constructor TActionSprite.Create(ASprite: ISkImage; ASpriteIsEmpty: Boolean);
begin
  FSprite := ASprite;
  FIsEmpty := ASpriteIsEmpty;
end;

end.
