unit GMSTilerMain;

interface
// {$DEFINE LAYERED}

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, System.Skia,
  FMX.Skia, FMX.Layouts, FMX.Controls.Presentation, FMX.StdCtrls, FMX.Objects,
  System.Generics.Defaults, System.Generics.Collections, FMX.Menus, FMX.TreeView,
  FMX.Memo.Types, FMX.ScrollBox, FMX.Memo,
  LayoutCSV, FMX.TabControl;

type
  TSkDrawProc = reference to procedure(const ACanvas: ISkCanvas; const ADest: TRectF);
  TLayerPaintProc =  reference to procedure(const ACanvas: ISkCanvas; const ADest: TRectF; const Image: ISkImage);
  TImageFormat = (Sheet, Strip);

  TCompositeSheet = class;

  TCompositeImage = class
  strict private
    FParent: TCompositeSheet;
{$IFDEF LAYERED}
    FLayer: TLayout;
{$ENDIF}
    FSprite: ISkImage;
    FSkImage: ISkImage;
    FSizeX: Integer;
    FSizeY: Integer;
    FFrameSizeX: Integer;
    FFrameSizeY: Integer;
    FFrames: Integer;
    FSpareFrames: Integer;
    FFormat: TImageFormat;
  public
    constructor Create(AOwner: TLayout);
    destructor Destroy(); override;
    function LoadSheet(AParent: TCompositeSheet; const AFilename: String; const SpriteSizeX: Integer; const SpriteSizeY: Integer; const FrameCount: Integer): Boolean;
    property Image: ISkImage read FSkImage write FSkImage;
    property SizeX: Integer read FSizeX write FSizeX;
    property SizeY: Integer read FSizeY write FSizeY;
    property FrameSizeX: Integer read FFrameSizeX write FFrameSizeX;
    property FrameSizeY: Integer read FFrameSizeY write FFrameSizeY;
    property Frames: Integer read FFrames write FFrames;
    property SpareFrames: Integer read FSpareFrames write FSpareFrames;
    property Format: TImageFormat read FFormat write FFormat;
    property Sprite: ISkImage read FSprite write FSprite;
  end;

  TCompositeSheet = class
  strict private
    FSheets: TObjectList<TCompositeImage>;
    FFormat: TSheetFormat;
    FFrameSizeX: Integer;
    FFrameSizeY: Integer;
    FFrames: Integer;
  public
    constructor Create(const AFormat: TSheetFormat; const ASpriteSizeX: Integer; const ASpriteSizeY: Integer; const AFrameCount: Integer);
    destructor Destroy(); override;
    procedure AddSheet(const AFilename: String);
    property FrameSizeX: Integer read FFrameSizeX write FFrameSizeX;
    property FrameSizeY: Integer read FFrameSizeY write FFrameSizeY;
    property Frames: Integer read FFrames write FFrames;
    property Format: TSheetFormat read FFormat write FFormat;
  end;

  TForm1 = class(TForm)
    Layout1: TLayout;
    MainMenu1: TMainMenu;
    MenuItem1: TMenuItem;
    MenuItem2: TMenuItem;
    TabControl1: TTabControl;
    TabItem1: TTabItem;
    LayoutTree: TLayout;
    TreeView1: TTreeView;
    Layout3: TLayout;
    Button1: TButton;
    Button2: TButton;
    Layout4: TLayout;
    LayoutLayer: TLayout;
    TabItem2: TTabItem;
    Memo1: TMemo;
    Layout2: TLayout;
    Button3: TButton;
    procedure Button1Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure MenuItem2Click(Sender: TObject);
    procedure LayoutLayerPaint(Sender: TObject; Canvas: TCanvas;
      const ARect: TRectF);
    procedure Button3Click(Sender: TObject);
  private
    Images: TObjectList<TCompositeImage>;
    FDrawProc: TSkDrawProc;
    FPaintLayerProc: TLayerPaintProc;
    procedure CompositeToBitmap(const AWidth, AHeight: Integer; const ADrawProc: TSkDrawProc);
    procedure AddImage(const AFilename: String; const SpriteSizeX: Integer; const SpriteSizeY: Integer; const FrameCount: Integer);
    procedure TestLoad;
    procedure DebugMessage(const AMsg: String);
{$IFDEF LAYERED}
    procedure PaintLayer(const ACanvas: ISkCanvas; const ADest: TRectF;
      const Image: ISkImage);
{$ELSE}
    procedure PaintComposite(const ACanvas: ISkCanvas; const ADest: TRectF);
{$ENDIF}
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

const
{$IF DEFINED(MSWINDOWS)}
  BaseDir: String = 'D:/work/assets/PVG/';
  SheetDir: String = 'RPG_Tools/RPGTools_CharacterPieces_1/Male/';
{$ELSEIF DEFINED(OSX64)}
  BaseDir: String = '/Volumes/Seagate4T/Assets/2D/PVG/';
  SheetDir: String = 'RPG_Tools/RPGTools_CharacterPieces_1/Male/';
{$ELSEIF DEFINED(LINUX64)}
  BaseDir: String = '/home/simon/PVG/';
  SheetDir: String = 'RPG_Tools/RPGTools_CharacterPieces_1/Male/';
{$ENDIF}

implementation

{$R *.fmx}

uses
  Math, FMX.Skia.Canvas, TileUtils;

procedure TForm1.AddImage(const AFilename: String; const SpriteSizeX: Integer; const SpriteSizeY: Integer; const FrameCount: Integer);
var
  CI: TCompositeImage;
begin
  CI := TCompositeImage.Create(LayoutLayer);
  if(Ci.LoadSheet(Nil, AFilename, SpriteSizeX, SpriteSizeY, FrameCount)) then
    begin
      Images.Add(CI);
      Memo1.Lines.Add(Format('%s - %s',[AFilename, SHAFile(AFilename)]));
    end
  else
    Memo1.Lines.Add('Failed : ' + AFilename);
end;

procedure ScanComposite(const APathname: String);
begin

end;

procedure TForm1.Button1Click(Sender: TObject);
begin
  TestLoad();
end;

procedure TForm1.TestLoad();
begin
  AddImage(BaseDir + SheetDir + 'Shadow/Spritesheet.png', 200, 200, 2496);
  AddImage(BaseDir + SheetDir + 'Base/RTP_1/Spritesheet.png', 200, 200, 2496);

  AddImage(BaseDir + SheetDir + 'Top/RTP_1/Spritesheet.png', 200, 200, 2496);
  AddImage(BaseDir + SheetDir + 'Bottom/RTP_1/Spritesheet.png', 200, 200, 2496);

  AddImage(BaseDir + SheetDir + 'Hair/RTP_1/Spritesheet.png', 200, 200, 2496);
  AddImage(BaseDir + SheetDir + 'Head/RTP_1/Spritesheet.png', 200, 200, 2496);
  AddImage(BaseDir + SheetDir + 'FacialHair/RTP_1/Spritesheet.png', 200, 200, 2496);

  AddImage(BaseDir + SheetDir + 'Weapons/RTP_Sword/Spritesheet.png', 200, 200, 2496);
  AddImage(BaseDir + SheetDir + 'Weapons/RTP_Shield/Spritesheet.png', 200, 200, 2496);

  LayoutLayer.RePaint;
end;

procedure TForm1.MenuItem2Click(Sender: TObject);

var
  fl: TFileDirectoryList;
  gid: Integer;
{
  sl: TSheetLayout;
  dl: TDirectionLayout;
}
begin
{
  dl := TDirectionLayout.Create;
  dl.ImportLayoutCSV(BaseDir + 'dirs.csv');
  Memo1.Lines.Add(dl.Dump);
  dl.Free;

  sl := TSheetLayout.Create(TSheetFormat.Character);
  sl.ImportLayoutCSV(BaseDir + 'character.csv');
  Memo1.Lines.Add(sl.Dump);
  sl.Free;

  sl := TSheetLayout.Create(TSheetFormat.Monster);
  sl.ImportLayoutCSV(BaseDir + 'monster.csv');
  Memo1.Lines.Add(sl.Dump);
  sl.Free;
}
//  ScanComposite(BaseDir);
  fl := TFileDirectoryList.Create;
  gid := 0;
  ScanFiles(BaseDir + SheetDir + '../..',
   '',
   ['.png'],
   fl,
   gid,
   DebugMessage);
   fl.free;
end;

procedure TForm1.Button2Click(Sender: TObject);
begin
{$IFNDEF LAYERED}
  CompositeToBitmap(200, 200, PaintComposite);
{$ENDIF}
end;

procedure TForm1.Button3Click(Sender: TObject);
begin
  Memo1.Lines.Clear;
end;

procedure TForm1.CompositeToBitmap(const AWidth, AHeight: Integer;
  const ADrawProc: TSkDrawProc);
var
  LSurface: ISkSurface;
  LStream: TMemoryStream;
  LBitMap: TBitMap;
begin
  if Assigned(ADrawProc) then
    begin
      LSurface := TSkSurface.MakeRaster(AWidth, AHeight);
      LSurface.Canvas.Clear(TAlphaColors.Null);
      ADrawProc(LSurface.Canvas, RectF(0, 0, AWidth, AHeight));

      LStream := TMemoryStream.Create;
      LSurface.MakeImageSnapshot.EncodeToStream(LStream);
      LBitmap := TBitmap.Create;
      LBitmap.LoadFromStream(LStream);
      LBitmap.SaveToFile('test2.png');
      LBitmap.Free;
      LStream.Free;
    end;
end;

procedure TForm1.DebugMessage(const AMsg: String);
begin
  Memo1.Lines.Add(AMsg);
end;

procedure TForm1.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  Images.Free;
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
  Images := TObjectList<TCompositeImage>.Create;
{$IFDEF LAYERED}
  FPaintLayerProc := PaintLayer;
{$ELSE}
  FDrawProc := PaintComposite;
{$ENDIF}
  // TestLoad();
end;

{$IFDEF LAYERED}
procedure TForm1.PaintLayer(const ACanvas: ISkCanvas; const ADest: TRectF; const Image: ISkImage);
var
  LPaint1: ISkPaint;
  I: Integer;
  scale, hscale, vscale: Single;
  SpriteRect, RenderRect: TRectF;
begin
  SpriteRect := RectF(0, 0, 200, 200);

  RenderRect := FitInsideContainer(SpriteRect, ADest);

  LPaint1 := TSkPaint.Create;
  LPaint1.AntiAlias := False;
  ACanvas.DrawImageRect(Image, SpriteRect, RenderRect, LPaint1);
end;
{$ELSE}
procedure TForm1.PaintComposite(const ACanvas: ISkCanvas; const ADest: TRectF);
var
  LPaint: ISkPaint;
  I: Integer;
  RenderRect: TRectF;
  SpriteRect, Bound, BoundRect: TRect;
  FitScale: TFitScale;
begin
  SpriteRect := Rect(0, 0, 200, 200);
  BoundRect := Rect(0,0,0,0);

  FitScale := GetFitScale(SpriteRect, ADest);
  RenderRect := FitInsideContainer(SpriteRect, ADest, FitScale);

  LPaint := TSkPaint.Create;
  LPaint.AntiAlias := False;
  for I := 0 to Images.Count -1 do
    begin
      if(Assigned(Images[I].Sprite)) then
        begin
        {$O-}
        Bound := GetBoundingRect(SpriteRect, Images[I].Sprite.PeekPixels, 0);
        BoundRect := GetLayerRect(Bound, BoundRect);
        if(Memo1.Lines.Count < Images.Count) then
          begin
            Memo1.Lines.Add(Format('Bound #%d  (%4d x %4d)-(%4d x %4d) : (%4d x %4d)', [I, Bound.Left, Bound.Top, Bound.Right, Bound.Bottom, Bound.Width, Bound.Height]));
            if(Memo1.Lines.Count = Images.Count) then
              Memo1.Lines.Add(Format('Bound Max (%4d x %4d)-(%4d x %4d) : (%4d x %4d)', [BoundRect.Left, BoundRect.Top, BoundRect.Right, BoundRect.Bottom, BoundRect.Width, BoundRect.Height]));
          end;
        {$O+}
        ACanvas.DrawImageRect(Images[I].Sprite, SpriteRect, RenderRect, LPaint);
        end;
    end;

//    var b: TSkBlender;
//    b.MakeMode(TSkBlendMode.DestATop);
    // BoundRect := FitInsideContainer(Boundrect, SpriteRect);
    BoundRect := EncloseRect(BoundRect, 2);
    LPaint.Color := $FFFF0000;
//    LPaint.Blender := b;
    var tl, tr, bl, br: TPointF;
    tl := PointF(FitScale.Offset.X + (BoundRect.Left * FitScale.Scale), FitScale.Offset.Y + (BoundRect.Top * FitScale.Scale));
    tr := PointF(FitScale.Offset.X + (BoundRect.Right * FitScale.Scale), FitScale.Offset.Y + (BoundRect.Top * FitScale.Scale));
    bl := PointF(FitScale.Offset.X + (BoundRect.Left * FitScale.Scale), FitScale.Offset.Y + (BoundRect.Bottom * FitScale.Scale));
    br := PointF(FitScale.Offset.X + (BoundRect.Right * FitScale.Scale), FitScale.Offset.Y + (BoundRect.Bottom * FitScale.Scale));
    ACanvas.DrawLine(tl, tr, LPaint);
    ACanvas.DrawLine(tl, bl, LPaint);
    ACanvas.DrawLine(tr, br, LPaint);
    ACanvas.DrawLine(bl, br, LPaint);

end;
{$ENDIF}

procedure TForm1.LayoutLayerPaint(Sender: TObject; Canvas: TCanvas;
  const ARect: TRectF);
var
  LCanvas: ISkCanvas;
{$IFDEF LAYERED}
  I: Integer;
{$ENDIF}
begin
{$IFDEF LAYERED}
  if Assigned(FPaintLayerProc) then
    begin
      for I := 0 to Images.Count -1 do
        begin
          LCanvas := TSkCanvasCustom(Canvas).Canvas;
          FPaintLayerProc(LCanvas, ARect, Images[I].Sprite);
        end;
    end;
{$ELSE}
  if Assigned(FDrawProc) then
    begin
      LCanvas := TSkCanvasCustom(Canvas).Canvas;
      FDrawProc(LCanvas, ARect);
    end;
{$ENDIF}
end;

{ TCompositeImage }

constructor TCompositeImage.Create(AOwner: TLayout);
begin
{$IFDEF LAYERED}
  FLayer := TLayout.Create(AOwner);
  FLayer.Parent := AOwner;
  FLayer.Align := TAlignLayout.Client;
{$ENDIF}
end;

destructor TCompositeImage.Destroy;
begin
  {$IFDEF LAYERED}
  FLayer.Free;
  {$ENDIF}
  inherited;
end;

function TCompositeImage.LoadSheet(AParent: TCompositeSheet; const AFilename: String; const SpriteSizeX: Integer; const SpriteSizeY: Integer; const FrameCount: Integer): Boolean;
var
  LPaint: ISkPaint;
  LSurface: ISkSurface;
begin
  Result := False;
  FParent := AParent;

  if(not FileExists(AFilename)) then
    Exit;

  FSkImage := TSkImage.MakeFromEncodedFile(AFilename);
  FFrameSizeX := SpriteSizeX;
  FFrameSizeY := SpriteSizeX;
  FSizeX := FSkImage.Width;
  FSizeY := FSkImage.Height;

  if((FSizeX = 0) or (FSizeY = 0)) then
    begin
      Raise Exception.CreateFmt('Empty FrameSize for %s', [AFilename]);
    end;

  if((FSizeX mod FFrameSizeX) <> 0) then
    begin
      Raise Exception.CreateFmt('Suspect FrameSize for %s', [AFilename]);
    end;
  if((FSizeY mod FFrameSizeY) <> 0) then
    begin
      Raise Exception.CreateFmt('Suspect FrameSize for %s', [AFilename]);
    end;

  FFormat := TImageFormat.Sheet;
  FFrames := (FSizeX div FFrameSizeX) * (FSizeY div FFrameSizeY);
  FSpareFrames := FFrames - FrameCount;
  if (FSpareFrames < 0) then
    begin
      Raise Exception.CreateFmt('Suspect SpareFrames for %s', [AFilename]);
    end;

  // SBDbg
  LPaint := TSkPaint.Create;
  LSurface := TSkSurface.MakeRaster(8 * FFrameSizeX, FFrameSizeY);
  LSurface.Canvas.Clear(TAlphaColors.Null);
  LSurface.Canvas.DrawImageRect(
    FSkImage,
    RectF(0, 0, 8 * FFrameSizeX, FFrameSizeY),
    RectF(0, 0, 8 * FFrameSizeX, FFrameSizeY),
    LPaint);
  Sprite := LSurface.MakeImageSnapshot;
//      ADrawProc(LSurface.Canvas, RectF(0, 0, AWidth, AHeight));

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
    FSheets := TObjectList<TCompositeImage>.Create;
    FFormat := AFormat;
    FFrameSizeX := ASpriteSizeX;
    FFrameSizeY := ASpriteSizeY;
    FFrames := AFrameCount;
end;

destructor TCompositeSheet.Destroy;
begin
  FSheets.Free();
  inherited;
end;

end.
