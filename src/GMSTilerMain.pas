unit GMSTilerMain;

interface
{$DEFINE IMAGELOADUSESTREAM}
{$DEFINE USECHARATER}

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, System.Skia,
  FMX.Skia, FMX.Layouts, FMX.Controls.Presentation, FMX.StdCtrls, FMX.Objects,
  System.Generics.Defaults, System.Generics.Collections, FMX.Menus, FMX.TreeView,
  FMX.Memo.Types, FMX.ScrollBox, FMX.Memo, FMX.TabControl,
  LayoutCSV, SpriteAtlas;

type
  TSkDrawProc = reference to procedure(const ACanvas: ISkCanvas; const ADest: TRectF; const SpriteRect: TRectF);
  TLayerPaintProc =  reference to procedure(const ACanvas: ISkCanvas; const ADest: TRectF; const Image: ISkImage);

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
    DoneLayerSize: Boolean;
    Images: TObjectList<TSpriteSheet>;
    FDrawProc: TSkDrawProc;
    procedure CompositeToBitmap(const AWidth, AHeight: Integer; const ADrawProc: TSkDrawProc);
    procedure AddImage(const ASheetFormat: TSheetFormat; const AFilename: String; const Layer: String; const SpriteSizeX: Integer; const SpriteSizeY: Integer; const FrameCount: Integer);
    procedure TestLoad;
    procedure DebugMessage(const AMsg: String);
    procedure PaintComposite(const ACanvas: ISkCanvas; const ADest: TRectF; const SpriteRect: TRectF);
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form1: TForm1;
  Counter: Integer;

const
{$IF DEFINED(MSWINDOWS)}
  BaseDir: String = 'D:/work/assets/PVG/';
  {$IF DEFINED(USECHARATER)}
  SheetDir: String = 'RPG_Tools/RPGTools_CharacterPieces_1/Male/';
  {$ELSE}
  SheetDir: String = 'Otherworlds/OtherWorlds_Monsters/Dragon/';
  {$ENDIF}
{$ELSEIF DEFINED(OSX64)}
  BaseDir: String = '/Volumes/Seagate4T/Assets/2D/PVG/';
  {$IF DEFINED(USECHARATER)}
  SheetDir: String = 'RPG_Tools/RPGTools_CharacterPieces_1/Male/';
  {$ELSE}
  SheetDir: String = 'Otherworlds/OtherWorlds_Monsters/Dragon/';
  {$ENDIF}
{$ELSEIF DEFINED(LINUX64)}
  BaseDir: String = '/home/simon/PVG/';
  {$IF DEFINED(USECHARATER)}
  SheetDir: String = 'RPG_Tools/RPGTools_CharacterPieces_1/Male/';
  {$ELSE}
  SheetDir: String = 'Otherworlds/OtherWorlds_Monsters/Dragon/';
  {$ENDIF}
{$ENDIF}

implementation

{$R *.fmx}

uses
  Math, FMX.Skia.Canvas, TileUtils,
  DateUtils;

procedure TForm1.AddImage(const ASheetFormat: TSheetFormat; const AFilename: String; const Layer: String; const SpriteSizeX: Integer; const SpriteSizeY: Integer; const FrameCount: Integer);
var
  CI: TSpriteSheet;
begin
  CI := TSpriteSheet.Create;
  if(Ci.LoadSheet(ASheetFormat, AFilename, Layer, SpriteSizeX, SpriteSizeY, FrameCount)) then
    begin
      Images.Add(CI);
      Memo1.Lines.Add(Format('%s - %s',[AFilename, SHAFile(AFilename)]));
    end
  else
    Memo1.Lines.Add('Failed : ' + AFilename);
end;

procedure TForm1.Button1Click(Sender: TObject);
var
  Layout: TSheetLayout;
  Direction: TDirectionLayout;
begin
  Memo1.Lines.Clear;

  if(Assigned(SheetLayouts)) then
    begin
      if(SheetLayouts.TryGetValue(TSheetFormat.Character, Layout)) then
        begin
          Memo1.Lines.Add(Format('Cols : %d, Rows : %d, Frames : %d',[Layout.ColCount, Layout.RowCount, Layout.FrameCount]));
          Memo1.Lines.Add(Layout.Dump);
        end;
      if(SheetLayouts.TryGetValue(TSheetFormat.Monster, Layout)) then
        begin
          Memo1.Lines.Add(Format('Cols : %d, Rows : %d, Frames : %d',[Layout.ColCount, Layout.RowCount, Layout.FrameCount]));
          Memo1.Lines.Add(Layout.Dump);
        end;
    end;
  if(Assigned(DirectionLayouts)) then
    begin
      if(DirectionLayouts.TryGetValue(TDirectionFormat.Directions8, Direction)) then
        Memo1.Lines.Add(Direction.Dump);
    end;

  DoneLayerSize := False;
  Images.Clear;
  TestLoad();
end;

procedure TForm1.TestLoad();
var
  elapsed: Int64;
  from: TDateTime;
begin
  from := Now;
  {$IF DEFINED(USECHARATER)}
  AddImage(TSheetFormat.Character, BaseDir + SheetDir + 'Shadow/Spritesheet.png', 'shadow', 200, 200, 2496);
  AddImage(TSheetFormat.Character, BaseDir + SheetDir + 'Base/RTP_1/Spritesheet.png', 'base', 200, 200, 2496);

  AddImage(TSheetFormat.Character, BaseDir + SheetDir + 'Top/RTP_1/Spritesheet.png', 'top', 200, 200, 2496);
  AddImage(TSheetFormat.Character, BaseDir + SheetDir + 'Bottom/RTP_1/Spritesheet.png', 'bottom', 200, 200, 2496);

  AddImage(TSheetFormat.Character, BaseDir + SheetDir + 'Hair/RTP_1/Spritesheet.png', 'hair', 200, 200, 2496);
  AddImage(TSheetFormat.Character, BaseDir + SheetDir + 'Head/RTP_1/Spritesheet.png', 'head', 200, 200, 2496);
  AddImage(TSheetFormat.Character, BaseDir + SheetDir + 'FacialHair/RTP_1/Spritesheet.png', 'facial_hair', 200, 200, 2496);
//  AddImage(TSheetFormat.Character, BaseDir + SheetDir + 'Weapons/RTP_Xbow/Spritesheet.png', 'xbow', 200, 200, 2496);
  AddImage(TSheetFormat.Character, BaseDir + SheetDir + 'Weapons/RTP_Sword/Spritesheet.png', 'sword', 200, 200, 2496);
  AddImage(TSheetFormat.Character, BaseDir + SheetDir + 'Weapons/RTP_Shield/Spritesheet.png', 'shield', 200, 200, 2496);
  {$ELSE}
  AddImage(TSheetFormat.Monster, BaseDir + SheetDir + 'Spritesheet_Shadow.png', 'shadow', 800, 800, 440);
  AddImage(TSheetFormat.Monster, BaseDir + SheetDir + 'Spritesheet.png', 'monster', 800, 800, 440);
  {$ENDIF}


  elapsed := DateUtils.MilliSecondsBetween(Now, from);
  Memo1.Lines.Add(Format('Load Time : %1.3f',[Single(elapsed / 1000)]));

  LayoutLayer.RePaint;
end;

procedure TForm1.MenuItem2Click(Sender: TObject);
var
  fl: TFileDirectoryList;
  gid: Integer;
begin
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
  DoneLayerSize := True;
  CompositeToBitmap(200 * 8, 200, PaintComposite);
end;

procedure TForm1.Button3Click(Sender: TObject);
begin
  Memo1.Lines.Clear;
end;

procedure TForm1.CompositeToBitmap(const AWidth, AHeight: Integer;
  const ADrawProc: TSkDrawProc);
var
  LSurface: ISkSurface;
begin
  if Assigned(ADrawProc) then
    begin
      LSurface := TSkSurface.MakeRaster(AWidth, AHeight);
      LSurface.Canvas.Clear(TAlphaColors.Null);
      ADrawProc(LSurface.Canvas, RectF(0, 0, AWidth, AHeight), RectF(0, 0, AWidth, AHeight));
      {
      LStream := TMemoryStream.Create;
      LSurface.MakeImageSnapshot.EncodeToStream(LStream);
      LBitmap := TBitmap.Create;
      LBitmap.LoadFromStream(LStream);
      LBitmap.SaveToFile('test2.png');
      LBitmap.Free;
      LStream.Free;
      }
      GrabSprite(LSurface, 'newtest.png');

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
  Images := TObjectList<TSpriteSheet>.Create;
  FDrawProc := PaintComposite;
  // TestLoad();
end;

procedure TForm1.PaintComposite(const ACanvas: ISkCanvas; const ADest: TRectF; const SpriteRect: TRectF);
var
  LPaint: ISkPaint;
  I: Integer;
  RenderRect: TRectF;
//  Bound: TRect;
  BoundRect: TRect;
  FitScale: TFitScale;
begin
  BoundRect := Rect(0,0,0,0);

  FitScale := GetFitScale(SpriteRect, ADest);
  RenderRect := FitInsideContainer(SpriteRect, ADest, FitScale);

  LPaint := TSkPaint.Create;
  LPaint.AntiAlias := False;
  for I := 0 to Images.Count -1 do
    begin
      if(Assigned(Images[I].Sprite)) then
        begin
        {
          Bound := GetBoundingRect(SpriteRect, Images[I].Sprite.PeekPixels, 0);
          BoundRect := GetLayerRect(Bound, BoundRect);
          if(not DoneLayerSize) then
            begin
              Memo1.Lines.Add(Format('Bound #%d  (%4d x %4d)-(%4d x %4d) : (%4d x %4d)', [I, Bound.Left, Bound.Top, Bound.Right, Bound.Bottom, Bound.Width, Bound.Height]));
              if(I = (Images.Count-1)) then
                begin
                  Memo1.Lines.Add(Format('Bound Max (%4d x %4d)-(%4d x %4d) : (%4d x %4d)', [BoundRect.Left, BoundRect.Top, BoundRect.Right, BoundRect.Bottom, BoundRect.Width, BoundRect.Height]));
                  DoneLayerSize := True;
                end;
            end;
          }
          ACanvas.DrawImageRect(Images[I].Sprite, SpriteRect, RenderRect, LPaint);
        end;
    end;
{
    BoundRect := EncloseRect(BoundRect, 2);
    LPaint.Color := $FFFF0000;
    var tl, tr, bl, br: TPointF;
    tl := PointF(FitScale.Offset.X + (BoundRect.Left * FitScale.Scale), FitScale.Offset.Y + (BoundRect.Top * FitScale.Scale));
    tr := PointF(FitScale.Offset.X + (BoundRect.Right * FitScale.Scale), FitScale.Offset.Y + (BoundRect.Top * FitScale.Scale));
    bl := PointF(FitScale.Offset.X + (BoundRect.Left * FitScale.Scale), FitScale.Offset.Y + (BoundRect.Bottom * FitScale.Scale));
    br := PointF(FitScale.Offset.X + (BoundRect.Right * FitScale.Scale), FitScale.Offset.Y + (BoundRect.Bottom * FitScale.Scale));
    ACanvas.DrawLine(tl, tr, LPaint);
    ACanvas.DrawLine(tl, bl, LPaint);
    ACanvas.DrawLine(tr, br, LPaint);
    ACanvas.DrawLine(bl, br, LPaint);
}

end;

procedure TForm1.LayoutLayerPaint(Sender: TObject; Canvas: TCanvas;
  const ARect: TRectF);
var
  LCanvas: ISkCanvas;
begin
  if Assigned(FDrawProc) then
    begin
      LCanvas := TSkCanvasCustom(Canvas).Canvas;
      FDrawProc(LCanvas, ARect, Rect(0,0,200,200));
    end;
end;

end.
