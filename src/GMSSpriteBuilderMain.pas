unit GMSSpriteBuilderMain;

interface
{$DEFINE IMAGELOADUSESTREAM}
{$DEFINE USECHARATER}

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, System.Skia,
  FMX.Skia, FMX.Layouts, FMX.Controls.Presentation, FMX.StdCtrls, FMX.Objects,
  System.Generics.Defaults, System.Generics.Collections, FMX.Menus, FMX.TreeView,
  FMX.Memo.Types, FMX.ScrollBox, FMX.Memo, FMX.TabControl,
  System.Math.Vectors, FMX.Controls3D, FMX.Layers3D,
  LayoutCSV, SpriteAtlas, SkComponents;

type
  TSkDrawProc = reference to procedure(const SpriteIndex: Integer; const ACanvas: ISkCanvas; const ADest: TRectF; const SpriteRect: TRectF);

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
    mmLog: TMemo;
    Layout2: TLayout;
    Button3: TButton;
    fsbLayer: TFramedVertScrollBox;
    procedure Button1Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure MenuItem2Click(Sender: TObject);
    procedure LayoutLayerPaint(Sender: TObject; Canvas: TCanvas;
      const ARect: TRectF);
    procedure Button3Click(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
  private
    DoneLayerSize: Boolean;
    Images: TObjectList<TSpriteSheet>;
    FDrawProc: TSkDrawProc;
    procedure CompositeToBitmap(const AWidth, AHeight: Integer; const ADrawProc: TSkDrawProc);
    procedure AddImage(const ASheetFormat: TSheetFormat; const AFilename: String; const Layer: String);
    procedure TestLoad;
    procedure DebugMessage(const AMsg: String);
    procedure PaintComposite(const SpriteIndex: Integer; const ACanvas: ISkCanvas; const ADest: TRectF; const SpriteRect: TRectF);
    procedure LogMemo(s: String);
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
  DateUtils, GMSimpleLog;

procedure TForm1.AddImage(const ASheetFormat: TSheetFormat; const AFilename: String; const Layer: String);
var
  CI: TSpriteSheet;
begin
  CI := TSpriteSheet.Create;
  if(Ci.LoadSheet(ASheetFormat, AFilename, Layer)) then
    begin
      Images.Add(CI);
      GMSLog(Format('%s - %s',[AFilename, SHAFile(AFilename)]));
    end
  else
    GMSLog('Failed : ' + AFilename);
end;

procedure TForm1.Button1Click(Sender: TObject);
var
  Layout: TSheetLayout;
  Direction: TDirectionLayout;
begin
  mmLog.Lines.Clear;

  if(Assigned(SheetLayouts)) then
    begin
      if(SheetLayouts.TryGetValue(TSheetFormat.Character, Layout)) then
        begin
          GMSLog(Format('Cols : %d, Rows : %d, Frames : %d',[Layout.ColCount, Layout.RowCount, Layout.FrameCount]));
          GMSLog(Layout.Dump);
        end;
      if(SheetLayouts.TryGetValue(TSheetFormat.Monster, Layout)) then
        begin
          GMSLog(Format('Cols : %d, Rows : %d, Frames : %d',[Layout.ColCount, Layout.RowCount, Layout.FrameCount]));
          GMSLog(Layout.Dump);
        end;
    end;
  if(Assigned(DirectionLayouts)) then
    begin
      if(DirectionLayouts.TryGetValue(TDirectionFormat.Directions8, Direction)) then
        GMSLog(Direction.Dump);
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
  AddImage(TSheetFormat.Character, BaseDir + SheetDir + 'Shadow/Spritesheet.png', 'shadow');
  AddImage(TSheetFormat.Character, BaseDir + SheetDir + 'Base/RTP_1/Spritesheet.png', 'base');

  AddImage(TSheetFormat.Character, BaseDir + SheetDir + 'Top/RTP_1/Spritesheet.png', 'top');
  AddImage(TSheetFormat.Character, BaseDir + SheetDir + 'Bottom/RTP_1/Spritesheet.png', 'bottom');

  AddImage(TSheetFormat.Character, BaseDir + SheetDir + 'Hair/RTP_1/Spritesheet.png', 'hair');
  AddImage(TSheetFormat.Character, BaseDir + SheetDir + 'Head/RTP_1/Spritesheet.png', 'head');
  AddImage(TSheetFormat.Character, BaseDir + SheetDir + 'FacialHair/RTP_1/Spritesheet.png', 'facial_hair');
//  AddImage(TSheetFormat.Character, BaseDir + SheetDir + 'Weapons/RTP_Xbow/Spritesheet.png', 'xbow');
  AddImage(TSheetFormat.Character, BaseDir + SheetDir + 'Weapons/RTP_Sword/Spritesheet.png', 'sword');
  AddImage(TSheetFormat.Character, BaseDir + SheetDir + 'Weapons/RTP_Shield/Spritesheet.png', 'shield');
  {$ELSE}
  AddImage(TSheetFormat.Monster, BaseDir + SheetDir + 'Spritesheet_Shadow.png', 'shadow');
  AddImage(TSheetFormat.Monster, BaseDir + SheetDir + 'Spritesheet.png', 'monster');
  {$ENDIF}


  elapsed := DateUtils.MilliSecondsBetween(Now, from);
  GMSLog(Format('Load Time : %1.3f',[Single(elapsed / 1000)]));

  LayoutLayer.RePaint;
end;

procedure TForm1.MenuItem2Click(Sender: TObject);
var
  fl: TFileDirectoryList;
  gid: Integer;
begin
  fl := TFileDirectoryList.Create;
  gid := 0;
  ScanFiles(BaseDir + SheetDir + '..',
   '',
   ['.png'],
   fl,
   gid);
   fl.free;
end;

procedure TForm1.Button2Click(Sender: TObject);
begin
  DoneLayerSize := True;
  CompositeToBitmap(200 * 8, 200, PaintComposite);
end;

procedure TForm1.Button3Click(Sender: TObject);
begin
  mmLog.Lines.Clear;
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
      ADrawProc(10, LSurface.Canvas, RectF(0, 0, AWidth, AHeight), RectF(0, 0, AWidth, AHeight));
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
  GMSLog(AMsg);
end;

procedure TForm1.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  Images.Free;
end;

procedure TForm1.FormCreate(Sender: TObject);
var
  I: Integer;
  bl: TBorderLayout;
  test: TCircle;
begin
  GMSLog := LogMemo;
  TabControl1.ActiveTab := TabItem1;
  Images := TObjectList<TSpriteSheet>.Create;
  FDrawProc := PaintComposite;
  // TestLoad();
  for I := 0 to 8 do
    begin
      bl := TBorderLayout.Create(fsbLayer);
      bl.Width := 100;
      bl.Height := 100;
      bl.Parent := fsbLayer;
      bl.Align := TAlignLayout.Top;
      fsbLayer.Content.AddObject(bl);
      Test := TCircle.Create(bl);
      Test.Align := TAlignLayout.Client;
      Test.Parent := bl;
    end;
end;

procedure TForm1.FormDestroy(Sender: TObject);
begin
  GMSLog := GMSNullLogger;
end;

procedure TForm1.PaintComposite(const SpriteIndex: Integer; const ACanvas: ISkCanvas; const ADest: TRectF; const SpriteRect: TRectF);
var
  LPaint: ISkPaint;
  I: Integer;
  RenderRect: TRectF;
  Bound: TRect;
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
      if(Assigned(Images[I].Sprites[SpriteIndex].Sprite)) then
        begin
          {
          Bound := GetBoundingRect(SpriteRect, Images[I].Sprites[I].Sprite.PeekPixels, 0);
          BoundRect := GetLayerRect(Bound, BoundRect);

          if(not DoneLayerSize) then
            begin
              GMSLog(Format('Bound #%d  (%4d x %4d)-(%4d x %4d) : (%4d x %4d)', [I, Bound.Left, Bound.Top, Bound.Right, Bound.Bottom, Bound.Width, Bound.Height]));
              if(I = (Images.Count-1)) then
                begin
                  GMSLog(Format('Bound Max (%4d x %4d)-(%4d x %4d) : (%4d x %4d)', [BoundRect.Left, BoundRect.Top, BoundRect.Right, BoundRect.Bottom, BoundRect.Width, BoundRect.Height]));
                  DoneLayerSize := True;
                end;
            end;
          }
          ACanvas.DrawImageRect(Images[I].Sprites[SpriteIndex].Sprite, SpriteRect, RenderRect, LPaint);
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
    LPaint.Color := $FFFF0000;
    ACanvas.DrawLine(ADest.TopLeft, PointF(ADest.Right, ADest.Top), LPaint);
    ACanvas.DrawLine(PointF(ADest.Left, ADest.Top+1), PointF(ADest.Left, ADest.Bottom-1), LPaint);
    ACanvas.DrawLine(PointF(ADest.Right-1, ADest.Top+1), PointF(ADest.Right-1, ADest.Bottom-1), LPaint);
    ACanvas.DrawLine(PointF(ADest.Left, ADest.Bottom-1), PointF(ADest.Right, ADest.Bottom-1), LPaint);
end;

procedure TForm1.LayoutLayerPaint(Sender: TObject; Canvas: TCanvas;
  const ARect: TRectF);
var
  LCanvas: ISkCanvas;
begin
  if Assigned(FDrawProc) then
    begin
      LCanvas := TSkCanvasCustom(Canvas).Canvas;
      FDrawProc(10, LCanvas, ARect, Rect(0,0,200,200));
    end;
end;

procedure TForm1.LogMemo(s: String);
begin
  mmLog.Lines.Add(s);
end;

end.
