unit GMSTilerMain;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, System.Skia,
  FMX.Skia, FMX.Layouts, FMX.Controls.Presentation, FMX.StdCtrls, FMX.Objects,
  System.Generics.Defaults, System.Generics.Collections, FMX.Menus, FMX.TreeView,
  FMX.Memo.Types, FMX.ScrollBox, FMX.Memo;

type
  TSkDrawProc = reference to procedure(const ACanvas: ISkCanvas; const ADest: TRectF);
  TImageFormat = (Sheet, Strip);
  TSheetFormat = (Character, Monster);
  TActionType = (Loop, Singular, PingPong, UnAnimated, NeedsEdit);
  TActionTypeSet = Set of TActionType;

  TCompositeSheet = class;

  TCompositeImage = class
  strict private
    FParent: TCompositeSheet;
    FSkImage: ISkImage;
    FSizeX: Integer;
    FSizeY: Integer;
    FFrameSizeX: Integer;
    FFrameSizeY: Integer;
    FFrames: Integer;
    FSpareFrames: Integer;
    FFormat: TImageFormat;
  public
    function LoadSheet(AParent: TCompositeSheet; const AFilename: String; const SpriteSizeX: Integer; const SpriteSizeY: Integer; const FrameCount: Integer): Boolean;
    property Image: ISkImage read FSkImage write FSkImage;
    property SizeX: Integer read FSizeX write FSizeX;
    property SizeY: Integer read FSizeY write FSizeY;
    property FrameSizeX: Integer read FFrameSizeX write FFrameSizeX;
    property FrameSizeY: Integer read FFrameSizeY write FFrameSizeY;
    property Frames: Integer read FFrames write FFrames;
    property SpareFrames: Integer read FSpareFrames write FSpareFrames;
    property Format: TImageFormat read FFormat write FFormat;
  end;

  TSheetLayoutItem = class
  strict private
    FAction: string;
    FFrames: Integer;
    FActionType: TActionTypeSet;
    FActionFrrames: Integer;
    FActionDirections: Integer;
  public
    property Action: string read FAction write FAction;
    property Frames: Integer read FFrames write FFrames;
    property ActionType: TActionTypeSet read FActionType write FActionType;
    property ActionFrrames: Integer read FActionFrrames write FActionFrrames;
    property ActionDirections: Integer read FActionDirections write FActionDirections;
  end;

  TSheetLayout = class
  strict private
    FLayout: TObjectList<TCompositeImage>;
    FFormat: TSheetFormat;
  public
    constructor Create(const AFormat: TSheetFormat);
    destructor Destroy(); override;
    procedure ImportLayoutCSV(const AFilename: String);
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
    Layout2: TLayout;
    Layout3: TLayout;
    Layout4: TLayout;
    Layout5: TLayout;
    Button1: TButton;
    SkPaintBox1: TSkPaintBox;
    Button2: TButton;
    MainMenu1: TMainMenu;
    MenuItem1: TMenuItem;
    MenuItem2: TMenuItem;
    TreeView1: TTreeView;
    Memo1: TMemo;
    procedure Button1Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure MenuItem2Click(Sender: TObject);
    procedure SkPaintBox1Draw(ASender: TObject; const ACanvas: ISkCanvas;
      const ADest: TRectF; const AOpacity: Single);
  private
    Images: TObjectList<TCompositeImage>;
    FDrawProc: TSkDrawProc;
    procedure PaintComposite(const ACanvas: ISkCanvas; const ADest: TRectF);
    procedure CompositeToBitmap(const AWidth, AHeight: Integer; const ADrawProc: TSkDrawProc);
    procedure AddImage(const AFilename: String; const SpriteSizeX: Integer; const SpriteSizeY: Integer; const FrameCount: Integer);
    procedure TestLoad;
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

const
{$IF DEFINED(MSWINDOWS)}
  BaseDir: String = 'D:/work/assets/PVG/RPG_Tools/RPGTools_CharacterPieces_1/Male/';
{$ELSEIF DEFINED(OSX64)}
  BaseDir: String = '/Volumes/Seagate4T/Assets/2D/PVG/RPG_Tools/RPGTools_CharacterPieces_1/Male/';
{$ENDIF}

implementation

{$R *.fmx}

uses
  FMX.Skia.Canvas;

procedure TForm1.AddImage(const AFilename: String; const SpriteSizeX: Integer; const SpriteSizeY: Integer; const FrameCount: Integer);
var
  CI: TCompositeImage;
begin
  CI := TCompositeImage.Create;
  if(Ci.LoadSheet(Nil, AFilename, SpriteSizeX, SpriteSizeY, FrameCount)) then
    Images.Add(CI)
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
  AddImage(BaseDir + 'Shadow/Spritesheet.png', 200, 200, 2496);
  AddImage(BaseDir + 'Base/RTP_1/Spritesheet.png', 200, 200, 2496);
  AddImage(BaseDir + 'Bottom/RTP_1/Spritesheet.png', 200, 200, 2496);
  AddImage(BaseDir + 'FacialHair/RTP_1/Spritesheet.png', 200, 200, 2496);
  AddImage(BaseDir + 'Hair/RTP_1/Spritesheet.png', 200, 200, 2496);
  AddImage(BaseDir + 'Head/RTP_1/Spritesheet.png', 200, 200, 2496);
  AddImage(BaseDir + 'Top/RTP_1/Spritesheet.png', 200, 200, 2496);
  AddImage(BaseDir + 'Weapons/RTP_Sword/Spritesheet.png', 200, 200, 2496);
  AddImage(BaseDir + 'Weapons/RTP_Shield/Spritesheet.png', 200, 200, 2496);
  SkPaintBox1.Redraw;
end;

procedure TForm1.MenuItem2Click(Sender: TObject);
begin
  ScanComposite(BaseDir);
end;

procedure TForm1.Button2Click(Sender: TObject);
begin
  CompositeToBitmap(8 * 200, 200, PaintComposite);
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

procedure TForm1.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  Images.Free;
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
  FDrawProc := PaintComposite;
  Images := TObjectList<TCompositeImage>.Create;
end;

procedure TForm1.PaintComposite(const ACanvas: ISkCanvas; const ADest: TRectF);
var
  LPaint1: ISkPaint;
  I: Integer;
begin
  LPaint1 := TSkPaint.Create;
  LPaint1.AntiAlias := False;
  for I := 0 to Images.Count -1 do
    begin
      if(Assigned(Images[I].Image)) then
        ACanvas.DrawImage(Images[I].Image, 0, 0, LPaint1);
    end;
end;

procedure TForm1.SkPaintBox1Draw(ASender: TObject; const ACanvas: ISkCanvas;
  const ADest: TRectF; const AOpacity: Single);
begin
  if Assigned(FDrawProc) then
    FDrawProc(ACanvas, ADest);
end;

{ TCompositeImage }

function TCompositeImage.LoadSheet(AParent: TCompositeSheet; const AFilename: String; const SpriteSizeX: Integer; const SpriteSizeY: Integer; const FrameCount: Integer): Boolean;
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

{ TSheetLayout }

constructor TSheetLayout.Create(const AFormat: TSheetFormat);
begin
  FFormat := AFormat;
  FLayout := TObjectList<TCompositeImage>.Create;
end;

destructor TSheetLayout.Destroy;
begin
  FLayout.Free;
  inherited;
end;

procedure TSheetLayout.ImportLayoutCSV(const AFilename: String);
begin
  // Placeholder
end;

end.
