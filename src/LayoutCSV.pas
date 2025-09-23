unit LayoutCSV;

interface

uses
  System.SysUtils, System.IOUtils, System.Types, System.Classes,
  System.Generics.Collections, System.Generics.Defaults,
  System.TypInfo,
  System.Rtti;

type
  TActionType = (Loop, Singular, PingPong, UnAnimated, NeedsEdit, Offset);
  TActionTypeSet = Set of TActionType;
  TDirectionFormat = (Directions8);

  TSheetLayoutItem = class
  private
    FAction: string;
    FFirstFrame: Integer;
    FFrames: Integer;
    FActionType: TActionTypeSet;
    FActionFrames: Integer;
    FActionDirections: Integer;
  public
    constructor Create;
    property Action: string read FAction write FAction;
    property FirstFrame: Integer read FFirstFrame write FFirstFrame;
    property Frames: Integer read FFrames write FFrames;
    property ActionType: TActionTypeSet read FActionType write FActionType;
    property ActionFrames: Integer read FActionFrames write FActionFrames;
    property ActionDirections: Integer read FActionDirections write FActionDirections;
  end;
  TSheetLayoutItemList = TObjectList<TSheetLayoutItem>;

  TSheetSpec = class
  private
    FFilename: String;
    FColCount: Integer;
    FRowCount: Integer;
    FFrameCount: Integer;
    FSpareCount: Integer;
  public
    property Filename: String read FFilename write FFilename;
    property ColCount: Integer read FColCount write FColCount;
    property RowCount: Integer read FRowCount write FRowCount;
    property FrameCount: Integer read FFrameCount write FFrameCount;
    property SpareCount: Integer read FSpareCount write FSpareCount;
  end;

  TSheetLayout = class
  private
    FItems: TSheetLayoutItemList;
    FFormat: String;
    FFilename: String;
    FColCount: Integer;
    FRowCount: Integer;
    FFrameCount: Integer;
    FSpareCount: Integer;
    function ParseCSV(const S: String; const Linenum: Integer = 0): TSheetLayoutItem;
    function ActionSetFromString(const s: String): TActionTypeSet;
    function NormalizeAction(const S: String): String;
  public
    constructor Create;
    destructor Destroy(); override;
    procedure ImportLayoutCSV(const AFilename: String);
    procedure Clear;
    function Dump: String;
    property Items: TSheetLayoutItemList read FItems write FItems;
    property Format: String read FFormat write FFormat;
    property Filename: String read FFilename write FFilename;
    property ColCount: Integer read FColCount write FColCount;
    property RowCount: Integer read FRowCount write FRowCount;
    property FrameCount: Integer read FFrameCount write FFrameCount;
    property SpareCount: Integer read FSpareCount write FSpareCount;
  end;

  TDirectionLayoutItem = class
  private
    FCompassShort: string;
    FCompassLong: string;
    FCompassAngle: Integer;
  public
    property CompassShort: string read FCompassShort write FCompassShort;
    property CompassLong: string read FCompassLong write FCompassLong;
    property CompassAngle: Integer read FCompassAngle write FCompassAngle;
  end;
  TDirectionLayoutItemList = TObjectList<TDirectionLayoutItem>;


  TDirectionLayout = class
  private
    FLayout: TDirectionLayoutItemList;
    function ParseCSV(const S: String; const Linenum: Integer = 0): TDirectionLayoutItem;
  public
    constructor Create;
    destructor Destroy(); override;
    procedure ImportLayoutCSV(const AFilename: String);
    procedure Clear;
    function Dump: String;
  end;

  TSheetLayoutDict = TObjectDictionary<String, TSheetLayout>;
  TSheetLayoutList = TObjectList<TSheetLayout>;
  TDirectionLayoutDict = TObjectDictionary<TDirectionFormat, TDirectionLayout>;

procedure LoadLayouts();
procedure  UnLoadLayouts();

var
  SheetLayouts: TSheetLayoutDict;
  DirectionLayouts: TDirectionLayoutDict;

const
{$IF DEFINED(MSWINDOWS)}
  LayoutDir: String = 'D:/work/assets/PVG/';
{$ELSEIF DEFINED(OSX64)}
  LayoutDir: String = '/Volumes/Seagate4T/Assets/2D/PVG/';
{$ELSEIF DEFINED(LINUX64)}
  LayoutDir: String = '/home/simon/PVG/';
{$ENDIF}

  CHECK_DBLQUOTE = $22;
  CHECK_COMMA    = $2C;
  CHECK_MINUS    = $2D;
  CHECK_PERIOD   = $2E;
  CHECK_DIGIT_0  = $30;
  CHECK_DIGIT_9  = $39;
  CHECK_UCASE_A  = $41;
  CHECK_UCASE_Z  = $5A;
  CHECK_LCASE_A  = $61;
  CHECK_LCASE_Z  = $7A;
  CHECK_SLASH    = $5C;

  CHECK_UCASE_W  = $57;
  CHECK_UCASE_U  = $55;
  CHECK_UCASE_B  = $42;
  CHECK_UCASE_R  = $52;
  CHECK_UCASE_G  = $47;

implementation

uses JsonSerializer, JsonSettings;

{ TSheetLayoutItem }

constructor TSheetLayoutItem.Create;
begin
  inherited;
end;

{ TSheetLayout }

procedure TSheetLayout.Clear;
begin
  FItems.Clear;
end;

constructor TSheetLayout.Create;
begin
  inherited;
  FItems := TSheetLayoutItemList.Create(True);
end;

destructor TSheetLayout.Destroy;
begin
  if(Assigned(FItems)) then
    FItems.Free;
  inherited;
end;

function TSheetLayout.Dump: String;
var
  I: Integer;
  R: TSheetLayoutItem;
begin
  Result := '';
  for I := 0 to FItems.Count - 1 do
    begin
      R := FItems[I];
      Result := Result +
                System.SysUtils.Format('%2d - %-20s - %4d - %2d - %2d - %2d' + sLineBreak, [
                  I, R.Action, R.FirstFrame, R.Frames, R.ActionFrames, R.ActionDirections
                  ]);
    end;
end;

procedure TSheetLayout.ImportLayoutCSV(const AFilename: String);
var
  sr: TStreamReader;
  I: Integer;
  S: String;
  Rec: TSheetLayoutItem;
  FrameCount: Integer;
begin
  sr := Nil;
  FrameCount := 0;
  try
    if FileExists(AFilename) then
      begin
        sr := TStreamReader.Create(AFileName);
        I := 0;
        while not sr.EndOfStream do
          begin
            Inc(I);
            S := sr.ReadLine;
            if Length(S) = 0 then
              begin
                Continue;
              end;
            if I = 1 then
              Continue;
            Rec := ParseCSV(s, I);
            Rec.FirstFrame := FrameCount;
            FrameCount := FrameCount + Rec.Frames;
            FItems.Add(Rec);
          end;
      end;
  finally
    sr.Free;
  end;
end;

function TSheetLayout.NormalizeAction(const S: String): String;
var
  l: Integer;
  i: Integer;
  NextUpper: Boolean;
  function DoSkip(const S: String): Boolean;
  var
    I: Integer;
  const
    SkipChars: String = ' -';
  begin
    Result := False;

    for I := 1 to Length(SkipChars) do
      if S = SkipChars[I] then
        Result := True;
  end;
begin
  Result := '';
  NextUpper := True;
  // Length of string
  l := Length(S);

  // Loop over the string
  for i := 1 to l do
    begin
      if NextUpper then
         begin
          Result := Result + String(S[I]).ToUpper;
          NextUpper := False;
         end
      else if(DoSkip(S[I])) then
          NextUpper := True
      else
          Result := Result + S[I];


    end;
end;

function TSheetLayout.ParseCSV(const S: String; const Linenum: Integer): TSheetLayoutItem;
var
  l: Integer;
  i: Integer;
  FieldNum: Integer;
  QuotedField: Boolean;
  EscapedChar: Boolean;
  OutRec: TSheetLayoutItem;
  TempText: String;
  C: Char;
begin
  QuotedField := False;
  EscapedChar := False;
  FieldNum := 0;

  OutRec := TSheetLayoutItem.Create;

  // Length of string
  l := Length(S);

  // Loop over the string
  for i := 1 to l do
    begin
    if EscapedChar then
      begin
        if (ord(S[i]) = CHECK_DBLQUOTE) then
          TempText := TempText + '"'
        else
          begin
            C := S[i];
            raise Exception.Create('Escaped Field Found - Unhandled (' + IntToStr(Linenum + 1) + ', ' + IntToStr(i + 1) + ', ' + IntToHex(Ord(C)) + ')' + sLineBreak + S);
          end;
        EscapedChar := False;
      end
    else if (ord(S[i]) = CHECK_SLASH) then
      begin
        if EscapedChar then
        begin
          raise Exception.Create('Escaped Field Found - Unhandled by EscapedChar');
        end
      else
        begin
          EscapedChar := True;
        end;
      end
    else if (ord(S[i]) = CHECK_DBLQUOTE) and not(EscapedChar) then
      begin
        if QuotedField then
        begin
          QuotedField := False;
        end
      else
        begin
          QuotedField := True;
        end;
      end
    else if (ord(S[i]) = CHECK_COMMA) and not(EscapedChar) and (QuotedField) then
      begin
        TempText := TempText + ','
      end
    else if (ord(S[i]) = CHECK_COMMA) and not(EscapedChar) and not(QuotedField) then
      begin
        TempText := TempText.Trim;
        case FieldNum of
          0: OutRec.Action := NormalizeAction(TempText);
          1: OutRec.Frames := StrToIntDef(Temptext, -1);
          2: OutRec.ActionType := ActionSetFromString(TempText);
          3: OutRec.ActionFrames := StrToIntDef(Temptext, -1);
          4: OutRec.ActionDirections := StrToIntDef(Temptext, -1);
        else
          raise Exception.Create('Fieldnum count overflow');
        end;

        Inc(FieldNum);
        TempText := '';
      end
    else
      begin
        TempText := TempText + S[i];
      end;
    end;

    // Catch Trailing field
    TempText := TempText.Trim;
    case FieldNum of
      0: OutRec.Action := NormalizeAction(TempText);
      1: OutRec.Frames := StrToIntDef(Temptext, -1);
      2: OutRec.ActionType := ActionSetFromString(TempText);
      3: OutRec.ActionFrames := StrToIntDef(Temptext, -1);
      4: OutRec.ActionDirections := StrToIntDef(Temptext, -1);
    else
      raise Exception.Create('Fieldnum count overflow');
    end;

    Result := OutRec;
end;

{ TSheetLayoutItem }

function TSheetLayout.ActionSetFromString(const s: String): TActionTypeSet;
var
  I: Integer;
begin
// TActionType = (Loop, Singular, PingPong, UnAnimated, NeedsEdit);
  Result := [];
  for I := 1 to Length(S) do
    begin
      if S[I] = 'L' then
        Include(Result, TActionType.Loop)
      else if S[I] = 'S' then
        Include(Result, TActionType.Singular)
      else if S[I] = 'P' then
        Include(Result, TActionType.PingPong)
      else if S[I] = 'U' then
        Include(Result, TActionType.UnAnimated)
      else if S[I] = 'E' then
        Include(Result, TActionType.NeedsEdit)
      else if S[I] = 'O' then
        Include(Result, TActionType.Offset)
      else
        Raise Exception.Create('Unknown Option ' + S[I] + ' in ActionSetFromString')
    end;
end;

{ TDirectionLayout }

procedure TDirectionLayout.Clear;
begin
  FLayout.Clear;
end;

constructor TDirectionLayout.Create;
begin
  FLayout := TObjectList<TDirectionLayoutItem>.Create(True);
end;

destructor TDirectionLayout.Destroy;
begin
  FLayout.Free;
  inherited;
end;

function TDirectionLayout.Dump: String;
var
  I: Integer;
  R: TDirectionLayoutItem;
begin
  Result := '';
  for I := 0 to FLayout.Count - 1 do
    begin
      R := Flayout[I];
      Result := Result +
                System.SysUtils.Format('%2d - %-4s - %-12s - %3d' + sLineBreak, [
                  I, R.CompassShort, R.CompassLong, R.CompassAngle
                  ]);
    end;
end;

procedure TDirectionLayout.ImportLayoutCSV(const AFilename: String);
var
  sr: TStreamReader;
  I: Integer;
  S: String;
  Rec: TDirectionLayoutItem;
begin
  sr := Nil;
  try
    if FileExists(AFilename) then
      begin
        sr := TStreamReader.Create(AFileName);
        I := 0;
        while not sr.EndOfStream do
          begin
            Inc(I);
            S := sr.ReadLine;
            if Length(S) = 0 then
              begin
                Continue;
              end;
            if I = 1 then
              Continue;
            Rec := ParseCSV(s, I);
            FLayout.Add(Rec);
          end;
      end;
  finally
    sr.Free;
  end;
end;

function TDirectionLayout.ParseCSV(const S: String; const Linenum: Integer): TDirectionLayoutItem;
var
  l: Integer;
  i: Integer;
  FieldNum: Integer;
  QuotedField: Boolean;
  EscapedChar: Boolean;
  OutRec: TDirectionLayoutItem;
  TempText: String;
  C: Char;
begin
  QuotedField := False;
  EscapedChar := False;
  FieldNum := 0;

  OutRec := TDirectionLayoutItem.Create;

  // Length of string
  l := Length(S);

  // Loop over the string
  for i := 1 to l do
    begin
    if EscapedChar then
      begin
        if (ord(S[i]) = CHECK_DBLQUOTE) then
          TempText := TempText + '"'
        else
          begin
            C := S[i];
            raise Exception.Create('Escaped Field Found - Unhandled (' + IntToStr(Linenum + 1) + ', ' + IntToStr(i + 1) + ', ' + IntToHex(Ord(C)) + ')' + sLineBreak + S);
          end;
        EscapedChar := False;
      end
    else if (ord(S[i]) = CHECK_SLASH) then
      begin
        if EscapedChar then
        begin
          raise Exception.Create('Escaped Field Found - Unhandled by EscapedChar');
        end
      else
        begin
          EscapedChar := True;
        end;
      end
    else if (ord(S[i]) = CHECK_DBLQUOTE) and not(EscapedChar) then
      begin
        if QuotedField then
        begin
          QuotedField := False;
        end
      else
        begin
          QuotedField := True;
        end;
      end
    else if (ord(S[i]) = CHECK_COMMA) and not(EscapedChar) and (QuotedField) then
      begin
        TempText := TempText + ','
      end
    else if (ord(S[i]) = CHECK_COMMA) and not(EscapedChar) and not(QuotedField) then
      begin
        TempText := TempText.Trim;
        case FieldNum of
          0: OutRec.CompassShort := TempText;
          1: OutRec.CompassLong := TempText;
          2: OutRec.CompassAngle := StrToIntDef(TempText, -1);
        else
          raise Exception.Create('Fieldnum count overflow');
        end;

        Inc(FieldNum);
        TempText := '';
      end
    else
      begin
        TempText := TempText + S[i];
      end;
    end;

    // Catch Trailing field
    TempText := TempText.Trim;
    case FieldNum of
      0: OutRec.CompassShort := TempText;
      1: OutRec.CompassLong := TempText;
      2: OutRec.CompassAngle := StrToIntDef(TempText, -1);
    else
      raise Exception.Create('Fieldnum count overflow');
    end;

    Result := OutRec;
end;

procedure LoadLayouts();
var
  JsonLoaded: Boolean;
  Layout: TSheetLayout;
  Directions: TDirectionLayout;
begin
  JsonLoaded := False;
  if(not Assigned(SheetLayouts)) then
    begin
      SheetLayouts := TSheetLayoutDict.Create([doOwnsValues]);

      if(Assigned(Settings)) then
        JsonLoaded := LoadObjAsJson(SheetLayouts, Settings.AppHome, 'SheetLayouts.json');
      if(not JSonLoaded) then
        begin
          Layout := TSheetLayout.Create;
          Layout.ImportLayoutCSV(LayoutDir + 'character.csv');
          Layout.Format := 'Character';
          Layout.ColCount := 50;
          Layout.RowCount := 50;
          Layout.FrameCount := 2496;
          Layout.SpareCount := 4;
          SheetLayouts.Add('Character', Layout);

          Layout := TSheetLayout.Create;
          Layout.ImportLayoutCSV(LayoutDir + 'monster.csv');
          Layout.Format := 'Monster';
          Layout.ColCount := 21;
          Layout.RowCount := 21;
          Layout.FrameCount := 440;
          Layout.SpareCount := 1;
          SheetLayouts.Add('Monster', Layout);
        end;

//      if(Assigned(Settings)) then
//        SaveObjAsJson(SheetLayouts, Settings.AppHome, 'SheetLayouts.json');

      DirectionLayouts := TDirectionLayoutDict.Create([doOwnsValues]);
      Directions := TDirectionLayout.Create;
      Directions.ImportLayoutCSV(LayoutDir + 'dirs.csv');
      DirectionLayouts.Add(TDirectionFormat.Directions8, Directions);
    end;
end;

procedure UnLoadLayouts();
begin
  if(Assigned(DirectionLayouts)) then
    DirectionLayouts.Free;
  if(Assigned(SheetLayouts)) then
    SheetLayouts.Free;
end;

end.
