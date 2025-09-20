unit SkComponents;

interface

uses
  System.Classes,
  System.UITypes,
  System.Types,
  FMX.Types,
  FMX.Controls,
  FMX.Layouts,
  FMX.Graphics;


type
  /// TBorderLayout extends TLayout to draw a rectangular border around its clients
  /// and automatically inset its children by that same thickness.
  TBorderLayout = class(TLayout)
  private
    FBorderColor: TAlphaColor;
    FBorderThickness: Single;
    procedure SetBorderColor(const Value: TAlphaColor);
    procedure SetBorderThickness(const Value: Single);
    procedure UpdatePadding;
  protected
    procedure Paint; override;
  public
    constructor Create(AOwner: TComponent); override;
  published
    /// Color of the border line
    property BorderColor: TAlphaColor
      read FBorderColor
      write SetBorderColor
      default TAlphaColorRec.Black;
    /// Thickness (in pixels) of the border line; also used as padding
    property BorderThickness: Single
      read FBorderThickness
      write SetBorderThickness;
  end;

procedure Register;

implementation

procedure Register;
begin
  RegisterComponents('Samples', [TBorderLayout]);
end;

{ TBorderLayout }

constructor TBorderLayout.Create(AOwner: TComponent);
begin
  inherited;
  // Defaults
  FBorderColor     := TAlphaColorRec.LightGray;
  FBorderThickness := 1.0;
  // Make sure we paint ourselves
  HitTest := True;
  // Initialize padding so children are inset
  UpdatePadding;
end;

procedure TBorderLayout.SetBorderColor(const Value: TAlphaColor);
begin
  if FBorderColor <> Value then
  begin
    FBorderColor := Value;
    Repaint;
  end;
end;

procedure TBorderLayout.SetBorderThickness(const Value: Single);
begin
  if FBorderThickness <> Value then
  begin
    FBorderThickness := Value;
    UpdatePadding;
    Repaint;
  end;
end;

// Update the built-in Padding of this control so children are laid out inside
// the border
procedure TBorderLayout.UpdatePadding;
begin
  // Padding is a TBounds<TMargins> in FMX; we set all four sides equal:
  Padding.Rect := TRectF.Create(FBorderThickness,  // Left
                                FBorderThickness,  // Top
                                FBorderThickness,  // Right
                                FBorderThickness); // Bottom
end;

procedure TBorderLayout.Paint;
var
  R: TRectF;
begin
  inherited;
  if FBorderThickness > 0 then
  begin
    // Inflate rectangle by half the stroke to keep the line fully visible
    R := RectF(0, 0, Width, Height);
    R.Inflate(-FBorderThickness / 2, -FBorderThickness / 2);

    Canvas.Stroke.Kind      := TBrushKind.Solid;
    Canvas.Stroke.Color     := FBorderColor;
    Canvas.Stroke.Thickness := FBorderThickness;

    // DrawRect parameters: Rect, XRadius, YRadius, Corners, Opacity
    Canvas.DrawRect(R,
                    0,
                    0,
                    AllCorners,
                    1.0);
  end;
end;

end.
