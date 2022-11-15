unit Frame.Timeline;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, ExtCtrls, CastleSpineMixer, Graphics,
  Menus, Generics.Collections;

type
  TFrameTimeRec = record
    X1, Y1, X2, Y2: Integer;
    MixerItem: TCastleSpineMixerMixerItem;
    AnchorItem: TCastleSpineMixerAnchorItem;
  end;
  TFrameTimeRecList = specialize TList<TFrameTimeRec>;

  { TFrameTimeline }

  TFrameTimeline = class(TFrame)
    MenuItemDeleteAnchor: TMenuItem;
    PaintBoxTimeline: TPaintBox;
    ContextMenu: TPopupMenu;
    ScrollBoxTimeline: TScrollBox;
    procedure MenuItemDeleteAnchorClick(Sender: TObject);
    procedure PaintBoxTimelineMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure PaintBoxTimelineMouseMove(Sender: TObject; Shift: TShiftState; X,
      Y: Integer);
    procedure PaintBoxTimelineMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure PaintBoxTimelinePaint(Sender: TObject);
  private
    FIsMouseDown: Boolean;
    FrameTimeRecList: TFrameTimeRecList;
  public
    IsRepainted: Boolean;
    SelectedRec: TFrameTimeRec;
    SelectedTime,
    Zoom: Single;
    MouseX, MouseY: Integer;
    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;
    function CoordToTime(const AX: Integer): Single;     
    function TimeToCoord(const ATime: Single): Integer;
    procedure ForceRepaint;
    function IsInsideFrameTimeRec(X, Y: Integer; out ARec: TFrameTimeRec): Boolean;
  end;

implementation

{$R *.lfm}

uses
  Math,
  Frame.MixerItem,
  Form.Main;

{ TFrameTimeline }

const
  STEP = 0.01;

constructor TFrameTimeline.Create(TheOwner: TComponent);
begin
  inherited;
  Self.Zoom := 1;
  Self.IsRepainted := True;
  Self.SelectedTime := -1;
  Self.PaintBoxTimeline.ControlStyle := Self.PaintBoxTimeline.ControlStyle + [csOpaque];
  Self.FrameTimeRecList := TFrameTimeRecList.Create;
end;

destructor TFrameTimeline.Destroy;
begin
  Self.FrameTimeRecList.Free;
  inherited;
end;

procedure TFrameTimeline.PaintBoxTimelinePaint(Sender: TObject);
var
  I, J, X, Y: Integer;
  Modu: Integer = 5;
  F: Single = 0;
  AnimationItem: TCastleSpineMixerAnimationItem;
  TimelineWidth, TimelineHeight: Integer;
  S: String;
  MixerItem: TCastleSpineMixerMixerItem;
  AnchorItem: TCastleSpineMixerAnchorItem;
  Rec: TFrameTimeRec;
  IsHoverOnFrameTimeRec: Boolean = False;
begin
  // Only render timeline if animation is available
  if FormMain.ComboBoxAnimations.ItemIndex < 0 then Exit;
  //
  AnimationItem := FormMain.AnimationItem;

  // Calculate render area
  TimelineWidth := 60;
  TimelineHeight := Max(70 + 40 * AnimationItem.MixerList.Count, Self.ScrollBoxTimeline.Height - 1);
  Self.PaintBoxTimeline.Height := TimelineHeight;

  // Render ruler
  Self.PaintBoxTimeline.Canvas.Pen.Color := clBlack;
  Self.PaintBoxTimeline.Canvas.Brush.Color := clWhite;
  Self.PaintBoxTimeline.Canvas.Pen.Style := psSolid;
  Self.PaintBoxTimeline.Canvas.Pen.Width := 1;
  I := 0;
  if Self.Zoom < 0.5 then
    Modu := 20
  else if Self.Zoom < 1 then
    Modu := 10;
  while F <= AnimationItem.Duration do
  begin
    X := TimelineWidth - 30;
    if I mod Modu = 0 then
    begin
      S := FloatToStrF(F, ffFixed, 0, 3);
      Self.PaintBoxTimeline.Canvas.Line(X, 0, X, 10);
      Self.PaintBoxTimeline.Canvas.TextOut(X - Self.PaintBoxTimeline.Canvas.TextWidth(S) div 2, 15, S);
    end else
      Self.PaintBoxTimeline.Canvas.Line(X, 0, X, 5);
    F := F + STEP;
    TimelineWidth := Round(TimelineWidth + STEP * 1000 * Self.Zoom);
    Inc(I);
  end;
  TimelineWidth := TimelineWidth - Round(STEP * 1000 * Self.Zoom);
  Self.PaintBoxTimeline.Width := TimelineWidth;

  // Render playing
  Self.PaintBoxTimeline.Canvas.Pen.Style := psSolid;
  Self.PaintBoxTimeline.Canvas.Pen.Color := clGreen;
  X := Self.TimeToCoord(EditorSpineMixer.Time);
  Self.PaintBoxTimeline.Canvas.Line(X, 0, X, Self.PaintBoxTimeline.Height - 1);

  // Render selected coord
  if Self.SelectedTime >= 0 then
  begin
    Self.PaintBoxTimeline.Canvas.Pen.Color := clBlue;
    X := Self.TimeToCoord(Self.SelectedTime);
    Self.PaintBoxTimeline.Canvas.Line(X - 7, 0, X + 7, 0);
    Self.PaintBoxTimeline.Canvas.Line(X - 7, 0, X, 16);
    Self.PaintBoxTimeline.Canvas.Line(X + 7, 0, X, 16);
    Self.PaintBoxTimeline.Canvas.Line(X, 16, X, Self.PaintBoxTimeline.Height - 1);

    S := FloatToStrF(Self.SelectedTime, ffFixed, 0, 3);
    Self.PaintBoxTimeline.Canvas.TextOut(
      X - 8 - Self.PaintBoxTimeline.Canvas.TextWidth(S),
      12 + Self.PaintBoxTimeline.Canvas.TextHeight(S),
      S
    );
    Self.PaintBoxTimeline.Canvas.Pen.Color := clBlack;
  end;

  // Render mixers
  Self.FrameTimeRecList.Clear;
  Self.PaintBoxTimeline.Canvas.Pen.Style := psSolid;
  for I := 0 to AnimationItem.MixerList.Count - 1 do
  begin
    MixerItem := AnimationItem.MixerList.Items[I] as TCastleSpineMixerMixerItem;
    Y := (AnimationItem.MixerList.Count - I - 1) * 40 + 70;
    Self.PaintBoxTimeline.Canvas.Brush.Color := $D0D0D0;
    Self.PaintBoxTimeline.Canvas.FillRect(30, Y - 9, TimelineWidth - 30, Y + 9);

    Self.PaintBoxTimeline.Canvas.Brush.Color := clWhite;
    S := MixerItem.Name;
    Self.PaintBoxTimeline.Canvas.TextOut(34, Y - Self.PaintBoxTimeline.Canvas.TextHeight(S) - 12, S);

    // Render anchors
    for J := 0 to MixerItem.AnchorList.Count - 1 do
    begin
      AnchorItem := MixerItem.AnchorList.Items[J] as TCastleSpineMixerAnchorItem;
      X := Self.TimeToCoord(AnchorItem.Time);
      Rec.X1 := X - 3;
      Rec.Y1 := Y - 9;
      Rec.X2 := X + 3;
      Rec.Y2 := Y + 9;
      Rec.AnchorItem := AnchorItem;
      Rec.MixerItem := MixerItem;
      Self.FrameTimeRecList.Add(Rec);

      if Rec.AnchorItem = Self.SelectedRec.AnchorItem then
        Self.PaintBoxTimeline.Canvas.Brush.Color := clGreen
      else
        Self.PaintBoxTimeline.Canvas.Brush.Color := clRed;
      Self.PaintBoxTimeline.Canvas.FillRect(X - 3, Y - 9, X + 3, Y + 9);
    end;
    Self.PaintBoxTimeline.Canvas.Brush.Color := clWhite;
  end;

  // Render mouse hover
  IsHoverOnFrameTimeRec := Self.IsInsideFrameTimeRec(Self.MouseX, Self.MouseY, Rec);
  if (MouseX >= 30) and (MouseX <= TimelineWidth - 30) and (not IsHoverOnFrameTimeRec) then
  begin
    Self.PaintBoxTimeline.Canvas.Pen.Style := psDot;
    Self.PaintBoxTimeline.Canvas.Line(Self.MouseX, 0, Self.MouseX, Self.PaintBoxTimeline.Height - 1);
    S := FloatToStrF(Self.CoordToTime(Self.MouseX), ffFixed, 0, 3);
    Self.PaintBoxTimeline.Canvas.TextOut(
      MouseX - Self.PaintBoxTimeline.Canvas.TextWidth(S),
      MouseY - Self.PaintBoxTimeline.Canvas.TextHeight(S),
      S
    );
  end;

  Self.IsRepainted := True;
end;

procedure TFrameTimeline.PaintBoxTimelineMouseMove(Sender: TObject;
  Shift: TShiftState; X, Y: Integer);
var
  I: Integer;
begin
  Self.MouseX := X;
  Self.MouseY := Y;
  if not FormMain.TimerPlay.Enabled then
    Self.ForceRepaint;
  if Self.FIsMouseDown then
  begin
    if (X >= 30) and (X <= Self.PaintBoxTimeline.Width - 30) then
    begin
      Self.SelectedTime := Self.CoordToTime(X);
    end else
      Self.SelectedTime := -1;
    // Update mixer values on UI
    for I := 0 to FormMain.FrameMixer.ScrollBoxMixer.ControlCount - 1 do
    begin
      TFrameMixerItem(FormMain.FrameMixer.ScrollBoxMixer.Controls[I]).IsManualUpdate := True;
      TFrameMixerItem(FormMain.FrameMixer.ScrollBoxMixer.Controls[I]).TrackBarValueUpdate(Self.SelectedTime);
      TFrameMixerItem(FormMain.FrameMixer.ScrollBoxMixer.Controls[I]).IsManualUpdate := False;
    end;
  end;
end;

procedure TFrameTimeline.PaintBoxTimelineMouseUp(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  Self.FIsMouseDown := False;
end;

procedure TFrameTimeline.PaintBoxTimelineMouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var
  Rec: TFrameTimeRec;
  I: Integer;
begin
  if not Self.IsInsideFrameTimeRec(X, Y, Rec) then
  begin
    Self.SelectedRec.AnchorItem := nil;
    if (X >= 30) and (X <= Self.PaintBoxTimeline.Width - 30) then
    begin
      Self.SelectedTime := Self.CoordToTime(X);
    end else
      Self.SelectedTime := -1;
  end else
  begin
    Self.SelectedTime := Rec.AnchorItem.Time;
    Self.SelectedRec := Rec;
    Self.ForceRepaint;
  end;
  // Update mixer values on UI
  for I := 0 to FormMain.FrameMixer.ScrollBoxMixer.ControlCount - 1 do
  begin
    TFrameMixerItem(FormMain.FrameMixer.ScrollBoxMixer.Controls[I]).IsManualUpdate := True;
    TFrameMixerItem(FormMain.FrameMixer.ScrollBoxMixer.Controls[I]).TrackBarValueUpdate(Self.SelectedTime); 
    TFrameMixerItem(FormMain.FrameMixer.ScrollBoxMixer.Controls[I]).IsManualUpdate := False;
  end;
  Self.ForceRepaint;
  Self.FIsMouseDown := True;
end;

procedure TFrameTimeline.MenuItemDeleteAnchorClick(Sender: TObject);
begin
  if Self.SelectedRec.AnchorItem <> nil then
  begin
    Self.SelectedRec.MixerItem.DeleteAnchor(Self.SelectedRec.AnchorItem.Time);
    Self.SelectedRec.AnchorItem := nil;
    Self.ForceRepaint;
  end;
end;

function TFrameTimeline.CoordToTime(const AX: Integer): Single;
begin
  Result := StrToFloat(FloatToStrF((AX - 30) / (Self.PaintBoxTimeline.Width - 60) * FormMain.AnimationItem.Duration, ffFixed, 0, 3));
  if Result > FormMain.AnimationItem.Duration then
    Result := -1;
end;

function TFrameTimeline.TimeToCoord(const ATime: Single): Integer;
begin
  if ATime > FormMain.AnimationItem.Duration then
    Result := -1
  else
    Result := Round(ATime / FormMain.AnimationItem.Duration * (Self.PaintBoxTimeline.Width - 60)) + 30;
end;

procedure TFrameTimeline.ForceRepaint;
begin
  if Self.IsRepainted then
  begin
    Self.IsRepainted := False;
    Self.PaintBoxTimeline.Invalidate;
  end;
end;

function TFrameTimeline.IsInsideFrameTimeRec(X, Y: Integer; out ARec: TFrameTimeRec): Boolean;
var
  I: Integer;
begin
  Result := False;
  for I := 0 to Self.FrameTimeRecList.Count - 1 do
  begin
    ARec := Self.FrameTimeRecList[I];
    if (ARec.X1 <= X) and (ARec.X2 >= X) and (ARec.Y1 <= Y) and (ARec.Y2 >= Y) then
    begin
      Exit(True);
    end;
  end;
end;

end.

