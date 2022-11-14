unit Frame.Timeline;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, ExtCtrls, CastleSpineMixer, Graphics;

type

  { TFrameTimeline }

  TFrameTimeline = class(TFrame)
    PaintBoxTimeline: TPaintBox;
    ScrollBoxTimeline: TScrollBox;
    procedure PaintBoxTimelineMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure PaintBoxTimelineMouseMove(Sender: TObject; Shift: TShiftState; X,
      Y: Integer);
    procedure PaintBoxTimelineMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure PaintBoxTimelinePaint(Sender: TObject);
  private
    FIsMouseDown: Boolean;
  public
    FIsRepainted: Boolean;
    SelectedTime,
    Zoom: Single;
    MouseX, MouseY: Integer;
    function CoordToTime(const AX: Integer): Single;     
    function TimeToCoord(const ATime: Single): Integer;
    procedure ForceRepaint;
  end;

implementation

{$R *.lfm}

uses
  Math,
  Form.Main;

{ TFrameTimeline }

const
  STEP = 0.01;

procedure TFrameTimeline.PaintBoxTimelinePaint(Sender: TObject);
var
  I, X, Y: Integer;
  Modu: Integer = 5;
  F: Single = 0;
  AnimationItem: TCastleSpineMixerAnimationItem;
  TimelineWidth, TimelineHeight: Integer;
  S: String;
begin
  // Only render timeline if animation is available
  if FormMain.ComboBoxAnimations.ItemIndex < 0 then Exit;
  //
  AnimationItem := FormMain.AnimationItem;

  // Calculate render area
  TimelineWidth := 60;
  TimelineHeight := Max(75, Self.ScrollBoxTimeline.Height - 1);
  Self.PaintBoxTimeline.Height := TimelineHeight;

  // Render ruler
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
      S := FloatToStrF(F, fffixed, 0, 3);
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

  // Render mouse hover
  if (MouseX >= 30) and (MouseX <= TimelineWidth - 30) then
  begin
    Self.PaintBoxTimeline.Canvas.Pen.Style := psDot;
    Self.PaintBoxTimeline.Canvas.Line(Self.MouseX, 0, Self.MouseX, Self.PaintBoxTimeline.Height - 1);
    S := FloatToStrF(Self.CoordToTime(Self.MouseX), fffixed, 0, 3);
    Self.PaintBoxTimeline.Canvas.TextOut(
      MouseX - Self.PaintBoxTimeline.Canvas.TextWidth(S),
      MouseY - Self.PaintBoxTimeline.Canvas.TextHeight(S),
      S
    );
  end;

  // Render playing
  Self.PaintBoxTimeline.Canvas.Pen.Style := psSolid;
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
    Self.PaintBoxTimeline.Canvas.Pen.Color := clBlack;
  end;

  // Render mixers
  Self.PaintBoxTimeline.Canvas.Pen.Width := 3;
  Self.PaintBoxTimeline.Canvas.Pen.Style := psSolid;
  for I := 0 to {AnimationItem.MixerList.Count}2 do
  begin
    Y := I * 50 + 40;
    Self.PaintBoxTimeline.Canvas.Line(30, Y, TimelineWidth - 30, Y);
  end;
  Self.FIsRepainted := True;
end;

procedure TFrameTimeline.PaintBoxTimelineMouseMove(Sender: TObject;
  Shift: TShiftState; X, Y: Integer);
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
  end;
end;

procedure TFrameTimeline.PaintBoxTimelineMouseUp(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  Self.FIsMouseDown := False;
end;

procedure TFrameTimeline.PaintBoxTimelineMouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  if (X >= 30) and (X <= Self.PaintBoxTimeline.Width - 30) then
  begin
    Self.SelectedTime := Self.CoordToTime(X);
  end else
    Self.SelectedTime := -1;
  Self.ForceRepaint;
  Self.FIsMouseDown := True;
end;

function TFrameTimeline.CoordToTime(const AX: Integer): Single;
begin
  Result := (AX - 30) / (Self.PaintBoxTimeline.Width - 60) * FormMain.AnimationItem.Duration;
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
  if Self.FIsRepainted then
  begin
    Self.FIsRepainted := False;
    Self.PaintBoxTimeline.Invalidate;
  end;
end;

end.

