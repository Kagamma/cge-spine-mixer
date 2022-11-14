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
    procedure PaintBoxTimelineMouseMove(Sender: TObject; Shift: TShiftState; X,
      Y: Integer);
    procedure PaintBoxTimelinePaint(Sender: TObject);
  private

  public
    Zoom: Single;
    MouseX, MouseY: Integer;
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
  AnimationItem := TCastleSpineMixerAnimationItem(FormMain.ComboBoxAnimations.Items.Objects[FormMain.ComboBoxAnimations.ItemIndex]);

  // Calculate render area
  TimelineWidth := 60;
  TimelineHeight := Max(75, Self.ScrollBoxTimeline.Height - 1);
  Self.PaintBoxTimeline.Height := TimelineHeight;

  Self.PaintBoxTimeline.Canvas.Clear;
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
    S := FloatToStrF((MouseX - 30) / (TimelineWidth - 60) * AnimationItem.Duration, fffixed, 0, 3);
    Self.PaintBoxTimeline.Canvas.TextOut(
      MouseX - Self.PaintBoxTimeline.Canvas.TextWidth(S),
      MouseY - Self.PaintBoxTimeline.Canvas.TextHeight(S),
      S
    );
  end;

  // Render mixers
  Self.PaintBoxTimeline.Canvas.Pen.Width := 3;
  Self.PaintBoxTimeline.Canvas.Pen.Style := psSolid;
  for I := 0 to {AnimationItem.MixerList.Count}2 do
  begin
    Y := I * 50 + 40;
    Self.PaintBoxTimeline.Canvas.Line(30, Y, TimelineWidth - 30, Y);
  end;
end;

procedure TFrameTimeline.PaintBoxTimelineMouseMove(Sender: TObject;
  Shift: TShiftState; X, Y: Integer);
begin
  Self.PaintBoxTimeline.Invalidate;
  Self.MouseX := X;
  Self.MouseY := Y;
end;

end.

