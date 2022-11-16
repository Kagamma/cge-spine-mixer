unit Form.CurveEditor;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls, Buttons,
  CastleVectors, CastleSpineMixer;

type

  { TFormCurveEditor }

  TFormCurveEditor = class(TForm)
    ButtonCancel: TBitBtn;
    ButtonOk: TBitBtn;
    PanelCurveEditor: TPanel;
    procedure ButtonCancelClick(Sender: TObject);
    procedure ButtonOkClick(Sender: TObject);
    procedure FormMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure FormMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
    procedure FormMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure FormShow(Sender: TObject);
    procedure PanelCurveEditorPaint(Sender: TObject);
  private
    IsMouseDown: Boolean;
    SelectedControlPoint: Integer;
    procedure UpdateKeyItemControlPoints;
  public
    ControlPoints: array[0..1] of TVector2;
    ControlPointsBackup: array[0..1] of TVector2;
  end;

var
  FormCurveEditor: TFormCurveEditor;

implementation

{$R *.lfm}

uses
  Form.Main;

{ TFormCurveEditor }

procedure TFormCurveEditor.UpdateKeyItemControlPoints;
begin
  FormMain.FrameTimeline.SelectedRec.KeyItem.CX1 := Self.ControlPoints[0].X / Self.PanelCurveEditor.Width;
  FormMain.FrameTimeline.SelectedRec.KeyItem.CY1 := 1 - Self.ControlPoints[0].Y / Self.PanelCurveEditor.Height;
  FormMain.FrameTimeline.SelectedRec.KeyItem.CX2 := Self.ControlPoints[1].X / Self.PanelCurveEditor.Width;
  FormMain.FrameTimeline.SelectedRec.KeyItem.CY2 := 1 - Self.ControlPoints[1].Y / Self.PanelCurveEditor.Height;
end;

procedure TFormCurveEditor.ButtonCancelClick(Sender: TObject);
begin
  // Revert KeyItem
  FormMain.FrameTimeline.SelectedRec.KeyItem.CX1 := Self.ControlPointsBackup[0].X;
  FormMain.FrameTimeline.SelectedRec.KeyItem.CY1 := Self.ControlPointsBackup[0].Y;
  FormMain.FrameTimeline.SelectedRec.KeyItem.CX2 := Self.ControlPointsBackup[1].X;
  FormMain.FrameTimeline.SelectedRec.KeyItem.CY2 := Self.ControlPointsBackup[1].Y;
  Self.Hide;
end;

procedure TFormCurveEditor.ButtonOkClick(Sender: TObject);
begin
  UpdateKeyItemControlPoints;
  Self.Hide;
end;

procedure TFormCurveEditor.FormMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
var
  I: Integer;
begin
  IsMouseDown := True;
  SelectedControlPoint := -1;
  for I := 0 to 1 do
  begin
    if (X >= ControlPoints[I].X - 6) and (X <= ControlPoints[I].X + 6) and
       (Y >= ControlPoints[I].Y - 6) and (Y <= ControlPoints[I].Y + 6) then
    begin
      SelectedControlPoint := I;
      Self.PanelCurveEditor.Invalidate;
      Break;
    end;
  end;
end;

procedure TFormCurveEditor.FormMouseMove(Sender: TObject; Shift: TShiftState;
  X, Y: Integer);
begin
  if (Self.IsMouseDown) and (SelectedControlPoint >= 0) and
     (X >= 0) and (X < Self.PanelCurveEditor.Width) and
     (Y >= 0) and (Y < Self.PanelCurveEditor.Height) then
  begin
    ControlPoints[SelectedControlPoint] := Vector2(X, Y);
    Self.PanelCurveEditor.Invalidate;
  end;
end;

procedure TFormCurveEditor.FormMouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  Self.IsMouseDown := False;
  Self.SelectedControlPoint := -1;
end;

procedure TFormCurveEditor.FormShow(Sender: TObject);
var
  I: Integer;
begin
  for I := 0 to 1 do
    Self.ControlPointsBackup[I] := Self.ControlPoints[I];
  Self.ControlPoints[0].X := Self.ControlPoints[0].X * Self.PanelCurveEditor.Width;
  Self.ControlPoints[0].Y := Self.PanelCurveEditor.Height - Self.ControlPoints[0].Y * Self.PanelCurveEditor.Height;
  Self.ControlPoints[1].X := Self.ControlPoints[1].X * Self.PanelCurveEditor.Width;
  Self.ControlPoints[1].Y := Self.PanelCurveEditor.Height - Self.ControlPoints[1].Y * Self.PanelCurveEditor.Height;
  Self.SelectedControlPoint := -1;
end;

procedure TFormCurveEditor.PanelCurveEditorPaint(Sender: TObject);
var
  I, X, Y: Integer;
  KeyItem: TCastleSpineMixerKeyItem;
begin
  inherited;
  KeyItem := FormMain.FrameTimeline.SelectedRec.KeyItem;

  Self.PanelCurveEditor.Canvas.Pen.Color := clBlack;
  Self.PanelCurveEditor.Canvas.Pen.Width := 1;
  Self.PanelCurveEditor.Canvas.Pen.Style := psSolid;
  Self.PanelCurveEditor.Canvas.Line(0, 0, 0, Self.PanelCurveEditor.Height - 1);
  Self.PanelCurveEditor.Canvas.Line(0, Self.PanelCurveEditor.Height - 1, Self.PanelCurveEditor.Width - 1, Self.PanelCurveEditor.Height - 1);

  Self.PanelCurveEditor.Canvas.Line(0, 0, 10, 0);     
  Self.PanelCurveEditor.Canvas.Line(0, Self.PanelCurveEditor.Height div 2, 6, Self.PanelCurveEditor.Height div 2);                                   
  Self.PanelCurveEditor.Canvas.Line(Self.PanelCurveEditor.Width - 1, Self.PanelCurveEditor.Height - 1, Self.PanelCurveEditor.Width - 1, Self.PanelCurveEditor.Height - 11);
  Self.PanelCurveEditor.Canvas.Line(Self.PanelCurveEditor.Width div 2, Self.PanelCurveEditor.Height - 1, Self.PanelCurveEditor.Width div 2, Self.PanelCurveEditor.Height - 7);

  Self.PanelCurveEditor.Canvas.Pen.Style := psDot;
  for I := 0 to 1 do
  begin
    X := Round(Self.ControlPoints[I].X);
    Y := Round(Self.ControlPoints[I].Y);  
    Self.PanelCurveEditor.Canvas.Pen.Color := clBlack;
    Self.PanelCurveEditor.Canvas.Brush.Color := clWhite;
    if I = 0 then
      Self.PanelCurveEditor.Canvas.Line(0, Self.PanelCurveEditor.Height - 1, X, Y)
    else
      Self.PanelCurveEditor.Canvas.Line(Self.PanelCurveEditor.Width - 1, 0, X, Y);
  end;
  for I := 0 to 1 do
  begin
    X := Round(Self.ControlPoints[I].X);
    Y := Round(Self.ControlPoints[I].Y);
    if SelectedControlPoint = I then
      Self.PanelCurveEditor.Canvas.Brush.Color := clGreen
    else
      Self.PanelCurveEditor.Canvas.Brush.Color := clRed;
    Self.PanelCurveEditor.Canvas.FillRect(X - 6, Y - 6, X + 6, Y + 6);
  end;

  Self.PanelCurveEditor.Canvas.Pen.Color := clBlack;
  Self.PanelCurveEditor.Canvas.Pen.Width := 3;
  Self.PanelCurveEditor.Canvas.Pen.Style := psSolid;
  Self.UpdateKeyItemControlPoints;
  KeyItem.CalculateBezier;
  for I := 0 to High(KeyItem.BezierCurvePoints) do
  begin
    if I = 0 then
      Self.PanelCurveEditor.Canvas.MoveTo(
        Round(KeyItem.BezierCurvePoints[I].X * Self.PanelCurveEditor.Width),
        Self.PanelCurveEditor.Height - Round(KeyItem.BezierCurvePoints[I].Y * Self.PanelCurveEditor.Height)
      )
    else
      Self.PanelCurveEditor.Canvas.LineTo(
        Round(KeyItem.BezierCurvePoints[I].X * Self.PanelCurveEditor.Width),
        Self.PanelCurveEditor.Height - Round(KeyItem.BezierCurvePoints[I].Y * Self.PanelCurveEditor.Height)
      );
  end;
end;

end.

