unit Frame.MixerItem;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, ExtCtrls, StdCtrls, ComCtrls, Spin,
  Buttons, CastleSpineMixer;

type

  { TFrameMixerItem }

  TFrameMixerItem = class(TFrame)
    LabelValue: TLabel;
    LabelName: TLabel;
    Panel1: TPanel;
    Panel2: TPanel;
    Panel3: TPanel;
    Panel4: TPanel;
    ButtonDelete: TSpeedButton;
    ButtonKey: TSpeedButton;
    TrackBarValue: TTrackBar;
    procedure ButtonDeleteClick(Sender: TObject);
    procedure ButtonKeyClick(Sender: TObject);
    procedure TrackBarValueChange(Sender: TObject);
    procedure TrackBarValueKeyUp(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure TrackBarValueMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure TrackBarValueMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
  private
    FIsDirty: Boolean;
  public
    IsManualUpdate: Boolean;
    MixerOwner: TWinControl;
    MixerItem: TCastleSpineMixerMixerItem;
    constructor Create(TheOwner: TComponent); override;
    procedure TrackBarValueUpdate(ATime: Single);
  end;

implementation

{$R *.lfm}

uses
  Form.Main,
  Utils.Undo;

{ TFrameMixerItem }

constructor TFrameMixerItem.Create(TheOwner: TComponent);
begin
  inherited;
  Self.IsManualUpdate := False;
end;

procedure TFrameMixerItem.TrackBarValueChange(Sender: TObject);
var
  V: Single;
begin
  if Self.IsManualUpdate then
    Exit;
  if not Self.FIsDirty then
  begin
    UndoSystem.Mark;
    Self.FIsDirty := True;
  end;
  V := TrackBarValue.Position / 1000;
  LabelValue.Caption := FloatToStrF(V, ffFixed, 0, 3);
  // Add time / value pair
  if FormMain.FrameTimeline.SelectedTime >= 0 then
  begin
    MixerItem.AddKey(FormMain.FrameTimeline.SelectedTime, V);
    EditorSpineMixer.SetInitialPose(FormMain.AnimationItem.Name);
  end;
  FormMain.FrameTimeline.ForceRepaint;
end;

procedure TFrameMixerItem.TrackBarValueKeyUp(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  Self.FIsDirty := False;
end;

procedure TFrameMixerItem.TrackBarValueMouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  Self.FIsDirty := False;
end;

procedure TFrameMixerItem.TrackBarValueMouseUp(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  Self.FIsDirty := False;
end;

procedure TFrameMixerItem.ButtonDeleteClick(Sender: TObject);
begin
  FormMain.AnimationItem.DeleteMixer(Self.MixerItem);
  EditorSpineMixer.SetInitialPose(FormMain.AnimationItem.Name);
  FormMain.FrameMixer.RefreshMixerList;     
  FormMain.FrameTimeline.ForceRepaint;
end;

procedure TFrameMixerItem.ButtonKeyClick(Sender: TObject);
begin
  // This is treat as trackbar value change
  Self.TrackBarValueChange(Sender);
end;

procedure TFrameMixerItem.TrackBarValueUpdate(ATime: Single);
var
  V: Single;
begin
  V := Self.MixerItem.GetValue(ATime);
  LabelValue.Caption := FloatToStrF(V, ffFixed, 0, 3);
  TrackBarValue.Position := Round(V * 1000);
end;

end.

