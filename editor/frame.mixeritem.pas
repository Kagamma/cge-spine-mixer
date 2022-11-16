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
    TrackBarValue: TTrackBar;
    procedure ButtonDeleteClick(Sender: TObject);
    procedure TrackBarValueChange(Sender: TObject);
  private

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
  Form.Main;

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

procedure TFrameMixerItem.ButtonDeleteClick(Sender: TObject);
begin
  Self.MixerOwner.RemoveControl(Self);
  FormMain.AnimationItem.DeleteMixer(Self.MixerItem);
  FormMain.FrameTimeline.ForceRepaint;
  EditorSpineMixer.SetInitialPose(FormMain.AnimationItem.Name);
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

