unit Frame.MixerItem;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, ExtCtrls, StdCtrls, ComCtrls, Spin,
  Buttons, CastleSpineMixer;

type

  { TFrameMixerItem }

  TFrameMixerItem = class(TFrame)
    EditStringValue: TEdit;
    LabelValue: TLabel;
    LabelName: TLabel;
    Panel1: TPanel;
    Panel2: TPanel;
    PanelMixer: TPanel;
    Panel4: TPanel;
    ButtonDelete: TSpeedButton;
    ButtonKey: TSpeedButton;
    TrackBarValue: TTrackBar;
    procedure ButtonDeleteClick(Sender: TObject);
    procedure ButtonKeyClick(Sender: TObject);
    procedure EditStringValueChange(Sender: TObject);
    procedure EditStringValueEditingDone(Sender: TObject);
    procedure TrackBarValueChange(Sender: TObject);
    procedure TrackBarValueKeyUp(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure TrackBarValueMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure TrackBarValueMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
  private
    FMixerItem: TCastleSpineMixerMixerItem;
    FIsDirty: Boolean;
    procedure SetMixerItem(V: TCastleSpineMixerMixerItem);
  public
    IsManualUpdate: Boolean;
    MixerOwner: TWinControl;
    constructor Create(TheOwner: TComponent); override;
    procedure UpdateUI(ATime: Single);

    property MixerItem: TCastleSpineMixerMixerItem read FMixerItem write SetMixerItem;
  end;

implementation

{$R *.lfm}

uses
  Form.Main,
  Utils.Undo;

{ TFrameMixerItem }

procedure TFrameMixerItem.SetMixerItem(V: TCastleSpineMixerMixerItem);
begin
  Self.TrackBarValue.Visible := V.Kind = mtMixer;    
  Self.EditStringValue.Visible := V.Kind = mtEvent;
  Self.LabelValue.Visible := V.Kind = mtMixer;
  Self.FMixerItem := V;
end;

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
  UndoSystem.Mark;
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

procedure TFrameMixerItem.EditStringValueChange(Sender: TObject);
begin
  if Self.IsManualUpdate then
    Exit;
  if not Self.FIsDirty then
  begin
    UndoSystem.Mark;
    Self.FIsDirty := True;
  end; 
  // Add time / value pair
  if FormMain.FrameTimeline.SelectedTime >= 0 then
  begin
    MixerItem.AddKey(FormMain.FrameTimeline.SelectedTime, Self.EditStringValue.Text);
  end;
  FormMain.FrameTimeline.ForceRepaint;
end;

procedure TFrameMixerItem.EditStringValueEditingDone(Sender: TObject);
begin
  Self.FIsDirty := False;
end;

procedure TFrameMixerItem.UpdateUI(ATime: Single);
var
  V: Single;
begin
  if Self.FMixerItem.Kind = mtMixer then
  begin
    V := Self.MixerItem.GetValue(ATime);
    LabelValue.Caption := FloatToStrF(V, ffFixed, 0, 3);
    TrackBarValue.Position := Round(V * 1000);
  end else
  if Self.FMixerItem.Kind = mtEvent then
  begin
    EditStringValue.Text := Self.MixerItem.GetStringValuePrecise(ATime);
  end;
end;

end.

