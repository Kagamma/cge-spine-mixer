unit Form.AddMixer;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, Buttons, StdCtrls,
  CastleSpineMixer;

type

  { TFormAddMixer }

  TFormAddMixer = class(TForm)
    ButtonCancel: TBitBtn;
    ButtonOk: TBitBtn;
    ComboBoxMixer: TComboBox;
    procedure ButtonCancelClick(Sender: TObject);
    procedure ButtonOkClick(Sender: TObject);
    procedure FormShow(Sender: TObject);
  private

  public
    procedure AddFrameMixer(MixerItem: TCastleSpineMixerMixerItem);
  end;

var
  FormAddMixer: TFormAddMixer;

implementation

{$R *.lfm}

uses
  Form.Main,
  Frame.MixerItem;

{ TFormAddMixer }

procedure TFormAddMixer.AddFrameMixer(MixerItem: TCastleSpineMixerMixerItem);
var
  FrameMixerItem: TFrameMixerItem;
begin
  if Self.ComboboxMixer.ItemIndex >= 0 then
  begin
    FrameMixerItem := TFrameMixerItem.Create(Self);
    FrameMixerItem.Name := 'Mixer' + IntToStr(Random($FFFFFFFF)) + IntToStr(Random($FFFFFFFF)) + IntToStr(Random($FFFFFFFF));
    FrameMixerItem.LabelName.Caption := MixerItem.Name;
    FrameMixerItem.MixerOwner := FormMain.FrameMixer.ScrollBoxMixer;
    FrameMixerItem.MixerItem := MixerItem;
    FormMain.FrameMixer.ScrollBoxMixer.InsertControl(FrameMixerItem);
  end;
end;

procedure TFormAddMixer.ButtonCancelClick(Sender: TObject);
begin
  Self.Hide;
end;

procedure TFormAddMixer.ButtonOkClick(Sender: TObject);
var
  MixerItem: TCastleSpineMixerMixerItem;
  MixerName: String;
begin
  if Self.ComboboxMixer.ItemIndex >= 0 then
  begin     
    MixerName := Self.ComboboxMixer.Items[Self.ComboboxMixer.ItemIndex];
    //
    MixerItem := FormMain.AnimationItem.AddMixer(MixerName);
    Self.AddFrameMixer(MixerItem);
    Self.Hide;
  end;
end;

procedure TFormAddMixer.FormShow(Sender: TObject);
var
  S: String;
  I: Integer;
  IsMixerExisted: Boolean;
begin
  ComboboxMixer.Text := '';
  ComboboxMixer.Items.Clear;
  // Load list of mixer, minus the one we already add
  for S in FormMain.StateMain.Spine.AnimationsList do
  begin
    IsMixerExisted := False;
    for I := 0 to FormMain.FrameMixer.ScrollBoxMixer.ControlCount - 1 do
    begin
      if TFrameMixerItem(FormMain.FrameMixer.ScrollBoxMixer.Controls[I]).MixerItem.Name = S then
      begin
        IsMixerExisted := True;
        break;
      end;
    end;
    // Only add mixers that do not exists
    if not IsMixerExisted then
      ComboboxMixer.AddItem(S, nil);
  end;
  ComboboxMixer.ItemIndex := 0;
end;

end.

