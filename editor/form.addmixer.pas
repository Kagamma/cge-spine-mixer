unit Form.AddMixer;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, Buttons, StdCtrls;

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

  end;

var
  FormAddMixer: TFormAddMixer;

implementation

{$R *.lfm}

uses
  Form.Main,
  Frame.MixerItem;

{ TFormAddMixer }

procedure TFormAddMixer.ButtonCancelClick(Sender: TObject);
begin
  Self.Hide;
end;

procedure TFormAddMixer.ButtonOkClick(Sender: TObject);
var
  MixerItem: TFrameMixerItem;
begin
  MixerItem := TFrameMixerItem.Create(Self);
  MixerItem.Name := 'C' + IntToStr(Random($ffffffff));
  MixerItem.MixerOwner := FormMain.FrameMixer.ScrollBoxMixer;
  FormMain.FrameMixer.ScrollBoxMixer.InsertControl(MixerItem);
  Self.Hide;
end;

procedure TFormAddMixer.FormShow(Sender: TObject);
begin
  // TODO: Load list of mixer, minus the one we already add
end;

end.

