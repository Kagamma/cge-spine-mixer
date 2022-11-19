unit Form.AddEvent;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, Buttons, StdCtrls,
  CastleSpineMixer;

type

  { TFormAddEvent }

  TFormAddEvent = class(TForm)
    ButtonCancel: TBitBtn;
    ButtonOk: TBitBtn;
    EditEventName: TEdit;
    procedure ButtonCancelClick(Sender: TObject);
    procedure ButtonOkClick(Sender: TObject);
    procedure FormShow(Sender: TObject);
  private

  public
  end;

var
  FormAddEvent: TFormAddEvent;

implementation

{$R *.lfm}

uses
  Form.Main,
  Form.AddMixer,
  Frame.MixerItem,
  Utils.Undo;

{ TFormAddEvent }

procedure TFormAddEvent.ButtonOkClick(Sender: TObject);
var
  MixerItem: TCastleSpineMixerMixerItem;
  MixerName: String;
  I: Integer;
begin
  if Self.EditEventName.Text = '' then
  begin
    ShowMessage('Event name cannot be empty.');
    Exit;
  end;
  for I := 0 to FormMain.FrameMixer.ScrollBoxMixer.ControlCount - 1 do
  begin
    if (TFrameMixerItem(FormMain.FrameMixer.ScrollBoxMixer.Controls[I]).MixerItem.Kind = smtEvent) and
       (TFrameMixerItem(FormMain.FrameMixer.ScrollBoxMixer.Controls[I]).MixerItem.Name = Self.EditEventName.Text) then
    begin
      ShowMessage('Event with this name is already existed.');
      Exit;
    end;
  end;
  UndoSystem.Mark;
  MixerName := Self.EditEventName.Text;
  //
  MixerItem := FormMain.AnimationItem.AddMixer(MixerName);
  MixerItem.Kind := smtEvent;
  FormAddMixer.AddFrameMixer(MixerItem);
  // Sort control
  FormMain.FrameMixer.RefreshMixerList;
  Self.Hide;
end;

procedure TFormAddEvent.FormShow(Sender: TObject);
begin
  Self.EditEventName.Text := '';
  Self.EditEventName.Focused;
end;

procedure TFormAddEvent.ButtonCancelClick(Sender: TObject);
begin
  Self.Hide;
end;

end.

