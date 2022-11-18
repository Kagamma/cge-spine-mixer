unit Frame.Mixer;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Menus, CastleSpineMixer, Dialogs;

type

  { TFrameMixer }

  TFrameMixer = class(TFrame)
    MenuItemAddEvent: TMenuItem;
    MenuItemAddMixer: TMenuItem;
    PopupMenuMixer: TPopupMenu;
    ScrollBoxMixer: TScrollBox;
    procedure MenuItemAddEventClick(Sender: TObject);
    procedure MenuItemAddMixerClick(Sender: TObject);
  private
  public
    procedure RefreshMixerList;
  end;

implementation

{$R *.lfm}

uses
  Form.Main,
  Form.AddEvent,
  Form.AddMixer;

{ TFrameMixer }

procedure TFrameMixer.MenuItemAddMixerClick(Sender: TObject);
begin
  if (FormMain.StateMain.Spine.URL <> '') and (FormMain.ComboBoxAnimations.ItemIndex >= 0) then
    FormAddMixer.Show
  else
    ShowMessage('You need to select (or create) an animation first.');
end;

procedure TFrameMixer.MenuItemAddEventClick(Sender: TObject);
begin
  if (FormMain.StateMain.Spine.URL <> '') and (FormMain.ComboBoxAnimations.ItemIndex >= 0) then
    FormAddEvent.Show
  else
    ShowMessage('You need to select (or create) an animation first.');
end;

procedure TFrameMixer.RefreshMixerList;
var
  I: Integer;
  MixerItem: TCastleSpineMixerMixerItem;
begin
  // Delete current mixer list
  for I := Self.ScrollBoxMixer.ControlCount - 1 downto 0 do
    Self.ScrollBoxMixer.Controls[I].Free;
  // Readd mixer list
  if FormMain.AnimationItem <> nil then
    for I := FormMain.AnimationItem.MixerList.Count - 1 downto 0 do
    begin
      MixerItem := FormMain.AnimationItem.MixerList.Items[I] as TCastleSpineMixerMixerItem;
      if (FormMain.EditMixerFilter.Text = '') or
         (LowerCase(MixerItem.Name).IndexOf(LowerCase(FormMain.EditMixerFilter.Text)) >= 0) then
        FormAddMixer.AddFrameMixer(MixerItem);
    end;
  FormMain.LabelMixerCount.Caption := IntToStr(FormMain.AnimationItem.MixerList.Count) + ' Mixer';
  if FormMain.AnimationItem.MixerList.Count > 1 then
    FormMain.LabelMixerCount.Caption := FormMain.LabelMixerCount.Caption + 's';
end;

end.

