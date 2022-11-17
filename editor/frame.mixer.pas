unit Frame.Mixer;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Menus, CastleSpineMixer, Dialogs;

type

  { TFrameMixer }

  TFrameMixer = class(TFrame)
    MenuItemAddMixer: TMenuItem;
    PopupMenuMixer: TPopupMenu;
    ScrollBoxMixer: TScrollBox;
    procedure MenuItemAddMixerClick(Sender: TObject);
  private
  public
    procedure RefreshMixerList;
  end;

implementation

{$R *.lfm}

uses
  Form.Main,
  Form.AddMixer;

{ TFrameMixer }

procedure TFrameMixer.MenuItemAddMixerClick(Sender: TObject);
begin
  if (FormMain.StateMain.Spine.URL <> '') and (FormMain.ComboBoxAnimations.ItemIndex >= 0) then
    FormAddMixer.Show
  else
    ShowMessage('You need to select (or create) an animation first.');
end;

procedure TFrameMixer.RefreshMixerList;
var
  I: Integer;
begin
  // Delete current mixer list
  for I := Self.ScrollBoxMixer.ControlCount - 1 downto 0 do
    Self.ScrollBoxMixer.Controls[I].Free;
  // Readd mixer list
  if FormMain.AnimationItem <> nil then
    for I := FormMain.AnimationItem.MixerList.Count - 1 downto 0 do
    begin
      FormAddMixer.AddFrameMixer(FormMain.AnimationItem.MixerList.Items[I] as TCastleSpineMixerMixerItem);
    end;
end;

end.

