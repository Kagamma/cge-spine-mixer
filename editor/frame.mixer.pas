unit Frame.Mixer;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Menus;

type

  { TFrameMixer }

  TFrameMixer = class(TFrame)
    MenuItemAddMixer: TMenuItem;
    PopupMenuMixer: TPopupMenu;
    ScrollBoxMixer: TScrollBox;
    procedure MenuItemAddMixerClick(Sender: TObject);
  private
  public

  end;

implementation

{$R *.lfm}

uses
  Form.AddMixer;

{ TFrameMixer }

procedure TFrameMixer.MenuItemAddMixerClick(Sender: TObject);
begin
  FormAddMixer.Show;
end;

end.

