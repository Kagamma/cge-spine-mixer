unit Form.RenameAnimation;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, Buttons,
  CastleSpineMixer;

type

  { TForm1 }

  { TFormRenameAnimation }

  TFormRenameAnimation = class(TForm)
    ButtonOk: TBitBtn;
    ButtonCancel: TBitBtn;
    EditAnimationName: TEdit;
    procedure ButtonCancelClick(Sender: TObject);
    procedure ButtonOkClick(Sender: TObject);
    procedure FormShow(Sender: TObject);
  private

  public

  end;

var
  FormRenameAnimation: TFormRenameAnimation;

implementation

{$R *.lfm}

uses
  Utils.Undo,
  Form.Main;

{ TFormRenameAnimation }

procedure TFormRenameAnimation.ButtonCancelClick(Sender: TObject);
begin
  Self.Hide;
end;

procedure TFormRenameAnimation.ButtonOkClick(Sender: TObject);
var
  I: Integer;
begin
  try
    UndoSystem.Mark;
    EditorSpineMixer.Data.RenameAnimation(FormMain.ComboBoxAnimations.ItemIndex, EditAnimationName.Text);
    // Refresh animation combobox
    FormMain.ComboBoxAnimations.Items[FormMain.ComboBoxAnimations.ItemIndex] := EditAnimationName.Text;
    //
    Self.Hide;
  except
    on E: Exception do
      ShowMessage(E.Message);
  end;
end;

procedure TFormRenameAnimation.FormShow(Sender: TObject);
begin
  // Cannot rename if no animations
  if FormMain.ComboBoxAnimations.Items.Count = 0 then
    Self.Hide;
  Self.EditAnimationName.Focused;
end;

end.

