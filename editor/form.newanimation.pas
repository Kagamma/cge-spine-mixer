unit Form.NewAnimation;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, Buttons,
  CastleSpineMixer;

type

  { TFormNewAnimation }

  TFormNewAnimation = class(TForm)
    ButtonOk: TBitBtn;
    ButtonCancel: TBitBtn;
    EditAnimationName: TEdit;
    procedure ButtonCancelClick(Sender: TObject);
    procedure ButtonOkClick(Sender: TObject);
    procedure FormDeactivate(Sender: TObject);
  private

  public
    procedure RefreshAnimation;
  end;

var
  FormNewAnimation: TFormNewAnimation;

implementation

{$R *.lfm}

uses
  Form.Main,
  Utils.Undo;

{ TFormNewAnimation }

procedure TFormNewAnimation.RefreshAnimation;
var
  I: Integer;
begin
  FormMain.ComboBoxAnimations.Clear;
  for I := 0 to EditorSpineMixer.Data.AnimationList.Count - 1 do
    FormMain.ComboBoxAnimations.AddItem(
      TCastleSpineMixerAnimationItem(EditorSpineMixer.Data.AnimationList.Items[I]).Name,
      EditorSpineMixer.Data.AnimationList.Items[I]
    );
end;

procedure TFormNewAnimation.ButtonCancelClick(Sender: TObject);
begin
  Self.Hide;
end;

procedure TFormNewAnimation.ButtonOkClick(Sender: TObject);
var
  I: Integer;
begin
  try
    UndoSystem.Mark;
    EditorSpineMixer.Data.AddAnimation(EditAnimationName.Text);
    // Refresh animation combobox
    FormMain.ComboBoxAnimations.Clear;
    for I := 0 to EditorSpineMixer.Data.AnimationList.Count - 1 do
      FormMain.ComboBoxAnimations.AddItem(
        TCastleSpineMixerAnimationItem(EditorSpineMixer.Data.AnimationList.Items[I]).Name,
        EditorSpineMixer.Data.AnimationList.Items[I]
      );
    // Focus on newly created animation
    FormMain.ComboBoxAnimations.ItemIndex := FormMain.ComboBoxAnimations.Items.Count - 1;
    FormMain.ComboBoxAnimationsChange(Self);
    //
    Self.Hide;
  except
    on E: Exception do
      ShowMessage(E.Message);
  end;
end;

procedure TFormNewAnimation.FormDeactivate(Sender: TObject);
begin
  Self.ButtonCancelClick(Sender);
end;

end.

