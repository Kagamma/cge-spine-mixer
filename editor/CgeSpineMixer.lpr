program CgeSpineMixer;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}
  cthreads,
  {$ENDIF}
  {$IFDEF HASAMIGA}
  athreads,
  {$ENDIF}
  Interfaces, // this includes the LCL widgetset
  Forms, castle_base, castle_components, castle_window,
  CastleSpineMixer, CastleSpine, Spine, Form.Main, Frame.MixerItem, Frame.Mixer,
  Frame.Viewer, Form.NewAnimation, Form.RenameAnimation, Frame.Timeline,
  Form.AddMixer, Form.CurveEditor, Utils.Undo, Form.AddEvent;

{$R *.res}

begin
  RequireDerivedFormResource:=True;
  Application.Scaled:=True;
  Application.Initialize;
  Application.CreateForm(TFormMain, FormMain);
  Application.CreateForm(TFormNewAnimation, FormNewAnimation);
  Application.CreateForm(TFormRenameAnimation, FormRenameAnimation);
  Application.CreateForm(TFormAddMixer, FormAddMixer);
  Application.CreateForm(TFormCurveEditor, FormCurveEditor);
  Application.CreateForm(TFormAddEvent, FormAddEvent);
  Application.Run;
end.

