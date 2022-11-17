unit Form.Main;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, Menus, ExtCtrls,
  StdCtrls, Buttons, ComCtrls, Spin, CastleControl, LCLType,
  CastleApplicationProperties, CastleUIControls, CastleUIState, FileUtil,
  CastleSceneCore, CastleScene, CastleVectors, CastleSpineMixer, CastleSpine,
  CastleViewport, CastleComponentSerialize,
  Frame.Mixer,
  Frame.Viewer,
  Frame.Timeline;

type

  { TFormMain }

  TStateMain = class(TUIState)
  private
  public
    Spine: TCastleSpine;
    Viewport: TCastleViewport;
    constructor Create(AOwner: TComponent); override;
    procedure Start; override;
  end;

  TFormMain = class(TForm)
    ComboBoxAnimations: TComboBox;
    FloatZoom: TFloatSpinEdit;
    FloatTime: TFloatSpinEdit;
    FloatSpeed: TFloatSpinEdit;
    ImageList: TImageList;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    LabelKind: TLabel;
    LabelTime: TLabel;
    MainMenu: TMainMenu;
    MenuItemNewMixerData: TMenuItem;
    MenuItemSaveMixerData: TMenuItem;
    MenuItemLoadMixerData: TMenuItem;
    MenuItemFile: TMenuItem;
    MenuItemLoadSpineModel: TMenuItem;
    OpenDialogMixer: TOpenDialog;
    OpenDialogSpine: TOpenDialog;
    PanelMixerItemKind: TPanel;
    PanelViewer: TPanel;
    Panel2: TPanel;
    Panel3: TPanel;
    Panel4: TPanel;
    PanelMixer: TPanel;
    Panel6: TPanel;
    Panel7: TPanel;
    Panel8: TPanel;
    PanelTimeline: TPanel;
    SaveDialogMixer: TSaveDialog;
    Separator1: TMenuItem;
    ButtonRenameAnimation: TSpeedButton;
    ButtonAddNewAnimation: TSpeedButton;
    ButtonDeleteAnimation: TSpeedButton;
    Separator2: TMenuItem;
    ButtonPlay: TSpeedButton;
    SpeedButton1: TSpeedButton;
    ButtonLinear: TSpeedButton;
    ButtonBezier: TSpeedButton;
    ButtonEditCurve: TSpeedButton;
    Splitter1: TSplitter;
    Splitter2: TSplitter;
    TimerUpdate: TTimer;
    TimerPlay: TTimer;
    procedure ButtonAddNewAnimationClick(Sender: TObject);
    procedure ButtonBezierClick(Sender: TObject);
    procedure ButtonDeleteAnimationClick(Sender: TObject);
    procedure ButtonLinearClick(Sender: TObject);
    procedure ButtonPlayClick(Sender: TObject);
    procedure ButtonRenameAnimationClick(Sender: TObject);
    procedure ComboBoxAnimationsChange(Sender: TObject);
    procedure FloatSpeedChange(Sender: TObject);
    procedure FloatTimeChange(Sender: TObject);
    procedure FloatZoomChange(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure MenuItemLoadMixerDataClick(Sender: TObject);
    procedure MenuItemLoadSpineModelClick(Sender: TObject);
    procedure MenuItemNewMixerDataClick(Sender: TObject);
    procedure MenuItemSaveMixerDataClick(Sender: TObject);
    procedure SpeedButton1Click(Sender: TObject);
    procedure ButtonEditCurveClick(Sender: TObject);
    procedure TimerPlayStartTimer(Sender: TObject);
    procedure TimerPlayTimer(Sender: TObject);
    procedure TimerUpdateTimer(Sender: TObject);
  private
  public
    FrameMixer: TFrameMixer;
    FrameViewer: TFrameSpineViewer;
    FrameTimeline: TFrameTimeline;
    AnimationItem: TCastleSpineMixerAnimationItem;
    StateMain: TStateMain;
  end;

var
  FormMain: TFormMain;

implementation

{$R *.lfm}

uses
  Form.NewAnimation,
  Form.RenameAnimation,
  Form.CurveEditor;

constructor TStateMain.Create(AOwner: TComponent);
begin
  inherited;
  DesignUrl := 'castle-data:/ui.castle-user-interface';
end;

procedure TStateMain.Start;
begin
  inherited;
  Self.Spine := DesignedComponent('Spine') as TCastleSpine;
  Self.Viewport := DesignedComponent('Viewport') as TCastleViewport;
  Self.Spine.AddBehavior(EditorSpineMixer);
end;

{ TFormMain }

procedure TFormMain.FormCreate(Sender: TObject);
begin
  EditorSpineMixer := TCastleSpineMixerBehavior.Create(Self);
  EditorSpineMixer.Data := TCastleSpineMixerData.Create(EditorSpineMixer);
  //
  FrameMixer := TFrameMixer.Create(Self);
  PanelMixer.InsertControl(FrameMixer);
  //
  FrameViewer := TFrameSpineViewer.Create(Self);
  PanelViewer.InsertControl(FrameViewer);
  //
  FrameTimeline := TFrameTimeline.Create(Self);
  PanelTimeline.InsertControl(FrameTimeline);
  // Load viewer ui data
  TCastleControl.MainControl := FrameViewer.CastleControlViewer;
  FrameViewer.CastleControlViewer.Container.BackgroundColor := Vector4(0, 0, 0, 0);
  StateMain := TStateMain.Create(Self);
  TUIState.Current := StateMain;
end;

procedure TFormMain.ButtonAddNewAnimationClick(Sender: TObject);
begin
  FormNewAnimation.Show;
end;

procedure TFormMain.ButtonBezierClick(Sender: TObject);
begin
  Self.FrameTimeline.SelectedRec.KeyItem.Kind := mktBezier;
  Self.FrameTimeline.ForceRepaint;
end;

procedure TFormMain.ButtonDeleteAnimationClick(Sender: TObject);
begin
  if Self.ComboBoxAnimations.ItemIndex >= 0 then
    if MessageDlg('Deletion', 'Do you want to delete "' + Self.ComboBoxAnimations.Items[Self.ComboBoxAnimations.ItemIndex] + '" animation?', mtConfirmation, [mbYes, mbNo], 0) = mrYes then
    begin
      try
        EditorSpineMixer.Data.DeleteAnimation(Self.ComboBoxAnimations.ItemIndex);
        // Refresh combobox animation
        Self.ComboBoxAnimations.Items.Delete(Self.ComboBoxAnimations.ItemIndex);
        Self.ComboBoxAnimations.Text := '';
        Self.ComboBoxAnimations.ItemIndex := Self.ComboBoxAnimations.Items.Count - 1;
        FormMain.ComboBoxAnimationsChange(Self);
      except
        on E: Exception do
          ShowMessage(E.Message);
      end;
    end;
end;

procedure TFormMain.ButtonLinearClick(Sender: TObject);
begin
  Self.FrameTimeline.SelectedRec.KeyItem.Kind := mktLinear;
  Self.FrameTimeline.ForceRepaint;
end;

procedure TFormMain.ButtonPlayClick(Sender: TObject);
begin
  if Self.ComboBoxAnimations.ItemIndex >= 0 then
    TimerPlay.Enabled := not TimerPlay.Enabled;
  if (TimerPlay.Enabled) and (Self.AnimationItem <> nil) then
  begin
    ButtonPlay.ImageIndex := 16;
    EditorSpineMixer.PlayAnimation(Self.AnimationItem.Name, Self.FrameTimeline.SelectedTime);
  end else
  begin
    ButtonPlay.ImageIndex := 2;
    EditorSpineMixer.StopAnimation;
    if Self.AnimationItem <> nil then
      EditorSpineMixer.SetInitialPose(Self.AnimationItem.Name);
  end;
  Self.FrameTimeline.ForceRepaint;
end;

procedure TFormMain.ButtonRenameAnimationClick(Sender: TObject);
begin
  if Self.AnimationItem = nil then Exit;
  FormRenameAnimation.EditAnimationName.Text := Self.AnimationItem.Name;
  FormRenameAnimation.Show;
end;

procedure TFormMain.ComboBoxAnimationsChange(Sender: TObject);
begin
  FormMain.TimerPlay.Enabled := False;
  if Self.ComboBoxAnimations.ItemIndex >= 0 then
  begin
    //
    Self.AnimationItem := TCastleSpineMixerAnimationItem(Self.ComboBoxAnimations.Items.Objects[Self.ComboBoxAnimations.ItemIndex]);
    // Reflect time value
    Self.FloatTime.Value := Self.AnimationItem.Duration;
  end else
    Self.AnimationItem := nil;
  EditorSpineMixer.Time := 0;
  LabelTime.Caption := FloatToStrF(EditorSpineMixer.Time, ffFixed, 0, 3);
  ButtonPlay.ImageIndex := 2;
  // Stop animation
  EditorSpineMixer.StopAnimation;
  // Unselected key
  Self.FrameTimeline.DeselectedKey;
  // Refresh mixer list
  Self.FrameMixer.RefreshMixerList;
  // Redraw timeline
  Self.FrameTimeline.ForceRepaint;
end;

procedure TFormMain.FloatSpeedChange(Sender: TObject);
begin
  Self.StateMain.Spine.TimePlayingSpeed := FloatSpeed.Value;
end;

procedure TFormMain.FloatTimeChange(Sender: TObject);
begin
  if Self.ComboBoxAnimations.ItemIndex >= 0 then
  begin
    // Change animation time
    Self.AnimationItem.Duration := FloatTime.Value;
    // Redraw timeline
    Self.FrameTimeline.ForceRepaint;
  end;
end;

procedure TFormMain.FloatZoomChange(Sender: TObject);
begin
if Self.ComboBoxAnimations.ItemIndex >= 0 then
  begin
    Self.FrameTimeline.Zoom := FloatZoom.Value;
    // Redraw timeline
    Self.FrameTimeline.ForceRepaint;
  end;
end;

procedure TFormMain.MenuItemLoadMixerDataClick(Sender: TObject);
begin
  if OpenDialogMixer.Execute then
  begin
    // Load new mixer data
    EditorSpineMixer.URL := OpenDialogMixer.FileName;
    //                          
    FormNewAnimation.RefreshAnimation;
    if Self.ComboBoxAnimations.Items.Count > 0 then
      Self.ComboBoxAnimations.ItemIndex := 0
    else
      Self.ComboBoxAnimations.ItemIndex := -1;
    Self.ComboBoxAnimationsChange(Sender);
  end;
end;

procedure TFormMain.MenuItemLoadSpineModelClick(Sender: TObject);
begin
  if OpenDialogSpine.Execute then
  begin
    try
      Self.StateMain.Spine.URL := OpenDialogSpine.FileName;
      Self.StateMain.Viewport.AssignDefaultCamera;
      Self.MenuItemNewMixerData.Enabled := True;      
      Self.MenuItemLoadMixerData.Enabled := True;
      Self.MenuItemSaveMixerData.Enabled := True;
    except
      on E: Exception do
        ShowMessage(E.Message);
    end;
  end;
end;

procedure TFormMain.MenuItemNewMixerDataClick(Sender: TObject);
var
  I: Integer;
begin
  // Create new mixer data
  EditorSpineMixer.Data.Free;
  EditorSpineMixer.Data := TCastleSpineMixerData.Create(EditorSpineMixer);
  //
  EditorSpineMixer.Time := 0;
  FormNewAnimation.RefreshAnimation;
  Self.ComboBoxAnimations.ItemIndex := -1;
  Self.ComboBoxAnimationsChange(Sender);
end;

procedure TFormMain.MenuItemSaveMixerDataClick(Sender: TObject);
begin
  if SaveDialogMixer.Execute then
  begin
    ComponentSave(EditorSpineMixer.Data, SaveDialogMixer.FileName);
  end;
end;

procedure TFormMain.SpeedButton1Click(Sender: TObject);
begin
  FrameMixer.MenuItemAddMixerClick(Sender);
end;

procedure TFormMain.ButtonEditCurveClick(Sender: TObject);
begin
  FormCurveEditor.ControlPoints[0] := Vector2(Self.FrameTimeline.SelectedRec.KeyItem.CX1, Self.FrameTimeline.SelectedRec.KeyItem.CY1);       
  FormCurveEditor.ControlPoints[1] := Vector2(Self.FrameTimeline.SelectedRec.KeyItem.CX2, Self.FrameTimeline.SelectedRec.KeyItem.CY2);
  FormCurveEditor.Show;
end;

procedure TFormMain.TimerPlayStartTimer(Sender: TObject);
begin
  // EditorSpineMixer.Time := 0;
end;

procedure TFormMain.TimerPlayTimer(Sender: TObject);
begin
  LabelTime.Caption := FloatToStrF(EditorSpineMixer.Time, ffFixed, 0, 3);
  // Redraw timeline
  Self.FrameTimeline.ForceRepaint;
end;

procedure TFormMain.TimerUpdateTimer(Sender: TObject);
begin
  if Self.FrameTimeline.SelectedRec.KeyItem <> nil then
  begin
    Self.PanelMixerItemKind.Visible := True;
    case Self.FrameTimeline.SelectedRec.KeyItem.Kind of
      mktBezier:
        begin
          LabelKind.Caption := 'Bezier';
          Self.ButtonEditCurve.Enabled := True;
        end
      else
        begin
          LabelKind.Caption := 'Linear';
          Self.ButtonEditCurve.Enabled := False;
        end;
    end;
  end else
  begin
    Self.PanelMixerItemKind.Visible := False;
  end;
end;

end.

