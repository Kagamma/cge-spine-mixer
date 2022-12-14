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
    ButtonAddEvent: TSpeedButton;
    CheckBoxAutoKey: TCheckBox;
    CheckBoxFilterMixer: TCheckBox;
    CheckBoxFilterEvent: TCheckBox;
    ComboBoxAnimations: TComboBox;
    EditMixerFilter: TEdit;
    FloatZoom: TFloatSpinEdit;
    FloatTime: TFloatSpinEdit;
    FloatSpeed: TFloatSpinEdit;
    ImageList: TImageList;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    LabelMixerCount: TLabel;
    LabelKind: TLabel;
    LabelTime: TLabel;
    MainMenu: TMainMenu;
    MenuItemRedo: TMenuItem;
    MenuItemUndo: TMenuItem;
    MenuItemEdit: TMenuItem;
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
    ButtonAddMixer: TSpeedButton;
    ButtonLinear: TSpeedButton;
    ButtonBezier: TSpeedButton;
    ButtonEditCurve: TSpeedButton;
    ButtonCloneAnimation: TSpeedButton;
    Splitter1: TSplitter;
    Splitter2: TSplitter;
    TimerUpdate: TTimer;
    TimerPlay: TTimer;
    procedure ButtonAddEventClick(Sender: TObject);
    procedure ButtonAddNewAnimationClick(Sender: TObject);
    procedure ButtonBezierClick(Sender: TObject);
    procedure ButtonCloneAnimationClick(Sender: TObject);
    procedure ButtonDeleteAnimationClick(Sender: TObject);
    procedure ButtonLinearClick(Sender: TObject);
    procedure ButtonPlayClick(Sender: TObject);
    procedure ButtonRenameAnimationClick(Sender: TObject);
    procedure CheckBoxFilterEventChange(Sender: TObject);
    procedure CheckBoxFilterMixerChange(Sender: TObject);
    procedure ComboBoxAnimationsChange(Sender: TObject);
    procedure EditMixerFilterChange(Sender: TObject);
    procedure FloatSpeedChange(Sender: TObject);
    procedure FloatTimeChange(Sender: TObject);
    procedure FloatZoomChange(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
    procedure FormCreate(Sender: TObject);
    procedure MenuItemLoadMixerDataClick(Sender: TObject);
    procedure MenuItemLoadSpineModelClick(Sender: TObject);
    procedure MenuItemNewMixerDataClick(Sender: TObject);
    procedure MenuItemRedoClick(Sender: TObject);
    procedure MenuItemSaveMixerDataClick(Sender: TObject);
    procedure ButtonAddMixerClick(Sender: TObject);
    procedure ButtonEditCurveClick(Sender: TObject);
    procedure MenuItemUndoClick(Sender: TObject);
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
  Form.CurveEditor,
  Form.AddEvent,
  Utils.Undo;

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
  if Self.StateMain.Spine.URL = '' then
    ShowMessage('You need to load a spine model first.')
  else
    FormNewAnimation.Show;
end;

procedure TFormMain.ButtonAddEventClick(Sender: TObject);
begin
  FrameMixer.MenuItemAddEventClick(Sender);
end;

procedure TFormMain.ButtonBezierClick(Sender: TObject);
begin   
  UndoSystem.Mark;
  Self.FrameTimeline.SelectedRec.KeyItem.Kind := smktBezier;
  Self.FrameTimeline.ForceRepaint;
end;

procedure TFormMain.ButtonCloneAnimationClick(Sender: TObject);
var
  CloneName: String;
begin
  if Self.AnimationItem = nil then
  begin
    ShowMessage('You need to select (or create) an animation first.');
    Exit;
  end;
  UndoSystem.Mark;
  CloneName := AnimationItem.Name + ' - clone';
  EditorSpineMixer.Data.CloneAnimation(AnimationItem.Name, CloneName);
  Self.ComboBoxAnimations.AddItem(
    CloneName,
    EditorSpineMixer.Data.FindAnimation(CloneName)
  );
  Self.ComboBoxAnimations.ItemIndex := Self.ComboBoxAnimations.Items.Count - 1;
  Self.ComboBoxAnimationsChange(Sender);
end;

procedure TFormMain.ButtonDeleteAnimationClick(Sender: TObject);
begin
  if Self.AnimationItem = nil then
  begin
    ShowMessage('You need to select (or create) an animation first.');
    Exit;
  end;
  if Self.ComboBoxAnimations.ItemIndex >= 0 then
    if MessageDlg('Deletion', 'Do you want to delete "' + Self.ComboBoxAnimations.Items[Self.ComboBoxAnimations.ItemIndex] + '" animation?', mtConfirmation, [mbYes, mbNo], 0) = mrYes then
    begin
      try
        UndoSystem.Mark;
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
  UndoSystem.Mark;
  Self.FrameTimeline.SelectedRec.KeyItem.Kind := smktLinear;
  Self.FrameTimeline.ForceRepaint;
end;

procedure TFormMain.ButtonPlayClick(Sender: TObject);
begin
  if Self.ComboBoxAnimations.ItemIndex >= 0 then
    TimerPlay.Enabled := not TimerPlay.Enabled;
  if (TimerPlay.Enabled) and (Self.AnimationItem <> nil) then
  begin
    ButtonPlay.ImageIndex := 16;
    EditorSpineMixer.PlayAnimation(Self.AnimationItem.Name, True, Self.FrameTimeline.SelectedTime);
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
  if Self.AnimationItem = nil then
  begin
    ShowMessage('You need to select (or create) an animation first.');
    Exit;
  end;
  FormRenameAnimation.EditAnimationName.Text := Self.AnimationItem.Name;
  FormRenameAnimation.Show;
end;

procedure TFormMain.CheckBoxFilterEventChange(Sender: TObject);
begin
  // Stop animation
  EditorSpineMixer.StopAnimation;
  // Unselected key
  Self.FrameTimeline.DeselectedKey;
  // Refresh mixer list
  Self.FrameMixer.RefreshMixerList;
  // Redraw timeline
  Self.FrameTimeline.ForceRepaint;
end;

procedure TFormMain.CheckBoxFilterMixerChange(Sender: TObject);
begin
  // Stop animation
  EditorSpineMixer.StopAnimation;
  // Unselected key
  Self.FrameTimeline.DeselectedKey;
  // Refresh mixer list
  Self.FrameMixer.RefreshMixerList;
  // Redraw timeline
  Self.FrameTimeline.ForceRepaint;
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

procedure TFormMain.EditMixerFilterChange(Sender: TObject);
begin                  
  FormMain.FrameMixer.RefreshMixerList;
  FormMain.FrameTimeline.ForceRepaint;
end;

procedure TFormMain.FloatSpeedChange(Sender: TObject);
begin
  Self.StateMain.Spine.TimePlayingSpeed := FloatSpeed.Value;
end;

procedure TFormMain.FloatTimeChange(Sender: TObject);
begin
  if Self.ComboBoxAnimations.ItemIndex >= 0 then
  begin
    UndoSystem.Mark;
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

procedure TFormMain.FormCloseQuery(Sender: TObject; var CanClose: Boolean);
begin
  CanClose := True;
  if (UndoSystem.IsDirty) and (MessageDlg('You have unsaved changes. Do you still want to quit?', mtConfirmation, [mbOk, mbCancel], 0) = mrCancel) then
    CanClose := False;
end;

procedure TFormMain.MenuItemLoadMixerDataClick(Sender: TObject);
begin
  if OpenDialogMixer.Execute then
  begin
    UndoSystem.Clear;
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
      Self.Caption := 'Castle Spine Mixer - ' + OpenDialogSpine.FileName;
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
  UndoSystem.Clear;
  // Create new mixer data
  EditorSpineMixer.Data.Free;
  EditorSpineMixer.Data := TCastleSpineMixerData.Create(EditorSpineMixer);
  //
  EditorSpineMixer.Time := 0;
  FormNewAnimation.RefreshAnimation;
  Self.ComboBoxAnimations.ItemIndex := -1;
  Self.ComboBoxAnimationsChange(Sender);
end;

procedure TFormMain.MenuItemRedoClick(Sender: TObject);
begin
  UndoSystem.Redo;
  Self.FrameTimeline.SelectedRec.KeyItem := nil;
  FormNewAnimation.RefreshAnimation;
  if Self.ComboBoxAnimations.Items.Count > 0 then
    Self.ComboBoxAnimations.ItemIndex := 0
  else
    Self.ComboBoxAnimations.ItemIndex := -1;
  Self.ComboBoxAnimationsChange(Sender);
end;

procedure TFormMain.MenuItemSaveMixerDataClick(Sender: TObject);
begin
  if SaveDialogMixer.Execute then
  begin
    ComponentSave(EditorSpineMixer.Data, SaveDialogMixer.FileName);
    UndoSystem.IsDirty := False;
  end;
end;

procedure TFormMain.ButtonAddMixerClick(Sender: TObject);
begin
  FrameMixer.MenuItemAddMixerClick(Sender);
end;

procedure TFormMain.ButtonEditCurveClick(Sender: TObject);
begin
  FormCurveEditor.ControlPoints[0] := Vector2(Self.FrameTimeline.SelectedRec.KeyItem.CX1, Self.FrameTimeline.SelectedRec.KeyItem.CY1);       
  FormCurveEditor.ControlPoints[1] := Vector2(Self.FrameTimeline.SelectedRec.KeyItem.CX2, Self.FrameTimeline.SelectedRec.KeyItem.CY2);
  FormCurveEditor.Show;
end;

procedure TFormMain.MenuItemUndoClick(Sender: TObject);
begin
  UndoSystem.Undo;
  Self.FrameTimeline.SelectedRec.KeyItem := nil;
  FormNewAnimation.RefreshAnimation;
  if Self.ComboBoxAnimations.Items.Count > 0 then
    Self.ComboBoxAnimations.ItemIndex := 0
  else
    Self.ComboBoxAnimations.ItemIndex := -1;
  Self.ComboBoxAnimationsChange(Sender);
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
      smktBezier:
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
  Self.MenuItemUndo.Enabled := UndoSystem.CanUndo;     
  Self.MenuItemRedo.Enabled := UndoSystem.CanRedo;
  Self.ButtonPlay.Enabled := (Self.StateMain.Spine.URL <> '') and (Self.ComboBoxAnimations.ItemIndex >= 0);
end;

end.

