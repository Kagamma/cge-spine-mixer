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
    LabelTime: TLabel;
    MainMenu: TMainMenu;
    MenuItemNewMixerData: TMenuItem;
    MenuItemSaveMixerData: TMenuItem;
    MenuItemLoadMixerData: TMenuItem;
    MenuItemFile: TMenuItem;
    MenuItemLoadSpineModel: TMenuItem;
    OpenDialogMixer: TOpenDialog;
    OpenDialogSpine: TOpenDialog;
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
    Splitter1: TSplitter;
    Splitter2: TSplitter;
    TimerPlay: TTimer;
    procedure ButtonAddNewAnimationClick(Sender: TObject);
    procedure ButtonDeleteAnimationClick(Sender: TObject);
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
    procedure TimerPlayStartTimer(Sender: TObject);
    procedure TimerPlayTimer(Sender: TObject);
  private
  public
    FrameMixer: TFrameMixer;
    FrameViewer: TFrameSpineViewer;
    FrameTimeline: TFrameTimeline;
    AnimationItem: TCastleSpineMixerAnimationItem;
    StateMain: TStateMain;
    Ticks: QWord;
  end;

var
  FormMain: TFormMain;

implementation

{$R *.lfm}

uses
  Form.NewAnimation,
  Form.RenameAnimation;

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

procedure TFormMain.ButtonPlayClick(Sender: TObject);
begin
  if Self.ComboBoxAnimations.ItemIndex >= 0 then
    TimerPlay.Enabled := not TimerPlay.Enabled;
  if (TimerPlay.Enabled) and (Self.AnimationItem <> nil) then
  begin
    ButtonPlay.ImageIndex := 16;
    EditorSpineMixer.PlayAnimation(Self.AnimationItem.Name);
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
    Self.ComboBoxAnimations.ItemIndex := 0;
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
    except
      on E: Exception do
        ShowMessage(E.Message);
    end;
  end;
end;

procedure TFormMain.MenuItemNewMixerDataClick(Sender: TObject);
begin
  // Delete old mixer data
  FreeAndNil(EditorSpineMixer);
  // Create new mixer data
  EditorSpineMixer := TCastleSpineMixerBehavior.Create(Self);
  EditorSpineMixer.Data := TCastleSpineMixerData.Create(EditorSpineMixer);
  //
  Self.StateMain.Spine.AddBehavior(EditorSpineMixer);
  //
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

procedure TFormMain.TimerPlayStartTimer(Sender: TObject);
begin
  EditorSpineMixer.Time := 0;
  Ticks := GetTickCount64;
end;

procedure TFormMain.TimerPlayTimer(Sender: TObject);
begin
  LabelTime.Caption := FloatToStrF(EditorSpineMixer.Time, ffFixed, 0, 3);
  // Redraw timeline
  Self.FrameTimeline.ForceRepaint;
end;

end.

