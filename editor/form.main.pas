unit Form.Main;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, Menus, ExtCtrls,
  StdCtrls, Buttons, ComCtrls, Spin, CastleControl, LCLType,
  CastleApplicationProperties, CastleUIControls, CastleUIState, FileUtil,
  CastleSceneCore, CastleScene, CastleVectors, CastleSpineMixer, CastleSpine,
  CastleViewport,
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
    ImageList: TImageList;
    Label1: TLabel;
    Label2: TLabel;
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
    procedure FloatTimeChange(Sender: TObject);
    procedure FloatZoomChange(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
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
end;

{ TFormMain }

procedure TFormMain.FormCreate(Sender: TObject);
begin
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

procedure TFormMain.FormKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if (Key = VK_DELETE) and (Self.FrameTimeline.SelectedRec.AnchorItem <> nil) then
  begin

  end;
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
        EditorSpineMixer.DeleteAnimation(Self.ComboBoxAnimations.ItemIndex);
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
  if TimerPlay.Enabled then
    ButtonPlay.ImageIndex := 16
  else
    ButtonPlay.ImageIndex := 2;
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
  end;
  EditorSpineMixer.Time := 0;
  LabelTime.Caption := FloatToStrF(EditorSpineMixer.Time, ffFixed, 0, 3);
  ButtonPlay.ImageIndex := 2;
  // Refresh mixer list
  Self.FrameMixer.RefreshMixerList;
  // Redraw timeline
  Self.FrameTimeline.ForceRepaint;
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

  end;
end;

procedure TFormMain.MenuItemLoadSpineModelClick(Sender: TObject);
begin
  if OpenDialogSpine.Execute then
  begin
    try
      Self.StateMain.Spine.URL := OpenDialogSpine.FileName;
      // Self.StateMain.Viewport.AssignDefaultCamera;
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
end;

procedure TFormMain.MenuItemSaveMixerDataClick(Sender: TObject);
begin
  if SaveDialogMixer.Execute then
  begin

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
var
  Dt: Single;
begin
  Dt := (GetTickCount64 - Ticks) / 1000;
  EditorSpineMixer.Time := EditorSpineMixer.Time + Dt;
  if EditorSpineMixer.Time > Self.AnimationItem.Duration then
    EditorSpineMixer.Time := 0;
  Ticks := GetTickCount64;
  LabelTime.Caption := FloatToStrF(EditorSpineMixer.Time, ffFixed, 0, 3);
  // Redraw timeline
  Self.FrameTimeline.Update;
end;

initialization
  EditorSpineMixer := TCastleSpineMixerBehavior.Create(nil);

finalization
  FreeAndNil(EditorSpineMixer);

end.

