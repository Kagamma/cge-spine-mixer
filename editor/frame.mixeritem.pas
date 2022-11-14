unit Frame.MixerItem;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, ExtCtrls, StdCtrls, ComCtrls, Spin,
  Buttons;

type

  { TFrameMixerItem }

  TFrameMixerItem = class(TFrame)
    LabelValue: TLabel;
    LabelName: TLabel;
    Panel1: TPanel;
    Panel2: TPanel;
    Panel3: TPanel;
    Panel4: TPanel;
    ButtonDelete: TSpeedButton;
    TrackBarValue: TTrackBar;
    procedure ButtonDeleteClick(Sender: TObject);
    procedure TrackBarValueChange(Sender: TObject);
  private

  public
    MixerOwner: TWinControl;
  end;

implementation

{$R *.lfm}

{ TFrameMixerItem }

procedure TFrameMixerItem.TrackBarValueChange(Sender: TObject);
begin
  LabelValue.Caption := FloatToStr(TrackBarValue.Position / 100);
end;

procedure TFrameMixerItem.ButtonDeleteClick(Sender: TObject);
begin
  Self.MixerOwner.RemoveControl(Self);
end;

end.

