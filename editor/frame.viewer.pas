unit Frame.Viewer;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, CastleControl, CastleSpine, CastleLCLUtils, CastleKeysMouse;

type

  { TFrameSpineViewer }

  TFrameSpineViewer = class(TFrame)
    CastleControlViewer: TCastleControl;
    procedure CastleControlViewerMotion(Sender: TObject;
      const Event: TInputMotion);
  private

  public
  end;

implementation

{$R *.lfm}

uses
  Form.Main;

{ TFrameSpineViewer }

procedure TFrameSpineViewer.CastleControlViewerMotion(Sender: TObject;
  const Event: TInputMotion);
begin
 // FormMain.FrameTimeline.PaintBoxTimeline.Update;
end;

end.

