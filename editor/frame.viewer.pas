unit Frame.Viewer;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, CastleControl, CastleSpine;

type

  { TFrameSpineViewer }

  TFrameSpineViewer = class(TFrame)
    CastleControlViewer: TCastleControl;
  private

  public
    Spine: TCastleSpine;
  end;

implementation

{$R *.lfm}

end.

