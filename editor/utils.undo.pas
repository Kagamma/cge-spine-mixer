unit Utils.Undo;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, CastleSpineMixer;

type
  TUndoSystem = class
  private
    FBackupList: TStringList;
    FUndoCount: Integer;
  public
    constructor Create;
    destructor Destroy; override;
    function CanUndo: Boolean; 
    function CanRedo: Boolean;
    procedure Clear;
    procedure Mark;
    procedure Undo;  
    procedure Redo;
  end;

var
  UndoSystem: TUndoSystem;

implementation

uses
  Math;

const
  MAX_UNDO = 50;

constructor TUndoSystem.Create;
begin
  inherited;
  Self.FBackupList := TStringList.Create;
end;

destructor TUndoSystem.Destroy;
begin
  Self.FBackupList.Free;
  inherited;
end;

procedure TUndoSystem.Clear;
begin
  Self.FBackupList.Clear;
  Self.FUndoCount := 0;
end;

function TUndoSystem.CanUndo: Boolean;
begin
  Result := (Self.FBackupList.Count > 0) and (Self.FUndoCount > 0);
end;

function TUndoSystem.CanRedo: Boolean;
begin
  Result := Self.FUndoCount < Self.FBackupList.Count - 1;
end;

procedure TUndoSystem.Mark;
var
  I: Integer;
begin
  while Self.FBackupList.Count > Self.FUndoCount do
    Self.FBackupList.Pop;
  Self.FBackupList.Add(EditorSpineMixer.DataToJSONString);
  if Self.FBackupList.Count > 100 then
    Self.FBackupList.Delete(0);
  Self.FUndoCount := Min(Self.FUndoCount + 1, MAX_UNDO);
end;

procedure TUndoSystem.Undo;
begin
  if (Self.FBackupList.Count = 0) or (Self.FUndoCount = 0) then Exit;
  // For redo back to current state
  if Self.FUndoCount = Self.FBackupList.Count then
    Self.FBackupList.Add(EditorSpineMixer.DataToJSONString);
  //
  Self.FUndoCount := Max(0, Self.FUndoCount - 1);
  EditorSpineMixer.JSONStringToData(Self.FBackupList[Self.FUndoCount]);
end;

procedure TUndoSystem.Redo;
begin                                                         
  Self.FUndoCount := Min(Self.FUndoCount + 1, Self.FBackupList.Count);
  EditorSpineMixer.JSONStringToData(Self.FBackupList[Self.FUndoCount]);
end;

initialization
  UndoSystem := TUndoSystem.Create;

finalization
  UndoSystem.Free;

end.

