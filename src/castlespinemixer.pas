unit CastleSpineMixer;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, CastleVectors, CastleComponentSerialize,
  {$ifdef CASTLE_DESIGN_MODE}
  PropEdits, CastlePropEdits, CastleDebugTransform, Forms, Controls, Graphics, Dialogs,
  ButtonPanel, StdCtrls, ExtCtrls, CastleInternalExposeTransformsDialog,
  {$endif}
  CastleTransform;

type
  TCastleSpineMixerAnchorItem = class(TCollectionItem)
  private
    FTime: Single; // Anchor time
    FValue: Single; // Anchor value
  public
  published
    property Time: Single read FTime write FTime;
    property Value: Single read FValue write FValue;
  end;

  TCastleSpineMixerAnchorList = class(TCollection)
  private
  public
  published
  end;

  TCastleSpineMixerMixerItem = class(TCollectionItem)
  private
    FName: String; // Mixer name
    FAnchorList: TCastleSpineMixerAnchorList; // List of anchors
  public
    constructor Create(ACollection: TCollection); override;
    destructor Destroy; override;
    function AddAnchor(ATime, AValue: Single): TCastleSpineMixerAnchorItem;
    procedure DeleteAnchor(ATime: Single);
    function GetValue(ATime: Single): Single;
  published
    property Name: String read FName write FName;  
    property AnchorList: TCastleSpineMixerAnchorList read FAnchorList;
  end;

  TCastleSpineMixerMixerList = class(TCollection)
  private
  public
  published
  end;

  TCastleSpineMixerAnimationItem = class(TCollectionItem)
  private
    FDuration: Single; // Animation length, in seconds
    FMixerList: TCastleSpineMixerMixerList; // List of mixers
    FName: String; // Animation name
  public
    constructor Create(ACollection: TCollection); override;
    destructor Destroy; override;
    { Add a new mixer }
    function AddMixer(MixerName: String): TCastleSpineMixerMixerItem;
    { Delete mixer }
    procedure DeleteMixer(MixerItem: TCastleSpineMixerMixerItem);
  published
    property Duration: Single read FDuration write FDuration;
    property MixerList: TCastleSpineMixerMixerList read FMixerList;
    property Name: String read FName write FName;
  end;

  TCastleSpineMixerAnimationList = class(TCollection)
  private
  public
    constructor Create(AItemClass: TCollectionItemClass);
    destructor Destroy; override;
  published
  end;

  TCastleSpineMixerBehavior = class(TCastleBehavior)
  private
    FAnimationList: TCastleSpineMixerAnimationList; // List of animations
    FTime: Single;
    FURL: String;
    procedure SetTime(const ATime: Single);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    { Add a new animation }
    function AddAnimation(AnimationName: String): TCastleSpineMixerAnimationItem;
    { Rename animation }
    procedure RenameAnimation(AIndex: Cardinal; AnimationName: String);
    { Delete animation }
    procedure DeleteAnimation(AIndex: Cardinal);
  published
    property AnimationList: TCastleSpineMixerAnimationList read FAnimationList;
    property Time: Single read FTime write SetTime;
    property URL: String read FURL write FURL;
  end;

var
  EditorSpineMixer: TCastleSpineMixerBehavior; // This is used by mixer editor

implementation

uses
  Math;

function Lerp(const X1, X2, T: Single): Single; inline;
begin
  Result := X1 + T * (X2 - X1);
end;

// ----- TCastleSpineMixerAnchorItem -----

// ----- TCastleSpineMixerAnchorList -----

// ----- TCastleSpineMixerMixerItem -----

function DoAnchorCompare(Item1, Item2: TCollectionItem): Integer;
begin
  if TCastleSpineMixerAnchorItem(Item1).Time < TCastleSpineMixerAnchorItem(Item2).Time then
    Result := -1
  else
  if TCastleSpineMixerAnchorItem(Item1).Time > TCastleSpineMixerAnchorItem(Item2).Time then
    Result := 1
  else
    Result := 0;
end;

constructor TCastleSpineMixerMixerItem.Create(ACollection: TCollection);
begin
  inherited;
  Self.FAnchorList := TCastleSpineMixerAnchorList.Create(TCastleSpineMixerAnchorItem);
end;

destructor TCastleSpineMixerMixerItem.Destroy;
begin
  Self.FAnchorList.Free;
  inherited;
end;

function TCastleSpineMixerMixerItem.AddAnchor(ATime, AValue: Single): TCastleSpineMixerAnchorItem;
var
  I: Integer;
begin
  Result := nil;
  for I := 0 to Self.FAnchorList.Count - 1 do
  begin
    if TCastleSpineMixerAnchorItem(Self.FAnchorList.Items[I]).Time = ATime then
    begin
      Result := Self.FAnchorList.Items[I] as TCastleSpineMixerAnchorItem;
      break;
    end;
  end;
  if Result = nil then
    Result := Self.FAnchorList.Add as TCastleSpineMixerAnchorItem;
  Result.Time := ATime;
  Result.Value := AValue;
  Self.FAnchorList.Sort(@DoAnchorCompare);
end;

procedure TCastleSpineMixerMixerItem.DeleteAnchor(ATime: Single);
var
  I: Integer;
begin
  for I := 0 to Self.FAnchorList.Count - 1 do
  begin
    if TCastleSpineMixerAnchorItem(Self.FAnchorList.Items[I]).Time = ATime then
    begin
      Self.FAnchorList.Items[I].Free;
      break;
    end;
  end;
end;

function TCastleSpineMixerMixerItem.GetValue(ATime: Single): Single;
var
  I: Integer;
  PrevAnchor,
  NextAnchor: TCastleSpineMixerAnchorItem;
begin
  PrevAnchor := nil;
  NextAnchor := nil;
  Result := 0;
  for I := 0 to Self.FAnchorList.Count - 1 do
  begin
    NextAnchor := Self.FAnchorList.Items[I] as TCastleSpineMixerAnchorItem;
    if NextAnchor.Time <= ATime then
      PrevAnchor := NextAnchor
    else
    if NextAnchor.Time > ATime then
      break;
  end;
  if (NextAnchor <> nil) and (PrevAnchor <> nil) then
  begin
    if PrevAnchor.Time = NextAnchor.Time then
      Result := NextAnchor.Value
    else
      Result := Lerp(PrevAnchor.Value, NextAnchor.Value, (ATime - PrevAnchor.Time) / (NextAnchor.Time - PrevAnchor.Time));
  end else
  if (NextAnchor <> nil) and (PrevAnchor = nil) then
    Result := NextAnchor.Value;
end;

// ----- TCastleSpineMixerMixerList -----

// ----- TCastleSpineMixerAnimationItem -----

constructor TCastleSpineMixerAnimationItem.Create(ACollection: TCollection);
begin
  inherited;
  Self.FMixerList := TCastleSpineMixerMixerList.Create(TCastleSpineMixerMixerItem);
  Self.FDuration := 1;
end;

destructor TCastleSpineMixerAnimationItem.Destroy;
begin
  Self.FMixerList.Free;
  inherited;
end;

function TCastleSpineMixerAnimationItem.AddMixer(MixerName: String): TCastleSpineMixerMixerItem;
var
  I: Integer;
begin
  for I := 0 to Self.FMixerList.Count - 1 do
    if TCastleSpineMixerMixerItem(Self.FMixerList.Items[I]).Name = MixerName then
      raise Exception.Create('Duplicate mixers are not allowed');
  Result := Self.FMixerList.Add as TCastleSpineMixerMixerItem;
  Result.Name := MixerName;
end;

procedure TCastleSpineMixerAnimationItem.DeleteMixer(MixerItem: TCastleSpineMixerMixerItem);
var
  I: Integer;
begin
  for I := 0 to Self.FMixerList.Count - 1 do
  begin
    if Self.FMixerList.Items[I] = MixerItem then
    begin
      Self.FMixerList.Items[I].Free;
      break;
    end;
  end;
end;

// ----- TCastleSpineMixerAnimationList -----

constructor TCastleSpineMixerAnimationList.Create(AItemClass: TCollectionItemClass);
begin
  inherited;
end;

destructor TCastleSpineMixerAnimationList.Destroy;
begin
  inherited;
end;

// ----- TCastleSpineMixerBehavior -----

procedure TCastleSpineMixerBehavior.SetTime(const ATime: Single);
begin
  Self.FTime := Max(0, ATime);
end;

constructor TCastleSpineMixerBehavior.Create(AOwner: TComponent);
begin
  inherited;
  Self.FAnimationList := TCastleSpineMixerAnimationList.Create(TCastleSpineMixerAnimationItem);
  Self.FTime := 0;
end;

destructor TCastleSpineMixerBehavior.Destroy;
begin
  Self.FAnimationList.Free;
  inherited;
end;

function TCastleSpineMixerBehavior.AddAnimation(AnimationName: String): TCastleSpineMixerAnimationItem;
var
  I: Integer;
begin
  for I := 0 to Self.FAnimationList.Count - 1 do
    if TCastleSpineMixerAnimationItem(Self.FAnimationList.Items[I]).Name = AnimationName then
      raise Exception.Create('Duplicate animations are not allowed');
  Result := Self.FAnimationList.Add as TCastleSpineMixerAnimationItem;
  Result.Name := AnimationName;
end;

procedure TCastleSpineMixerBehavior.RenameAnimation(AIndex: Cardinal; AnimationName: String);
var
  Animation: TCastleSpineMixerAnimationItem;
  I: Integer;
begin
  if AIndex > Self.FAnimationList.Count - 1 then
    raise Exception.Create('List out of bound');
  for I := 0 to Self.FAnimationList.Count - 1 do
    if (AIndex <> I) and (TCastleSpineMixerAnimationItem(Self.FAnimationList.Items[I]).Name = AnimationName) then
      raise Exception.Create('Duplicate animations are not allowed');
  Animation := TCastleSpineMixerAnimationItem(Self.FAnimationList.Items[AIndex]);
  Animation.Name := AnimationName;
end;

procedure TCastleSpineMixerBehavior.DeleteAnimation(AIndex: Cardinal);
begin
  if AIndex > Self.FAnimationList.Count - 1 then
    raise Exception.Create('List out of bound');
  Self.FAnimationList.Items[AIndex].Free;
end;

initialization
  RegisterSerializableComponent(TCastleSpineMixerBehavior, 'Spine Mixer Behavior');
  {$ifdef CASTLE_DESIGN_MODE}
  RegisterPropertyEditor(TypeInfo(AnsiString), TCastleSpineMixerBehavior, 'URL',
    TSceneURLPropertyEditor);
  {$endif}

end.

