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
  TCastleSpineMixerKeyItem = class(TCollectionItem)
  private
    FTime: Single; // Key time
    FValue: Single; // Key value
  public
  published
    property Time: Single read FTime write FTime;
    property Value: Single read FValue write FValue;
  end;

  TCastleSpineMixerKeyList = class(TCollection)
  private
  public
  published
  end;

  TCastleSpineMixerMixerItem = class(TCollectionItem)
  private
    FName: String; // Mixer name
    FKeyList: TCastleSpineMixerKeyList; // List of Keys
  public
    constructor Create(ACollection: TCollection); override;
    destructor Destroy; override;
    function AddKey(ATime, AValue: Single): TCastleSpineMixerKeyItem;
    procedure DeleteKey(ATime: Single);
    function GetValue(ATime: Single): Single;
  published
    property Name: String read FName write FName;  
    property KeyList: TCastleSpineMixerKeyList read FKeyList;
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

  TCastleSpineMixerData = class(TComponent)
  private
    FAnimationList: TCastleSpineMixerAnimationList; // List of animations
    FTime: Single;
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
  end;

  TCastleSpineMixerBehavior = class(TCastleBehavior)
  private
    FData: TCastleSpineMixerData;
    FURL: String;
  public
    property Data: TCastleSpineMixerData read FData write FData;
  published
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

// ----- TCastleSpineMixerKeyItem -----

// ----- TCastleSpineMixerKeyList -----

// ----- TCastleSpineMixerMixerItem -----

function DoKeyCompare(Item1, Item2: TCollectionItem): Integer;
begin
  if TCastleSpineMixerKeyItem(Item1).Time < TCastleSpineMixerKeyItem(Item2).Time then
    Result := -1
  else
  if TCastleSpineMixerKeyItem(Item1).Time > TCastleSpineMixerKeyItem(Item2).Time then
    Result := 1
  else
    Result := 0;
end;

constructor TCastleSpineMixerMixerItem.Create(ACollection: TCollection);
begin
  inherited;
  Self.FKeyList := TCastleSpineMixerKeyList.Create(TCastleSpineMixerKeyItem);
end;

destructor TCastleSpineMixerMixerItem.Destroy;
begin
  Self.FKeyList.Free;
  inherited;
end;

function TCastleSpineMixerMixerItem.AddKey(ATime, AValue: Single): TCastleSpineMixerKeyItem;
var
  I: Integer;
begin
  Result := nil;
  for I := 0 to Self.FKeyList.Count - 1 do
  begin
    if TCastleSpineMixerKeyItem(Self.FKeyList.Items[I]).Time = ATime then
    begin
      Result := Self.FKeyList.Items[I] as TCastleSpineMixerKeyItem;
      break;
    end;
  end;
  if Result = nil then
    Result := Self.FKeyList.Add as TCastleSpineMixerKeyItem;
  Result.Time := ATime;
  Result.Value := AValue;
  Self.FKeyList.Sort(@DoKeyCompare);
end;

procedure TCastleSpineMixerMixerItem.DeleteKey(ATime: Single);
var
  I: Integer;
begin
  for I := 0 to Self.FKeyList.Count - 1 do
  begin
    if TCastleSpineMixerKeyItem(Self.FKeyList.Items[I]).Time = ATime then
    begin
      Self.FKeyList.Items[I].Free;
      break;
    end;
  end;
end;

function TCastleSpineMixerMixerItem.GetValue(ATime: Single): Single;
var
  I: Integer;
  PrevKey,
  NextKey: TCastleSpineMixerKeyItem;
begin
  PrevKey := nil;
  NextKey := nil;
  Result := 0;
  for I := 0 to Self.FKeyList.Count - 1 do
  begin
    NextKey := Self.FKeyList.Items[I] as TCastleSpineMixerKeyItem;
    if NextKey.Time <= ATime then
      PrevKey := NextKey
    else
    if NextKey.Time > ATime then
      break;
  end;
  if (NextKey <> nil) and (PrevKey <> nil) then
  begin
    if PrevKey.Time = NextKey.Time then
      Result := NextKey.Value
    else
      Result := Lerp(PrevKey.Value, NextKey.Value, (ATime - PrevKey.Time) / (NextKey.Time - PrevKey.Time));
  end else
  if (NextKey <> nil) and (PrevKey = nil) then
    Result := NextKey.Value;
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

// ----- TCastleSpineMixerData -----

procedure TCastleSpineMixerData.SetTime(const ATime: Single);
begin
  Self.FTime := Max(0, ATime);
end;

constructor TCastleSpineMixerData.Create(AOwner: TComponent);
begin
  inherited;
  Self.FAnimationList := TCastleSpineMixerAnimationList.Create(TCastleSpineMixerAnimationItem);
  Self.FTime := 0;
end;

destructor TCastleSpineMixerData.Destroy;
begin
  Self.FAnimationList.Free;
  inherited;
end;

function TCastleSpineMixerData.AddAnimation(AnimationName: String): TCastleSpineMixerAnimationItem;
var
  I: Integer;
begin
  for I := 0 to Self.FAnimationList.Count - 1 do
    if TCastleSpineMixerAnimationItem(Self.FAnimationList.Items[I]).Name = AnimationName then
      raise Exception.Create('Duplicate animations are not allowed');
  Result := Self.FAnimationList.Add as TCastleSpineMixerAnimationItem;
  Result.Name := AnimationName;
end;

procedure TCastleSpineMixerData.RenameAnimation(AIndex: Cardinal; AnimationName: String);
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

procedure TCastleSpineMixerData.DeleteAnimation(AIndex: Cardinal);
begin
  if AIndex > Self.FAnimationList.Count - 1 then
    raise Exception.Create('List out of bound');
  Self.FAnimationList.Items[AIndex].Free;
end;

initialization
  RegisterSerializableComponent(TCastleSpineMixerBehavior, 'Spine Mixer Behavior');
  RegisterSerializableComponent(TCastleSpineMixerData, 'Spine Mixer Data');
  {$ifdef CASTLE_DESIGN_MODE}
  RegisterPropertyEditor(TypeInfo(AnsiString), TCastleSpineMixerBehavior, 'URL',
    TSceneURLPropertyEditor);
  {$endif}

end.

