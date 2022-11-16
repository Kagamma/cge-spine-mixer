unit CastleSpineMixer;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, CastleVectors, CastleComponentSerialize,
  {$ifdef CASTLE_DESIGN_MODE}
  PropEdits, CastlePropEdits, CastleDebugTransform, Forms, Controls, Graphics, Dialogs,
  ButtonPanel, StdCtrls, ExtCtrls, CastleInternalExposeTransformsDialog,
  {$endif}
  CastleTransform, CastleSpine, spine, CastleClassUtils;

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
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    { Add a new animation }
    function AddAnimation(AnimationName: String): TCastleSpineMixerAnimationItem;
    { Rename animation }
    procedure RenameAnimation(AIndex: Cardinal; AnimationName: String);
    { Delete animation }
    procedure DeleteAnimation(AIndex: Cardinal);
    { Find animation }
    function FindAnimation(AnimationName: String): TCastleSpineMixerAnimationItem;
  published
    property AnimationList: TCastleSpineMixerAnimationList read FAnimationList;
  end;

  TCastleSpineMixerBehavior = class(TCastleBehavior)
  private
    FData: TCastleSpineMixerData;
    FURL: String;
    FTime: Single;
    { True = playing animation }
    FIsPlaying: Boolean;
    { Reference to current animation }
    FCurrentAnimationItem: TCastleSpineMixerAnimationItem;
    { }
    FIsCheckAutoAnimation: Boolean;
    FAutoAnimation: String;
    procedure SetTime(const ATime: Single);
    procedure SetSetAutoAnimation(const AAnimation: String);
    procedure LoadData(const AURL: String);
  protected
    procedure Update(const SecondsPassed: Single; var RemoveMe: TRemoveType); override;
  public
    {$ifdef CASTLE_DESIGN_MODE}
    function PropertySections(const PropertyName: String): TPropertySections; override;
    {$endif}
    function PlayAnimation(const AnimationName: String): Boolean;     
    procedure StopAnimation;
    { This mainly use to set initial pose based on time value }
    procedure SetInitialPose(const AnimationName: String);

    property Data: TCastleSpineMixerData read FData write FData;
    property Time: Single read FTime write SetTime;
  published
    property URL: String read FURL write LoadData;
    property AutoAnimation: String read FAutoAnimation write SetSetAutoAnimation;
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

{ ----- TExposeTransformsPropertyEditor ----- }

{$ifdef CASTLE_DESIGN_MODE}
type
  { Property editor to select an animation on TCastleSceneCore. }
  TSceneAutoAnimationPropertyEditor = class(TStringPropertyEditor)
  public
    function GetAttributes: TPropertyAttributes; override;
    procedure GetValues(Proc: TGetStrProc); override;
    procedure SetValue(const NewValue: String); override;
  end;

function TSceneAutoAnimationPropertyEditor.GetAttributes: TPropertyAttributes;
begin
  Result := [paMultiSelect, paValueList, paSortList, paRevertable];
end;

procedure TSceneAutoAnimationPropertyEditor.GetValues(Proc: TGetStrProc);
var
  Mixer: TCastleSpineMixerBehavior;
  I: Integer;
begin
  Proc('');
  Mixer := GetComponent(0) as TCastleSpineMixerBehavior;
  for I := 0 to Mixer.Data.AnimationList.Count - 1 do
    Proc(TCastleSpineMixerAnimationItem(Mixer.Data.AnimationList.Items[I]).Name);
end;

procedure TSceneAutoAnimationPropertyEditor.SetValue(const NewValue: String);
var
  Mixer: TCastleSpineMixerBehavior;
begin
  inherited SetValue(NewValue);
  Mixer := GetComponent(0) as TCastleSpineMixerBehavior;
  Mixer.AutoAnimation := NewValue;
end;
{$endif}

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

constructor TCastleSpineMixerData.Create(AOwner: TComponent);
begin
  inherited;
  Self.FAnimationList := TCastleSpineMixerAnimationList.Create(TCastleSpineMixerAnimationItem);
end;

destructor TCastleSpineMixerData.Destroy;
begin
  Self.FAnimationList.Free;
  inherited;
end;

function TCastleSpineMixerData.AddAnimation(AnimationName: String): TCastleSpineMixerAnimationItem;
begin
  if Self.FindAnimation(AnimationName) <> nil then
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

function TCastleSpineMixerData.FindAnimation(AnimationName: String): TCastleSpineMixerAnimationItem;
var
  I: Integer;
begin
  for I := 0 to Self.FAnimationList.Count - 1 do
    if TCastleSpineMixerAnimationItem(Self.FAnimationList.Items[I]).Name = AnimationName then
      Exit(TCastleSpineMixerAnimationItem(Self.FAnimationList.Items[I]));
  Result := nil;
end;

// ----- TCastleSpineMixerBehavior -----

{$ifdef CASTLE_DESIGN_MODE}
function TCastleSpineMixerBehavior.PropertySections(
  const PropertyName: String): TPropertySections;
begin
  if (PropertyName = 'URL')
    or (PropertyName = 'AutoAnimation')then
    Result := [psBasic]
  else
    Result := inherited PropertySections(PropertyName);
end;
{$endif}

procedure TCastleSpineMixerBehavior.SetTime(const ATime: Single);
begin
  Self.FTime := Max(0, ATime);
end;

procedure TCastleSpineMixerBehavior.LoadData(const AURL: String);
begin
  Self.FURL := AURL;
  if Self.FData <> nil then
    FreeAndNil(Self.FData);
  if AURL <> '' then
  begin
    Self.FData := ComponentLoad(AURL, Self) as TCastleSpineMixerData;
  end;
  Self.FIsCheckAutoAnimation := True;
end;

procedure TCastleSpineMixerBehavior.SetSetAutoAnimation(const AAnimation: String);
begin
  Self.FAutoAnimation := AAnimation;
  Self.FIsCheckAutoAnimation := True;
end;

procedure TCastleSpineMixerBehavior.Update(const SecondsPassed: Single; var RemoveMe: TRemoveType);
var
  Spine: TCastleSpine;
  I, J, Track: Integer;
  MixerItem: TCastleSpineMixerMixerItem;
  Params: TCastleSpinePlayAnimationParameters;  
  TrackEntry: PspTrackEntry;
begin
  inherited;
  if not (Self.Parent is TCastleSpine) then Exit;
  //
  Spine := TCastleSpine(Self.Parent);
  if not Spine.IsGLContextInitialized then Exit;
  //
  if Self.FIsCheckAutoAnimation then
  begin
    Self.PlayAnimation(Self.FAutoAnimation);
    FIsCheckAutoAnimation := False;
  end;
  if not Self.FIsPlaying then Exit;

  // Recalculate initial time again
  Track := 0;
  for I := 0 to Self.FCurrentAnimationItem.MixerList.Count - 1 do
  begin
    MixerItem := TCastleSpineMixerMixerItem(Self.FCurrentAnimationItem.MixerList.Items[I]);
    for J := 0 to Spine.AnimationsList.Count - 1 do
    begin
      // Found animation
      if MixerItem.Name = Spine.AnimationsList[J] then
      begin
        TrackEntry := Spine.TrackEntries[Track];
        TrackEntry^.trackTime := TrackEntry^.animation^.duration * MixerItem.GetValue(Self.FTime);
        Inc(Track);
      end;
    end;
  end;
  //
  Self.FTime := Self.FTime + SecondsPassed * Spine.TimePlayingSpeed;
  if Self.FTime > Self.FCurrentAnimationItem.Duration then
    Self.FTime := 0;
end;

function TCastleSpineMixerBehavior.PlayAnimation(const AnimationName: String): Boolean;
var
  Spine: TCastleSpine;
  I, J, Track: Integer;
  MixerItem: TCastleSpineMixerMixerItem;
  Params: TCastleSpinePlayAnimationParameters;
begin
  if not (Self.Parent is TCastleSpine) then Exit;
  if AnimationName = '' then
  begin
    Self.StopAnimation;
    Exit;
  end;
  //
  Spine := TCastleSpine(Self.Parent);
  Self.FCurrentAnimationItem := Self.Data.FindAnimation(AnimationName);
  if Self.FCurrentAnimationItem <> nil then
  begin
    Self.FTime := 0;
    Self.FIsPlaying := True;
    // Stop current animations
    Spine.StopAnimation;
    // Loop through list of mixers and check it with spine animation, if available, we play it
    Track := 0;
    for I := 0 to Self.FCurrentAnimationItem.MixerList.Count - 1 do
    begin
      MixerItem := TCastleSpineMixerMixerItem(Self.FCurrentAnimationItem.MixerList.Items[I]);
      for J := 0 to Spine.AnimationsList.Count - 1 do
      begin
        // Found animation
        if MixerItem.Name = Spine.AnimationsList[J] then
        begin
          // Mark it as playing
          // TODO: Proper handle play animation, instead of hard setting
          Params.Name := MixerItem.Name;
          Params.TransitionDuration := 0;
          Params.Track := Track;
          Params.Forward := True;
          Params.InitialTime := Self.FTime * MixerItem.GetValue(Self.FTime);
          Params.TimeScale := 0;
          Params.Loop := True;
          Spine.PlayAnimation(Params);
          Inc(Track);
        end;
      end;
    end;
    // Force spine to start animating immediately
    Spine.InternalPlayAnimation;
    //
    Result := True;
  end else
  begin
    Self.FIsPlaying := False;
    Result := False;
  end;
end;

procedure TCastleSpineMixerBehavior.StopAnimation; 
var
  Spine: TCastleSpine;
begin
  if not (Self.Parent is TCastleSpine) then Exit;
  //      
  Spine := TCastleSpine(Self.Parent);
  Spine.StopAnimation;
  Self.FIsPlaying := False;
end;

procedure TCastleSpineMixerBehavior.SetInitialPose(const AnimationName: String);
var
  Spine: TCastleSpine;
  I, J, Track: Integer;
  Params: TCastleSpinePlayAnimationParameters;
  TrackEntry: PspTrackEntry;
  MixerItem: TCastleSpineMixerMixerItem;
begin
  inherited;
  if not (Self.Parent is TCastleSpine) then Exit;
  //
  Self.FIsPlaying := False;
  Spine := TCastleSpine(Self.Parent);
  Self.FCurrentAnimationItem := Self.Data.FindAnimation(AnimationName);
  if Self.FCurrentAnimationItem = nil then Exit;
  // Loop through list of mixers and check it with spine animation, if available, we play it
  Track := 0;
  for I := 0 to Self.FCurrentAnimationItem.MixerList.Count - 1 do
  begin
    MixerItem := TCastleSpineMixerMixerItem(Self.FCurrentAnimationItem.MixerList.Items[I]);
    for J := 0 to Spine.AnimationsList.Count - 1 do
    begin
      // Found animation
      if MixerItem.Name = Spine.AnimationsList[J] then
      begin
        // Mark it as playing
        Params.Name := MixerItem.Name;
        Params.TransitionDuration := 0;
        Params.Track := Track;
        Params.Forward := False;
        Params.InitialTime := Self.FTime * MixerItem.GetValue(Self.FTime);
        Params.TimeScale := 0;
        Params.Loop := True;
        Spine.PlayAnimation(Params);
        Inc(Track);
      end;
    end;
  end;
  // Force spine to start animating immediately
  Spine.InternalPlayAnimation;
  // Recalculate initial time again
  Track := 0;
  for I := 0 to Self.FCurrentAnimationItem.MixerList.Count - 1 do
  begin
    MixerItem := TCastleSpineMixerMixerItem(Self.FCurrentAnimationItem.MixerList.Items[I]);
    for J := 0 to Spine.AnimationsList.Count - 1 do
    begin
      // Found animation
      if MixerItem.Name = Spine.AnimationsList[J] then
      begin
        TrackEntry := Spine.TrackEntries[Track];
        TrackEntry^.trackTime := TrackEntry^.animation^.duration * MixerItem.GetValue(Self.FTime);
        Inc(Track);
      end;
    end;
  end;
end;

initialization
  RegisterSerializableComponent(TCastleSpineMixerBehavior, 'Spine Mixer Behavior');
  RegisterSerializableComponent(TCastleSpineMixerData, 'Spine Mixer Data');
  {$ifdef CASTLE_DESIGN_MODE}
  RegisterPropertyEditor(TypeInfo(AnsiString), TCastleSpineMixerBehavior, 'URL',
    TSceneURLPropertyEditor);
  RegisterPropertyEditor(TypeInfo(AnsiString), TCastleSpineMixerBehavior, 'AutoAnimation',
    TSceneAutoAnimationPropertyEditor);
  {$endif}

end.

