unit CastleSpineMixer;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, CastleVectors, CastleComponentSerialize,
  {$ifdef CASTLE_DESIGN_MODE}
  PropEdits, CastlePropEdits, CastleDebugTransform, Forms, Controls, Graphics, Dialogs,
  ButtonPanel, StdCtrls, ExtCtrls, CastleInternalExposeTransformsDialog,
  {$endif}
  CastleTransform, CastleSpine, spine, CastleClassUtils, CastleCurves, fpjsonrtti,
  Generics.Collections;

type
  TCastleSpineMixerKeyType = (smktLinear, smktBezier);
  TCastleSpineMixerType = (smtMixer, smtEvent);
  TCastleSpineMixerEventType = (smetEvent, smetEnd);

  TCastleSpineMixerEvent = record
    Kind: TCastleSpineMixerEventType;
    Name,
    Value: String;
  end;

  TCastleSpineMixerEventNotify = procedure(const Event: TCastleSpineMixerEvent) of object;

  TCastleSpineMixerKeyItem = class(TCollectionItem)
  private
    FTime: Single; // Key time
    FValue: Single; // Key value
    FStringValue: String; // Key event value
    FKind: TCastleSpineMixerKeyType;
    FCX1, FCY1, FCX2, FCY2: Single;
    FIsBezierCached: Boolean;
    FActived: Boolean;
    procedure SetCX1(const V: Single);
    procedure SetCY1(const V: Single);
    procedure SetCX2(const V: Single);
    procedure SetCY2(const V: Single);
    function GetBezierValue(const T: Single): Single;
  public
    BezierCurvePoints: array[0..19] of TVector2;
    constructor Create(ACollection: TCollection); override;
    procedure CalculateBezier;
  published
    property Actived: Boolean read FActived write FActived default True;
    property Time: Single read FTime write FTime default 0.0;
    property Value: Single read FValue write FValue default 0.0;
    property StringValue: String read FStringValue write FStringValue;
    property Kind: TCastleSpineMixerKeyType read FKind write FKind default smktLinear;
    property CX1: Single read FCX1 write SetCX1 default 0.5;
    property CY1: Single read FCY1 write SetCY1 default 0.0;
    property CX2: Single read FCX2 write SetCX2 default 0.5;
    property CY2: Single read FCY2 write SetCY2 default 1.0;
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
    FKind: TCastleSpineMixerType;
  public
    constructor Create(ACollection: TCollection); override;
    destructor Destroy; override;
    procedure SortKey;
    function AddKey(ATime, AValue: Single): TCastleSpineMixerKeyItem; overload;
    function AddKey(ATime: Single; AValue: String): TCastleSpineMixerKeyItem; overload;
    procedure DeleteKey(ATime: Single);
    function GetValue(ATime: Single): Single;
    function GetStringValue(ATime: Single): TCastleSpineMixerKeyItem;
    function GetStringValuePrecise(ATime: Single): String;
  published
    property Name: String read FName write FName;
    property KeyList: TCastleSpineMixerKeyList read FKeyList;
    property Kind: TCastleSpineMixerType read FKind write FKind default smtMixer;
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
    { Clone animation }
    function CloneAnimation(AnimationName, NewAnimationName: String): TCastleSpineMixerAnimationItem;
  published
    property AnimationList: TCastleSpineMixerAnimationList read FAnimationList;
  end;

  TCastleSpineMixerEventTriggerDict = specialize TDictionary<String, TCastleSpineMixerKeyItem>;
  TCastleSpineMixerBehavior = class(TCastleBehavior)
  private
    FEventTriggerDict: TCastleSpineMixerEventTriggerDict;
    FData: TCastleSpineMixerData;
    FURL: String;
    FTime: Single;
    FOnEventNotify: TCastleSpineMixerEventNotify; // Used by Spine mixer's events
    { True = playing animation }
    FIsPlaying: Boolean;
    FIsLooped: Boolean;
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
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function DataToJSONString: String;
    procedure JSONStringToData(JSONString: String);
    function PlayAnimation(const AnimationName: String; const Loop: boolean; const InitialTime: Single = 0): Boolean;
    procedure StopAnimation;
    { This mainly use to set initial pose based on time value }
    procedure SetInitialPose(const AnimationName: String);

    property Data: TCastleSpineMixerData read FData write FData;
    property Time: Single read FTime write SetTime;
  published
    property URL: String read FURL write LoadData;
    property AutoAnimation: String read FAutoAnimation write SetSetAutoAnimation;
    property OnEventNotify: TCastleSpineMixerEventNotify read FOnEventNotify write FOnEventNotify;
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

function TCastleSpineMixerKeyItem.GetBezierValue(const T: Single): Single;
var
  I: Integer;
  F: Single;
begin
  if not Self.FIsBezierCached then
  begin
    Self.CalculateBezier;
  end;
  I := Floor(T * 20);
  F := Frac(T * 20);
  if I < High(Self.BezierCurvePoints) then
  begin
    Result := Lerp(Self.BezierCurvePoints[I].Y, Self.BezierCurvePoints[I + 1].Y, F);
  end else
  begin
    Result := 1;
  end;
end;

procedure TCastleSpineMixerKeyItem.SetCX1(const V: Single);
begin
  Self.FCX1 := V;
  Self.FIsBezierCached := False;
end;

procedure TCastleSpineMixerKeyItem.SetCY1(const V: Single);
begin
  Self.FCY1 := V;
  Self.FIsBezierCached := False;
end;

procedure TCastleSpineMixerKeyItem.SetCX2(const V: Single);
begin
  Self.FCX2 := V;
  Self.FIsBezierCached := False;
end;

procedure TCastleSpineMixerKeyItem.SetCY2(const V: Single);
begin
  Self.FCY2 := V;
  Self.FIsBezierCached := False;
end;

constructor TCastleSpineMixerKeyItem.Create(ACollection: TCollection);
begin
  inherited;
  Self.FCX1 := 0.5;
  Self.FCY1 := 0;
  Self.FCX2 := 0.5;
  Self.FCY2 := 1;
  Self.FIsBezierCached := False;
  Self.FActived := True;
end;

procedure TCastleSpineMixerKeyItem.CalculateBezier;
var
  I: Integer;
  ControlPoints: TCubicBezier2DPoints;
begin
  ControlPoints[0] := Vector2(0, 0);
  ControlPoints[1] := Vector2(Self.FCX1, Self.FCY1);
  ControlPoints[2] := Vector2(Self.FCX2, Self.FCY2);
  ControlPoints[3] := Vector2(1, 1);
  for I := 0 to High(BezierCurvePoints) do
  begin
    Self.BezierCurvePoints[I] := CubicBezier2D(I / High(Self.BezierCurvePoints), ControlPoints);
  end;
end;

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

function DoMixerCompare(Item1, Item2: TCollectionItem): Integer;
begin
  Result := CompareStr(TCastleSpineMixerMixerItem(Item1).Name, TCastleSpineMixerMixerItem(Item2).Name);
end;

constructor TCastleSpineMixerMixerItem.Create(ACollection: TCollection);
begin
  inherited;
  Self.FKeyList := TCastleSpineMixerKeyList.Create(TCastleSpineMixerKeyItem);
  Self.FKind := smtMixer;
end;

destructor TCastleSpineMixerMixerItem.Destroy;
begin
  Self.FKeyList.Free;
  inherited;
end;

procedure TCastleSpineMixerMixerItem.SortKey;
begin
  Self.FKeyList.Sort(@DoKeyCompare);
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
  SortKey;
end;

function TCastleSpineMixerMixerItem.AddKey(ATime: Single; AValue: String): TCastleSpineMixerKeyItem;
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
  Result.StringValue := AValue;
  SortKey;
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
  T: Single;
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
    if not PrevKey.Actived then
      Exit(-1);
    if PrevKey.Time = NextKey.Time then
      Result := NextKey.Value
    else
    begin
      case PrevKey.Kind of
        smktBezier:
          begin
            T := PrevKey.GetBezierValue((ATime - PrevKey.Time) / (NextKey.Time - PrevKey.Time));
            Result := Lerp(PrevKey.Value, NextKey.Value, T);
          end
        else
          begin
            T := (ATime - PrevKey.Time) / (NextKey.Time - PrevKey.Time);
            Result := Lerp(PrevKey.Value, NextKey.Value, T);
          end;
      end;
    end;
  end else
  if (NextKey <> nil) and (PrevKey = nil) then
    Result := NextKey.Value;
  Result := Min(0.999999, Max(Result, 0.000001));
end;

function TCastleSpineMixerMixerItem.GetStringValue(ATime: Single): TCastleSpineMixerKeyItem;
var
  I: Integer;
  PrevKey,
  NextKey: TCastleSpineMixerKeyItem;
begin
  PrevKey := nil;
  NextKey := nil;
  Result := nil;
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
    if not PrevKey.Actived then
      Exit(nil);
    Result := PrevKey;
  end;
end;

function TCastleSpineMixerMixerItem.GetStringValuePrecise(ATime: Single): String;
var
  I: Integer;
  NextKey: TCastleSpineMixerKeyItem;
begin
  Result := '';
  for I := 0 to Self.FKeyList.Count - 1 do
  begin
    NextKey := Self.FKeyList.Items[I] as TCastleSpineMixerKeyItem;
    if Abs(NextKey.Time - ATime) <= 0.001 then
      Result := NextKey.StringValue;
  end;
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
  Self.FMixerList.Sort(@DoMixerCompare);
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

function TCastleSpineMixerData.CloneAnimation(AnimationName, NewAnimationName: String): TCastleSpineMixerAnimationItem;
var
  JSONString: String;
  Streamer: TJSONStreamer;
  DeStreamer: TJSONDeStreamer;
  BaseAnim: TCastleSpineMixerAnimationItem;
begin
  BaseAnim := Self.FindAnimation(AnimationName);
  if BaseAnim = nil then
    raise Exception.Create('"' + AnimationName + '" not found');
  Streamer := TJSONStreamer.Create(nil);
  DeStreamer := TJSONDeStreamer.Create(nil);
  try
    JSONString := Streamer.ObjectToJSONString(BaseAnim);
    Result := Self.AddAnimation(NewAnimationName);
    DeStreamer.JSONToObject(JSONString, Result);
    Result.Name := NewAnimationName;
  finally
    Streamer.Free;
    DeStreamer.Free;
  end;
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

constructor TCastleSpineMixerBehavior.Create(AOwner: TComponent);
begin
  inherited;
  Self.FEventTriggerDict := TCastleSpineMixerEventTriggerDict.Create;
end;

destructor TCastleSpineMixerBehavior.Destroy;
begin
  Self.FEventTriggerDict.Free;
  inherited;
end;

function TCastleSpineMixerBehavior.DataToJSONString: String;
var
  Streamer: TJSONStreamer;
begin
  Streamer := TJSONStreamer.Create(nil);
  try
    Result := Streamer.ObjectToJSONString(Self.Data);
  finally
    Streamer.Free;
  end;
end;

procedure TCastleSpineMixerBehavior.JSONStringToData(JSONString: String);
var
  DeStreamer: TJSONDeStreamer;
begin
  if Self.FData <> nil then
    FreeAndNil(Self.FData);
  DeStreamer := TJSONDeStreamer.Create(nil);
  Self.FData := TCastleSpineMixerData.Create(Self);
  try
    DeStreamer.JSONToObject(JSONString, Self.FData);
  finally
    DeStreamer.Free;
  end;
end;

procedure TCastleSpineMixerBehavior.Update(const SecondsPassed: Single; var RemoveMe: TRemoveType);
var
  Spine: TCastleSpine;
  I, J, Track: Integer;
  V: Single;
  MixerItem: TCastleSpineMixerMixerItem;
  TrackEntry: PspTrackEntry;
  Params: TCastleSpinePlayAnimationParameters;
  KeyItem: TCastleSpineMixerKeyItem;
  Event: TCastleSpineMixerEvent;
begin
  inherited;
  if not (Self.Parent is TCastleSpine) then Exit;
  //
  Spine := TCastleSpine(Self.Parent);
  if not Spine.IsGLContextInitialized then Exit;
  //
  if Self.FIsCheckAutoAnimation then
  begin
    Self.PlayAnimation(Self.FAutoAnimation, Spine.AutoAnimationsLoop);
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
      if (MixerItem.Name = Spine.AnimationsList[J]) and (MixerItem.Kind = smtMixer) then
      begin
        TrackEntry := Spine.TrackEntries[Track];
        V := MixerItem.GetValue(Self.FTime);
        // Activate?
        if (TrackEntry = nil) and (V <> -1) then
        begin
          Params.Name := MixerItem.Name;
          Params.TransitionDuration := 0;
          Params.Track := Track;
          Params.Forward := True;
          Params.InitialTime := 0;
          Params.TimeScale := 0;
          Params.Loop := True;
          Spine.PlayAnimation(Params);
        end else
        // Deactivate?
        if (TrackEntry <> nil) and (V = -1) then
        begin
          Spine.StopAnimation(Track);
          // Not sure why we need a dummy. Without it, all animations stop working
          Spine.PlayAnimation('_____dummy_____', True, True, Track);
        end;
        if (TrackEntry <> nil) then
        begin
          TrackEntry^.trackTime := TrackEntry^.animation^.duration * MixerItem.GetValue(Self.FTime);
        end;
        Inc(Track);
      end else
      if (MixerItem.Kind = smtEvent) then
      begin
        if Self.OnEventNotify <> nil then
        begin
          KeyItem := MixerItem.GetStringValue(Self.FTime);
          if (KeyItem <> nil) and
             ((not Self.FEventTriggerDict.ContainsKey(MixerItem.Name)) or
             (Self.FEventTriggerDict[MixerItem.Name] <> KeyItem)) then
          begin
            Self.FEventTriggerDict.AddOrSetValue(MixerItem.Name, KeyItem);
            Event.Kind := smetEnd;
            Event.Name := MixerItem.Name;
            Event.Value := KeyItem.StringValue;
            Self.OnEventNotify(Event);
          end;
        end;
      end;
    end;
  end;
  //
  Self.FTime := Self.FTime + SecondsPassed * Spine.TimePlayingSpeed;
  //
  if Self.FTime > Self.FCurrentAnimationItem.Duration then
  begin
    Self.FTime := 0;
    Self.StopAnimation;
    if Self.OnEventNotify <> nil then
    begin
      Event.Kind := smetEnd;
      Event.Name := Self.FCurrentAnimationItem.Name;
      Event.Value := '';
      Self.OnEventNotify(Event);
    end;
    if Self.FIsLooped then
      Self.PlayAnimation(Self.FCurrentAnimationItem.Name, True);
  end;
end;

function TCastleSpineMixerBehavior.PlayAnimation(const AnimationName: String; const Loop: Boolean; const InitialTime: Single = 0): Boolean;
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
  Self.FEventTriggerDict.Clear;
  if Self.FCurrentAnimationItem <> nil then
  begin
    Self.FTime := Max(0, InitialTime);
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
        if (MixerItem.Kind = smtMixer) and (MixerItem.Name = Spine.AnimationsList[J]) then
        begin
          // Mark it as playing
          // TODO: Proper handle play animation, instead of hard setting
          if MixerItem.GetValue(Self.FTime) = -1 then
            Params.Name := '_____dummy_____'
          else
            Params.Name := MixerItem.Name;
          Params.TransitionDuration := 0;
          Params.Track := Track;
          Params.Forward := True;
          Params.InitialTime := 0;
          Params.TimeScale := 0;
          Params.Loop := True;
          Spine.PlayAnimation(Params);
          Inc(Track);
        end;
      end;
    end;
    // Force spine to start animating immediately
    Spine.InternalPlayAnimation;
    Self.FIsLooped := Loop;
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
      if (MixerItem.Kind = smtMixer) and (MixerItem.Name = Spine.AnimationsList[J]) then
      begin
        // Mark it as playing
        Params.Name := MixerItem.Name;
        Params.TransitionDuration := 0;
        Params.Track := Track;
        Params.Forward := False;
        Params.InitialTime := 0;
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
        TrackEntry^.trackTime := TrackEntry^.animation^.duration * (1 - MixerItem.GetValue(Self.FTime));
        Inc(Track);
      end;
    end;
  end;
end;

var
  I: Integer;

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

