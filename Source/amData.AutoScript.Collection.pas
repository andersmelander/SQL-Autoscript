unit amData.AutoScript.Collection;

(*
  Version: MPL 1.1/GPL 2.0/LGPL 2.1

  This Source Code Form is subject to the terms of the Mozilla Public
  License, v. 2.0. If a copy of the MPL was not distributed with this
  file, You can obtain one at http://mozilla.org/MPL/2.0/.

  Software distributed under the License is distributed on an "AS IS" basis,
  WITHOUT WARRANTY OF ANY KIND, either express or implied. See the License
  for the specific language governing rights and limitations under the License.

  The Original Code is amData.AutoScript.Collection

  The Initial Developer of the Original Code is Anders Melander.

  Portions created by the Initial Developer are Copyright (C) 2002
  the Initial Developer. All Rights Reserved.

  Contributor(s):
    -

  Alternatively, the contents of this file may be used under the terms of
  either the GNU General Public License Version 2 or later (the "GPL"), or
  the GNU Lesser General Public License Version 2.1 or later (the "LGPL"),
  in which case the provisions of the GPL or the LGPL are applicable instead
  of those above. If you wish to allow use of your version of this file only
  under the terms of either the GPL or the LGPL, and not to allow others to
  use your version of this file under the terms of the MPL, indicate your
  decision by deleting the provisions above and replace them with the notice
  and other provisions required by the GPL or the LGPL. If you do not delete
  the provisions above, a recipient may use your version of this file under
  the terms of any one of the MPL, the GPL or the LGPL.
*)

interface

uses
  Generics.Collections,
  Classes;

type
  TScriptSource = (ssEmbedded, ssFile);

  TScriptCollectionItem = class;

//------------------------------------------------------------------------------
//
//      TScriptDependencyCollectionItem
//
//------------------------------------------------------------------------------
  (*
  ** TScriptDependencyKind
  ** - dkRequire        Only run script if the dependency has been run.
  ** - dkDisallow       Do not run script if the dependency has been run.
  ** - dkSupercedes     The script supercedes the dependency.
  *)
  TScriptDependencyKind = (dkRequire, dkDisallow {, dkSupercedes});

  TScriptDependencyCollectionItem = class(TCollectionItem)
  strict private
    FDependency: TScriptCollectionItem;
    FKind: TScriptDependencyKind;
  private
    procedure SetDependency(const Value: TScriptCollectionItem);
  protected
    function GetDisplayName: string; override;
  public
    destructor Destroy; override;
  published
    property Dependency: TScriptCollectionItem read FDependency write SetDependency;
    property Kind: TScriptDependencyKind read FKind write FKind default dkRequire;
  end;

//------------------------------------------------------------------------------
//
//      TScriptDependencyCollection
//
//------------------------------------------------------------------------------
  TScriptDependencyCollection = class(TOwnedCollection)
  strict private
    function GetDependency(Index: Integer): TScriptDependencyCollectionItem;
    procedure SetDependency(Index: Integer; const Value: TScriptDependencyCollectionItem);
  public
    constructor Create(AOwner: TPersistent);

    property Dependencies[Index: Integer]: TScriptDependencyCollectionItem read GetDependency write SetDependency; default;
  end;

//------------------------------------------------------------------------------
//
//      TScriptCollectionItem
//
//------------------------------------------------------------------------------
  (*
    scContinue          Proceed to execute script.
    scSkip              Do not execute script but continue. Do not record script as handled.
    scSkipDisable       Do not execute script but continue. Record script as handled.
    scAbort             Do not execute script and abort further processing.
  *)
  TScriptContinue = (scContinue, scSkip, scSkipDisable, scAbort);

  TContinueEvent = procedure(Sender: TObject; var Continue: TScriptContinue) of object;

  TScriptCollectionItem = class(TCollectionItem)
  strict private
    FMilestone: boolean;
    FEnabled: boolean;
    FDescription: string;
    FNumber: string;
    FScript: TStrings;
    FFileName: string;
    FSource: TScriptSource;
    FLoadOnDemand: boolean;
    FNeedLoad: boolean;
    FGUID: string;
    FDependencies: TScriptDependencyCollection;
    FComment: string;
    FSubscriptions: TList<TScriptDependencyCollectionItem>;
    FOnBeforeExecute: TContinueEvent;
    FOnAfterExecute: TNotifyEvent;
  strict private
    procedure SetScript(const Value: TStrings);
    procedure SetNumber(const Value: string);
    function GetGUID: string;
    procedure SetGUID(const Value: string);
    procedure SetEnabled(const Value: boolean);

    procedure DoLoad;
    procedure ValidateNumber(const Value: string);
    procedure ValidateGUID(const Value: string);
  protected
    procedure Subscribe(Item: TScriptDependencyCollectionItem);
    procedure Unsubscribe(Item: TScriptDependencyCollectionItem);
  protected
    function GetDisplayName: string; override;
  public
    constructor Create(Collection: TCollection); override;
    destructor Destroy; override;

    procedure LoadFromFile(const AFileName: string; ALoadOnDemand: boolean = True);
    procedure Prepare;
    property Source: TScriptSource read FSource;
    property FileName: string read FFileName;
    property LoadOnDemand: boolean read FLoadOnDemand;
  published
    property GUID: string read GetGUID write SetGUID;
    property Number: string read FNumber write SetNumber;
    property Description: string read FDescription write FDescription;
    property Script: TStrings read FScript write SetScript;
    property Milestone: boolean read FMilestone write FMilestone default False;
    property Enabled: boolean read FEnabled write SetEnabled default True;
    property Dependencies: TScriptDependencyCollection read FDependencies;
    property Comment: string read FComment write FComment;

    property OnBeforeExecute: TContinueEvent read FOnBeforeExecute write FOnBeforeExecute;
    property OnAfterExecute: TNotifyEvent read FOnAfterExecute write FOnAfterExecute;
  end;

//------------------------------------------------------------------------------
//
//      TScriptCollection
//
//------------------------------------------------------------------------------
  TScriptCollection = class(TOwnedCollection)
  strict private
    FUpdateCount: integer;
    FChanged: boolean;
  strict private
    function GetScript(Index: Integer): TScriptCollectionItem;
    procedure SetScript(Index: Integer; const Value: TScriptCollectionItem);
  protected
    procedure Update(Item: TCollectionItem); override;
  public
    constructor Create(AOwner: TPersistent);

    procedure BeginUpdate; override;
    procedure EndUpdate; override;
    function IndexOf(const Number: string): integer;
    property Scripts[Index: Integer]: TScriptCollectionItem read GetScript write SetScript; default;
  end;

//------------------------------------------------------------------------------
//
//      TScriptContainer
//
//------------------------------------------------------------------------------
  [ComponentPlatforms(0)]
  TScriptContainer = class(TComponent)
  strict private
    FScripts: TScriptCollection;
  strict private
    procedure SetScripts(const Value: TScriptCollection);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  published
    property Scripts: TScriptCollection read FScripts write SetScripts;
  end;

//------------------------------------------------------------------------------
//------------------------------------------------------------------------------
//------------------------------------------------------------------------------

implementation

uses
  ActiveX,
  ComObj,
  SysUtils;

{ TScriptCollectionItem }

constructor TScriptCollectionItem.Create(Collection: TCollection);
begin
  inherited Create(Collection);
  FScript := TStringList.Create;
  FDependencies := TScriptDependencyCollection.Create(Self);
  FEnabled := True;
end;

destructor TScriptCollectionItem.Destroy;
begin
  FScript.Free;
  FDependencies.Free;

  if (FSubscriptions <> nil) then
  begin
    for var Item in FSubscriptions do
      Item.Dependency := nil;

    FSubscriptions.Free;
  end;

  inherited Destroy;
end;

function TScriptCollectionItem.GetDisplayName: string;
begin
  if (Number <> '') or (Description <> '') then
  begin
    Result := Number+': ';
    if (Enabled) then
      Result := Result+Description
    else
      Result := Result+'('+Description+')';
  end else
    Result := inherited GetDisplayName;
end;

procedure TScriptCollectionItem.Subscribe(Item: TScriptDependencyCollectionItem);
begin
  if (FSubscriptions = nil) then
    FSubscriptions := TList<TScriptDependencyCollectionItem>.Create;

  FSubscriptions.Add(Item);
end;

procedure TScriptCollectionItem.Unsubscribe(Item: TScriptDependencyCollectionItem);
begin
  if (FSubscriptions <> nil) then
    FSubscriptions.Remove(Item);
end;

procedure TScriptCollectionItem.Prepare;
begin
  if (Source = ssFile) then
    DoLoad;
end;

procedure TScriptCollectionItem.LoadFromFile(const AFileName: string;
  ALoadOnDemand: boolean);
begin
  FFileName := AFileName;
  FSource := ssFile;
  FLoadOnDemand := ALoadOnDemand;
  FNeedLoad := True;
  if (not FLoadOnDemand) then
    DoLoad;
end;

procedure TScriptCollectionItem.SetNumber(const Value: string);
begin
  ValidateNumber(Value);
  FNumber := Value;
  Changed(True);
end;

procedure TScriptCollectionItem.SetScript(const Value: TStrings);
begin
  FScript.Assign(Value);
end;

procedure TScriptCollectionItem.DoLoad;
begin
  if (Source = ssFile) and (FNeedLoad) then
  begin
    Script.LoadFromFile(FileName);
    FNeedLoad := False;
  end;
end;

function TScriptCollectionItem.GetGUID: string;
var
  NewGUID: TGUID;
begin
  if (FGUID = '') then
  begin
    OleCheck(CoCreateGuid(NewGUID));
    FGUID := GuidToString(NewGUID);
  end;
  Result := FGUID;
end;

procedure TScriptCollectionItem.SetEnabled(const Value: boolean);
begin
  if (FEnabled <> Value) then
  begin
    if (Value) then
      ValidateNumber(FNumber);
    FEnabled := Value;
  end;
end;

procedure TScriptCollectionItem.SetGUID(const Value: string);
var
  NewGUID: TGUID;
begin
  if (Value = '') then
  begin
    OleCheck(CoCreateGuid(NewGUID));
    FGUID := GuidToString(NewGUID);
  end else
  begin
    StringToGUID(Value); // Validate that new value is a GUID
    ValidateGUID(Value);
    FGUID := Value;
  end;
end;

procedure TScriptCollectionItem.ValidateNumber(const Value: string);
var
  i: integer;
begin
  for i := 0 to Collection.Count-1 do
    if (Collection.Items[i] <> Self) and
      (TScriptCollectionItem(Collection.Items[i]).Enabled) and
      (TScriptCollectionItem(Collection.Items[i]).Number = Value) then
      raise Exception.CreateFmt('Duplicate script numbers not allowed: %s', [Value]);
end;

procedure TScriptCollectionItem.ValidateGUID(const Value: string);
var
  i: integer;
begin
  for i := 0 to Collection.Count-1 do
    if (Collection.Items[i] <> Self) and
      (TScriptCollectionItem(Collection.Items[i]).Enabled) and
      (TScriptCollectionItem(Collection.Items[i]).GUID = Value) then
      raise Exception.CreateFmt('Duplicate script GUIDs not allowed: %s', [Value]);
end;

{ TScriptCollection }

procedure TScriptCollection.BeginUpdate;
begin
  inc(FUpdateCount);
end;

constructor TScriptCollection.Create(AOwner: TPersistent);
begin
  inherited Create(AOwner, TScriptCollectionItem);
end;

procedure TScriptCollection.EndUpdate;
begin
  dec(FUpdateCount);
  if (FUpdateCount = 0) and (FChanged) then
    Changed;
end;

function TScriptCollection.GetScript(Index: Integer): TScriptCollectionItem;
begin
  Result := TScriptCollectionItem(Items[Index]);
end;

function TScriptCollection.IndexOf(const Number: string): integer;
begin
  Result := Count-1;
  while (Result >= 0) do
    if (AnsiCompareText(Scripts[Result].Number, Number) = 0) then
      break
    else
      dec(Result);
end;

procedure TScriptCollection.SetScript(Index: Integer;
  const Value: TScriptCollectionItem);
begin
  Items[Index] := Value;
end;

procedure TScriptCollection.Update(Item: TCollectionItem);
var
  i: integer;
  List: TStringList;
begin
  FChanged := True;
  if (FUpdateCount = 0) then
  begin
    inherited Update(Item);
    FChanged := False;
    if (Item = nil) and (Count > 1) then
    begin
      List := TStringList.Create;
      try
        for i := 0 to Count-1 do
          List.AddObject(Scripts[i].Number, Scripts[i]);
        List.Sort;
        BeginUpdate;
        try
          for i := 0 to List.Count-1 do
            TCollectionItem(List.Objects[i]).Index := i;
        finally
          EndUpdate;
        end;
      finally
        List.Free;
      end;
    end;
  end;
end;

{ TScriptContainer }

constructor TScriptContainer.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FScripts := TScriptCollection.Create(Self);
end;

destructor TScriptContainer.Destroy;
begin
  FScripts.Free;
  inherited Destroy;
end;

procedure TScriptContainer.SetScripts(const Value: TScriptCollection);
begin
  FScripts.Assign(Value);
end;

{ TScriptDependencyCollectionItem }

destructor TScriptDependencyCollectionItem.Destroy;
begin
  if (FDependency <> nil) then
    FDependency.Unsubscribe(Self);

  inherited;
end;

function TScriptDependencyCollectionItem.GetDisplayName: string;
begin
  if (FDependency <> nil) then
  begin
    if (FDependency.Number <> '') then
      Result := FDependency.Number+': '
    else
      Result := '';
    if (FDependency.Enabled) then
      Result := Result+FDependency.GUID
    else
      Result := Result+'('+FDependency.GUID+')';
  end else
    Result := inherited GetDisplayName;
end;

procedure TScriptDependencyCollectionItem.SetDependency(const Value: TScriptCollectionItem);
var
  Owner: TScriptCollectionItem;
  Items: TList<TScriptCollectionItem>;

  procedure CheckItem(Item: TScriptCollectionItem);
  begin
    if (Item = nil) then
      Exit;

    if (Item = Owner) then
      raise Exception.Create('Invalid self-reference');

    if (Items.Contains(Item)) then
      raise Exception.Create('Cyclic reference');

    Items.Add(Item);

    for var DepencencyItem in Item.Dependencies do
      CheckItem(TScriptDependencyCollectionItem(DepencencyItem).Dependency);
  end;
begin
  if (FDependency = Value) then
    Exit;


  // Check for self reference and cycles
  Owner := TScriptCollectionItem(Collection.Owner);
  Items := TList<TScriptCollectionItem>.Create;
  try

    CheckItem(Value);

  finally
    Items.Free;
  end;

  if (FDependency <> nil) then
    FDependency.Unsubscribe(Self);

  FDependency := Value;

  if (FDependency <> nil) then
    FDependency.Subscribe(Self);
end;

{ TScriptDependencyCollection }

constructor TScriptDependencyCollection.Create(AOwner: TPersistent);
begin
  inherited Create(AOwner, TScriptDependencyCollectionItem);
end;

function TScriptDependencyCollection.GetDependency(Index: Integer): TScriptDependencyCollectionItem;
begin
  Result := TScriptDependencyCollectionItem(Items[Index]);
end;

procedure TScriptDependencyCollection.SetDependency(Index: Integer;
  const Value: TScriptDependencyCollectionItem);
begin
  Items[Index] := Value;
end;

end.
