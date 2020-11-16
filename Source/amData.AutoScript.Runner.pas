unit amData.AutoScript.Runner;

(*
  Version: MPL 1.1/GPL 2.0/LGPL 2.1

  This Source Code Form is subject to the terms of the Mozilla Public
  License, v. 2.0. If a copy of the MPL was not distributed with this
  file, You can obtain one at http://mozilla.org/MPL/2.0/.

  Software distributed under the License is distributed on an "AS IS" basis,
  WITHOUT WARRANTY OF ANY KIND, either express or implied. See the License
  for the specific language governing rights and limitations under the License.

  The Original Code is amData.AutoScript.Runner

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
//  StrUtils,
  System.SysUtils, System.Classes,
  amData.AutoScript.Collection,
  amData.AutoScript.Runner.API;

//------------------------------------------------------------------------------
//
//      TDataModuleAutoScriptRunner
//
//------------------------------------------------------------------------------
// Abstract auto script runner base class.
//------------------------------------------------------------------------------
type
  TPatchLevelItems = TDictionary<string, boolean>;

  TDataModuleAutoScriptRunner = class abstract(TDataModule, IUnknown, IAutoScriptRunner)
  strict private
    FRefCount: integer;
    FPatchLevelItems: TPatchLevelItems;
    FMileStone: string;

  strict protected
    procedure DoGetPatchLevelItems(Items: TPatchLevelItems); virtual; abstract;
    procedure DoRegisterScript(ScriptItem: TScriptCollectionItem); virtual; abstract;
    function DoExecuteScript(ScriptItem: TScriptCollectionItem): boolean; virtual; abstract;

    function InTransaction: boolean; virtual; abstract;
    procedure BeginTransaction; virtual; abstract;
    procedure EndTransaction(Commit: boolean); virtual; abstract;

  strict protected
    function GetPatchLevelItems: TPatchLevelItems;
    procedure RegisterScript(ScriptItem: TScriptCollectionItem);

    function DoBeforeExecute(ScriptItem: TScriptCollectionItem): TScriptContinue;
    procedure DoAfterExecute(ScriptItem: TScriptCollectionItem);

    property PatchLevelItems: TPatchLevelItems read GetPatchLevelItems;
    property MileStone: string read FMileStone;
  protected
    // IUnknown
    function _AddRef: Integer; stdcall;
    function _Release: Integer; stdcall;

    // IAutoScriptRunner
    function TestExecuteScript(ScriptItem: TScriptCollectionItem; Options: TAutoScriptRunnerOptions = [srCheckMileStone, srCheckPatchLevel]): boolean;
    function ExecuteScript(ScriptItem: TScriptCollectionItem; Options: TAutoScriptRunnerOptions = [srCheckMileStone, srCheckPatchLevel]): boolean;

  public
    constructor Create(const AConnectionString: string); reintroduce; virtual;
    destructor Destroy; override;
  end;

implementation

{%CLASSGROUP 'Vcl.Controls.TControl'}

{$R *.dfm}

//------------------------------------------------------------------------------
//
//      TDataModuleAutoScriptRunner
//
//------------------------------------------------------------------------------
constructor TDataModuleAutoScriptRunner.Create(const AConnectionString: string);
begin
  inherited Create(nil);

end;

destructor TDataModuleAutoScriptRunner.Destroy;
begin
  FPatchLevelItems.Free;
  inherited;
end;

//------------------------------------------------------------------------------

function TDataModuleAutoScriptRunner._AddRef: Integer;
begin
  Result := AtomicIncrement(FRefCount);
end;

function TDataModuleAutoScriptRunner._Release: Integer;
begin
  Result := AtomicDecrement(FRefCount);
  if (Result = 0) then
    Free;
end;

//------------------------------------------------------------------------------

procedure TDataModuleAutoScriptRunner.DoAfterExecute(ScriptItem: TScriptCollectionItem);
begin
  if (Assigned(ScriptItem.OnAfterExecute)) then
    ScriptItem.OnAfterExecute(ScriptItem);
end;

function TDataModuleAutoScriptRunner.DoBeforeExecute(ScriptItem: TScriptCollectionItem): TScriptContinue;
begin
  Result := scContinue;

  if (Assigned(ScriptItem.OnBeforeExecute)) then
    ScriptItem.OnBeforeExecute(ScriptItem, Result);
end;

//------------------------------------------------------------------------------

function TDataModuleAutoScriptRunner.GetPatchLevelItems: TPatchLevelItems;
begin
  if (FPatchLevelItems = nil) then
  begin
    FPatchLevelItems := TDictionary<string, boolean>.Create;

    (*
    ** Get all PATCH_LEVEL records, add them to dictionary
    *)
    DoGetPatchLevelItems(FPatchLevelItems);

    (*
    ** Find last milestone
    *)
    for var Item in FPatchLevelItems do
      if (Item.Value) and (Item.Key > FMilestone) then
        FMilestone := Item.Key;
  end;

  Result := FPatchLevelItems;
end;

//------------------------------------------------------------------------------

procedure TDataModuleAutoScriptRunner.RegisterScript(ScriptItem: TScriptCollectionItem);
begin
  var Number := ScriptItem.Number.ToUpper;

  PatchLevelItems.Add(Number, ScriptItem.Milestone);

  if (ScriptItem.Milestone) and (Number > FMileStone) then
    FMileStone := Number;

  // Update database
  DoRegisterScript(ScriptItem);
end;

//------------------------------------------------------------------------------

function TDataModuleAutoScriptRunner.ExecuteScript(ScriptItem: TScriptCollectionItem; Options: TAutoScriptRunnerOptions): boolean;
begin
  ScriptItem.Prepare;

  var Continue := DoBeforeExecute(ScriptItem);

  if (Continue = scAbort) then
    Abort;

  if (Continue = scSkip) then
    Exit(False);

  // Wrap in a transaction unless one was started on an outer level
  var DoTransaction := (Continue <> scSkipDisable) and (not InTransaction);
  if (DoTransaction) then
    BeginTransaction;
  try

    if (ScriptItem.Script.Count = 0) then
      // Nothing to execute - but pretend we did anyway
      Continue := scSkipDisable;

    if (Continue = scContinue) then
    begin

      // Execute script
      if (not DoExecuteScript(ScriptItem)) then
        raise Exception.CreateFmt('Script execution failed: %s', [ScriptItem.Number]);

      Result := True;
    end else
      // Don't execute but record as executed
      Result := False;

    // Mark script as executed
    if (srCheckPatchLevel in Options) then
      RegisterScript(ScriptItem);

    // Commit
    if (DoTransaction) and (InTransaction) then
      EndTransaction(True);
  except
    // Rollback
    if (DoTransaction) and (InTransaction) then
      EndTransaction(False);
    raise;
  end;

  DoAfterExecute(ScriptItem);
end;

//------------------------------------------------------------------------------

function TDataModuleAutoScriptRunner.TestExecuteScript(ScriptItem: TScriptCollectionItem; Options: TAutoScriptRunnerOptions): boolean;
begin
  if (not (srCheckPatchLevel in Options)) then
    Exit(True);

  (*
  ** If script has already been registered in PATCH_LEVEL table, it has
  ** already been executed and should be ignored.
  *)
  var UpperNumber := ScriptItem.Number.ToUpper;
  if ((srCheckMileStone in Options) and (UpperNumber <= FMileStone)) then
    Exit(False);

  if (PatchLevelItems.ContainsKey(UpperNumber)) then
    Exit(False);

  // Check dependencies
  for var i := 0 to ScriptItem.Dependencies.Count-1 do
    if (ScriptItem.Dependencies[i].Kind = dkDisallow) then
    begin
      var Dependency := ScriptItem.Dependencies[i].Dependency;
      if (Dependency = nil) then
        continue;

      if (Dependency.Enabled) and (PatchLevelItems.ContainsKey(Dependency.Number.ToUpper)) then
        Exit(False);
    end;

  Result := True;
end;

//------------------------------------------------------------------------------
//------------------------------------------------------------------------------
//------------------------------------------------------------------------------

end.

