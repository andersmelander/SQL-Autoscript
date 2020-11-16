unit amData.AutoScript;

(*
  Version: MPL 1.1/GPL 2.0/LGPL 2.1

  This Source Code Form is subject to the terms of the Mozilla Public
  License, v. 2.0. If a copy of the MPL was not distributed with this
  file, You can obtain one at http://mozilla.org/MPL/2.0/.

  Software distributed under the License is distributed on an "AS IS" basis,
  WITHOUT WARRANTY OF ANY KIND, either express or implied. See the License
  for the specific language governing rights and limitations under the License.

  The Original Code is amData.AutoScript

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
  System.SysUtils, System.Classes,
  amData.AutoScript.API,
  amData.AutoScript.Runner.API,
  amData.AutoScript.Collection;

//------------------------------------------------------------------------------
//
//      TDataModuleAutoScript
//
//------------------------------------------------------------------------------
// Base class for script container data module.
// Manages execution of script collections.
//------------------------------------------------------------------------------
// Derived classes should drop some TScriptContainer on the data module and
// override the Execute method to call ExecuteScripts on those script
// collections.
//------------------------------------------------------------------------------
type
  TDataModuleAutoScript = class abstract(TDataModule, IUnknown, IAutoScriptModule)
  strict private
    FRefCount: integer;
    FAutoScriptFolder: string;
  strict protected

    function CreateScriptRunner: IAutoScriptRunner; virtual; abstract;

    procedure MergeExternalScripts(Scripts: TScriptCollection; const Path: string);

    procedure ExecuteScript(const ScriptRunner: IAutoScriptRunner; Script: TScriptCollectionItem; Options: TAutoScriptRunnerOptions);
    procedure ExecuteScripts(const ScriptRunner: IAutoScriptRunner; Scripts: TScriptCollection; Options: TAutoScriptRunnerOptions);
  protected
    // IUnknown
    function _AddRef: Integer; stdcall;
    function _Release: Integer; stdcall;

    // IAutoScriptModule
    procedure Execute(Bootstrap: boolean = False); overload; virtual;
    procedure Execute(const ScriptRunner: IAutoScriptRunner; Bootstrap: boolean); overload; virtual; abstract;
    function GetAutoScriptFolder: string;
    procedure SetAutoScriptFolder(const Value: string);
    property AutoScriptFolder: string read GetAutoScriptFolder write SetAutoScriptFolder;
  public
    constructor Create; reintroduce; virtual;
  end;

implementation

{%CLASSGROUP 'Vcl.Controls.TControl'}

{$R *.dfm}

uses
  IOUtils;

//------------------------------------------------------------------------------
//
//      TDataModuleAutoScript
//
//------------------------------------------------------------------------------
constructor TDataModuleAutoScript.Create;
begin
  inherited Create(nil);
end;

//------------------------------------------------------------------------------

function TDataModuleAutoScript._AddRef: Integer;
begin
  Result := AtomicIncrement(FRefCount);
end;

function TDataModuleAutoScript._Release: Integer;
begin
  Result := AtomicDecrement(FRefCount);
  if (Result = 0) then
    Free;
end;

//------------------------------------------------------------------------------

function TDataModuleAutoScript.GetAutoScriptFolder: string;
begin
  Result := FAutoScriptFolder;
end;

procedure TDataModuleAutoScript.SetAutoScriptFolder(const Value: string);
begin
  FAutoScriptFolder := Value;
end;

//------------------------------------------------------------------------------

procedure TDataModuleAutoScript.Execute(Bootstrap: boolean);
begin
  var ScriptRunner := CreateScriptRunner;
  try

    Execute(ScriptRunner, Bootstrap);

  finally
    ScriptRunner := nil;
  end;
end;

//------------------------------------------------------------------------------

procedure TDataModuleAutoScript.ExecuteScript(const ScriptRunner: IAutoScriptRunner; Script: TScriptCollectionItem; Options: TAutoScriptRunnerOptions);
begin
  if (not Script.Enabled) then
    Exit;

  // Mark as processed to avoid endless loop via dependencies
  Script.Enabled := False;

  // Check dependencies
  for var i := 0 to Script.Dependencies.Count-1 do
  begin
    if (Script.Dependencies[i].Dependency = nil) then
      continue;

    if (Script.Dependencies[i].Kind = dkRequire) and (Script.Dependencies[i].Dependency.Enabled) then
      // Recurse to execute dependency
      ExecuteScript(ScriptRunner, Script.Dependencies[i].Dependency, Options);
  end;

  if (not ScriptRunner.TestExecuteScript(Script, Options)) then
    Exit;

  try

    ScriptRunner.ExecuteScript(Script, Options);

    if (Script.Source = ssFile) then
    begin
      // Rename the SQL script once it has been processed without
      // errors.
      var Filename := TPath.ChangeExtension(Script.FileName, '.old');

      if (TFile.Exists(Filename)) then
        TFile.Delete(Filename);

      TFile.Move(Script.FileName, Filename);
    end;

  except
    on E: Exception do
    begin
      var Filename: string;

      if (Script.Source = ssFile) then
        Filename := Script.FileName
      else
        Filename := TPath.Combine(FAutoScriptFolder, Script.Number);

      Filename := TPath.ChangeExtension(FileName, '.error');

      var List := TStringList.Create;
      try
        List.Text := E.Message;
        List.SaveToFile(Filename);
      finally
        List.Free;
      end;

      raise;
    end;
  end;

end;

//------------------------------------------------------------------------------

procedure TDataModuleAutoScript.ExecuteScripts(const ScriptRunner: IAutoScriptRunner; Scripts: TScriptCollection; Options: TAutoScriptRunnerOptions);
begin
  // Make sure we only run from the last milestone
  var DisableOlder: boolean := False;
  var FirstItem := 0;
  for var i := Scripts.Count-1 downto 0 do
  begin
    if (DisableOlder) then
      // Disable all scripts older than last milestone
      Scripts[i].Enabled := False
    else
    if (Scripts[i].Enabled) then
    begin
      FirstItem := i;
      DisableOlder := Scripts[i].Milestone;
    end;
  end;

  for var i := FirstItem to Scripts.Count-1 do
  begin
    var Script := Scripts[i];

    ExecuteScript(ScriptRunner, Script, Options);
  end;
end;

//------------------------------------------------------------------------------

procedure TDataModuleAutoScript.MergeExternalScripts(Scripts: TScriptCollection; const Path: string);
var
  Number, Description: string;
begin
  if (not TDirectory.Exists(Path)) then
    Exit;

  // Find script files based on wildcard.
  for var Filename in TDirectory.GetFiles(Path, '*.sql') do
  begin

    begin
      var ErrorFilename := TPath.ChangeExtension(Filename, '.error');
      // Delete old error logs.
      if (TFile.Exists(ErrorFilename)) then
        TFile.Delete(ErrorFilename);
    end;

    // Get the script ID and description from the filename
    begin
      var s := TPath.ChangeExtension(TPath.GetFileName(Filename), '');
      var n := Pos(' ', s);
      if (n > 0) then
      begin
        Number := Copy(s, 1, n-1);
        Description := Copy(s, n+1, MaxInt);
      end else
      begin
        Number := s;
        Description := s;
      end;
    end;

    // If there's already a script with the same number, disable the existing
    // script, so the new script can override it.
    begin
      var n := Scripts.IndexOf(Number);
      if (n <> -1) then
        Scripts.Delete(n);
    end;

    Scripts.BeginUpdate;
    try
      var Item := Scripts.Add as TScriptCollectionItem;

      Item.Number := Number;
      Item.Description := Description;

      Item.LoadFromFile(Filename);
    finally
      Scripts.EndUpdate;
    end;

  end;
end;

//------------------------------------------------------------------------------

end.

