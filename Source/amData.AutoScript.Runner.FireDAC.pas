unit amData.AutoScript.Runner.FireDAC;

(*
  Version: MPL 1.1/GPL 2.0/LGPL 2.1

  This Source Code Form is subject to the terms of the Mozilla Public
  License, v. 2.0. If a copy of the MPL was not distributed with this
  file, You can obtain one at http://mozilla.org/MPL/2.0/.

  Software distributed under the License is distributed on an "AS IS" basis,
  WITHOUT WARRANTY OF ANY KIND, either express or implied. See the License
  for the specific language governing rights and limitations under the License.

  The Original Code is amData.AutoScript.Runner.FireDAC

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
  Generics.collections,
  StrUtils,
  System.SysUtils, System.Classes,
  Data.DB,

  FireDAC.Stan.Intf, FireDAC.Stan.Option, FireDAC.Stan.Error, FireDAC.Stan.Def,
  FireDAC.Stan.Pool, FireDAC.Stan.Async, FireDAC.Stan.Param, FireDAC.Stan.Util,
  FireDAC.DatS, FireDAC.DApt.Intf, FireDAC.DApt,
  FireDAC.Phys.Intf, FireDAC.Phys,
  FireDAC.UI.Intf,
  FireDAC.VCLUI.Wait, // Not used but the IDE keeps adding it
  FireDAC.Comp.DataSet, FireDAC.Comp.Client, FireDAC.Comp.Script, FireDAC.Comp.ScriptCommands,

  amData.AutoScript.Collection,
  amData.AutoScript.Runner,
  amData.AutoScript.Runner.API;

//------------------------------------------------------------------------------
//
//      TDataModuleAutoScriptRunnerFireDAC
//
//------------------------------------------------------------------------------
// Script runner for FireDAC
//------------------------------------------------------------------------------
type
  TDataModule = TDataModuleAutoScriptRunner;

  TDataModuleAutoScriptRunnerFireDAC = class(TDataModule)
    FDScript: TFDScript;
    FDConnection: TFDConnection;
    QueryPatchLevel: TFDQuery;
    QueryAddPatchLevel: TFDQuery;
    FDTransaction: TFDTransaction;
    procedure FDScriptProgress(Sender: TObject);
    procedure FDScriptError(ASender, AInitiator: TObject; var AException: Exception);
  strict protected
    procedure DoGetPatchLevelItems(Items: TPatchLevelItems); override;
    procedure DoRegisterScript(ScriptItem: TScriptCollectionItem); override;
    function DoExecuteScript(ScriptItem: TScriptCollectionItem): boolean; override;

    function InTransaction: boolean; override;
    procedure BeginTransaction; override;
    procedure EndTransaction(Commit: boolean); override;
  public
    constructor Create(const AConnectionString: string); override;
  end;

//------------------------------------------------------------------------------
//------------------------------------------------------------------------------
//------------------------------------------------------------------------------

implementation

{%CLASSGROUP 'Vcl.Controls.TControl'}

{$R *.dfm}

uses
  System.Types {,
  Horizon.Core.Trace,
  Horizon.Core.ExceptionHandler.API};

//------------------------------------------------------------------------------
//
//      TDataModuleAutoScriptRunnerFireDAC
//
//------------------------------------------------------------------------------
constructor TDataModuleAutoScriptRunnerFireDAC.Create(const AConnectionString: string);
begin
  inherited Create(AConnectionString);

end;

//------------------------------------------------------------------------------

function TDataModuleAutoScriptRunnerFireDAC.DoExecuteScript(ScriptItem: TScriptCollectionItem): boolean;
begin
  if (not FDConnection.Connected) then
    FDConnection.Open;

  FDScript.Macros.Clear;

  // TODO : For now we do not support macros
  var Params: TStrings := nil;

  if (Params <> nil) then
  begin
    for var i := 0 to Params.Count-1 do
    begin
      var ParamName := Params.KeyNames[i];
      var Value := Params.Values[ParamName];

      var DataType: TFDMacroDataType;
      var ParamValue: Variant;

      var n: integer;
      var f: Double;

      if (TryStrToInt(Value, n)) then
      begin
        ParamValue := n;
        DataType := mdInteger;
      end else
      if (TryStrToFloat(Value, f)) then
      begin
        ParamValue := f;
        DataType := mdFloat;
      end else
      begin
        if (CharInSet(Value[1], ['"', ''''])) then
          ParamValue := Value.DeQuotedString(Value[i])
        else
          ParamValue := Value;
        DataType := mdString;
      end;

      var Macro := FDScript.Macros.Add;
      Macro.Name := ParamName;
      Macro.Value := ParamValue;
      Macro.DataType := DataType;

      FDScript.Params.Add(ParamName, ParamValue, ptInput);
    end;
  end;

  FDScript.SQLScripts.Clear;

  var Script := FDScript.SQLScripts.Add;
  Script.Name := ScriptItem.Number;
  Script.SQL.Assign(ScriptItem.Script);

  FDScript.ValidateAll;

  // Execute script, command by command
  Result := True;
  FDScript.Position := Point(0, 0); // Assigning new script does not reset position so we must do it manually.
  FDScript.ScriptOptions.CommandSeparator := ';'; // CommandSeparator isn't reset between executions
  while (Result) and (not FDScript.EOF) do
  begin
//    BugreportInfo.WriteString('AutoScript', ScriptItem.Number, Format('Executing [%d:%d]', [FDScript.Position.Y, FDScript.Position.X]));
    Result := FDScript.ExecuteStep;
  end;
end;

//------------------------------------------------------------------------------

procedure TDataModuleAutoScriptRunnerFireDAC.DoGetPatchLevelItems(Items: TPatchLevelItems);
begin
  if (not FDConnection.Connected) then
    FDConnection.Open;

  QueryPatchLevel.Open;
  try
    var FieldNumber := QueryPatchLevel.FieldByName('NUMBER');
    var FieldMilestone := QueryPatchLevel.FieldByName('MILESTONE');

    while (not QueryPatchLevel.EOF) do
    begin
      var Number := FieldNumber.AsString.ToUpper;
      var Milestone := FieldMilestone.AsBoolean;

      Items.AddOrSetValue(Number, Milestone); // Duplicates are an error but we ignore it

      QueryPatchLevel.Next;
    end;

  finally
    QueryPatchLevel.Close;
  end;
end;

//------------------------------------------------------------------------------

procedure TDataModuleAutoScriptRunnerFireDAC.DoRegisterScript(ScriptItem: TScriptCollectionItem);
begin
  if (not FDConnection.Connected) then
    FDConnection.Open;

  QueryAddPatchLevel.ParamByName('PARAMNUMBER').AsString := ScriptItem.Number;
  QueryAddPatchLevel.ParamByName('PARAMDESCRIPTION').AsString := ScriptItem.Description;
  QueryAddPatchLevel.ParamByName('PARAMMILESTONE').AsBoolean := ScriptItem.Milestone;

  QueryAddPatchLevel.ExecSQL;
end;

//------------------------------------------------------------------------------

function TDataModuleAutoScriptRunnerFireDAC.InTransaction: boolean;
begin
  Result := FDConnection.InTransaction;
end;

procedure TDataModuleAutoScriptRunnerFireDAC.BeginTransaction;
begin
  if (not FDConnection.InTransaction) then
    FDConnection.StartTransaction;
end;

procedure TDataModuleAutoScriptRunnerFireDAC.EndTransaction(Commit: boolean);
begin
  if (FDConnection.InTransaction) then
  begin
    if (Commit) then
      FDConnection.Commit
    else
      FDConnection.Rollback;
  end;
end;

//------------------------------------------------------------------------------

procedure TDataModuleAutoScriptRunnerFireDAC.FDScriptError(ASender, AInitiator: TObject; var AException: Exception);
begin
//  BugreportInfo.WriteString('AutoScript', '', Format('Error: %s', [AException.Message.Replace(#13, ' ')]));
end;

procedure TDataModuleAutoScriptRunnerFireDAC.FDScriptProgress(Sender: TObject);
begin
//  FreezeDetection.IndicateApplicationNotFrozen;
end;

//------------------------------------------------------------------------------

end.

