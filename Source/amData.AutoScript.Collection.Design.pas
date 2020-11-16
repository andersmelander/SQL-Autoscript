unit amData.AutoScript.Collection.Design;

(*
  Version: MPL 1.1/GPL 2.0/LGPL 2.1

  This Source Code Form is subject to the terms of the Mozilla Public
  License, v. 2.0. If a copy of the MPL was not distributed with this
  file, You can obtain one at http://mozilla.org/MPL/2.0/.

  Software distributed under the License is distributed on an "AS IS" basis,
  WITHOUT WARRANTY OF ANY KIND, either express or implied. See the License
  for the specific language governing rights and limitations under the License.

  The Original Code is amData.ScriptCollection.Design

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

procedure Register;

//------------------------------------------------------------------------------
//------------------------------------------------------------------------------
//------------------------------------------------------------------------------

implementation

uses
  Classes,
//  ColnEdit,
  SysUtils,
  Windows,
  VCL.Dialogs,
  VCL.Controls,

  DesignIntf,
  DesignEditors,

  amData.AutoScript.Collection;

const
  sDesignGroup = 'Melander';

//------------------------------------------------------------------------------
//
//      TScriptCollectionEditor
//
//------------------------------------------------------------------------------
type
  TScriptCollectionEditor = class(TComponentEditor)
  protected
    procedure DoImport(ScriptContainer: TScriptContainer);
  public
    procedure ExecuteVerb(Index: Integer); override;
    function GetVerb(Index: Integer): string; override;
    function GetVerbCount: Integer; override;
  end;

procedure TScriptCollectionEditor.DoImport(ScriptContainer: TScriptContainer);
var
  i, j: integer;
  s: string;
  Number, Description: string;
  Script: TScriptCollectionItem;
  Res: Word;
  AllChoice, AllSet: boolean;
begin
  with TOpenDialog.Create(nil) do
    try
      DefaultExt := 'sql';
      Filter := 'SQL scripts (*.sql)|*.sql|All files (*.*)|*.*';
      FilterIndex := 1;
      Options := [ofAllowMultiSelect, ofHideReadOnly, ofPathMustExist, ofFileMustExist, ofEnableSizing];
      Title := 'Import SQL scripts';
      if (Execute) then
      begin
        ScriptContainer.Scripts.BeginUpdate;
        try
          AllSet := False;
          AllChoice := False;
          for i := 0 to Files.Count-1 do
          begin
            s := ChangeFileExt(ExtractFileName(Files[i]), '');
            if (s = '') then
              continue;
            // Extract script ID
            if (Pos(' ', s) > 0) then
            begin
              Number := System.Copy(s, 1, Pos(' ', s)-1);
              Description := System.Copy(s, Pos(' ', s)+1, Length(s));
            end else
            if (s[1] in ['0'..'9']) then
            begin
              j := 1;
              while (s[j] in ['0'..'9']) do
                inc(j);
              Number := System.Copy(s, 1, j-1);
              Description := System.Copy(s, j, Length(s));
            end else
            begin
              Number := s;
              Description := s;
            end;

            j := ScriptContainer.Scripts.IndexOf(Number);
            if (j <> -1) then
            begin
              Script := ScriptContainer.Scripts[j];
              if (not AllSet) then
              begin
                Res := MessageDlg(Format('The collection already contains a script with the number "%s":'+#13+
                  'Old: %s'+#13+
                  'New: %s'+#13+#13+
                  'Do you wish the replace the existing script?', [Number, Script.Description, Description]),
                  mtConfirmation, [mbYes, mbNo, mbAbort, mbNoToAll, mbYesToAll], 0);
                case Res of
                  mrNo:
                    Continue;
                  mrNoToAll:
                    begin
                      AllChoice := False;
                      AllSet := True;
                      continue;
                    end;
                  mrYesToAll:
                    begin
                      AllChoice := True;
                      AllSet := True;
                    end;
                  mrAbort:
                    break;
                end;
              end else
                if (not AllChoice) then
                  continue;
            end else
              Script := TScriptCollectionItem.Create(ScriptContainer.Scripts);

            Script.Number := Number;
            Script.Description := Description;
            Script.Script.LoadFromFile(Files[i]);
          end;
        finally
          ScriptContainer.Scripts.EndUpdate;
        end;
        Designer.Modified;
      end;
    finally
      Free;
    end;
end;

procedure TScriptCollectionEditor.ExecuteVerb(Index: Integer);
var
  i: Integer;
begin
  i := inherited GetVerbCount;
  if (Index < i) then
    inherited
  else
  begin
    case Index - i of
      0: DoImport(TScriptContainer(Component));
    end;
  end;
end;

function TScriptCollectionEditor.GetVerb(Index: Integer): string;
var
  i: Integer;
begin
  i := inherited GetVerbCount;
  if (Index < i) then
    Result := inherited GetVerb(Index)
  else
    case Index - i of
      0: Result := 'Import SQL scripts...';
    end;
end;

function TScriptCollectionEditor.GetVerbCount: Integer;
begin
  Result := inherited GetVerbCount+1;
end;

//------------------------------------------------------------------------------
//
//      TScriptCollectionItemProperty
//
//------------------------------------------------------------------------------
type
  TIterateProc = function(Item: TPersistent;
    const Value: string; GetStrProc: TGetStrProc;
    const NewValue: string): boolean of object;

  TScriptCollectionItemProperty = class(TClassProperty)
  private
    function DoGetValues(Item: TPersistent;
      const Value: string; GetStrProc: TGetStrProc; const NewValue: string): boolean;
    function DoSetValue(Item: TPersistent;
      const Value: string; GetStrProc: TGetStrProc; const NewValue: string): boolean;
  protected
    function Iterate(Proc: TIterateProc; GetStrProc: TGetStrProc;
      const NewValue: string): boolean;
  public
    function GetAttributes: TPropertyAttributes; override;
    function GetEditLimit: Integer; override;
    function GetValue: string; override;
    procedure SetValue(const Value: string); override;
    procedure GetValues(Proc: TGetStrProc); override;
  end;

function TScriptCollectionItemProperty.DoGetValues(Item: TPersistent;
  const Value: string; GetStrProc: TGetStrProc;
  const NewValue: string): boolean;
begin
  ASSERT(Assigned(GetStrProc));
  GetStrProc(Value);
  Result := True;
end;

function TScriptCollectionItemProperty.DoSetValue(Item: TPersistent;
  const Value: string; GetStrProc: TGetStrProc;
  const NewValue: string): boolean;
begin
  Result := (AnsiCompareText(Value, NewValue) <> 0);
  if (not Result) then
    SetOrdValue(longInt(Item));
end;

function TScriptCollectionItemProperty.GetAttributes: TPropertyAttributes;
begin
  Result := [paValueList, paSortList];
end;

function TScriptCollectionItemProperty.GetEditLimit: Integer;
begin
  Result := 127;
end;

function TScriptCollectionItemProperty.GetValue: string;
begin
  if (GetOrdValue = 0) then
    Result := '(none)'
  else
    Result := TCollectionItem(GetOrdValue).DisplayName;
end;

type
  TCollectionCracker = class(TCollection);

procedure TScriptCollectionItemProperty.GetValues(Proc: TGetStrProc);
begin
  Iterate(DoGetValues, Proc, '');
end;

function TScriptCollectionItemProperty.Iterate(Proc: TIterateProc;
  GetStrProc: TGetStrProc; const NewValue: string): boolean;
begin
  Result := True;
  var Item := TCollectionItem(GetComponent(0));
  var OwnerItem := TCollectionCracker(Item.Collection).GetOwner as TCollectionItem;

  for var i := 0 to OwnerItem.Collection.Count-1 do
  begin
    if (OwnerItem.Collection.Items[i] = OwnerItem) then
      continue; // Don't include self

    Result := Proc(OwnerItem.Collection.Items[i], OwnerItem.Collection.Items[i].DisplayName, GetStrProc, NewValue);

    if (not Result) then
      break;
  end;
end;

procedure TScriptCollectionItemProperty.SetValue(const Value: string);
begin
  if (Iterate(DoSetValue, nil, Value)) then
    SetOrdValue(Longint(nil));
end;

(*
//------------------------------------------------------------------------------
//
//      TStringsModuleCreator
//
//------------------------------------------------------------------------------
type
  TStringsModuleCreator = class(TInterfacedObject, IOTACreator, IOTAModuleCreator)
  private
    FFileName: string;
    FStream: TStringStream;
    FAge: TDateTime;
  public
    constructor Create(const FileName: string; Stream: TStringStream; Age: TDateTime);
    destructor Destroy; override;
    { IOTACreator }
    function GetCreatorType: string;
    function GetExisting: Boolean;
    function GetFileSystem: string;
    function GetOwner: IOTAModule;
    function GetUnnamed: Boolean;
    { IOTAModuleCreator }
    function GetAncestorName: string;
    function GetImplFileName: string;
    function GetIntfFileName: string;
    function GetFormName: string;
    function GetMainForm: Boolean;
    function GetShowForm: Boolean;
    function GetShowSource: Boolean;
    function NewFormFile(const FormIdent, AncestorIdent: string): IOTAFile;
    function NewImplSource(const ModuleIdent, FormIdent, AncestorIdent: string): IOTAFile;
    function NewIntfSource(const ModuleIdent, FormIdent, AncestorIdent: string): IOTAFile;
    procedure FormCreated(const FormEditor: IOTAFormEditor);
  end;

  TOTAFile = class(TInterfacedObject, IOTAFile)
  private
    FSource: string;
    FAge: TDateTime;
  public
    constructor Create(const ASource: string; AAge: TDateTime);
    { IOTAFile }
    function GetSource: string;
    function GetAge: TDateTime;
  end;

{ TOTAFile }

constructor TOTAFile.Create(const ASource: string; AAge: TDateTime);
begin
  inherited Create;
  FSource := ASource;
  FAge := AAge;
end;

function TOTAFile.GetAge: TDateTime;
begin
  Result := FAge;
end;

function TOTAFile.GetSource: string;
begin
  Result := FSource;
end;

{ TStringsModuleCreator }

constructor TStringsModuleCreator.Create(const FileName: string; Stream: TStringStream;
  Age: TDateTime);
begin
  inherited Create;
  FFileName := FileName;
  FStream := Stream;
  FAge := Age;
end;

destructor TStringsModuleCreator.Destroy;
begin
  FStream.Free;
  inherited;
end;

procedure TStringsModuleCreator.FormCreated(const FormEditor: IOTAFormEditor);
begin
  { Nothing to do }
end;

function TStringsModuleCreator.GetAncestorName: string;
begin
  Result := '';
end;

function TStringsModuleCreator.GetCreatorType: string;
begin
  Result := sText;
end;

function TStringsModuleCreator.GetExisting: Boolean;
begin
  Result := False;
end;

function TStringsModuleCreator.GetFileSystem: string;
begin
  Result := sTStringsFileSystem;
end;

function TStringsModuleCreator.GetFormName: string;
begin
  Result := '';
end;

function TStringsModuleCreator.GetImplFileName: string;
begin
  Result := FFileName;
end;

function TStringsModuleCreator.GetIntfFileName: string;
begin
  Result := '';
end;

function TStringsModuleCreator.GetMainForm: Boolean;
begin
  Result := False;
end;

function TStringsModuleCreator.GetOwner: IOTAModule;
begin
  Result := nil;
end;

function TStringsModuleCreator.GetShowForm: Boolean;
begin
  Result := False;
end;

function TStringsModuleCreator.GetShowSource: Boolean;
begin
  Result := True;
end;

function TStringsModuleCreator.GetUnnamed: Boolean;
begin
  Result := False;
end;

function TStringsModuleCreator.NewFormFile(const FormIdent,
  AncestorIdent: string): IOTAFile;
begin
  Result := nil;
end;

function TStringsModuleCreator.NewImplSource(const ModuleIdent, FormIdent,
  AncestorIdent: string): IOTAFile;
begin
  Result := TOTAFile.Create(FStream.DataString, FAge);
end;

function TStringsModuleCreator.NewIntfSource(const ModuleIdent, FormIdent,
  AncestorIdent: string): IOTAFile;
begin
  Result := nil;
end;

//------------------------------------------------------------------------------
//
//      TSQLProperty
//
//------------------------------------------------------------------------------
// Enables editing the SQL in the code editor
//------------------------------------------------------------------------------
type
  TSQLProperty = class(TClassProperty)
  public
    function GetAttributes: TPropertyAttributes; override;
    procedure Edit; override;
  end;

procedure TSQLProperty.Edit;
var
  Ident: string;
  Component: TComponent;
  Stream: TStringStream;
  Age: TDateTime;
  ModuleServices: IOTAModuleServices;
  Module: IOTAModule;
  Editor: IOTAEditor;
begin
  Component := TComponent(GetComponent(0));
  ShowMessage(Component.ClassName);
  ModuleServices := BorlandIDEServices as IOTAModuleServices;
  if (TObject(Component) is TComponent) and
    (Component.Owner = Self.Designer.GetRoot) then
  begin
    Ident := Self.Designer.GetRoot.Name + DotSep + Component.Name + DotSep + GetName;
    Module := ModuleServices.FindModule(Ident);
  end else
    Module := nil;

  if (Module <> nil) and (Module.GetModuleFileCount > 0) then
    Module.GetModuleFileEditor(0).Show
  else
  begin
    Stream := TStringStream.Create('');
    TStrings(GetOrdValue).SaveToStream(Stream);
    Stream.Position := 0;
    Age := Now;
    Module := ModuleServices.CreateModule(TStringsModuleCreator.Create(Ident, Stream, Age));
    if Module <> nil then
    begin
      Module.Save(False, True);
      with StringsFileSystem.GetTStringsProperty(Ident, Component, GetName) do
        DiskAge := DateTimeToFileDate(Age);
      Editor := Module.GetModuleFileEditor(0);
      Editor.Show;
    end;
  end;
end;

function TSQLProperty.GetAttributes: TPropertyAttributes;
begin
  Result := inherited GetAttributes + [paDialog] - [paSubProperties];
end;

//------------------------------------------------------------------------------
//
//      TSQLComponentEditor
//
//------------------------------------------------------------------------------
type
  TSQLComponentEditor = class(TComponentEditor)
  private
    FEditor: TPropertyEditor;
    FContinue: Boolean;
  protected
    procedure CheckEdit(PropertyEditor: TPropertyEditor);
  public
    procedure Edit; override;
  end;

procedure TSQLComponentEditor.CheckEdit(PropertyEditor: TPropertyEditor);
begin
  if (FEditor = nil) and (PropertyEditor is TSQLProperty) then
    FEditor := PropertyEditor
  else
    PropertyEditor.Free;
end;

procedure TSQLComponentEditor.Edit;
var
  Components: TDesignerSelectionList;
begin
  Components := TDesignerSelectionList.Create;
  try
    Components.Add(Component);
    FEditor := nil;
    try
      GetComponentProperties(Components, [tkClass], Designer, CheckEdit);
      if FEditor <> nil then
        FEditor.Edit;
    finally
      FEditor.Free;
    end;
  finally
    Components.Free;
  end;
end;
*)

//------------------------------------------------------------------------------
//
//      Register
//
//------------------------------------------------------------------------------
procedure Register;
begin
  RegisterComponents(sDesignGroup, [TScriptContainer]);

  RegisterComponentEditor(TScriptContainer, TScriptCollectionEditor);
  RegisterPropertyEditor(TypeInfo(TScriptCollectionItem), nil, '', TScriptCollectionItemProperty);
//  RegisterPropertyEditor(TypeInfo(TStrings), TScriptCollectionItem, 'SQL', TSQLProperty);
end;

end.
