unit amData.AutoScript.API;

(*
  Version: MPL 1.1/GPL 2.0/LGPL 2.1

  This Source Code Form is subject to the terms of the Mozilla Public
  License, v. 2.0. If a copy of the MPL was not distributed with this
  file, You can obtain one at http://mozilla.org/MPL/2.0/.

  Software distributed under the License is distributed on an "AS IS" basis,
  WITHOUT WARRANTY OF ANY KIND, either express or implied. See the License
  for the specific language governing rights and limitations under the License.

  The Original Code is amData.AutoScript.API

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
  amData.AutoScript.Collection,
  amData.AutoScript.Runner.API;

type
  IAutoScriptModule = interface
    ['{34ECAAF7-DD61-4592-AEC1-274EDBB6E542}']
    procedure Execute(Bootstrap: boolean = False; Mask: TScriptMask = []); overload;
    procedure Execute(const ScriptRunner: IAutoScriptRunner; Bootstrap: boolean; Mask: TScriptMask = []); overload;

    function GetAutoScriptFolder: string;
    procedure SetAutoScriptFolder(const Value: string);

    property AutoScriptFolder: string read GetAutoScriptFolder write SetAutoScriptFolder;
  end;

implementation

end.

