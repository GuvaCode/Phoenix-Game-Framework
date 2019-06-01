////////////////////////////////////////////////////////////////////////////////
//                                                                            //
//    Phoenix Game Framework                                                  //
//                                                                            //
//    http://www.phoenixlib.net                                               //
//                                                                            //
//    The contents of this file are used with permission, subject to          //
//    the Mozilla Public License Version 1.1 (the "License"); you may         //
//    not use this file except in compliance with the License. You may        //
//    obtain a copy of the License at                                         //
//    http://www.mozilla.org/MPL/MPL-1.1.html                                 //
//                                                                            //
//    Software distributed under the License is distributed on an             //
//    "AS IS" basis, WITHOUT WARRANTY OF ANY KIND, either express or          //
//    implied. See the License for the specific language governing            //
//    rights and limitations under the License.                               //
//                                                                            //
////////////////////////////////////////////////////////////////////////////////
unit phxDirect3D9_Effect;
//< Effect implementation for Direct3D9 renderers

interface

{$I ../phxConfig.inc}

uses
  SysUtils, Classes,

  Direct3D9, D3DX9,

  phxLogger,
  phxTypes,
  phxDevice,
  phxEffect,

  phxDirect3D9,
  phxDirect3D9_Shader;

type

// Effect implementation for Direct3D9
//---------------------------------------------------------------------------

{ TPHXDirect3D9_Effect }

TPHXDirect3D9_Effect = class(TPHXEffect)
  private
    FDirect3D9Device: IDirect3DDevice9;
  public
    constructor Create(ADirect3D9Device: IDirect3DDevice9);
    destructor Destroy; override;

    property Direct3D9Device: IDirect3DDevice9 read FDirect3D9Device;
  end;


implementation

{ TPHXDirect3D9_Effect }

constructor TPHXDirect3D9_Effect.Create(ADirect3D9Device: IDirect3DDevice9);
begin
  inherited Create();

  FDirect3D9Device:= ADirect3D9Device;
end;

destructor TPHXDirect3D9_Effect.Destroy;
begin
  inherited Destroy;
end;

end.
