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
unit phxDirect3D9_Canvas;
//< Canvas implementation for Direct3D9 renderers

interface

{$I ../phxConfig.inc}

uses
  SysUtils, Classes,

  Direct3D9, D3DX9, DXTypes,

  phxLogger,
  phxTypes,
  phxDevice,
  phxCanvas,

  phxDirect3D9,
  phxDirect3D9_Shader;

type

// Canvas implementation for Direct3D9
//---------------------------------------------------------------------------
TPHXDirect3D9_Canvas = class(TPHXCanvas)
  private
    FDirect3D9Device: IDirect3DDevice9;

    Direct3D9IndexBuffer   : IDirect3DIndexBuffer9;
    Direct3D9VertexBuffer  : IDirect3DVertexBuffer9;

    // Create the index and vertex buffers
    function CreateBuffers: Boolean;
    function CreateShaders: Boolean;

    // Destroy the index buffers
    procedure DestroyBuffers;
    procedure DestroyShaders;

    procedure FillIndexBuffer(const Indicies: PIndexList; const Count: Integer);
    procedure FillVertexBuffer(const Verticies: PVertexList; const Count: Integer);
  protected
    procedure FlushCanvas; override;
  public
    constructor Create(ADirect3D9Device: IDirect3DDevice9);
    destructor Destroy; override;

    property Direct3D9Device: IDirect3DDevice9 read FDirect3D9Device;

    // Enable or disable clipping
    procedure SetClipEnabled(const Enabled: Boolean); override;
    // Set the clipping rectangle
    procedure SetClipRect(const Rect: TRecti); override;
    // Set the clipping rectangle
    procedure SetClipRect(const Rect: TRectf); override;
  end;



implementation

Uses Windows;



//------------------------------------------------------------------------------
procedure TPHXDirect3D9_Canvas.FillVertexBuffer(const Verticies: PVertexList; const Count: Integer);
var Status   : HRESULT;
var pVertices: Pointer;
var Index    : Integer;
var Vertex   : PCustomVertexArray;
begin
  Assert(Count <= 16384);

  Status:= Direct3D9VertexBuffer.Lock(0, Count * SizeOf(TCustomVertex), pVertices, 0);

  if Failed(Status) then raise Exception.Create('Failed to lock the vertex buffer.');

  Vertex:= pVertices;
  for Index := 0 to Count - 1 do
  begin

    Vertex^[Index].Position:= TD3DVector  (Verticies^[Index].Position);
    Vertex^[Index].Normal  := TD3DVector  (Verticies^[Index].Normal);
    Vertex^[Index].TexCoord:= TD3DXVector2(Verticies^[Index].TexCoord);

    Vertex^[Index].Color :=D3DCOLOR_COLORVALUE(Verticies^[Index].Color.Red, Verticies^[Index].Color.Green,Verticies^[Index].Color.Blue, Verticies^[Index].Color.Alpha);
  end;

//  CopyMemory(pVertices, @vertices, SizeOf(vertices));
  Direct3D9VertexBuffer.Unlock;
end;

//------------------------------------------------------------------------------
procedure TPHXDirect3D9_Canvas.FillIndexBuffer(const Indicies: PIndexList; const Count: Integer);
var I        : Integer;
var Status   : HRESULT;
var pIndicies: Pointer;
var Index    : PWord;
begin
  Status:= Direct3D9IndexBuffer.Lock(0, Count * SizeOf(Cardinal), pIndicies, D3DLOCK_DISCARD);

  if Failed(Status) then raise Exception.Create('Failed to lock the index buffer.');

  Index:= pIndicies;
  for I := 0 to Count - 1 do
  begin
    Index^:= Indicies[I];

    Inc(Index);
  end;
  Direct3D9IndexBuffer.Unlock;
end;

// TPHXDirect3D9_Canvas
//==============================================================================
constructor TPHXDirect3D9_Canvas.Create(ADirect3D9Device: IDirect3DDevice9);
begin
  inherited Create();

  FDirect3D9Device:= ADirect3D9Device;

  // Create the vertex and index buffers
  if not CreateBuffers then
  begin
    raise Exception.Create('Failed to create buffers');
  end;

  if not CreateShaders then
  begin
    raise Exception.Create('Failed to create shaders');
  end;
end;

//---------------------------------------------------------------------------
destructor TPHXDirect3D9_Canvas.Destroy;
begin
  DestroyBuffers;
  DestroyShaders;

  FDirect3D9Device:= nil;

  inherited Destroy;
end;

//---------------------------------------------------------------------------
function TPHXDirect3D9_Canvas.CreateBuffers: Boolean;
var Status: HRESULT;
begin
  Status:= Direct3D9Device.CreateIndexBuffer(CanvasCapacityIndices, D3DUSAGE_WRITEONLY, D3DFMT_INDEX32, D3DPOOL_DEFAULT, Direct3D9IndexBuffer, nil);

  if Failed(Status) then
  begin
    Result:= False;
    Exit;
  end;

  Status:= Direct3D9Device.CreateVertexBuffer(CanvasCapacityVertices * SizeOf(TCustomVertex), 0, D3DFVF_CUSTOMVERTEX, D3DPOOL_DEFAULT, Direct3D9VertexBuffer, nil);

  if Failed(Status) then
  begin
    Result:= False;
    Exit;
  end;

  Result:= True;
end;

//---------------------------------------------------------------------------
function TPHXDirect3D9_Canvas.CreateShaders: Boolean;
begin

  Result:= True;
end;

//---------------------------------------------------------------------------
procedure TPHXDirect3D9_Canvas.DestroyBuffers;
begin
 Direct3D9VertexBuffer:= nil;
 Direct3D9IndexBuffer:= nil;
end;

//---------------------------------------------------------------------------
procedure TPHXDirect3D9_Canvas.DestroyShaders;
begin

end;


//---------------------------------------------------------------------------
procedure TPHXDirect3D9_Canvas.FlushCanvas;
var D3DPrimitiveType: TD3DPrimitiveType;
begin
  FillVertexBuffer(Vertices.List, Vertices.Count);

  FillIndexBuffer(Indices.List, Indices.Count);

  Direct3D9Device.SetIndices(Direct3D9IndexBuffer);
  Direct3D9Device.SetStreamSource(0, Direct3D9VertexBuffer, 0, SizeOf(TCustomVertex));
  Direct3D9Device.SetFVF(D3DFVF_CUSTOMVERTEX);

  D3DPrimitiveType:= Direct3D9PrimitiveType[Primitive];


  Direct3D9Device.DrawIndexedPrimitive(D3DPrimitiveType, 0, 0, Vertices.Count, 0, Indices.Count div 3);
end;





procedure TPHXDirect3D9_Canvas.SetClipEnabled(const Enabled: Boolean);
begin

end;

procedure TPHXDirect3D9_Canvas.SetClipRect(const Rect: TRecti);
begin

end;

procedure TPHXDirect3D9_Canvas.SetClipRect(const Rect: TRectf);
begin

end;







end.
