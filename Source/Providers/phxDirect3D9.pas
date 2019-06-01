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
unit phxDirect3D9;
//< DirectX9 support

interface

{$I ../phxConfig.inc}

uses
  SysUtils, Classes,

  phxLogger,
  phxTypes,
  phxClasses,
  phxMath,
  phxEvents,
  phxDevice,
  phxGraphics,

  phxCanvas,
  phxEffect,
  phxTexture,

  Windows,

  Direct3D9,
  D3DX9
  ;

type

ENotImplementedException = class(Exception);

PRGBRec = ^TRGBRec;
TRGBRec = packed record
  r:byte;//<Red Value
  g:byte;//Green Value
  b:byte;//Blue Value
end;

PRGBARec = ^TRGBRec;
TRGBARec = packed record
  r:byte;//<Red Value
  g:byte;//<Green Value
  b:byte;//<Blue Value
  a:byte;//<Alpha Value
end;

type
// A structure for our custom vertex type
//------------------------------------------------------------------------------
TCustomVertex = packed record
 Position: TD3DVector;   // Vertex position
 Normal  : TD3DVector;   // Vertex normal
 Color   : Cardinal;     // The vertex color
 TexCoord: TD3DXVector2; // Texture co-ordinate
end;

PCustomVertex = ^TCustomVertex;


PCustomVertexArray = ^TCustomVertexArray;
TCustomVertexArray = array[0..$00FFFFFF] of TCustomVertex;

const

D3DFVF_CUSTOMVERTEX = (D3DFVF_XYZ or D3DFVF_NORMAL or D3DFVF_DIFFUSE or D3DFVF_TEX1);

//------------------------------------------------------------------------------
Direct3D9PrimitiveType: Array[TPHXPrimitiveType] of _D3DPRIMITIVETYPE = (
  // PHX_POINTS
  D3DPT_POINTLIST,
  // PHX_LINES
  D3DPT_LINELIST,
  // PHX_TRIANGLES
  D3DPT_TRIANGLELIST,
  // PHX_TRIANGLE_STRIP
  D3DPT_TRIANGLESTRIP,
  // PHX_TRIANGLE_FAN
  D3DPT_TRIANGLEFAN
);

type


{$REGION 'TPHXDirect3D9_Texture'}

//-----------------------------------------------------------------------------
TPHXDirect3D9_Texture = class(TPHXTexture)
	private
    FDirect3D9Device  : IDirect3DDevice9;
    FDirect3D9Texture9: IDirect3DTexture9;
  public
		// Creates a new, empty texture
		constructor Create; override;

    procedure Build; override;
    procedure Unload; override;

    property Direct3D9Device  : IDirect3DDevice9 read FDirect3D9Device write FDirect3D9Device;
    property Direct3D9Texture9: IDirect3DTexture9 read FDirect3D9Texture9 write FDirect3D9Texture9;
  end;

{$ENDREGION}

{$REGION 'TPHXDirect3D9_Buffer'}

//------------------------------------------------------------------------------
TPHXDirect3D9_Buffer = class(TPHXBuffer)
  private
    FDirect3D9Device      : IDirect3DDevice9;
    FDirect3D9IndexBuffer : IDirect3DIndexBuffer9;
    FDirect3D9VertexBuffer: IDirect3DVertexBuffer9;

    function CreateBuffers: Boolean;

    procedure DestroyBuffers;
  public
    constructor Create;
    destructor Destroy; override;

    procedure Resize(const VertexCapacity: Integer; const IndexCapacity: Integer); override;

    // Bind the buffer;
    procedure Bind; override;
    // Upload the buffer to the device
    procedure Upload; override;

    property Direct3D9Device: IDirect3DDevice9 read FDirect3D9Device write FDirect3D9Device;
    Property Direct3D9VertexBuffer: IDirect3DVertexBuffer9 read FDirect3D9VertexBuffer write FDirect3D9VertexBuffer;
    Property Direct3D9IndexBuffer: IDirect3DIndexBuffer9 read FDirect3D9IndexBuffer write FDirect3D9IndexBuffer;
  end;

{$ENDREGION}

{$REGION 'TPHXDirect3D9_Renderer'}

//-----------------------------------------------------------------------------
TPHXDirect3D9_Renderer = class(TInterfacedObject, IPHXDevice)
  private
    // Used to create the D3DDevice
    FDirect3D9      : IDirect3D9;
    // Our rendering device
    FDirect3D9Device: IDirect3DDevice9;

    ClearColor: TD3DColor;

    FBlending: TPHXBlendMode;
  protected
    // Get the name of the device
     function GetName: String; virtual;
     // Get the version of the device
     function GetVersion: String; virtual;
     // Return the provider target of this renderer
     function GetTarget: TPHXProviderTarget; virtual;
     // Get the current window size
     function GetWidth: Integer; virtual; abstract;
     // Get the current window size
     function GetHeight: Integer; virtual; abstract;
     // Get the window flags
     function GetFlags: TPHXWindowFlags; virtual; abstract;

     // Set the window title
     procedure SetTitle(const Title: String); virtual; abstract;
     // Set the window flags
     procedure SetFlags(const Flags: TPHXWindowFlags); virtual; abstract;
     // Set the icon for the window
     procedure SetIcon(const Icon: String); virtual; abstract;

    procedure InitializeDirect3D(const Width, Height: Integer);
  public
    constructor Create;
    destructor Destroy; override;

    // Enumerate all supported display modes
    procedure EnumDisplayModes(const Modes: TPHXDisplayModes); virtual; abstract;

    // Initialize the renderer
    procedure Initialize(const Parameters: TPHXDeviceParameters); virtual; abstract;
    // Reinitializes the renderer using a new display mode
    procedure Reinitialize(const Parameters: TPHXDeviceParameters); virtual; abstract;
    // Finalize the renderer
    procedure Finalize; virtual; abstract;

    // Update the renderer
    procedure Update; virtual;
    // Clear the back buffers
    procedure Clear; virtual;
    // Flip the front and back buffers
    procedure Flip; virtual;

    // # Factory functions

    // Creates a new render target
    function CreateRenderTarget: TPHXRenderTarget;
    // Creates a new canvas
    function CreateCanvas: TPHXCanvas;
    // Creates a new basic effect
    function CreateEffect: TPHXEffect;
    // Creates a new render buffer
    function CreateBuffer: TPHXBuffer;
    // Creates a new texture
    function CreateTexture: TPHXTexture;

    // Set the current render target
    procedure SetRenderTarget(Target: TPHXRenderTarget);

    /// SetViewport can be used to draw on part of the screen
    procedure SetViewport(const Viewport: TViewport);
    // Sets the background color
    procedure SetClearColor(const Color: TColor4f);

    // Change the blend mode
    procedure SetBlending(const Value: TPHXBlendMode);
    // Toggles wireframe
    procedure SetWireframe(const Value: Boolean);
    // Enable or disable writing into the depth buffer
    procedure SetDepthMask(const Value: Boolean);
    // Enable or disable depth testing
    procedure SetDepthTest(const Value: Boolean);

    // Used to create the D3DDevice
    property Direct3D9        : IDirect3D9 read FDirect3D9 write FDirect3D9;
    // Our rendering device
    property Direct3D9Device  : IDirect3DDevice9 read FDirect3D9Device write FDirect3D9Device;
  end;
{$ENDREGION}


procedure CopyPixels_A8      (Width, Height: Integer; SrcPixels: PByteArray; SrcFormat: TPHXPixelFormat; DstPixels: TD3DLockedRect);
procedure CopyPixels_L8      (Width, Height: Integer; SrcPixels: PByteArray; SrcFormat: TPHXPixelFormat; DstPixels: TD3DLockedRect);
procedure CopyPixels_X8R8G8B8(Width, Height: Integer; SrcPixels: PByteArray; SrcFormat: TPHXPixelFormat; DstPixels: TD3DLockedRect);
procedure CopyPixels_A8R8G8B8(Width, Height: Integer; SrcPixels: PByteArray; SrcFormat: TPHXPixelFormat; DstPixels: TD3DLockedRect);

implementation

uses
  phxDirect3D9_Canvas,
  phxDirect3D9_Effect;

{$REGION 'CopyPixels'}

//------------------------------------------------------------------------------
procedure CopyPixels_A8 (Width, Height: Integer; SrcPixels: PByteArray;  SrcFormat: TPHXPixelFormat; DstPixels: TD3DLockedRect);
begin

end;

//------------------------------------------------------------------------------
procedure CopyPixels_L8(Width, Height: Integer; SrcPixels: PByteArray; SrcFormat: TPHXPixelFormat; DstPixels: TD3DLockedRect);
begin

end;

//------------------------------------------------------------------------------
procedure CopyPixels_X8R8G8B8(Width, Height: Integer; SrcPixels: PByteArray; SrcFormat: TPHXPixelFormat; DstPixels: TD3DLockedRect);
var X, Y    : Integer;
var Getter  : TGetPixel;
var Color   : TPHXPixel;
var SrcPixel: PByte;
var DstPixel: PByte;
begin
  Getter:= GetPixelFormatGetter(SrcFormat);

  SrcPixel:= @SrcPixels^[0];
  for Y:= 0 to Height-1 do
  begin
    DstPixel:= Pointer( Integer(DstPixels.pBits) + (Y *  DstPixels.Pitch) );

  //  Rect
    for X:= 0 to Width-1 do
    begin
      Getter(SrcPixel, Color);

      DstPixel^:= Color.Blue;
      Inc(DstPixel);
      DstPixel^:= Color.Green;
      Inc(DstPixel);
      DstPixel^:= Color.Red;
      Inc(DstPixel);
      DstPixel^:= 255;
      Inc(DstPixel);
    end;
  end;
end;

//Type TPixel_A8R8G8B8 = record


//------------------------------------------------------------------------------
procedure CopyPixels_A8R8G8B8(Width, Height: Integer; SrcPixels: PByteArray; SrcFormat: TPHXPixelFormat; DstPixels: TD3DLockedRect);
var X, Y    : Integer;
var Getter  : TGetPixel;
var Color   : TPHXPixel;
var SrcPixel: PByte;
var DstPixel: PByte;
begin
  Getter:= GetPixelFormatGetter(SrcFormat);

  SrcPixel:= @SrcPixels^[0];
  for Y:= 0 to Height-1 do
  begin
    DstPixel:= Pointer( Integer(DstPixels.pBits) + (Y *  DstPixels.Pitch) );

  //  Rect
    for X:= 0 to Width-1 do
    begin
      Getter(SrcPixel, Color);
      //B
      //G
      //R

      DstPixel^:= Color.Blue;
      Inc(DstPixel);
      DstPixel^:= Color.Green;
      Inc(DstPixel);
      DstPixel^:= Color.Red;
      Inc(DstPixel);
      DstPixel^:= Color.Alpha;
      Inc(DstPixel);
    end;
  end;
end;

{$ENDREGION}

{$REGION 'TPHXDirect3D9_Texture'}

// TPHXDirect3D9_Texture
//==============================================================================
constructor TPHXDirect3D9_Texture.Create;
begin
  FDirect3D9Texture9:= nil;
  FDirect3D9Device  := nil;
end;

//------------------------------------------------------------------------------
procedure TPHXDirect3D9_Texture.Build;
var Direct3D9Format: TD3DFormat;
var Rect: TD3DLockedRect;
var Status: HResult;
begin
  Direct3D9Format:= D3DFMT_UNKNOWN;
  case Format of
    pfNone     : Exit;
    pfAlpha    : Exit;
    pfRGB      : Direct3D9Format := D3DFMT_X8R8G8B8;
    pfRGBA     : Direct3D9Format := D3DFMT_A8R8G8B8;
  end;

  Status:= Direct3D9Device.CreateTexture(Width, Height, 0, 0, Direct3D9Format, D3DPOOL_MANAGED, FDirect3D9Texture9, nil);

  if Failed(Status) then Exit;

  Status:= FDirect3D9Texture9.LockRect(0, Rect, nil, 0);

  if Failed(Status) then Exit;

  case Format of
    pfNone     : Exit;
    pfAlpha    : Exit;
    pfRGB      :
    begin
      CopyPixels_X8R8G8B8(Width, Height, Pixels, Format, Rect);
    end;
    // 32 bit RGBA texture
    pfRGBA:
    begin
      CopyPixels_A8R8G8B8(Width, Height, Pixels, Format, Rect);
    end;

  end;

  Direct3D9Texture9.UnlockRect(0);
end;

//------------------------------------------------------------------------------
procedure TPHXDirect3D9_Texture.Unload;
begin
  FDirect3D9Texture9:= nil;
end;

{$ENDREGION}



{$REGION 'TPHXDirect3D9_Buffer'}

// TPHXDirect3D9_Buffer
//==============================================================================
constructor TPHXDirect3D9_Buffer.Create;
begin
  inherited Create;

  // Create the vertex and index buffers
  if not CreateBuffers then
  begin
    raise Exception.Create('Failed to create index and vertex buffers');
  end;
end;

//------------------------------------------------------------------------------
destructor TPHXDirect3D9_Buffer.Destroy;
begin
  DestroyBuffers;
  inherited;
end;

//---------------------------------------------------------------------------
function TPHXDirect3D9_Buffer.CreateBuffers: Boolean;
var Status: HRESULT;
begin
  Status:= Direct3D9Device.CreateIndexBuffer(CanvasCapacityIndices, D3DUSAGE_WRITEONLY, D3DFMT_INDEX16, D3DPOOL_DEFAULT, FDirect3D9IndexBuffer, nil);

  if Failed(Status) then
  begin
    raise Exception.Create('Failed to CreateIndexBuffer');
  end;

  Status:= Direct3D9Device.CreateVertexBuffer(CanvasCapacityVertices * SizeOf(TCustomVertex), 0, D3DFVF_CUSTOMVERTEX, D3DPOOL_DEFAULT, FDirect3D9VertexBuffer, nil);

  if Failed(Status) then
  begin
    raise Exception.Create('Failed to CreateVertexBuffer');
  end;

  Result:= True;
end;

//---------------------------------------------------------------------------
procedure TPHXDirect3D9_Buffer.DestroyBuffers;
begin

end;

//---------------------------------------------------------------------------
procedure TPHXDirect3D9_Buffer.Resize(const VertexCapacity: Integer; const IndexCapacity: Integer);
begin
// glBindBuffer(GL_ARRAY_BUFFER, FBufferVertex);
// glBufferData(GL_ARRAY_BUFFER, SizeOf(TPHXVertex) * VertexCapacity, nil, GL_STATIC_DRAW);

// glBindBuffer(GL_ELEMENT_ARRAY_BUFFER, FBufferIndex);
// glBufferData(GL_ELEMENT_ARRAY_BUFFER, SizeOf(TPHXIndex) * IndexCapacity, nil, GL_STATIC_DRAW);
end;

//---------------------------------------------------------------------------
procedure TPHXDirect3D9_Buffer.Bind;
begin
 // glBindBuffer(GL_ARRAY_BUFFER, FBufferVertex);

  //glBindBuffer(GL_ELEMENT_ARRAY_BUFFER, FBufferIndex);
end;

//---------------------------------------------------------------------------
procedure TPHXDirect3D9_Buffer.Upload;
begin
  Assert(Vertices.Count <= BufferCapacityVertices);
  Assert(Indices .Count <= BufferCapacityIndices);
     (*
  glBindBuffer(GL_ARRAY_BUFFER, FBufferVertex);
  // Upload verticies
  if (Vertices.Count > 0) then
  begin
    glBufferSubData(GL_ARRAY_BUFFER, 0, SizeOf(TPHXVertex) *  Vertices.Count, Vertices.List);
  end;

  glBindBuffer(GL_ELEMENT_ARRAY_BUFFER, FBufferIndex);
  // Upload indices
  if (Indices.Count > 0) then
  begin
    glBufferSubData(GL_ELEMENT_ARRAY_BUFFER, 0, SizeOf(TPHXIndex) *  Indices.Count, Indices.List);
  end;
  *)
end;


{$ENDREGION}

{$REGION 'TPHXDirect3D9_Renderer'}

// TPHXDirect3D9_Renderer
//==============================================================================
constructor TPHXDirect3D9_Renderer.Create;
begin
  inherited;
  FBlending:= bmNormal;
end;

//------------------------------------------------------------------------------
destructor TPHXDirect3D9_Renderer.Destroy;
begin

  inherited;
end;

//------------------------------------------------------------------------------
function TPHXDirect3D9_Renderer.GetName: String;
begin
  Result:= 'Phoenix.Direct3D9';
end;

//------------------------------------------------------------------------------
function TPHXDirect3D9_Renderer.GetVersion: String;
begin
  Result:= '9.0c';
end;

//------------------------------------------------------------------------------
function TPHXDirect3D9_Renderer.GetTarget: TPHXProviderTarget;
begin
  Result:= ptDirect3D9;
end;

//------------------------------------------------------------------------------
procedure TPHXDirect3D9_Renderer.InitializeDirect3D(const Width, Height: Integer
  );
var Status: HRESULT;
var d3ddm: D3DDISPLAYMODE;
var FDirect3D9PresentParameters: TD3DPresentParameters;
begin
  FDirect3D9:= Direct3DCreate9(D3D_SDK_VERSION);

  if( FDirect3D9 = nil ) then
  begin
    raise Exception.Create('Failed to create the Direct3D object');
  end;
  FDirect3D9.GetAdapterDisplayMode( D3DADAPTER_DEFAULT, d3ddm );

  Status:= FDirect3D9.CheckDeviceFormat( D3DADAPTER_DEFAULT,
                                         D3DDEVTYPE_HAL,
                                         d3ddm.Format,
                                         D3DUSAGE_DEPTHSTENCIL,
                                         D3DRTYPE_SURFACE,
                                         D3DFMT_D24S8 );


  if( Status = D3DERR_NOTAVAILABLE ) then
  begin
    raise Exception.Create('D3DERR_NOTAVAILABLE');
  end;

   // Set up the structure used to create the D3DDevice. Most parameters are
  // zeroed out. We set Windowed to TRUE, since we want to do D3D in a
  // window, and then set the SwapEffect to "discard", which is the most
  // efficient method of presenting the back buffer to the display.  And
  // we request a back buffer format that matches the current desktop display
  // format.
  FillChar(FDirect3D9PresentParameters, SizeOf(FDirect3D9PresentParameters), 0);

  FDirect3D9PresentParameters.hDeviceWindow:= GetActiveWindow();
  FDirect3D9PresentParameters.Windowed := True;
  FDirect3D9PresentParameters.SwapEffect := D3DSWAPEFFECT_DISCARD;
  FDirect3D9PresentParameters.BackBufferFormat        := D3DFMT_UNKNOWN;
  FDirect3D9PresentParameters.BackBufferFormat       := d3ddm.Format;

  FDirect3D9PresentParameters.EnableAutoDepthStencil := TRUE;
  FDirect3D9PresentParameters.AutoDepthStencilFormat := D3DFMT_D24S8;
  FDirect3D9PresentParameters.PresentationInterval   := D3DPRESENT_INTERVAL_IMMEDIATE;

	FDirect3D9PresentParameters.BackBufferWidth       := Width;
	FDirect3D9PresentParameters.BackBufferHeight      := Height;

  Status:= Direct3D9.CreateDevice(D3DADAPTER_DEFAULT, D3DDEVTYPE_HAL, GetActiveWindow(), D3DCREATE_SOFTWARE_VERTEXPROCESSING, @FDirect3D9PresentParameters, FDirect3D9Device);

  if Failed(Status) then
  begin
    raise Exception.Create('Failed to create the Direct3D Device');
  end;

//  Direct3D9Device.SetTransform(D3DTS_WORLD     , MatWorld);
//  Direct3D9Device.SetTransform(D3DTS_VIEW      , MatView);
//  Direct3D9Device.SetTransform(D3DTS_PROJECTION, MatProjection);

  // Turn off culling
  Direct3D9Device.SetRenderState(D3DRS_CULLMODE, D3DCULL_NONE);

  // Turn off D3D lighting
  Direct3D9Device.SetRenderState(D3DRS_LIGHTING, iFalse);
  // Turn on the zbuffer
  Direct3D9Device.SetRenderState(D3DRS_ZENABLE, iTrue);

  Direct3D9Device.SetRenderState(D3DRS_COLORVERTEX, iTrue); // iTrue

//  Direct3D9Device.GetRenderTarget(0, FDirect3D9BackBuffer);
end;

//------------------------------------------------------------------------------
function TPHXDirect3D9_Renderer.CreateRenderTarget: TPHXRenderTarget;
begin
  raise ENotImplementedException.Create('TPHXDirect3D9_Renderer.CreateRenderTarget');
 // Result:= TPHXDirect3D9RenderTarget.Create;

 // TPHXDirect3D9RenderTarget(Result).Direct3D9Device:= Direct3D9Device;
  Result:= nil;
end;

//------------------------------------------------------------------------------
function TPHXDirect3D9_Renderer.CreateCanvas: TPHXCanvas;
begin
  Result:= TPHXDirect3D9_Canvas.Create(Direct3D9Device);
end;

//------------------------------------------------------------------------------
function TPHXDirect3D9_Renderer.CreateEffect: TPHXEffect;
begin
  raise ENotImplementedException.Create('TPHXDirect3D9_Renderer.CreateRenderTarget');
//  Result:= TPHXDirect3D9_Effect.Create(Direct3D9Device);
end;

//------------------------------------------------------------------------------
function TPHXDirect3D9_Renderer.CreateBuffer: TPHXBuffer;
begin
  Result:= TPHXDirect3D9_Buffer.Create;

  TPHXDirect3D9_Buffer(Result).Direct3D9Device:= Direct3D9Device;
end;

//------------------------------------------------------------------------------
function TPHXDirect3D9_Renderer.CreateTexture: TPHXTexture;
begin
  Result:= TPHXDirect3D9_Texture.Create;

  TPHXDirect3D9_Texture(Result).Direct3D9Device:= Direct3D9Device;
end;

//------------------------------------------------------------------------------
procedure TPHXDirect3D9_Renderer.SetRenderTarget(Target: TPHXRenderTarget);
begin

end;

//------------------------------------------------------------------------------
procedure TPHXDirect3D9_Renderer.SetViewport(const Viewport: TViewport);
var AViewport: TD3DViewport9;
var Status: HResult;
begin
  AViewport.X     := Viewport.X;
  AViewport.Y     := Viewport.Y;
  AViewport.Width := Viewport.Width;
  AViewport.Height:= Viewport.Height;
  AViewport.MinZ  := Viewport.MinDepth;
  AViewport.MaxZ  := Viewport.MaxDepth;

  Status:= Direct3D9Device.SetViewport(AViewport);

  if Failed(Status) then raise Exception.Create('Failed to set the viewport');
end;

//------------------------------------------------------------------------------
procedure TPHXDirect3D9_Renderer.SetClearColor(const Color: TColor4f);
begin
  ClearColor:= D3DCOLOR_COLORVALUE(Color.Red, Color.Green, Color.Blue, Color.Alpha);
end;

//------------------------------------------------------------------------------
procedure TPHXDirect3D9_Renderer.SetBlending(const Value: TPHXBlendMode);
begin
  case Value of
    // No blending
    bmNormal:
    begin
      Direct3D9Device.SetRenderState(D3DRS_ALPHABLENDENABLE, iFalse);
    end;
    // Alpha blending
    bmAlpha:
    begin
      // Enable blending if its disabled
      if FBlending = bmNormal then
      begin
        Direct3D9Device.SetRenderState(D3DRS_ALPHABLENDENABLE, iTrue);
      end;

      Direct3D9Device.SetRenderState(D3DRS_SRCBLEND , D3DBLEND_SRCALPHA);
      Direct3D9Device.SetRenderState(D3DRS_DESTBLEND, D3DBLEND_INVSRCALPHA);
    end;
    // Additive blending
    bmAdditive:
    begin
      // Enable blending if its disabled
      if FBlending = bmNormal then
      begin
        Direct3D9Device.SetRenderState(D3DRS_ALPHABLENDENABLE, iTrue);
      end;

      Direct3D9Device.SetRenderState(D3DRS_SRCBLEND , D3DBLEND_SRCALPHA);
      Direct3D9Device.SetRenderState(D3DRS_DESTBLEND, D3DBLEND_ONE);
    end;

    // Multiplication
    bmMultiply:
    begin
      // Enable blending if its disabled
      if FBlending = bmNormal then
      begin
        Direct3D9Device.SetRenderState(D3DRS_ALPHABLENDENABLE, iTrue);
      end;

      Direct3D9Device.SetRenderState(D3DRS_SRCBLEND , D3DBLEND_ZERO);
      Direct3D9Device.SetRenderState(D3DRS_DESTBLEND, D3DBLEND_SRCCOLOR);
    end;
    // Modulate
    bmModulate:
    begin
      // Enable blending if its disabled
      if FBlending = bmNormal then
      begin
        Direct3D9Device.SetRenderState(D3DRS_ALPHABLENDENABLE, iTrue);
      end;

      Direct3D9Device.SetRenderState(D3DRS_SRCBLEND , D3DBLEND_DESTCOLOR);
      Direct3D9Device.SetRenderState(D3DRS_DESTBLEND, D3DBLEND_SRCCOLOR);
    end;
    // Screen
    bmScreen:
    begin
      // Enable blending if its disabled
      if FBlending = bmNormal then
      begin
        Direct3D9Device.SetRenderState(D3DRS_ALPHABLENDENABLE, iTrue);
      end;

      Direct3D9Device.SetRenderState(D3DRS_SRCBLEND , D3DBLEND_ONE);
      Direct3D9Device.SetRenderState(D3DRS_DESTBLEND, D3DBLEND_INVSRCCOLOR);
    end;
  end;
  FBlending:= Value;
end;

//------------------------------------------------------------------------------
procedure TPHXDirect3D9_Renderer.SetWireframe(const Value: Boolean);
begin
  if Value then
  begin
    Direct3D9Device.SetRenderState(D3DRS_FILLMODE, D3DFILL_WIREFRAME);
  end else
  begin
    Direct3D9Device.SetRenderState(D3DRS_FILLMODE, D3DFILL_SOLID);
  end;
end;


//------------------------------------------------------------------------------
procedure TPHXDirect3D9_Renderer.SetDepthTest(const Value: Boolean);
begin
  if Value then
  begin
    Direct3D9Device.SetRenderState(D3DRS_ZENABLE, iTrue);
  end else
  begin
    Direct3D9Device.SetRenderState(D3DRS_ZENABLE, iFalse);
  end;
end;

//------------------------------------------------------------------------------
procedure TPHXDirect3D9_Renderer.SetDepthMask(const Value: Boolean);
begin
  if Value then
  begin
    Direct3D9Device.SetRenderState(D3DRS_ZWRITEENABLE, iTrue);
  end else
  begin
   Direct3D9Device.SetRenderState(D3DRS_ZWRITEENABLE, iFalse);
  end;
end;


//------------------------------------------------------------------------------
procedure TPHXDirect3D9_Renderer.Update;
begin
end;

//------------------------------------------------------------------------------
procedure TPHXDirect3D9_Renderer.Clear;
begin
  Direct3D9Device.Clear(0, nil, D3DCLEAR_TARGET , ClearColor, 1.0, 0);
end;

//------------------------------------------------------------------------------
procedure TPHXDirect3D9_Renderer.Flip;
begin
  // Present the backbuffer contents to the display
  Direct3D9Device.Present(nil, nil, 0, nil);
end;

{$ENDREGION}







end.
