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
unit phxOpenGL;
//< OpenGL classes

interface

{$I ../phxConfig.inc}

uses
  SysUtils, Classes,

  dglOpenGL,

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

  Windows;

// const
//  FLAG_RENDER_SDL = 100;
const

{$REGION 'OpenGL Constants'}

//------------------------------------------------------------------------------
glPixelFormat: array[TPHXPixelFormat] of glEnum = (
  GL_ZERO,
  GL_ALPHA,
  GL_RGB,
  GL_RGBA
  );

//------------------------------------------------------------------------------
glPixelFormatName: array[TPHXPixelFormat] of String = (
  'GL_ZERO',
  'GL_ALPHA',
  'GL_RGB',
  'GL_RGBA'
  );

//------------------------------------------------------------------------------
glInternalPixelFormat: array[TPHXPixelFormat] of glEnum = (
  GL_ZERO,
  GL_ALPHA,
  GL_RGB,
  GL_RGBA
  );

//------------------------------------------------------------------------------
glTextureFilter: array[TPHXTextureFilter] of glEnum = (
  GL_NEAREST,
  GL_LINEAR,
  GL_NEAREST_MIPMAP_NEAREST,
  GL_LINEAR_MIPMAP_NEAREST,
  GL_NEAREST_MIPMAP_LINEAR,
  GL_LINEAR_MIPMAP_LINEAR
  );

//------------------------------------------------------------------------------
glTextureWrap: array[TPHXTextureWrap] of glEnum = (
  GL_REPEAT,
  GL_CLAMP_TO_EDGE
  );

// Converts a TPHXPrimitiveType to the corresponding opengl constant
//------------------------------------------------------------------------------
glPrimitiveType: array[TPHXPrimitiveType] of TGLenum = (
  // PHX_POINTS: Draw the verticies as points.
  GL_POINTS,
  // PHX_LINES: Draw the verticies as lines
  GL_LINES,
  // PHX_TRIANGLES: Draw the verticeis as triangles
  GL_TRIANGLES,
  //PHX_TRIANGLE_STRIP: Draw the verticies as a triangle strip
  GL_TRIANGLE_STRIP,
  //PHX_TRIANGLE_FAN: Draw the verticies as a triangle fan
  GL_TRIANGLE_FAN
);

{$ENDREGION}


type

TPHXOpenGL_Renderer = class;
TPHXOpenGL_Texture = class;

// Render target using Frame Buffer Objects (FBO)
//-----------------------------------------------------------------------------
TPHXOpenGL_RenderTarget = class(TPHXRenderTarget)
  private
    FFBOHandle: Cardinal;
    FRBOHandle: Cardinal;
    FTEXHandle: Cardinal;

    FTexture: TPHXOpenGL_Texture;

    procedure Build;

    procedure CheckStatus;
  public
    constructor Create;
    destructor Destroy; override;

    procedure Resize(const AWidth, AHeight: Integer); override;


    function GetTexture: TPHXTexture; override;

    procedure SaveToBitmap(Bitmap: TPHXBitmap);  override;
    procedure Clear; override;

    property TEXHandle: Cardinal read FTEXHandle;
    property FBOHandle: Cardinal read FFBOHandle;
    property RBOHandle: Cardinal read FRBOHandle;
  public
    class procedure Bind(const Target: TPHXOpenGL_RenderTarget);
  end;

// Fallback render target using glCopyTexSubImage2D
//-----------------------------------------------------------------------------
TPHXOpenGL_RenderTargetCopyTexImage = class(TPHXRenderTarget)
  private
    FHandleTexture: Cardinal;
  public
    constructor Create;

    // Unload the texture
    procedure Clear; override;

    procedure Resize(const AWidth, AHeight: Integer); override;

    function GetTexture: TPHXTexture; override;

    procedure SaveToBitmap(Bitmap: TPHXBitmap);  override;
  end;

{$REGION 'TPHXOpenGL_Texture'}

//-----------------------------------------------------------------------------
TPHXOpenGL_Texture = class(TPHXTexture)
  private
    FHandle: TGLuint;
  public
    constructor Create; override;

    procedure Build; override;
    procedure Unload; override;

    Property Handle: TGLuint read FHandle write FHandle;
  end;

{$ENDREGION}

{$REGION 'TPHXOpenGL_Buffer'}

//------------------------------------------------------------------------------
TPHXOpenGL_Buffer = class(TPHXBuffer)
  private
    FRenderer: TPHXOpenGL_Renderer;

    // Handle for the vertex buffer
    FBufferVertex: GLuint;
    // Handle for the index buffer
    FBufferIndex : GLuint;

    // Capacity for the vertex buffer
    FCapacityVertex: Integer;
    // Capacity for the index buffer
    FCapacityIndex : Integer;

    // Create the index and vertex buffers
    function CreateBuffers: Boolean;
    // Destroy the index buffers
    procedure DestroyBuffers;
  public
    constructor Create(ARenderer: TPHXOpenGL_Renderer);
    destructor Destroy; override;

    procedure Resize(const VertexCapacity: Integer; const IndexCapacity: Integer); override;

    // Bind the buffer;
    procedure Bind; override;
    // Upload the buffer to the device
    procedure Upload; override;

    // Handle for the vertex buffer
    property BufferVertex: GLuint read FBufferVertex;
    // Handle for the index buffer
    property BufferIndex : GLuint read FBufferIndex ;
  end;

{$ENDREGION}

{$REGION 'TPHXOpenGL_Renderer'}

// http://duriansoftware.com/joe/An-intro-to-modern-OpenGL.-Chapter-2.2:-Shaders.html
//-----------------------------------------------------------------------------
TPHXOpenGL_Renderer = class(TInterfacedObject, IPHXDevice)
  private
    FBlending: TPHXBlendMode;

    SupportsFrameBufferObject: Boolean;
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

    procedure InitializeOpenGL;
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
  end;

{$ENDREGION}

implementation

uses
  phxOpenGL_Canvas,
  phxOpenGL_Effect;

//-----------------------------------------------------------------------------
procedure ResetOpenGLState;
begin
  //glShadeModel(GL_SMOOTH);

//  glEnable(GL_COLOR_MATERIAL);
  glEnable(GL_POLYGON_SMOOTH);

  // Bugfix for http://code.google.com/p/phoenixlib/issues/detail?id=50
  glDisable(GL_POLYGON_SMOOTH);
end;


// TPHXOpenGL_RenderTarget
//==============================================================================
constructor TPHXOpenGL_RenderTarget.Create;
begin
  inherited;

  FTexture:= TPHXOpenGL_Texture.Create;
  FTexture.Name:= 'RenderTarget';

  FFBOHandle:= GL_NONE;
end;

//-----------------------------------------------------------------------------
destructor TPHXOpenGL_RenderTarget.Destroy;
begin
  FTexture.Free;

  if FTEXHandle <> GL_NONE  then
  begin
    glDeleteTextures(1, @FTEXHandle);

    FTEXHandle:= GL_NONE;
  end;

  if FFBOHandle <> GL_NONE  then
  begin
    glDeleteFramebuffersEXT(1, @FFBOHandle);

    FFBOHandle:= GL_NONE;
  end;

  if FRBOHandle <> GL_NONE  then
  begin
    glDeleteRenderbuffersEXT(1, @FRBOHandle);

    FRBOHandle:= GL_NONE;
  end;
end;

//-----------------------------------------------------------------------------
procedure TPHXOpenGL_RenderTarget.Resize(const AWidth, AHeight: Integer);
begin
  inherited Resize(AWidth, AHeight);

  Build;
end;

//-----------------------------------------------------------------------------
procedure TPHXOpenGL_RenderTarget.Clear;
begin
  // clear buffer
//  glClearColor(1, 1, 1, 1);
  glClear(GL_COLOR_BUFFER_BIT or GL_DEPTH_BUFFER_BIT);
end;

//-----------------------------------------------------------------------------
procedure TPHXOpenGL_RenderTarget.SaveToBitmap(Bitmap: TPHXBitmap);
var Buffer  : PByte;
var SrcPixel: PCardinal;
var DstPixel: PCardinal;
var x,y:integer;
begin
  if FTEXHandle = 0 then Exit;

  Bitmap.Width := Width;
  Bitmap.Height:= Height;
  Bitmap.Format:= pfRGBA;

  glBindTexture(GL_TEXTURE_2D, FTEXHandle);

  GetMem(Buffer, Width * Height * 4);
  try
    glGetTexImage(GL_TEXTURE_2D, 0, GL_BGRA, GL_UNSIGNED_BYTE, Buffer);

    SrcPixel:= PCardinal(Buffer);
    for y := 0 to Height-1 do
    begin
      DstPixel:= PCardinal(Bitmap.Scanline(Y));
      for x := 0 to Width-1 do
      begin
        DstPixel^ := SrcPixel^;

        inc(SrcPixel);
        inc(DstPixel);
      end;
    end;
  finally
    FreeMem(Buffer, Width * Height * 4);
  end;
end;

//-----------------------------------------------------------------------------
class procedure TPHXOpenGL_RenderTarget.Bind(const Target: TPHXOpenGL_RenderTarget);
begin
  if Assigned(Target) then
  begin
    glBindFramebuffer(GL_FRAMEBUFFER, Target.FBOHandle);
  end else
  begin
    // switch back to window-system-provided framebuffer
    glBindFramebuffer(GL_FRAMEBUFFER, 0);
  end;
end;
     (*
//-----------------------------------------------------------------------------
procedure TPHXOpenGL_RenderTarget.Build;
begin
  // create a texture object
  glGenTextures(1, @FTEXHandle);
  glBindTexture(GL_TEXTURE_2D, FTEXHandle);

  glTexParameterf(GL_TEXTURE_2D, GL_TEXTURE_MAG_FILTER, GL_LINEAR);
 // glTexParameterf(GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, GL_LINEAR);//GL_LINEAR_MIPMAP_LINEAR);
  glTexParameterf(GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, GL_LINEAR_MIPMAP_LINEAR);

  glTexParameterf(GL_TEXTURE_2D, GL_TEXTURE_WRAP_S, GL_CLAMP_TO_EDGE);
  glTexParameterf(GL_TEXTURE_2D, GL_TEXTURE_WRAP_T, GL_CLAMP_TO_EDGE);

  glTexImage2D(GL_TEXTURE_2D, 0, GL_RGBA8, Width, Height, 0, GL_RGBA, GL_UNSIGNED_BYTE, nil);
  glBindTexture(GL_TEXTURE_2D, 0);

  FTexture.Width:= Width;
  FTexture.Height:= Height;
  FTexture.Handle:= FTEXHandle;

  // create a framebuffer object, you need to delete them when program exits.
  glGenFramebuffers(1, @FBOHandle);
  glBindFramebuffer(GL_FRAMEBUFFER, FBOHandle);

  // create a renderbuffer object to store depth info
  // NOTE: A depth renderable image should be attached the FBO for depth test.
  // If we don't attach a depth renderable image to the FBO, then
  // the rendering output will be corrupted because of missing depth test.
  // If you also need stencil test for your rendering, then you must
  // attach additional image to the stencil attachement point, too.
  glGenRenderbuffers(1, @RBOHandle);
  glBindRenderbuffer(GL_RENDERBUFFER, RBOHandle);
  glRenderbufferStorage(GL_RENDERBUFFER, GL_DEPTH_COMPONENT, Width, Height);
  glBindRenderbuffer(GL_RENDERBUFFER, 0);

  // attach a texture to FBO color attachement point
  glFramebufferTexture2D(GL_RENDERBUFFER, GL_COLOR_ATTACHMENT0, GL_TEXTURE_2D, FTEXHandle, 0);

  // attach a renderbuffer to depth attachment point
  glFramebufferRenderbuffer(GL_FRAMEBUFFER, GL_DEPTH_ATTACHMENT, GL_RENDERBUFFER, RBOHandle);

  //@ disable color buffer if you don't attach any color buffer image,
  //@ for example, rendering depth buffer only to a texture.
  //@ Otherwise, glCheckFramebufferStatusEXT will not be complete.
 // glDrawBuffer(GL_NONE);
  //glReadBuffer(GL_NONE);
  CheckStatus ;

  glBindFramebuffer(GL_FRAMEBUFFER, 0);
end;

*)

//-----------------------------------------------------------------------------
procedure TPHXOpenGL_RenderTarget.Build;
begin
  // create a texture object
  glGenTextures(1, @FTEXHandle);
  glBindTexture(GL_TEXTURE_2D, FTEXHandle);
  glTexParameterf(GL_TEXTURE_2D, GL_TEXTURE_MAG_FILTER, GL_LINEAR);
  //glTexParameterf(GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, GL_LINEAR);
  glTexParameterf(GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, GL_LINEAR_MIPMAP_LINEAR);
  //glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_S, GL_CLAMP);
  //glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_T, GL_CLAMP);
  glTexParameterf(GL_TEXTURE_2D, GL_TEXTURE_WRAP_S, GL_CLAMP_TO_EDGE);
  glTexParameterf(GL_TEXTURE_2D, GL_TEXTURE_WRAP_T, GL_CLAMP_TO_EDGE);
//  glTexParameteri(GL_TEXTURE_2D, GL_GENERATE_MIPMAP, GL_TRUE); // automatic mipmap generation included in OpenGL v1.4

  glTexImage2D(GL_TEXTURE_2D, 0, GL_RGBA8, Width, Height, 0, GL_RGBA, GL_UNSIGNED_BYTE, nil);
  glBindTexture(GL_TEXTURE_2D, 0);

      // create a framebuffer object, you need to delete them when program exits.
  glGenFramebuffersEXT(1, @FBOHandle);
  glBindFramebufferEXT(GL_FRAMEBUFFER_EXT, FBOHandle);

  // create a renderbuffer object to store depth info
  // NOTE: A depth renderable image should be attached the FBO for depth test.
  // If we don't attach a depth renderable image to the FBO, then
  // the rendering output will be corrupted because of missing depth test.
  // If you also need stencil test for your rendering, then you must
  // attach additional image to the stencil attachement point, too.
  glGenRenderbuffersEXT(1, @RBOHandle);
  glBindRenderbufferEXT(GL_RENDERBUFFER_EXT, RBOHandle);
  glRenderbufferStorageEXT(GL_RENDERBUFFER_EXT, GL_DEPTH_COMPONENT, Width, Height);
  glBindRenderbufferEXT(GL_RENDERBUFFER_EXT, 0);

  // attach a texture to FBO color attachement point
  glFramebufferTexture2DEXT(GL_FRAMEBUFFER_EXT, GL_COLOR_ATTACHMENT0_EXT, GL_TEXTURE_2D, FTEXHandle, 0);

  // attach a renderbuffer to depth attachment point
  glFramebufferRenderbufferEXT(GL_FRAMEBUFFER_EXT, GL_DEPTH_ATTACHMENT_EXT, GL_RENDERBUFFER_EXT, RBOHandle);

  //@ disable color buffer if you don't attach any color buffer image,
  //@ for example, rendering depth buffer only to a texture.
  //@ Otherwise, glCheckFramebufferStatusEXT will not be complete.
  //glDrawBuffer(GL_NONE);
  //glReadBuffer(GL_NONE);

  // check FBO status
//  printFramebufferInfo();
//  bool status = checkFramebufferStatus();
//  if(!status)
//      fboUsed = false;
  CheckStatus ;

  glBindFramebufferEXT(GL_FRAMEBUFFER_EXT, 0);
end;

//-----------------------------------------------------------------------------
function TPHXOpenGL_RenderTarget.GetTexture: TPHXTexture;
begin
  glBindTexture(GL_TEXTURE_2D, FTEXHandle);
  glGenerateMipmapEXT(GL_TEXTURE_2D);
  glBindTexture(GL_TEXTURE_2D, 0);

  Result:= FTexture;
end;

// http://www.opengl.org/wiki/GL_EXT_framebuffer_object
//-----------------------------------------------------------------------------
procedure TPHXOpenGL_RenderTarget.CheckStatus;
var Status: GlEnum;
begin
  Status := glCheckFramebufferStatus(GL_FRAMEBUFFER);
  {
  GL_FRAMEBUFFER_ATTACHMENT_OBJECT_TYPE = $8CD0;
  GL_FRAMEBUFFER_ATTACHMENT_OBJECT_NAME = $8CD1;
  GL_FRAMEBUFFER_ATTACHMENT_TEXTURE_LEVEL = $8CD2;
  GL_FRAMEBUFFER_ATTACHMENT_TEXTURE_CUBE_MAP_FACE = $8CD3;
  GL_FRAMEBUFFER_ATTACHMENT_TEXTURE_LAYER = $8CD4;
  GL_FRAMEBUFFER_COMPLETE = $8CD5;
  GL_FRAMEBUFFER_INCOMPLETE_ATTACHMENT = $8CD6;
  GL_FRAMEBUFFER_INCOMPLETE_MISSING_ATTACHMENT = $8CD7;
  GL_FRAMEBUFFER_INCOMPLETE_DRAW_BUFFER = $8CDB;
  GL_FRAMEBUFFER_INCOMPLETE_READ_BUFFER = $8CDC;
  GL_FRAMEBUFFER_UNSUPPORTED = $8CDD;
  }

  case Status of
    GL_FRAMEBUFFER_COMPLETE:
    begin

    end;
    // Incomplete attachment
    GL_FRAMEBUFFER_INCOMPLETE_ATTACHMENT:
    begin
      TPHXLogger.Error('TPHXOpenGLRenderTarget.CheckStatus', 'Incomplete attachment');

      raise Exception.Create('Incomplete attachment');
    end;
    // Missing attachment
    GL_FRAMEBUFFER_INCOMPLETE_MISSING_ATTACHMENT:
    begin
      TPHXLogger.Error('TPHXOpenGLRenderTarget.CheckStatus', 'Missing attachment');

      raise Exception.Create('Missing attachment');
    end;
    // Incomplete draw buffer
    GL_FRAMEBUFFER_INCOMPLETE_DRAW_BUFFER:
    begin
      TPHXLogger.Error('TPHXOpenGLRenderTarget.CheckStatus', 'Incomplete draw buffer');

      raise Exception.Create('Incomplete draw buffer');
    end;
    // Incomplete read buffer
    GL_FRAMEBUFFER_INCOMPLETE_READ_BUFFER:
    begin
      TPHXLogger.Error('TPHXOpenGLRenderTarget.CheckStatus', 'Incomplete read buffer');

      raise Exception.Create('Incomplete read buffer');
    end;
    // Framebufferobjects unsupported
    GL_FRAMEBUFFER_UNSUPPORTED:
    begin
      TPHXLogger.Error('TPHXOpenGLRenderTarget.CheckStatus', 'Framebufferobjects unsupported');

      raise Exception.Create('Framebufferobjects unsupported');
    end;
  end;
end;



// TPHXOpenGL_RenderTargetCopyTexImage
//==============================================================================
constructor TPHXOpenGL_RenderTargetCopyTexImage.Create;
begin
  inherited;
end;

//-----------------------------------------------------------------------------
function TPHXOpenGL_RenderTargetCopyTexImage.GetTexture: TPHXTexture;
begin
  glBindTexture(GL_TEXTURE_2D, FHandleTexture);
  glCopyTexSubImage2D(GL_TEXTURE_2D, 0, 0, 0, 0, 0, Width, Height);

  glGenerateMipmapEXT(GL_TEXTURE_2D);
  glBindTexture(GL_TEXTURE_2D, 0);

//  glBindTexture(GL_TEXTURE_2D, FTEXHandle);
//  glGenerateMipmapEXT(GL_TEXTURE_2D);
//  glBindTexture(GL_TEXTURE_2D, 0);

  Result:= TPHXOpenGL_Texture.Create;

  TPHXOpenGL_Texture(Result).Handle:= FHandleTexture;
end;

//-----------------------------------------------------------------------------
procedure TPHXOpenGL_RenderTargetCopyTexImage.Resize(const AWidth, AHeight: Integer);
begin
  inherited Resize(AWidth, AHeight);

    // create a texture object
  glGenTextures(1, @FHandleTexture);
  glBindTexture(GL_TEXTURE_2D, FHandleTexture);
  glTexParameterf(GL_TEXTURE_2D, GL_TEXTURE_MAG_FILTER, GL_LINEAR);
  //glTexParameterf(GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, GL_LINEAR);
  glTexParameterf(GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, GL_LINEAR_MIPMAP_LINEAR);
  //glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_S, GL_CLAMP);
  //glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_T, GL_CLAMP);
  glTexParameterf(GL_TEXTURE_2D, GL_TEXTURE_WRAP_S, GL_CLAMP_TO_EDGE);
  glTexParameterf(GL_TEXTURE_2D, GL_TEXTURE_WRAP_T, GL_CLAMP_TO_EDGE);
//  glTexParameteri(GL_TEXTURE_2D, GL_GENERATE_MIPMAP, GL_TRUE); // automatic mipmap generation included in OpenGL v1.4
  glTexImage2D(GL_TEXTURE_2D, 0, GL_RGBA8, Width, Height, 0, GL_RGBA, GL_UNSIGNED_BYTE, nil);
  glBindTexture(GL_TEXTURE_2D, 0);
end;

//-----------------------------------------------------------------------------
procedure TPHXOpenGL_RenderTargetCopyTexImage.Clear;
begin
end;

//-----------------------------------------------------------------------------
procedure TPHXOpenGL_RenderTargetCopyTexImage.SaveToBitmap(Bitmap: TPHXBitmap);
var Buffer  : PByte;
var SrcPixel: PCardinal;
var DstPixel: PCardinal;
var x,y:integer;
begin
  if FHandleTexture = 0 then Exit;

  Bitmap.Width := Width;
  Bitmap.Height:= Height;
  Bitmap.Format:= pfRGBA;

  glBindTexture(GL_TEXTURE_2D, FHandleTexture);

  GetMem(Buffer, Width * Height * 4);
  try
    glGetTexImage(GL_TEXTURE_2D, 0, GL_BGRA, GL_UNSIGNED_BYTE, Buffer);

    SrcPixel:= PCardinal(Buffer);
    for y := 0 to Height-1 do
    begin
      DstPixel:= PCardinal(Bitmap.Scanline(Y));
      for x := 0 to Width-1 do
      begin
        DstPixel^ := SrcPixel^;

        inc(SrcPixel);
        inc(DstPixel);
      end;
    end;
  finally
    FreeMem(Buffer, Width * Height * 4);
  end;
  glBindTexture(GL_TEXTURE_2D, 0);
end;



{$REGION 'TPHXOpenGL_Texture'}

// TPHXOpenGLTexture
//==============================================================================
constructor TPHXOpenGL_Texture.Create;
begin
  inherited Create;
  FHandle:= GL_NONE;
end;

//------------------------------------------------------------------------------
procedure TPHXOpenGL_Texture.Unload;
begin
  if FHandle <> GL_NONE then
  begin
    glDeleteTextures(1, @FHandle);

    FHandle:= GL_NONE;
  end;
end;

//------------------------------------------------------------------------------
procedure TPHXOpenGL_Texture.Build;
var MaxTextureSize: Integer;
begin
  {$IFNDEF FPC}
  if dglOpenGL.GL_LibHandle = nil then
  begin
    //TPHXLog.Error(' TPHXOpenGLTexture.Build', _LINE_, 'OpenGL not initialized');
    Exit;
  end;
  {$ENDIF}

  glGetIntegerv(GL_MAX_TEXTURE_SIZE, @MaxTextureSize);

  if FHandle <> GL_NONE then
  begin
    glDeleteTextures(1, @FHandle);

    FHandle:= GL_NONE;
  end;

  if(Format = pfNone) then
  begin
    //TPHXLog.Warn(' TPHXOpenGLTexture.Build', _LINE_, 'No texture format specified');
    Exit;
  end;

  if(Pixels = nil) then
  begin
    //TPHXLog.Error(' TPHXOpenGLTexture.Build', _LINE_, 'No texture data');
    Exit;
  end;

  if(Width > MaxTextureSize) or (Height > MaxTextureSize) then
  begin
    TPHXLogger.Error('TPHXOpenGL_Texture.Build', 'Texture size %dx%d is larger then maximum texture size (%d) ', [Width, Height, MaxTextureSize]);

    Exit;
  end;
//  TPHXLog.Info(' TPHXOpenGLTexture.Build', _LINE_, 'B ', [Width, Height, MaxTextureSize]);

  // Generate a new texture id
  glGenTextures(1, @FHandle);

  if (FHandle = 0)then
  begin
    TPHXLogger.Error('TPHXOpenGL_Texture.Build', 'Failed to generate texture.');
  end;
  // Bind the texture
  glBindTexture(GL_TEXTURE_2D, FHandle);

  // Activate mipmap generation if required
 // if Settings.Mipmaps then
 // begin
//      glTexParameteri( GL_TEXTURE_2D, GL_GENERATE_MIPMAP, GL_TRUE );
//    end else
//    begin
//      glTexParameteri( GL_TEXTURE_2D, GL_GENERATE_MIPMAP, GL_FALSE);
 // end;

  // Upload the texture data to OpenGL
  glTexImage2D(GL_TEXTURE_2D, 0, glInternalPixelFormat[Format], Width, Height, 0, glPixelFormat[Format], GL_UNSIGNED_BYTE, Pixels);

  // Set the texture wraps
  glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_S, glTextureWrap[Settings.WrapS] );
  glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_T, glTextureWrap[Settings.WrapT] );

  // Set the texture magnification function
  glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MAG_FILTER, glTextureFilter[Settings.FilterMag]);

  // If using mipmaps all texture minifications is supported...
  if Settings.Mipmaps then
  begin
    glGenerateMipmap(GL_TEXTURE_2D);

    glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, glTextureFilter[Settings.FilterMin] );
  end else
  begin
    // .. else convert the filter to the corresponging linear filters
    if Settings.FilterMin in [tfNearest, tfNearestMipmapNearest, tfNearestMipmapLinear] then
    begin
      glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, GL_NEAREST)
    end else
    begin
      glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, GL_LINEAR);
    end;
  end;
end;

{$ENDREGION}

{$REGION 'TPHXOpenGL_Buffer'}

// TPHXOpenGL_Buffer
//==============================================================================
constructor TPHXOpenGL_Buffer.Create(ARenderer: TPHXOpenGL_Renderer);
begin
  inherited Create;

  FRenderer:= ARenderer;

  FCapacityVertex:= BufferCapacityVertices;
  FCapacityIndex := BufferCapacityIndices;

  // Create the vertex and index buffers
  if not CreateBuffers then
  begin
    raise Exception.Create('Failed to create index and vertex buffers');
  end;
end;

//------------------------------------------------------------------------------
destructor TPHXOpenGL_Buffer.Destroy;
begin
  DestroyBuffers;
  inherited;
end;

//---------------------------------------------------------------------------
function TPHXOpenGL_Buffer.CreateBuffers: Boolean;
begin
 glGenBuffers(1, @FBufferVertex);
 glBindBuffer(GL_ARRAY_BUFFER, FBufferVertex);
 glBufferData(GL_ARRAY_BUFFER, SizeOf(TPHXVertex) * FCapacityVertex, nil, GL_STATIC_DRAW);

 glGenBuffers(1, @FBufferIndex);
 glBindBuffer(GL_ELEMENT_ARRAY_BUFFER, FBufferIndex);
 glBufferData(GL_ELEMENT_ARRAY_BUFFER, SizeOf(TPHXIndex) * FCapacityIndex, nil, GL_STATIC_DRAW);

 Result:= (glGetError = GL_NO_ERROR) and (FBufferVertex <> 0) and (FBufferIndex <> 0);
end;

//---------------------------------------------------------------------------
procedure TPHXOpenGL_Buffer.DestroyBuffers;
begin
  if FBufferIndex <> GL_NONE then
  begin
    glBindBuffer(GL_ELEMENT_ARRAY_BUFFER, GL_NONE);
    glDeleteBuffers(1, @FBufferIndex);

    FBufferIndex:= GL_NONE;
  end;

  if FBufferVertex <> GL_NONE then
  begin
    glBindBuffer(GL_ARRAY_BUFFER, GL_NONE);
    glDeleteBuffers(1, @FBufferVertex);

    FBufferVertex:= GL_NONE;
  end;
end;

//---------------------------------------------------------------------------
procedure TPHXOpenGL_Buffer.Resize(const VertexCapacity: Integer; const IndexCapacity: Integer);
begin
  if FCapacityVertex <> VertexCapacity then
  begin
    FCapacityVertex:= VertexCapacity;

    glBindBuffer(GL_ARRAY_BUFFER, FBufferVertex);
    glBufferData(GL_ARRAY_BUFFER, SizeOf(TPHXVertex) * FCapacityVertex, nil, GL_STATIC_DRAW);
  end;

  if FCapacityIndex <> IndexCapacity then
  begin
    FCapacityIndex := IndexCapacity;

    glBindBuffer(GL_ELEMENT_ARRAY_BUFFER, FBufferIndex);
    glBufferData(GL_ELEMENT_ARRAY_BUFFER, SizeOf(TPHXIndex) * FCapacityIndex, nil, GL_STATIC_DRAW);
  end;
end;

//---------------------------------------------------------------------------
procedure TPHXOpenGL_Buffer.Bind;
begin
  glBindBuffer(GL_ARRAY_BUFFER, FBufferVertex);

  glBindBuffer(GL_ELEMENT_ARRAY_BUFFER, FBufferIndex);
end;

//---------------------------------------------------------------------------
procedure TPHXOpenGL_Buffer.Upload;
begin
  if (Vertices.Count > FCapacityVertex) or (Indices.Count > FCapacityIndex) then
  begin
    Resize(Vertices.Count, Indices.Count);
  end;


 // Assert(Vertices.Count <= BufferCapacityVertices);
  //Assert(Indices .Count <= BufferCapacityIndices);

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
end;

{$ENDREGION}


{$REGION 'TPHXOpenGLRenderer'}

// TPHXOpenGLRenderer
//==============================================================================
constructor TPHXOpenGL_Renderer.Create;
begin
  inherited;
  FBlending:= bmNormal;
end;

//------------------------------------------------------------------------------
destructor TPHXOpenGL_Renderer.Destroy;
begin

  inherited;
end;

//------------------------------------------------------------------------------
function TPHXOpenGL_Renderer.GetName: String;
begin
  Result:= 'Phoenix.OpenGL';
end;
        (*
//------------------------------------------------------------------------------
function TPHXOpenGL_Renderer.GetRenderer: String;
var Renderer: AnsiString;
begin
//  {$IFNDEF FPC}
  if (dglOpenGL.GL_LibHandle <> nil) then
 // {$ELSE}
 // if (GL.LibGL <> 0) then
 // {$ENDIF}
  begin
     Renderer:= glGetString(GL_RENDERER);

     Result:= String(Renderer);
  end else
  begin
    Result:= '';
  end;
end;

//------------------------------------------------------------------------------
function TPHXOpenGL_Renderer.GetVendor: String;
var Vendor: AnsiString;
begin
 // {$IFNDEF FPC}
  if (dglOpenGL.GL_LibHandle <> nil) then
 // {$ELSE}
 // if (GL.LibGL <> 0) then
 // {$ENDIF}
  begin
    Vendor:= glGetString(GL_VENDOR);

    Result:= String(Vendor);
  end else
  begin
    Result:= '';
  end;
end;
         *)
//------------------------------------------------------------------------------
function TPHXOpenGL_Renderer.GetVersion: String;
var Version: AnsiString;
begin
 // {$IFNDEF FPC}
  if (dglOpenGL.GL_LibHandle <> nil) then
  //{$ELSE}
  //if (GL.LibGL <> 0) then
  //{$ENDIF}
  begin
     Version:= glGetString(GL_VERSION);

     Result:= String(Version);
  end else
  begin
    Result:= '';
  end;
end;

//------------------------------------------------------------------------------
function TPHXOpenGL_Renderer.GetTarget: TPHXProviderTarget;
begin
  Result:= ptOpenGL_3;
end;


//------------------------------------------------------------------------------
procedure TPHXOpenGL_Renderer.InitializeOpenGL;
begin
  SupportsFrameBufferObject:= dglCheckExtension('GL_EXT_framebuffer_object');
end;

//------------------------------------------------------------------------------
function TPHXOpenGL_Renderer.CreateRenderTarget: TPHXRenderTarget;
begin

  if SupportsFrameBufferObject then
  begin
    Result:= TPHXOpenGL_RenderTarget.Create;
  end else
  begin
    Result:= TPHXOpenGL_RenderTargetCopyTexImage.Create;
  end;

  // TODO: rendertargets
 // Result:= nil;
end;

//------------------------------------------------------------------------------
function TPHXOpenGL_Renderer.CreateCanvas: TPHXCanvas;
begin
  Result:= TPHXOpenGL_Canvas.Create(Self);
end;

//------------------------------------------------------------------------------
function TPHXOpenGL_Renderer.CreateEffect: TPHXEffect;
begin
  Result:= TPHXOpenGL_Effect.Create(Self);
end;

//------------------------------------------------------------------------------
function TPHXOpenGL_Renderer.CreateBuffer: TPHXBuffer;
begin
  Result:= TPHXOpenGL_Buffer.Create(Self);
end;

//------------------------------------------------------------------------------
function TPHXOpenGL_Renderer.CreateTexture: TPHXTexture;
begin
  Result:= TPHXOpenGL_Texture.Create;
end;

//------------------------------------------------------------------------------
procedure TPHXOpenGL_Renderer.SetRenderTarget(Target: TPHXRenderTarget);
begin
  if SupportsFrameBufferObject then
  begin
    TPHXOpenGL_RenderTarget.Bind( TPHXOpenGL_RenderTarget(Target) );
  end else
  begin
    if Assigned(Target) then
    begin
      //  glClearColor(1, 1, 1, 1);
      glClear(GL_COLOR_BUFFER_BIT or GL_DEPTH_BUFFER_BIT);

     // glPushAttrib(GL_COLOR_BUFFER_BIT or GL_PIXEL_MODE_BIT); // for GL_DRAW_BUFFER and GL_READ_BUFFER
      glDrawBuffer(GL_BACK);
      glReadBuffer(GL_BACK);

//      glClearColor(1, 1, 1, 1);
  //    glClear(GL_COLOR_BUFFER_BIT or GL_DEPTH_BUFFER_BIT);

    end else
    begin
    //  glPopAttrib(); // GL_COLOR_BUFFER_BIT | GL_DEPTH_BUFFER_BIT
    end;

  //  Result:= TPHXOpenGLRenderTargetCopyTexImage.Create;
  //1
  end;
end;

//------------------------------------------------------------------------------
procedure TPHXOpenGL_Renderer.SetViewport(const Viewport: TViewport);
begin
  glViewport(Viewport.X, Viewport.Y, Viewport.Width, Viewport.Height);
end;

//------------------------------------------------------------------------------
procedure TPHXOpenGL_Renderer.SetClearColor(const Color: TColor4f);
begin
  glClearColor(Color.Red, Color.Green, Color.Blue, Color.Alpha);
end;

//------------------------------------------------------------------------------
procedure TPHXOpenGL_Renderer.SetBlending(const Value: TPHXBlendMode);
begin
  case Value of
    // No blending
    bmNormal:
    begin
      glDisable(GL_BLEND);
    end;
    // Alpha blending
    bmAlpha:
    begin
      // Enable blending if its disabled
      if FBlending = bmNormal then
      begin
        glEnable(GL_BLEND);
      end;

      glBlendFunc(GL_SRC_ALPHA, GL_ONE_MINUS_SRC_ALPHA);
    end;
    // Additive blending
    bmAdditive:
    begin
      // Enable blending if its disabled
      if FBlending = bmNormal then
      begin
        glEnable(GL_BLEND);
      end;

      glBlendFunc(GL_SRC_ALPHA, GL_ONE);
    end;

    // Multiplication
    bmMultiply:
    begin
      // Enable blending if its disabled
      if FBlending = bmNormal then
      begin
        glEnable(GL_BLEND);
      end;

      glBlendFunc(GL_ZERO, GL_SRC_COLOR);
    end;
    // Modulate
    bmModulate:
    begin
      // Enable blending if its disabled
      if FBlending = bmNormal then
      begin
        glEnable(GL_BLEND);
      end;

      glBlendFunc(GL_DST_COLOR, GL_SRC_COLOR);
    end;
    // Screen
    bmScreen:
    begin
      // Enable blending if its disabled
      if FBlending = bmNormal then
      begin
        glEnable(GL_BLEND);
      end;

       glBlendFunc(GL_ONE,GL_ONE_MINUS_SRC_COLOR);
    end;
  end;
  FBlending:= Value;
end;

//------------------------------------------------------------------------------
procedure TPHXOpenGL_Renderer.SetWireframe(const Value: Boolean);
begin
  if Value then
  begin
    glPolygonMode(GL_FRONT_AND_BACK, GL_LINE);
  end else
  begin
    glPolygonMode(GL_FRONT_AND_BACK, GL_FILL);
  end;
end;


//------------------------------------------------------------------------------
procedure TPHXOpenGL_Renderer.SetDepthTest(const Value: Boolean);
begin
  if Value then
  begin
    glEnable(GL_DEPTH_TEST);
  end else
  begin
    glDisable(GL_DEPTH_TEST);
  end;
end;

//------------------------------------------------------------------------------
procedure TPHXOpenGL_Renderer.SetDepthMask(const Value: Boolean);
begin
  if Value then
  begin
    {$IFDEF FPC}
    glDepthMask(ByteBool(GL_TRUE));
    {$ELSE}
    glDepthMask(ByteBool(GL_TRUE));
    {$ENDIF}
  end else
  begin
    {$IFDEF FPC}
    glDepthMask(ByteBool(GL_FALSE));
    {$ELSE}
    glDepthMask(ByteBool(GL_FALSE));
    {$ENDIF}
  end;
end;


//------------------------------------------------------------------------------
procedure TPHXOpenGL_Renderer.Update;
begin
end;

//------------------------------------------------------------------------------
procedure TPHXOpenGL_Renderer.Clear;
begin
  glClear( GL_COLOR_BUFFER_BIT or GL_DEPTH_BUFFER_BIT );//or GL_SCISSOR_BIT);
end;

//------------------------------------------------------------------------------
procedure TPHXOpenGL_Renderer.Flip;
begin
end;

{$ENDREGION}



initialization
//  TPHXFactory.Add(TPHXOpenGLRenderer);
finalization
//  TPHXFactory.Remove(TPHXOpenGLRenderer);
end.
