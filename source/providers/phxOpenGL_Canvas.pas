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
unit phxOpenGL_Canvas;
//< Canvas implementation for OpenGL renderers

interface

{$I ../phxConfig.inc}

uses
  SysUtils, Classes,

  dglOpenGL,

  phxLogger,
  phxTypes,
  phxClasses,
  phxDevice,
  phxCanvas,

  phxOpenGL,
  phxOpenGL_Shader;

type

// Attribute locations
//---------------------------------------------------------------------------
TShaderAttributes = record
  Position : GLint;
  Texcoord : GLint;
  Color    : GLint;
end;

// Uniform locations
//---------------------------------------------------------------------------
TShaderUniforms = record
  Viewsize : GLint;
  Texture  : GLint;
  Transform: GLint;
end;

//---------------------------------------------------------------------------
TShaderProg = record
  // Program handle
  Handle: GLuint;
  // Attribute locations
  Attrib: TShaderAttributes;
  // Uniform locations
  Uniform: TShaderUniforms;
end;

// Canvas implementation for OpenGL
//---------------------------------------------------------------------------
TPHXOpenGL_Canvas = class(TPHXCanvas)
  private
    FRenderer: TPHXOpenGL_Renderer;

    // Scales pixel coordinates to virtual device coordinates (-1.0 to 1.0)
    ViewportScale: TVector2f;

    FClipEnabled: Boolean;

    // Handle for the vertex buffer
    FBufferVertex: GLuint;
    // Handle for the index buffer
    FBufferIndex : GLuint;

    // The vertex shader
    FVertShader: GLUint;
    // Fragment shader for the color shader
    FFragShaderColor: GLuint;
    // Fragment shader for the texture shader
    FFragShaderTexture: GlUint;

    // Shader program for colored programs
    FProgColored: TShaderProg;
    // Shader program for textured programs
    FProgTextured: TShaderProg;

    // The active program
    FActive: GLuint;
    // Attribute locations
    FAttrib_Position : GLint;
    FAttrib_Texcoord : GLint;
    FAttrib_Color    : GLint;

    // Change the active program
    procedure ActivateProg(const Prog: TShaderProg);

    // Create the index and vertex buffers
    function CreateBuffers: Boolean;
    function CreateShaders: Boolean;
    function CreatePrograms: Boolean;

    // Destroy the index buffers
    procedure DestroyBuffers;
    procedure DestroyShaders;
    procedure DestroyPrograms;
    procedure Notification(Notification: TPHXNotification);
  protected
    procedure FlushCanvas; override;
  public
    constructor Create(ARenderer: TPHXOpenGL_Renderer);
    destructor Destroy; override;

    procedure SetClipEnabled(const Enabled: Boolean); override;
    procedure SetClipRect(const Rect: TRecti); override;
    procedure SetClipRect(const Rect: TRectf); override;

    // The opengl renderer
    property Renderer: TPHXOpenGL_Renderer read FRenderer;
  end;


implementation

const
// gl_Position =  vec4( (position.x - viewsize.x) / viewsize.x, -(position.y - viewsize.y) / viewsize.y , position.z, 1.0);'         +#13#10+

// Vertex shader for the canvas
//------------------------------------------------------------------------------
ShaderSource_Vert : AnsiString =
'#version 110'                                                          +#13#10+
''                                                                      +#13#10+
'uniform vec2 viewsize;'                                                +#13#10+
'uniform mat4 transform;'                                               +#13#10+

'attribute vec2 texcoord;'                                              +#13#10+
'attribute vec4 color;'                                                 +#13#10+
'attribute vec3 position;'                                              +#13#10+
''                                                                      +#13#10+
'varying vec2 var_Coord;'                                               +#13#10+
'varying vec4 var_Color;'                                               +#13#10+
''                                                                      +#13#10+
'void main()'                                                           +#13#10+
'{'                                                                     +#13#10+
'  vec4 spos;'                                                          +#13#10+
'  vec2 vpos;'                                                          +#13#10+
''                                                                      +#13#10+
'  spos = transform * vec4(position, 0);'                               +#13#10+
''                                                                      +#13#10+
// Converts from screen coordinates to normalized device coordinate
'  vpos.x =  (spos.x - viewsize.x) / viewsize.x;'                       +#13#10+
'  vpos.y = -(spos.y - viewsize.y) / viewsize.y;'                       +#13#10+
''                                                                      +#13#10+
'	 gl_Position =  vec4(vpos.xy, position.z, 1.0);'                      +#13#10+
''                                                                      +#13#10+
'  var_Coord = texcoord;'                                               +#13#10+
'  var_Color = color;'                                                  +#13#10+
'}'
;

// Fragment shader for the color shader
//------------------------------------------------------------------------------
ShaderSource_FragColored : AnsiString =
'#version 110'                                                          +#13#10+
''                                                                      +#13#10+
'varying vec2 var_Coord;'                                               +#13#10+
'varying vec4 var_Color;'                                               +#13#10+
''                                                                      +#13#10+
'void main()'                                                           +#13#10+
'{'                                                                     +#13#10+
// Alpha testing (0.003 = 1/255)
'    if (var_Color.w < 0.00392156862745098039) discard;'                +#13#10+
''                                                                      +#13#10+
'    gl_FragColor = var_Color;'                                         +#13#10+
'}'
;

// Fragment shader for the texture shader
//------------------------------------------------------------------------------
ShaderSource_Frag_Textured : AnsiString =
'#version 110'                                                          +#13#10+
''                                                                      +#13#10+
'uniform sampler2D texture;'                                            +#13#10+
''                                                                      +#13#10+
'varying vec2 var_Coord;'                                               +#13#10+
'varying vec4 var_Color;'                                               +#13#10+
''                                                                      +#13#10+
'void main()'                                                           +#13#10+
'{'                                                                     +#13#10+
// Alpha testing (0.003 = 1/255)
'    if (var_Color.w < 0.00392156862745098039) discard;'                +#13#10+
''                                                                      +#13#10+
'    gl_FragColor = var_Color * texture2D(texture, var_Coord);'         +#13#10+
'}'
;






// TPHXOpenGL_Canvas
//==============================================================================
constructor TPHXOpenGL_Canvas.Create(ARenderer: TPHXOpenGL_Renderer);
begin
  inherited Create();

  TPHXNotifications.Add(Notification);

  FRenderer:= ARenderer;

  // Create the vertex and index buffers
  if not CreateBuffers then
  begin
    raise Exception.Create('Failed to create buffers');
  end;

  if not CreateShaders then
  begin
    raise Exception.Create('Failed to create shaders');
  end;

  if not CreatePrograms then
  begin
    raise Exception.Create('Failed to create programs');
  end;
end;

//---------------------------------------------------------------------------
destructor TPHXOpenGL_Canvas.Destroy;
begin
  TPHXNotifications.Remove(Notification);

  DestroyBuffers;
  DestroyPrograms;
  DestroyShaders;

  inherited;
end;

//------------------------------------------------------------------------------
procedure TPHXOpenGL_Canvas.Notification(Notification: TPHXNotification);
begin
  if Notification = dnContextCreated then
  begin
    // Create the vertex and index buffers
    if not CreateBuffers then
    begin
      raise Exception.Create('Failed to create buffers');
    end;

    if not CreateShaders then
    begin
      raise Exception.Create('Failed to create shaders');
    end;

    if not CreatePrograms then
    begin
      raise Exception.Create('Failed to create programs');
    end;
  end;
end;


//---------------------------------------------------------------------------
function TPHXOpenGL_Canvas.CreateBuffers: Boolean;
begin
 glGenBuffers(1, @FBufferVertex);
 glBindBuffer(GL_ARRAY_BUFFER, FBufferVertex);
 glBufferData(GL_ARRAY_BUFFER, SizeOf(TPHXVertex) * CanvasCapacityVertices, nil, GL_STREAM_DRAW);

 glGenBuffers(1, @FBufferIndex);
 glBindBuffer(GL_ELEMENT_ARRAY_BUFFER, FBufferIndex);
 glBufferData(GL_ELEMENT_ARRAY_BUFFER, SizeOf(TPHXIndex) * CanvasCapacityIndices, nil, GL_STREAM_DRAW);

 Result:= (glGetError = GL_NO_ERROR) and (FBufferVertex <> 0) and (FBufferIndex <> 0);
end;

//---------------------------------------------------------------------------
procedure TPHXOpenGL_Canvas.DestroyBuffers;
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
function TPHXOpenGL_Canvas.CreateShaders: Boolean;
begin
  FVertShader:= CompileShader(GL_VERTEX_SHADER_ARB, ShaderSource_Vert);

  FFragShaderColor  := CompileShader(GL_FRAGMENT_SHADER_ARB, ShaderSource_FragColored);
  FFragShaderTexture:= CompileShader(GL_FRAGMENT_SHADER_ARB, ShaderSource_Frag_Textured);

  Result:= (glGetError = GL_NO_ERROR) and (FVertShader <> 0) and (FFragShaderColor <> 0) and (FFragShaderTexture <> 0);
end;

//---------------------------------------------------------------------------
procedure TPHXOpenGL_Canvas.DestroyShaders;
begin
  if FVertShader <> GL_NONE then
  begin
    glDeleteShader(FVertShader);
    FVertShader:= GL_NONE;
  end;

  if FFragShaderColor <> GL_NONE then
  begin
    glDeleteShader(FFragShaderColor);

    FFragShaderColor:= GL_NONE;
  end;

  if FFragShaderTexture <> GL_NONE then
  begin
    glDeleteShader(FFragShaderTexture);

    FFragShaderTexture:= GL_NONE;
  end;

end;
    //FProgColored: TShaderProg;
   // FProgTextured: TShaderProg;

//---------------------------------------------------------------------------
function TPHXOpenGL_Canvas.CreatePrograms: Boolean;
begin
  // Shader program for the color shader
  FProgColored.Handle:= CompileProgram(FVertShader, FFragShaderColor);
    // Attribute locations
    FProgColored.Attrib.Position := glGetAttribLocation(FProgColored.Handle, 'position');
    FProgColored.Attrib.Texcoord := glGetAttribLocation(FProgColored.Handle, 'texcoord');
    FProgColored.Attrib.Color    := glGetAttribLocation(FProgColored.Handle, 'color');
    // Uniform locations
    FProgColored.Uniform.Viewsize := glGetUniformLocation(FProgColored.Handle, 'viewsize');
    FProgColored.Uniform.Transform:= glGetUniformLocation(FProgColored.Handle, 'transform');
    FProgColored.Uniform.Texture := GL_NONE;

  // Shader program for the textured shader
  FProgTextured.Handle:= CompileProgram(FVertShader, FFragShaderTexture);
    // Attribute locations
    FProgTextured.Attrib.Position := glGetAttribLocation(FProgTextured.Handle, 'position');
    FProgTextured.Attrib.Texcoord := glGetAttribLocation(FProgTextured.Handle, 'texcoord');
    FProgTextured.Attrib.Color    := glGetAttribLocation(FProgTextured.Handle, 'color');
    // Uniform locations
    FProgTextured.Uniform.Viewsize := glGetUniformLocation(FProgTextured.Handle, 'viewsize');
    FProgTextured.Uniform.Transform:= glGetUniformLocation(FProgTextured.Handle, 'transform');
    FProgTextured.Uniform.Texture  := glGetUniformLocation(FProgTextured.Handle, 'texture');

  Result:= (glGetError = GL_NO_ERROR) and (FProgColored.Handle <> 0) and (FProgTextured.Handle <> 0);
end;

//---------------------------------------------------------------------------
procedure TPHXOpenGL_Canvas.DestroyPrograms;
begin
  if FProgTextured.Handle <> GL_NONE then
  begin
    glDeleteProgram(FProgTextured.Handle);

    FProgTextured.Handle:= GL_NONE;
  end;

  if FProgColored.Handle <> GL_NONE then
  begin
    glDeleteProgram(FProgColored.Handle);

    FProgColored.Handle:= GL_NONE;
  end;
end;

//---------------------------------------------------------------------------
procedure TPHXOpenGL_Canvas.ActivateProg(const Prog: TShaderProg);
begin
  glUseProgram(Prog.Handle);

  if FActive <> Prog.Handle then
  begin
    FActive:= Prog.Handle;

    if FActive = FProgTextured.Handle then
    begin
      FAttrib_Position := FProgTextured.Attrib.Position;
      FAttrib_Texcoord := FProgTextured.Attrib.Texcoord;
      FAttrib_Color    := FProgTextured.Attrib.Color;
    end else
    begin
      FAttrib_Position := FProgColored.Attrib.Position;
      FAttrib_Texcoord := FProgColored.Attrib.Texcoord;
      FAttrib_Color    := FProgColored.Attrib.Color;
    end;
  end;
end;


//---------------------------------------------------------------------------
procedure TPHXOpenGL_Canvas.FlushCanvas;
var Viewport: array[0..3] of GLint;
begin
  Inc(FDrawCalls);

  Assert(Vertices.Count <= CanvasCapacityVertices);
  Assert(Indices .Count <= CanvasCapacityIndices);

  glGetIntegerv(GL_VIEWPORT, @Viewport[0]);
  ViewportScale.x:= Viewport[2] * 0.5;
  ViewportScale.y:= Viewport[3] * 0.5;

  // Upload verticies
  glBindBuffer(GL_ARRAY_BUFFER, FBufferVertex);
  if (Vertices.Count > 0) then
  begin
    glBufferSubData(GL_ARRAY_BUFFER, 0, SizeOf(TPHXVertex) *  Vertices.Count, Vertices.List);
  end;

  // Upload indices
  glBindBuffer(GL_ELEMENT_ARRAY_BUFFER, FBufferIndex);
  if (Indices.Count > 0) then
  begin
    glBufferSubData(GL_ELEMENT_ARRAY_BUFFER, 0, SizeOf(TPHXIndex) *  Indices.Count, Indices.List);
  end;

  FRenderer.SetBlending(Blending);

  // Draw using the textured shader
  if Texture <> nil then
  begin
    glActiveTexture(GL_TEXTURE0);
    glBindTexture(GL_TEXTURE_2D, TPHXOpenGL_Texture(Texture).Handle);

    ActivateProg(FProgTextured);

    glUniformMatrix4fv(FProgTextured.Uniform.Transform, 1, False, @Transform);

    glUniform1i (FProgTextured.Uniform.Texture , 0);
    glUniform2fv(FProgTextured.Uniform.Viewsize, 1, @ViewportScale);
  end else
  // Draw using the colored shader
  begin
    ActivateProg(FProgColored);

    glUniformMatrix4fv(FProgColored.Uniform.Transform, 1, False, @Transform);

    glUniform2fv(FProgColored.Uniform.Viewsize, 1, @ViewportScale);
  end;

  glVertexAttribPointer(FAttrib_Position, 3, GL_FLOAT, false, SizeOf(TPHXVertex), VertexOffsetPosition);
  glVertexAttribPointer(FAttrib_Texcoord, 2, GL_FLOAT, false, SizeOf(TPHXVertex), VertexOffsetTexCoord);
  glVertexAttribPointer(FAttrib_Color   , 4, GL_FLOAT, false, SizeOf(TPHXVertex), VertexOffsetColor);

  glEnableVertexAttribArray(FAttrib_Position);
  glEnableVertexAttribArray(FAttrib_Texcoord);
  glEnableVertexAttribArray(FAttrib_Color);

  // Draw the elements
  glDrawElements(glPrimitiveType[Primitive], Indices.Count, GL_UNSIGNED_INT, nil);
end;

//---------------------------------------------------------------------------
procedure TPHXOpenGL_Canvas.SetClipEnabled(const Enabled: Boolean);
begin
  if FClipEnabled <> Enabled then
  begin
    FClipEnabled:= Enabled;

    Flush;

    if Enabled then
    begin
      glEnable(GL_SCISSOR_TEST);
    end else
    begin
      glDisable(GL_SCISSOR_TEST);
    end;
  end;
end;

//---------------------------------------------------------------------------
procedure TPHXOpenGL_Canvas.SetClipRect(const Rect: TRectf);
var X, Y    : Integer;
var W, H    : Integer;
var Viewport: array[0..3] of GLint;
begin
  glGetIntegerv(GL_VIEWPORT, @Viewport[0]);

  X:= Round(Rect.Left);
  Y:= Round(Rect.Top);
  W:= Round(Rect.Right  - Rect.Left);
  H:= Round(Rect.Bottom - Rect.Top);

  glScissor(X, Viewport[3]-Y-H,  W , H);
end;

//---------------------------------------------------------------------------
procedure TPHXOpenGL_Canvas.SetClipRect(const Rect: TRecti);
var X, Y    : Integer;
var W, H    : Integer;
var Viewport: array[0..3] of GLint;
begin
  glGetIntegerv(GL_VIEWPORT, @Viewport[0]);

  X:= Rect.Left;
  Y:= Rect.Top;
  W:= Rect.Right  - Rect.Left;
  H:= Rect.Bottom - Rect.Top;

  glScissor(X, Viewport[3]-Y-H,  W , H);
end;












end.
