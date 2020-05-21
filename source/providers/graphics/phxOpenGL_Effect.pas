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
unit phxOpenGL_Effect;
//< Effect implementation for OpenGL renderers

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
  phxEffect,

  phxOpenGL,
  phxOpenGL_Shader;




type
// http://stackoverflow.com/questions/6099280/glsl-vertex-lighting-shader

// http://msdn.microsoft.com/en-us/library/bb464051.aspx
// http://blogs.msdn.com/b/shawnhar/archive/2010/04/22/effect-api-changes-in-xna-game-studio-4-0.aspx

// http://blogs.msdn.com/b/shawnhar/archive/2012/03/02/spritebatch-and-basiceffect-for-c-direct3d-11.aspx
// http://msdn.microsoft.com/en-us/library/microsoft.xna.framework.graphics.basiceffect_members.aspx

// http://xbox.create.msdn.com/en-US/education/catalog/sample/stock_effects


// Attribute locations
//---------------------------------------------------------------------------
TShaderAttributes = record
  Position : GLint;
  Texcoord : GLint;
  Normal   : GLint;
  Color    : GLint;
end;

// Uniform locations
//---------------------------------------------------------------------------
TShaderUniforms = record
  View   : GLint;
  World  : GLint;
  Proj   : GLint;
  Texture: GLint;

  NormalMatrix  : GLint;
  LightPosition : GLint;
  LightAmbient  : GLint;
  LightDiffuse  : GLint;
  LightSpecular : GLint;
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

//-----------------------------------------------------------------------------
TPHXOpenGL_Effect = class(TPHXEffect)
  private
    FRenderer: TPHXOpenGL_Renderer;

    // The vertex shader
    FVertShader: GLUint;
    FVertShaderLightDirectional: GLUint;

    // Fragment shader for the color shader
    FFragShaderColor: GLuint;
    // Fragment shader for the texture shader
    FFragShaderTexture: GlUint;

    // Shader program for the color shader
    FProgColored: TShaderProg;
    // Shader program for the textured shader
    FProgTextured: TShaderProg;

    // Shader program for the color shader
    FProgLightColored: TShaderProg;
    // Shader program for the textured shader
    FProgLightTextured: TShaderProg;

    FCurrent: TShaderProg;
          (*

     // Attribute locations
    FAttrib_Position : GLint;
    FAttrib_Texcoord : GLint;
    FAttrib_Normal   : GLint;
    FAttrib_Color    : GLint;

    // Uniform locations
    FUniform_View   : GLint;
    FUniform_World  : GLint;
    FUniform_Proj   : GLint;
    FUniform_Texture: GLint;

    FUniform_NormalMatrix  : GLint;
    FUniform_LightPosition : GLint;
    FUniform_LightAmbient  : GLint;
    FUniform_LightDiffuse  : GLint;
    FUniform_LightSpecular : GLint;

    *)
   // function GetShaderIndex: Integer;


    // Change the active program
    procedure ActivateProg(const Prog: TShaderProg);

    function CreateShaders: Boolean;
    function CreatePrograms: Boolean;

    procedure DestroyPrograms;
    procedure DestroyShaders;
    procedure SetupShader;
  protected
    procedure Changed; override;
  public
    constructor Create(ARenderer: TPHXOpenGL_Renderer);
    destructor Destroy; override;

    procedure Bind;

    procedure Render(Buffer: TPHXBuffer); overload; override;
    procedure Render(Buffer: TPHXBuffer; const Start, Count: Integer); overload; override;

    procedure RenderEx(Buffer: TPHXBuffer; Decl: TPHXVertexDeclaration); override;
  end;

implementation

(*

CommonVSOutput ComputeCommonVSOutputWithLighting(float4 position, float3 normal, uniform int numLights)
{
    CommonVSOutput vout;

    float4 pos_ws = mul(position, World);
    float3 eyeVector = normalize(EyePosition - pos_ws.xyz);
    float3 worldNormal = normalize(mul(normal, WorldInverseTranspose));

    ColorPair lightResult = ComputeLights(eyeVector, worldNormal, numLights);

    vout.Pos_ps = mul(position, WorldViewProj);
    vout.Diffuse = float4(lightResult.Diffuse, DiffuseColor.a);
    vout.Specular = lightResult.Specular;
    vout.FogFactor = ComputeFogFactor(position);

    return vout;
}
http://stackoverflow.com/questions/5823438/opengl-gl-normalmatrix


http://athile.net/library/blog/?p=582
*)

const

(**
http://www.opengl.org/discussion_boards/showthread.php/173241-GLSL-shader-for-replicating-fixed-func-pipeline

in vec2 vUV;
in vec3 vVertex;
in vec3 vNormal;

smooth out vec2 vTexCoord;
smooth out vec4 color;

uniform mat3 N;
uniform mat4 MV;
uniform mat4 MVP;
uniform vec4 lightPos;
uniform vec4 mat_ambient;
uniform vec4 mat_diffuse;
uniform vec4 mat_specular;
uniform float mat_shininess;

uniform vec4 light_ambient;
uniform vec4 light_diffuse;
uniform vec4 light_specular;

void main()
{
   vTexCoord = vUV;
   vec3 Nr = normalize(N*vNormal); //transform the normal vector by the normal matrix (inverse transpose of the modelview matrix)
   vec4 esPos = MV*vec4(vVertex,1);
   vec3 ecPosition3 = (vec3 (esPos.xyz)) / esPos.w;
   vec3 eye = vec3 (0.0, 0.0, 1.0);

   // Compute vector from surface to light position
   vec3 L = lightPos.xyz - ecPosition3;

   // Normalize the vector from surface to light position
   L = normalize(L);
   vec3 halfVector = normalize( eye + L);

   vec4 A = mat_ambient*light_ambient;
   float diffuse = max(dot(Nr,L),0.0);
   float pf=0;
   if (diffuse == 0.0)
   {
       pf = 0.0;
   }
   else
   {
       pf = max( pow(dot(Nr,halfVector), mat_shininess), 0.);

   }
   vec4 S = light_specular*mat_specular* pf;
   vec4 D = diffuse*mat_diffuse*light_diffuse;
   color = A + D + S;
   gl_Position = MVP*vec4(vVertex,1);
}


http://jonmacey.blogspot.de/2010/11/imitating-opengl-fixed-functionality_11.html

http://www.dhpoware.com/demos/glslVertexLighting.html
*)


// Vertex shader for OpenGL2.1
//------------------------------------------------------------------------------
ShaderSource_Vert : AnsiString =
'#version 110'                                                          +#13#10+
''                                                                      +#13#10+
'uniform mat4 view;'                                                    +#13#10+
'uniform mat4 world;'                                                   +#13#10+
'uniform mat4 proj;'                                                    +#13#10+

'attribute vec2 in_Texcoord;'                                           +#13#10+
'attribute vec4 in_Color;'                                              +#13#10+
'attribute vec3 in_Normal;'                                             +#13#10+
'attribute vec3 in_Position;'                                           +#13#10+
''                                                                      +#13#10+
'varying vec2 var_Coord;'                                               +#13#10+
'varying vec4 var_Color;'                                               +#13#10+
''                                                                      +#13#10+
'void main()'                                                           +#13#10+
'{'                                                                     +#13#10+
'	 gl_Position = proj * view * world * vec4(in_Position, 1.0);'         +#13#10+

'  var_Coord = in_Texcoord;'                                            +#13#10+
'  var_Color = in_Color;'                                               +#13#10+
'}'
;

// Per-vertex Blinn-Phong shader for a single point light source.
//------------------------------------------------------------------------------
//ShaderSource_Vert_Lighting : AnsiString =
//'';

(*

Per-vertex Blinn-Phong shader for a single point light source.

Specular reflections are calculated using a local viewer lighting model
(i.e., as if GL_LIGHT_MODEL_LOCAL_VIEWER is set to GL_TRUE).

[vert]

void main()
{
    gl_Position = gl_ModelViewProjectionMatrix * gl_Vertex;
    gl_TexCoord[0] = gl_MultiTexCoord0;

    vec4 eyeCoordPos = gl_ModelViewMatrix * gl_Vertex;
    vec3 pos = vec3(eyeCoordPos) / eyeCoordPos.w;
    vec3 l = vec3(gl_LightSource[0].position) - pos;

    float d = length(l);
    float atten = 1.0 / (gl_LightSource[0].constantAttenuation +
                         gl_LightSource[0].linearAttenuation * d +
                         gl_LightSource[0].quadraticAttenuation * d * d);

    l = normalize(l);

    vec3 n = normalize(gl_NormalMatrix * gl_Normal);
    vec3 v = normalize(-pos);
    vec3 h = normalize(l + v);

    float nDotL = max(0.0, dot(n, l));
    float nDotH = max(0.0, dot(n, h));
    float power = (nDotL == 0.0) ? 0.0 : pow(nDotH, gl_FrontMaterial.shininess);

    vec4 ambient = gl_FrontLightProduct[0].ambient * atten;
    vec4 diffuse = gl_FrontLightProduct[0].diffuse * nDotL * atten;
    vec4 specular = gl_FrontLightProduct[0].specular * power * atten;

    gl_FrontColor = gl_FrontLightModelProduct.sceneColor + ambient + diffuse + specular;
}

*)

(*
Per-vertex Blinn-Phong lighting shader for a single directional light source.

Specular reflections are calculated using a local viewer lighting model
(i.e., as if GL_LIGHT_MODEL_LOCAL_VIEWER is set to GL_TRUE).

[vert]

void main()
{
    gl_Position = gl_ModelViewProjectionMatrix * gl_Vertex;
    gl_TexCoord[0] = gl_MultiTexCoord0;

    vec4 eyeCoordPos = gl_ModelViewMatrix * gl_Vertex;
    vec3 pos = vec3(eyeCoordPos) / eyeCoordPos.w;

    vec3 l = normalize(gl_LightSource[0].position.xyz);
    vec3 n = normalize(gl_NormalMatrix * gl_Normal);
    vec3 v = normalize(-pos);
    vec3 h = normalize(l + v);

    float nDotL = max(0.0, dot(n, l));
    float nDotH = max(0.0, dot(n, h));
    float power = (nDotL == 0.0) ? 0.0 : pow(nDotH, gl_FrontMaterial.shininess);

    vec4 ambient = gl_FrontLightProduct[0].ambient;
    vec4 diffuse = gl_FrontLightProduct[0].diffuse * nDotL;
    vec4 specular = gl_FrontLightProduct[0].specular * power;

    gl_FrontColor = gl_FrontLightModelProduct.sceneColor + ambient + diffuse + specular;
}

[frag]

uniform sampler2D colorMap;

void main()
{
    gl_FragColor = gl_Color * texture2D(colorMap, gl_TexCoord[0].st);
}
*)

// Per-vertex Blinn-Phong lighting shader for a single directional light source.
//------------------------------------------------------------------------------
ShaderSource_Vert_Lighting_Directional : AnsiString =
'#version 110'                                                          +#13#10+
''                                                                      +#13#10+
'uniform mat4 view;'                                                    +#13#10+
'uniform mat4 world;'                                                   +#13#10+
'uniform mat4 proj;'                                                    +#13#10+

'attribute vec3 in_Position;'                                           +#13#10+
'attribute vec3 in_Normal;'                                             +#13#10+
'attribute vec2 in_Texcoord;'                                           +#13#10+
'attribute vec4 in_Color;'                                              +#13#10+

'uniform mat3 in_NormalMatrix;'                                         +#13#10+
'uniform vec3 in_LightPosition;'                                        +#13#10+
'uniform vec4 in_LightAmbient;'                                        +#13#10+
'uniform vec4 in_LightDiffuse;'                                        +#13#10+
'uniform vec4 in_LightSpecular;'                                        +#13#10+
''                                                                      +#13#10+
'varying vec2 var_Coord;'                                               +#13#10+
'varying vec4 var_Color;'                                               +#13#10+
''                                                                      +#13#10+
'void main()'                                                           +#13#10+
'{'                                                                     +#13#10+
'	 gl_Position = proj * view * world * vec4(in_Position, 1.0);'         +#13#10+
''                                                                      +#13#10+
'  vec4 eye = view * world * vec4(in_Position, 1.0);'                   +#13#10+
'  vec3 pos = vec3(eye) / eye.w;'                                       +#13#10+

'  vec3 l = normalize(in_LightPosition);'                                 +#13#10+
'  vec3 n = normalize(in_NormalMatrix * in_Normal);'                    +#13#10+
'  vec3 v = normalize(-pos);'                                           +#13#10+
'  vec3 h = normalize(l + v);'                                          +#13#10+

'  float nDotL = max(0.0, dot(n, l));'                                  +#13#10+
'  float nDotH = max(0.0, dot(n, h));'                                  +#13#10+
'  float power = (nDotL == 0.0) ? 0.0 : pow(nDotH, 50.0);'                +#13#10+  // 50 = gl_FrontMaterial.shininess

'  vec4 ambient = in_LightAmbient;'                                     +#13#10+
'  vec4 diffuse = in_LightDiffuse * nDotL;'                             +#13#10+
'  vec4 specular = in_LightSpecular * power;'                           +#13#10+

'  var_Coord = in_Texcoord;'                                            +#13#10+
'  var_Color = in_Color + ambient + diffuse + specular;'                +#13#10+
'}'
;


// http://www.swiftless.com/tutorials/glsl/6_materials.html







// Fragment shader for OpenGL2.1
//------------------------------------------------------------------------------
ShaderSource_Frag_Colored : AnsiString =
'#version 110'                                                          +#13#10+
''                                                                      +#13#10+
'varying vec2 var_Coord;'                                               +#13#10+
'varying vec4 var_Color;'                                               +#13#10+
''                                                                      +#13#10+
'void main()'                                                           +#13#10+
'{'                                                                     +#13#10+
'    gl_FragColor = var_Color;'                                         +#13#10+
'}'
;
// Fragment shader for OpenGL2.1
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
'    gl_FragColor = var_Color * texture2D(texture, var_Coord);'         +#13#10+
'}'
;





(*
uniform vec3 LightPosition; //already transformed by view matrix

vec3 N = NormalMatrix * normal;
vec4 P = Modelview * position;  //no view
vec3 L = normalize(LightPosition - P.xyz);
float df = max(0.0, dot(N, L.xyz));
vec3 final_color = AmbientMaterial + df * DiffuseMaterial;
colorVarying = vec4(final_color,1);
gl_Position = Projection * Modelview * position;

*)


//const ShaderSource: array[0..4] of AnsiString = (
//  {$i Effect_VSBasic.inc"
// ShaderSource_Frag_Colored
// ShaderSource_Frag_Colored

//------------------------------------------------------------------------------
const ShaderPrograms: array[0..31] of Integer =(
    0,      // basic
    1,      // no fog
    2,      // vertex color
    3,      // vertex color, no fog
    4,      // texture
    5,      // texture, no fog
    6,      // texture + vertex color
    7,      // texture + vertex color, no fog

    8,      // vertex lighting
    8,      // vertex lighting, no fog
    9,      // vertex lighting + vertex color
    9,      // vertex lighting + vertex color, no fog
    10,     // vertex lighting + texture
    10,     // vertex lighting + texture, no fog
    11,     // vertex lighting + texture + vertex color
    11,     // vertex lighting + texture + vertex color, no fog

    12,     // one light
    12,     // one light, no fog
    13,     // one light + vertex color
    13,     // one light + vertex color, no fog
    14,     // one light + texture
    14,     // one light + texture, no fog
    15,     // one light + texture + vertex color
    15,     // one light + texture + vertex color, no fog

    16,     // pixel lighting
    16,     // pixel lighting, no fog
    17,     // pixel lighting + vertex color
    17,     // pixel lighting + vertex color, no fog
    18,     // pixel lighting + texture
    18,     // pixel lighting + texture, no fog
    19,     // pixel lighting + texture + vertex color
    19      // pixel lighting + texture + vertex color, no fog
);

(*

:GetCurrentShaderPermutation()
{
    int permutation = 0;

    // Use optimized shaders if fog is disabled.
    if (!fog.enabled)
    {
        permutation += 1;
    }

    // Support vertex coloring?
    if (vertexColorEnabled)
    {
        permutation += 2;
    }

    // Support texturing?
    if (textureEnabled)
    {
        permutation += 4;
    }

    if (lightingEnabled)
    {
        if (preferPerPixelLighting)
        {
            // Do lighting in the pixel shader.
            permutation += 24;
        }
        else if (!lights.lightEnabled[1] && !lights.lightEnabled[2])
        {
            // Use the only-bother-with-the-first-light shader optimization.
            permutation += 16;
        }
        else
        {
            // Compute all three lights in the vertex shader.
            permutation += 8;
        }
    }

    return permutation;
}


function TPHXOpenGL_Effect.GetShaderIndex: Integer;
begin
  Result:= 0;

  // Support texturing?
  if (Texture <> nil) then Inc(Result, 4);

  //LightingEnabled
end; *)

// TPHXOpenGL_Effect
//==============================================================================
constructor TPHXOpenGL_Effect.Create(ARenderer: TPHXOpenGL_Renderer);
begin
  inherited Create;

  FRenderer:= ARenderer;

  if not CreateShaders then
  begin
    raise Exception.Create('Failed to create shaders');
  end;
  if not CreatePrograms then
  begin
    raise Exception.Create('Failed to create programs');
  end;
end;


//------------------------------------------------------------------------------
destructor TPHXOpenGL_Effect.Destroy;
begin
  DestroyPrograms;
  DestroyShaders;
  inherited;
end;

//---------------------------------------------------------------------------
function TPHXOpenGL_Effect.CreateShaders: Boolean;
begin
  FVertShader                := CompileShader(GL_VERTEX_SHADER_ARB, ShaderSource_Vert);
  FVertShaderLightDirectional:= CompileShader(GL_VERTEX_SHADER_ARB, ShaderSource_Vert_Lighting_Directional);

  FFragShaderColor  := CompileShader(GL_FRAGMENT_SHADER_ARB, ShaderSource_Frag_Colored);
  FFragShaderTexture:= CompileShader(GL_FRAGMENT_SHADER_ARB, ShaderSource_Frag_Textured);

  Result:= glGetError = GL_NONE;
end;

//---------------------------------------------------------------------------
procedure TPHXOpenGL_Effect.DestroyShaders;
begin
  if FVertShader <> GL_NONE then
  begin
    glDeleteShader(FVertShader);
    FVertShader:= GL_NONE;
  end;
  if FVertShaderLightDirectional <> GL_NONE then
  begin
    glDeleteShader(FVertShaderLightDirectional);
    FVertShaderLightDirectional:= GL_NONE;
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

//---------------------------------------------------------------------------
function TPHXOpenGL_Effect.CreatePrograms: Boolean;
begin
  FProgColored .Handle:= CompileProgram(FVertShader, FFragShaderColor);
  FProgTextured.Handle:= CompileProgram(FVertShader, FFragShaderTexture);

  // Shader program for the color shader
  FProgLightColored .Handle:= CompileProgram(FVertShaderLightDirectional, FFragShaderColor);
  FProgLightTextured.Handle:= CompileProgram(FVertShaderLightDirectional, FFragShaderTexture);

  Result:= glGetError = GL_NONE;
end;


//---------------------------------------------------------------------------
procedure TPHXOpenGL_Effect.DestroyPrograms;
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


// Change the active program
//---------------------------------------------------------------------------
procedure TPHXOpenGL_Effect.ActivateProg(const Prog: TShaderProg);
begin
  // Link the program
  glUseProgram(Prog.Handle);

  if FCurrent.Handle <> Prog.Handle then
  begin
    FCurrent:= Prog;

    // Attribute locations
    FCurrent.Attrib.Position := glGetAttribLocation(FCurrent.Handle, 'in_Position');
    FCurrent.Attrib.Texcoord := glGetAttribLocation(FCurrent.Handle, 'in_Texcoord');
    FCurrent.Attrib.Normal   := glGetAttribLocation(FCurrent.Handle, 'in_Normal');
    FCurrent.Attrib.Color    := glGetAttribLocation(FCurrent.Handle, 'in_Color');

    // Uniform locations
    FCurrent.Uniform.View   := glGetUniformLocation(FCurrent.Handle, 'view');
    FCurrent.Uniform.World  := glGetUniformLocation(FCurrent.Handle, 'world');
    FCurrent.Uniform.Proj   := glGetUniformLocation(FCurrent.Handle, 'proj');
    FCurrent.Uniform.Texture:= glGetUniformLocation(FCurrent.Handle, 'texture');

    FCurrent.Uniform.NormalMatrix := glGetUniformLocation(FCurrent.Handle, 'in_NormalMatrix');
    FCurrent.Uniform.LightPosition:= glGetUniformLocation(FCurrent.Handle, 'in_LightPosition');
    FCurrent.Uniform.LightAmbient := glGetUniformLocation(FCurrent.Handle, 'in_LightAmbient');
    FCurrent.Uniform.LightDiffuse := glGetUniformLocation(FCurrent.Handle, 'in_LightDiffuse');
    FCurrent.Uniform.LightSpecular:= glGetUniformLocation(FCurrent.Handle, 'in_LightSpecular');
  end;
end;

//---------------------------------------------------------------------------
procedure TPHXOpenGL_Effect.Bind;
var NormalMatrix: TMatrix3f;
begin
  if LightingEnabled then
  begin
    NormalMatrix:= Matrix_InverseTranspose(Matrix3f(Matrix_Multiply(View, World)));

    // Draw using the textured shader
    if Texture <> nil then
    begin
      glActiveTexture(GL_TEXTURE0);
      glBindTexture(GL_TEXTURE_2D, TPHXOpenGL_Texture(Texture).Handle);

      ActivateProg(FProgLightTextured);

      glUniformMatrix4fv(FCurrent.Uniform.View    , 1, false, @View);
      glUniformMatrix4fv(FCurrent.Uniform.World   , 1, false, @World);
      glUniformMatrix4fv(FCurrent.Uniform.Proj    , 1, false, @Projection);
      glUniform1i       (FCurrent.Uniform.Texture , 0);

      glUniformMatrix3fv(FCurrent.Uniform.NormalMatrix  , 1, false, @NormalMatrix);
      glUniform3fv      (FCurrent.Uniform.LightPosition , 1, @LightPosition);
      glUniform4fv      (FCurrent.Uniform.LightAmbient  , 1, @LightAmbient);
      glUniform4fv      (FCurrent.Uniform.LightDiffuse  , 1, @LightDiffuse);
      glUniform4fv      (FCurrent.Uniform.LightSpecular , 1, @LightSpecular);
    end;

  end else
  begin
    // Draw using the textured shader
    if Texture <> nil then
    begin
      glActiveTexture(GL_TEXTURE0);
      glBindTexture(GL_TEXTURE_2D, TPHXOpenGL_Texture(Texture).Handle);

      ActivateProg(FProgTextured);

      glUniformMatrix4fv(FCurrent.Uniform.View     , 1, false, @View);
      glUniformMatrix4fv(FCurrent.Uniform.World   , 1, false, @World);
      glUniformMatrix4fv(FCurrent.Uniform.Proj    , 1, false, @Projection);
      glUniform1i       (FCurrent.Uniform.Texture , 0);
    end else
    // Draw using the colored shader
    begin
      ActivateProg(FProgColored);

      glUniformMatrix4fv(FCurrent.Uniform.View    , 1, false, @View);
      glUniformMatrix4fv(FCurrent.Uniform.World   , 1, false, @World);
      glUniformMatrix4fv(FCurrent.Uniform.Proj    , 1, false, @Projection);
      glUniform1i       (FCurrent.Uniform.Texture , 0);
    end;
  end;


  // Enable position attribute
  if FCurrent.Attrib.Position >= 0 then
  begin
    glVertexAttribPointer(FCurrent.Attrib.Position, 3, GL_FLOAT, false, SizeOf(TPHXVertex), VertexOffsetPosition);

    glEnableVertexAttribArray(FCurrent.Attrib.Position);
  end;
  // Enable normal attribute
  if FCurrent.Attrib.Normal >= 0 then
  begin
    glVertexAttribPointer(FCurrent.Attrib.Normal  , 4, GL_FLOAT, false, SizeOf(TPHXVertex), VertexOffsetNormal);

    glEnableVertexAttribArray(FCurrent.Attrib.Normal);
  end;
  // Enable texcoord attribute
  if FCurrent.Attrib.Texcoord >= 0 then
  begin
    glVertexAttribPointer(FCurrent.Attrib.Texcoord, 2, GL_FLOAT, false, SizeOf(TPHXVertex), VertexOffsetTexCoord);

    glEnableVertexAttribArray(FCurrent.Attrib.Texcoord);
  end;
  // Enable color attribute
  if FCurrent.Attrib.Color >= 0 then
  begin
    glVertexAttribPointer(FCurrent.Attrib.Color   , 4, GL_FLOAT, false, SizeOf(TPHXVertex), VertexOffsetColor);

    glEnableVertexAttribArray(FCurrent.Attrib.Color);
  end;
end;

//---------------------------------------------------------------------------
procedure TPHXOpenGL_Effect.Render(Buffer: TPHXBuffer);
begin
  Buffer.Bind;

  Bind;

  glDrawElements(glPrimitiveType[Buffer.Primitive], Buffer.Indices.Count, GL_UNSIGNED_INT, nil);
end;

//---------------------------------------------------------------------------
procedure TPHXOpenGL_Effect.Render(Buffer: TPHXBuffer; const Start, Count: Integer);
begin
  Buffer.Bind;

  Bind;

  glDrawRangeElements(glPrimitiveType[Buffer.Primitive], Start, Start + Count, Count, GL_UNSIGNED_INT, nil);
end;

//---------------------------------------------------------------------------
procedure TPHXOpenGL_Effect.SetupShader;
var NormalMatrix: TMatrix3f;
begin
  if LightingEnabled then
  begin
    NormalMatrix:= Matrix_InverseTranspose(Matrix3f(Matrix_Multiply(View, World)));

    // Draw using the textured shader
    if Texture <> nil then
    begin
      glActiveTexture(GL_TEXTURE0);
      glBindTexture(GL_TEXTURE_2D, TPHXOpenGL_Texture(Texture).Handle);

      ActivateProg(FProgLightTextured);

      glUniformMatrix4fv(FCurrent.Uniform.View    , 1, false, @View);
      glUniformMatrix4fv(FCurrent.Uniform.World   , 1, false, @World);
      glUniformMatrix4fv(FCurrent.Uniform.Proj    , 1, false, @Projection);
      glUniform1i       (FCurrent.Uniform.Texture , 0);

      glUniformMatrix3fv(FCurrent.Uniform.NormalMatrix  , 1, false, @NormalMatrix);
      glUniform3fv      (FCurrent.Uniform.LightPosition , 1, @LightPosition);
      glUniform4fv      (FCurrent.Uniform.LightAmbient  , 1, @LightAmbient);
      glUniform4fv      (FCurrent.Uniform.LightDiffuse  , 1, @LightDiffuse);
      glUniform4fv      (FCurrent.Uniform.LightSpecular , 1, @LightSpecular);
    end;

  end else
  begin
    // Draw using the textured shader
    if Texture <> nil then
    begin
      glActiveTexture(GL_TEXTURE0);
      glBindTexture(GL_TEXTURE_2D, TPHXOpenGL_Texture(Texture).Handle);

      ActivateProg(FProgTextured);

      glUniformMatrix4fv(FCurrent.Uniform.View     , 1, false, @View);
      glUniformMatrix4fv(FCurrent.Uniform.World   , 1, false, @World);
      glUniformMatrix4fv(FCurrent.Uniform.Proj    , 1, false, @Projection);
      glUniform1i       (FCurrent.Uniform.Texture , 0);
    end else
    // Draw using the colored shader
    begin
      ActivateProg(FProgColored);

      glUniformMatrix4fv(FCurrent.Uniform.View    , 1, false, @View);
      glUniformMatrix4fv(FCurrent.Uniform.World   , 1, false, @World);
      glUniformMatrix4fv(FCurrent.Uniform.Proj    , 1, false, @Projection);
      glUniform1i       (FCurrent.Uniform.Texture , 0);
    end;
  end;
end;

//---------------------------------------------------------------------------
procedure TPHXOpenGL_Effect.RenderEx(Buffer: TPHXBuffer; Decl: TPHXVertexDeclaration);
begin
  SetupShader;

  Buffer.Bind;

  // Enable position attribute
  if (vcPosition in Decl.Components) and (FCurrent.Attrib.Position >= 0) then
  begin
    glVertexAttribPointer(FCurrent.Attrib.Position, 3, GL_FLOAT, false, Decl.Size, Pointer(Decl.Offsets[vcPosition]));

    glEnableVertexAttribArray(FCurrent.Attrib.Position);
  end;
  // Enable normal attribute
  if (vcNormal in Decl.Components) and (FCurrent.Attrib.Normal >= 0) then
  begin
    glVertexAttribPointer(FCurrent.Attrib.Normal  , 4, GL_FLOAT, false, Decl.Size, Pointer(Decl.Offsets[vcNormal]));

    glEnableVertexAttribArray(FCurrent.Attrib.Normal);
  end;
  // Enable texcoord attribute
  if (vcCoord1 in Decl.Components) and (FCurrent.Attrib.Texcoord >= 0) then
  begin
    glVertexAttribPointer(FCurrent.Attrib.Texcoord, 2, GL_FLOAT, false, Decl.Size, Pointer(Decl.Offsets[vcCoord1]));

    glEnableVertexAttribArray(FCurrent.Attrib.Texcoord);
  end;
  // Enable color attribute
  if (vcColor in Decl.Components) and (FCurrent.Attrib.Color >= 0) then
  begin
    glVertexAttribPointer(FCurrent.Attrib.Color   , 4, GL_FLOAT, false, Decl.Size, Pointer(Decl.Offsets[vcColor]));

    glEnableVertexAttribArray(FCurrent.Attrib.Color);
  end;

  glDrawElements(glPrimitiveType[Buffer.Primitive], Buffer.Indices.Count, GL_UNSIGNED_INT, nil);

  glDisableVertexAttribArray(FCurrent.Attrib.Position);
  glDisableVertexAttribArray(FCurrent.Attrib.Normal);
  glDisableVertexAttribArray(FCurrent.Attrib.Texcoord);
  glDisableVertexAttribArray(FCurrent.Attrib.Color);
end;





(*

//---------------------------------------------------------------------------
procedure TPHXOpenGL_Effect.Render;
begin
  Inc(FDrawCount);

  Assert(Vertices.Count <= SIZE_VERTEX_CACHE);
  Assert(Indices.Count <= SIZE_INDEX_CACHE);

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


  // Draw using the textured shader
  if Texture <> nil then
  begin
    glActiveTexture(GL_TEXTURE0);
    glBindTexture(GL_TEXTURE_2D, TPHXOpenGL_Texture(Texture).Handle);

    ActivateProg(FProgTextured);

    glUniformMatrix4fv(FUniform_View     , 1, false, @View);
    glUniformMatrix4fv(FUniform_World   , 1, false, @World);
    glUniformMatrix4fv(FUniform_Proj    , 1, false, @Projection);
    glUniform1i       (FUniform_Texture , 0);

    glVertexAttribPointer(FAttrib_Position, 3, GL_FLOAT, false, SizeOf(TPHXVertex), VertexOffsetPosition);
    glVertexAttribPointer(FAttrib_Texcoord, 2, GL_FLOAT, false, SizeOf(TPHXVertex), VertexOffsetTexCoord);
 //   glVertexAttribPointer(FAttrib_Normal  , 4, GL_FLOAT, false, SizeOf(TPHXVertex), VertexOffsetNormal);
    glVertexAttribPointer(FAttrib_Color   , 4, GL_FLOAT, false, SizeOf(TPHXVertex), VertexOffsetColor);

    glEnableVertexAttribArray(FAttrib_Position);
    glEnableVertexAttribArray(FAttrib_Texcoord);
   // glEnableVertexAttribArray(FAttrib_Normal);
    glEnableVertexAttribArray(FAttrib_Color);

    // Draw the elements
    glDrawElements(glPrimitiveType[PHX_TRIANGLES], Indices.Count, GL_UNSIGNED_SHORT, nil);
  end else
  // Draw using the colored shader
  begin
    ActivateProg(FProgColored);

    glUniformMatrix4fv(FUniform_View    , 1, false, @View);
    glUniformMatrix4fv(FUniform_World   , 1, false, @World);
    glUniformMatrix4fv(FUniform_Proj    , 1, false, @Projection);
    glUniform1i       (FUniform_Texture , 0);

    glVertexAttribPointer(FAttrib_Position, 3, GL_FLOAT, false, SizeOf(TPHXVertex), VertexOffsetPosition);
    glVertexAttribPointer(FAttrib_Texcoord, 2, GL_FLOAT, false, SizeOf(TPHXVertex), VertexOffsetTexCoord);
    glVertexAttribPointer(FAttrib_Normal  , 4, GL_FLOAT, false, SizeOf(TPHXVertex), VertexOffsetNormal);
    glVertexAttribPointer(FAttrib_Color   , 4, GL_FLOAT, false, SizeOf(TPHXVertex), VertexOffsetColor);

    glEnableVertexAttribArray(FAttrib_Position);
    glEnableVertexAttribArray(FAttrib_Texcoord);
    glEnableVertexAttribArray(FAttrib_Normal);
    glEnableVertexAttribArray(FAttrib_Color);

    // Draw the elements
    glDrawElements(glPrimitiveType[PHX_TRIANGLES], Indices.Count, GL_UNSIGNED_SHORT, nil);
  end;
end;

*)

//------------------------------------------------------------------------------
procedure TPHXOpenGL_Effect.Changed;
begin
//  FRenderer.SetTexture(Texture, PHX_TEXTURE_1);
//  FRenderer.SetShader(Shader);

//  FShader.Parameters['view'         ].SetValue(View);
//  FShader.Parameters['world'        ].SetValue(World);
 // FShader.Parameters['texture'      ].SetValue(0);
 // FShader.Parameters['textureenable'].SetValue( Ord(TextureEnabled));
end;






end.
