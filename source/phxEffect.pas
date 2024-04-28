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
unit phxEffect;
//< Device independent implementation of pixel and fragment shaders for 3D scenes.

interface

{$I phxConfig.inc}

uses
  SysUtils, Classes,

  phxLogger,
  phxTypes,
  phxClasses,
  phxMath,
  phxTexture,
  phxGraphics;

type

// The effect is a device independent implementation of pixel and fragment shaders.
//------------------------------------------------------------------------------
TPHXCustomEffect = class(TObject)
  private
    FName: String;
  protected
    procedure Changed; virtual;
  public
    constructor Create;
    destructor Destroy; override;

    // Render all verticies in a buffer
    procedure Render(Buffer: TPHXBuffer); overload; virtual; abstract;
    // Render verticies from a buffer
    procedure Render(Buffer: TPHXBuffer; const Start, Count: Integer); overload; virtual; abstract;

    procedure RenderEx(Buffer: TPHXBuffer; Decl: TPHXVertexDeclaration); virtual; abstract;

    // Name of the effect
    property Name: String read FName write FName;
  end;

// The effect is a device independent implementation of pixel and fragment shaders.
//------------------------------------------------------------------------------
TPHXEffect = class(TPHXCustomEffect)
  private
    FProjection: TMatrix4f;
    FView      : TMatrix4f;
    FWorld     : TMatrix4f;

    FTexture: TPHXTexture;

    FLightingEnabled: Boolean;

    FLightPosition: TVector3f;
    FLightAmbient : TColor4f;
    FLightDiffuse : TColor4f;
    FLightSpecular: TColor4f;

    procedure SetView(const Value: TMatrix4f);
    procedure SetWorld(const Value: TMatrix4f);
    procedure SetProjection(const Value: TMatrix4f);

    procedure SetTexture(const Value: TPHXTexture);
  public
    constructor Create;
    destructor Destroy; override;

    // The Projection matrix
    property Projection: TMatrix4f read FProjection write SetProjection;
    // The view matrix
    property View: TMatrix4f read FView write SetView;
    // The world matrix
    property World: TMatrix4f read FWorld write SetWorld;

    // The texture to be applied by this effect.
    property Texture: TPHXTexture read FTexture write SetTexture;

    // Enables and disables lighting
    property LightingEnabled: Boolean read FLightingEnabled write FLightingEnabled;
    // Position of the light
    property LightPosition: TVector3f read FLightPosition write FLightPosition;
    // Ambient light color
    property LightAmbient : TColor4f read FLightAmbient write FLightAmbient;
    // Diffuse light color
    property LightDiffuse : TColor4f read FLightDiffuse write FLightDiffuse;
    // Specular light color
    property LightSpecular: TColor4f read FLightSpecular write FLightSpecular;
  end;



implementation


// http://stackoverflow.com/questions/6686741/fragment-shader-glsl-for-texture-color-and-texture-color

//Public Property	View	Gets or sets the view matrix.
//Public Property	World	Gets or sets the world matrix.
// VertexColorEnabled	Enables use vertex colors for this effect.
//  VertexColorEnabled	Enables use vertex colors for this effect.
//  Public Property	Texture	Gets or sets a texture to be applied by this effect.
//Public Property	TextureEnabled	Enables textures for this effect.
//Public Property	Texture	Enables textures for this effect.


// http://msdn.microsoft.com/en-us/library/microsoft.xna.framework.graphics.basiceffect_members.aspx



// TPHXCustomEffect
//==============================================================================
constructor TPHXCustomEffect.Create;
begin

end;

//------------------------------------------------------------------------------
destructor TPHXCustomEffect.Destroy;
begin

  inherited;
end;

//------------------------------------------------------------------------------
procedure TPHXCustomEffect.Changed;
begin

end;


// TPHXEffect
//==============================================================================
constructor TPHXEffect.Create;
begin
  FView      := TMatrix4f.Identity;
  FWorld     := TMatrix4f.Identity;
  FProjection:= Matrix_CreateOrthographicLH(1024, 768, -1.0, 1000.0);

  FTexture       := nil;
 // FTextureEnabled:= True;

  FLightingEnabled:= False;

  FLightPosition := Vector3f(0.0, 1.0, 0.02);
  FLightAmbient  := Color4f(0.2, 0.2, 0.2, 0.2);
  FLightDiffuse  := Color4f(1.0, 1.0, 1.1, 0.0);
  FLightSpecular := Color4f(1.0, 1.0, 1.1, 0.0);


  (*
    GLfloat DiffuseLight[] = {1.0, 1.0, 1.0};
    GLfloat AmbientLight[] = {0.2, 0.2, 0.2};
    GLfloat SpecularLight[] = {1.0, 1.0, 1.0};

    glLightfv (GL_LIGHT0, GL_SPECULAR, SpecularLight);
    glLightfv (GL_LIGHT0, GL_DIFFUSE, DiffuseLight);
    glLightfv (GL_LIGHT0, GL_AMBIENT, AmbientLight);


  GLfloat mShininess[] = {50};

    GLfloat DiffuseMaterial[] = {1.0, 0.0, 0.0};
    GLfloat AmbientMaterial[] = {0.0, 0.0, 0.0};
    GLfloat SpecularMaterial[] = {1.0, 1.0, 1.0};

    glMaterialfv(GL_FRONT_AND_BACK, GL_DIFFUSE, DiffuseMaterial);
    glMaterialfv(GL_FRONT_AND_BACK, GL_AMBIENT, AmbientMaterial);
    glMaterialfv(GL_FRONT_AND_BACK, GL_SPECULAR, SpecularMaterial);
    glMaterialfv(GL_FRONT_AND_BACK, GL_SHININESS, mShininess);
    *)
end;

//------------------------------------------------------------------------------
destructor TPHXEffect.Destroy;
begin
  inherited;
end;


//------------------------------------------------------------------------------
procedure TPHXEffect.SetProjection(const Value: TMatrix4f);
begin
  FProjection := Value;

  Changed;
end;

//------------------------------------------------------------------------------
procedure TPHXEffect.SetView(const Value: TMatrix4f);
begin
  FView := Value;

  Changed;
end;

//------------------------------------------------------------------------------
procedure TPHXEffect.SetWorld(const Value: TMatrix4f);
begin
  FWorld := Value;

  Changed;
end;

//------------------------------------------------------------------------------
procedure TPHXEffect.SetTexture(const Value: TPHXTexture);
begin
  if FTexture <> Value then
  begin
    FTexture := Value;

    Changed;
  end;
end;




end.
