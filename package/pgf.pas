{ This file was automatically created by Lazarus. Do not edit!
  This source is only used to compile and install the package.
 }

unit pgf;

{$warn 5023 off : no warning about unused units}
interface

uses
  phxApplication, phxCamera, phxCanvas, phxClasses, phxConsole, phxDevice, 
  phxEffect, phxEvents, phxFont, phxGraphics, phxGraphics_DDS, 
  phxGraphics_Vampyre, phxImage, phxInput, phxLogger, phxMath, phxModel, 
  phxParticle, phxParticleAffectors, phxParticleGraphs, phxParticlePresets, 
  phxParticleRenderers, phxPersistency, phxPrimitives, phxShape, phxSimpleGUI, 
  phxSimpleXML, phxSkin, phxSprite, phxSprite_Box2D, phxSpriteTerrain, 
  phxTexture, phxTiles, phxTimer, phxTranslation, phxTypes, phxOpenGL, 
  phxOpenGL_Shader, phxOpenGL_SDL2, phxOpenGL_GLFW3, phxOpenGL_Effect, 
  phxOpenGL_Canvas, phxAudio, phxAudio_Bass, phxGraphics_FreeImage, 
  LazarusPackageIntf;

implementation

procedure Register;
begin
end;

initialization
  RegisterPackage('pgf', @Register);
end.
