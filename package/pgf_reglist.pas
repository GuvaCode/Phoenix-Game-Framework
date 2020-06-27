unit pgf_reglist;

interface
{$I phxConfig.inc}

uses

phxApplication,

phxCamera, 
phxCanvas,
phxClasses, 
phxConsole, 
phxDevice, 
phxEffect,
phxEvents, 
phxFont, 

phxGraphics, 
phxGraphics_DDS, 

{$IFDEF PHX_VAMPIRE}phxGraphics_Vampyre, {$ENDIF}
{$IFDEF PHX_FREEIMAGE}phxGraphics_FreeImage,{$ENDIF}

phxImage,

phxInput,
phxLogger, 
phxMath, 
phxModel, 
phxParticle, 
phxParticleAffectors, 
phxParticleGraphs, 
phxParticlePresets, 
phxParticleRenderers, 

phxPersistency, 
phxPrimitives, 
phxShape, 
phxSimpleGUI, 
phxSimpleXML, 
phxSkin, 

phxSprite, 
phxSprite_Box2D, 
phxSpriteTerrain, 
phxTexture, 
phxTiles, 

phxTimer, 
phxTranslation, 
phxTypes, 

phxOpenGL, 
phxOpenGL_Shader,

{$IFDEF PHX_GLFW} phxOpenGL_GLFW3,{$ENDIF}
{$IFDEF PHX_SDL2} phxOpenGL_SDL2, {$ENDIF}


phxOpenGL_Effect,
phxOpenGL_Canvas, 

phxAudio, 
phxAudio_Bass, 
LazarusPackageIntf;

implementation

end.

