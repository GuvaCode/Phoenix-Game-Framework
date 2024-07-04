{ -= Phoenix config file =-}

{$WARN 4104 off : Implicit string type conversion from "$1" to "$2"}
{$WARN 4105 off : Implicit string type conversion with potential data loss from "$1" to "$2"}
{$WARN 4056 off : Conversion between ordinals and pointers is not portable}
{$WARN 4082 off : Converting pointers to signed integers may result in wrong comparison results and range errors, use an unsigned type instead.}

{$IFDEF FPC}
{$mode delphi}{$H+}
{$packrecords c}
{$ENDIF}

{$IFDEF darwin}
{$linkframework Cocoa}
{$linkframework SDL}
{$ENDIF}

{$DEFINE PHX_LOGGING}
{$DEFINE PHX_VERBOSE_LOGGING}

// Enable lua scripting
{$DEFINE PHX_LUA_SCRIPTING}

// Uses Vamire Imaging
{$DEFINE PHX_VAMPIRE}

// Uses FreeImage lib
{.$DEFINE PHX_FREEIMAGE}


// Uses GLFW
{$DEFINE PHX_GLFW}
 {.$DEFINE VK_VERSION_1_0}
{.$DEFINE GLFW3_LASTEST}
{.$DEFINE GLFW3_STATIC}
{.$DEFINE GLFW3_STATIC_LINKDLL}
// GLFW gonfig

// Uses SDL2 
{$DEFINE PHX_SDL2}





