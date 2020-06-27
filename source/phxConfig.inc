
{$IFDEF FPC}
{$mode delphi}{$H+}
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

// Uses SDL2 
{.$DEFINE PHX_SDL2}





