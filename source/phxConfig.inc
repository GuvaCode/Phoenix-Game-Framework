{$IFDEF FPC}
{$mode delphi}{$H+}
//{$z4}

{$ENDIF}








{$IFDEF darwin}
{$linkframework Cocoa}
{$linkframework SDL}
{$ENDIF}
// Load and save documents to xml using JclSimpleXml
//{$DEFINE DOCXML_JCL}
// Load and save documents to xml using the VCL Xml

//{$DEFINE DOCXML_VCL}
{$DEFINE PHX_LOGGING}
{$DEFINE PHX_VERBOSE_LOGGING}

// Enable lua scripting
{$DEFINE PHX_LUA_SCRIPTING}

// Uses Vamire Imaging
{$DEFINE PHX_VAMPIRE}





