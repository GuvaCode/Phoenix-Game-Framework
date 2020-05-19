{$IFDEF FPC}
{$mode delphi}{$H+}
//{$z4}
//{$macro on}
{$ENDIF}




{$ifdef windows}
	{$define apicall := stdcall}
{$else}
	{$define apicall := cdecl}
{$endif}

{$ifdef linux}
  {$ifdef i386}
    {$define linux-i386}
	{$endif}
	{$ifdef cpux86_64}
    {$define linux-x86_64}
  {$endif}
{$endif}



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





