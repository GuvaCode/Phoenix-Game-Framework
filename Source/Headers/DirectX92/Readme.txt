This package contains DirectX 9 programming API by Microsoft.

Original files can de downloaded from:
  You can download DirectX runtime from: http://www.microsoft.com/directx
  You can download DirectX SDK from: http://msdn.microsoft.com/directx

This DirectX conversion is Borland and JEDI compliant.

Additional examples can be downloaded from: http://www.clootie.ru

******************************************************************************* 
Quick start: 
<Borland tools users>: You can copy "*.pas" and "*.inc" files to directory accessible by your tool from "JEDI" subdirectory and just compile your projects.
If you want to use D3DX9 then you also need to copy d3dx9_XX.dll to some common directory (like "Windows\System" or "WinNT\System32") where it can be accessed.
******************************************************************************* 

Package can be divided in following sections:
1) JEDI compliant conversion is located in "JEDI" directory in "*.pas" and "*.inc" files. This directory also contains PAR files - from these files all PAS variations of the same names modules are generated (but these PAR files are not needed to compile your code).
This translation can be compiled with either dynamic linking of library (with exception for D3DX9, dxerr9) or static one.
There are keys defined to allow this functionality:
  {$DEFINE DYNAMIC_LINK_ALL} - allows dynamic linking at load time
  {$DEFINE DYNAMIC_LINK_EXPLICIT_ALL} - allows explicit (programmer defined) control over loading of libraries.
Or per DirectX part like:
  {$DEFINE DIRECT3D8_DYNAMIC_LINK} - allows dynamic linking at load time
  {$DEFINE DIRECT3D8_DYNAMIC_LINK_EXPLICIT} - allows explicit (programmer defined) control over loading of libraries.

This translation follows standards defined by JEDI and Borland for header translations. These standards allow using Object Pascal projects based on header translation not only in Delphi but in C++ Builder too. You can even mix C++ code using DirectX with Object Pascal one when C++ code knows nothing about your Object Pascal code.

2) Borland compliant conversion is located in "Borland_XXX" subdirectories. These are actually a preprocessed (simplified) versions of JEDI translation. Which links statically, doesn't include any IFDEF's, etc. "Borland_D4-5" directory contains Delphi4 and Delphi5 optimized code; "Borland_D6-7" contains Delphi6 and Delphi7 optimized code and "Borland_D9-10" contains Delphi9 (aka Delphi2005) and Delphi10 (aka Delphi2006 or TurboDelphi) optimized code.

3) Translation without {$EXTERNALSYM ...} and some other unnecessary for Delphi symbols. This translation can be used to study translation code due to its refined readability than previous two. It's located in "Compact\*.pas". This translation can also be treated as Delphi6 optimized translation but without C++Builder compatibility.

4) Example code in "Demo\SimplestD3d8.dpr". Demonstrates simple application using Direct3D8 (something like "Hello World!!!").

5) Compiled example code in "Bin\SimplestD3d8.exe".

=== !!! Note: Delphi 4-5 users !!! ============================
Due to ObjectPascal restriction on enums some enums, defined in original C++ headers translated not as pascal enums, but as pack of consts. And some enums defined included dummy elements like "D3DLIGHT_INVALID_0" where "INVALID" means what it's dummy element and "0" is underlying integer value of what element.

=== !!! Note: Borland compliant versions !!! =================== 
Due to requirements from Borland these versions do not contains any preprocessor commands (like IFDEF's). So as side effect these versions allow only retail version of D3DX9 library to be linked and only static linking is possible.

Alexey Barkovoy,
30-Oct-2006
