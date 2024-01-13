{ This file was automatically created by Lazarus. Do not edit!
  This source is only used to compile and install the package.
 }

unit phxEditorLibrary;

{$warn 5023 off : no warning about unused units}
interface

uses
  phxColorEditor, phxFontEx, phxGraphicsEx, phxTextureDialog, phxImageDialog, 
  phxEditor, phxDraw, LazarusPackageIntf;

implementation

procedure Register;
begin
  RegisterUnit('phxColorEditor', @phxColorEditor.Register);
  RegisterUnit('phxTextureDialog', @phxTextureDialog.Register);
  RegisterUnit('phxImageDialog', @phxImageDialog.Register);
  RegisterUnit('phxDraw', @phxDraw.Register);
end;

initialization
  RegisterPackage('phxEditorLibrary', @Register);
end.
