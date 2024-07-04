unit uImport;

interface

uses
  Windows, Messages, SysUtils, Classes,

  Generics.Collections,

  phxTypes,
  phxModel;

type

// FBX
//
// http://images.autodesk.com/adsk/files/fbx_sdk_programmers_guide_2009_3.pdf

//------------------------------------------------------------------------------
TImporter = class
  private
    FMesh: TPHXMesh;
  protected
    Procedure ImportMesh(Stream: TStream);  virtual; abstract;
  public
    constructor Create; virtual;
    destructor Destroy; override;

    Procedure Import(Mesh: TPHXMesh; Stream: TStream); overload;
    Procedure Import(Mesh: TPHXMesh; const FileName: String); overload;

    property Mesh: TPHXMesh read FMesh;
  end;

TImporterClass = class of TImporter;

//------------------------------------------------------------------------------
TImporterItem = record
  // Name of the format
  Name: String;
  // The file extension
  Ext: String;
  // The importer class
  Importer: TImporterClass;
end;

//------------------------------------------------------------------------------
TImporterFactory = class
  private
    class var FItems: TList<TImporterItem>;
  public
    class procedure Add(const Name: String;const Ext: String; Importer: TImporterClass);

    class function CreateImporterForFile(const FileName: String): TImporter;

    class property Items: TList<TImporterItem> read FItems;
  end;

implementation


// TImporterFactory
//==============================================================================
constructor TImporter.Create;
begin

end;

//------------------------------------------------------------------------------
destructor TImporter.Destroy;
begin

  inherited;
end;



//------------------------------------------------------------------------------
procedure TImporter.Import(Mesh: TPHXMesh; const FileName: String);
var Stream: TStream;
begin
  Stream:= TFileStream.Create(FileName, fmOpenRead);
  try
    Import(Mesh, Stream);
  finally
    Stream.Free;
  end;
end;


//------------------------------------------------------------------------------
procedure TImporter.Import(Mesh: TPHXMesh; Stream: TStream);
begin
  FMesh:= Mesh;

  ImportMesh(Stream);
end;


// TImporterFactory
//==============================================================================
class procedure TImporterFactory.Add(const Name: String; const Ext: String; Importer: TImporterClass);
var Item: TImporterItem;
begin
  Item.Name    := Name;
  Item.Ext     := Ext;
  Item.Importer:= Importer;

  Items.Add(Item);
end;

//------------------------------------------------------------------------------
class function TImporterFactory.CreateImporterForFile(const FileName: String): TImporter;
var Item: TImporterItem;
var Ext : String;
begin
  Ext:= ExtractFileExt(FileName);

  for Item in Items do
  begin
    if SameText(Item.Ext, Ext) then
    begin
      Result:= Item.Importer.Create;

      Exit;
    end;


  end;
  Result:= nil;
end;



initialization
  TImporterFactory.FItems:= TList<TImporterItem>.Create;
finalization
  TImporterFactory.FItems.Free;
end.
