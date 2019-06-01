unit uImportOBJ;

interface

uses
  Windows, Messages, SysUtils, Classes,

  uImport,

  phxTypes,
  phxMath,
  phxModel;

type

TColor = record           // Stores a RGB (0-1) Color
  R, G, B : Single;
end;

TVertex = record           // Stores X, Y, Z coordinates
  X, Y, Z : Single;
end;

TNormal = record
  X, Y, Z : Single;
end;

TTexCoord = record        // Stores texture coordinates
  U, V : Single;
end;

// Only supports triangular faces
TFace = Record
 Count : Integer;            // Number of vertices in faces
 vIndex : Array[0..2] of Integer;  // indexes to vertices
 tIndex : Array[0..2] of Integer;  // indexes to vertex textures
 nIndex : Array[0..2] of Integer;  // indexes to vertex normals
end;



TMaterial = Record        // Material Structure
 Name : String;
 Ambient   : TColor;
 Diffuse   : TColor;
 Specular  : TColor;
 Shininess : Single;
end;


TGroup = Record
  Name : String;
  Faces : Integer;            // Number of faces
  Face  : Array of TFace;     // The faces in the group
  mIndex : Integer;           // index to Material
end;

TModel = Record
  Name : String;
  MaterialFile : String;
  Vertices  : Integer;
  Normals   : Integer;
  TexCoords : Integer;
  Groups    : Integer;
  Materials : Integer;
  Vertex    : Array of TCoord;
  Normal    : Array of TCoord;
  TexCoord  : Array of TTexCoord;
  Group     : Array of TGroup;
  Material  : Array of TMaterial;
end;

// http://en.wikipedia.org/wiki/Obj
//------------------------------------------------------------------------------
TImporterOBJ = class(TImporter)
  private
   // Vertex: TPHXMeshVertex;

    numVerticies : Integer;
    theVerticies    : array of TVertex;

    numNormals  : Integer;
    theNormals  : array of TNormal;
    numTexCoords: Integer;
    theTexCoords: array of TTexCoord;

    numFaces: Integer;
    theFaces: array of TFace;

    //Group     : array of TGroup;
   // Material  : array of TMaterial;


    procedure ParseVertex(const Line: String);
    procedure ParseFace  (const Line: String);
  public
    constructor Create; override;
    destructor Destroy; override;

    Procedure LoadFromStream(Stream  : TStream);

    Procedure ImportMesh(Stream: TStream); override;

    procedure CopyModel(Mesh: TPHXMesh);
  end;

implementation

uses StrUtils;

//------------------------------------------------------------------------------
constructor TImporterOBJ.Create;
begin
  inherited;
end;

//------------------------------------------------------------------------------
destructor TImporterOBJ.Destroy;
begin

  inherited;
end;


//------------------------------------------------------------------------------
procedure TImporterOBJ.ImportMesh(Stream: TStream);
begin
  LoadFromStream(Stream);

  CopyModel(Mesh);
end;

//------------------------------------------------------------------------------
procedure TImporterOBJ.CopyModel(Mesh: TPHXMesh);
var I,J: Integer;
begin
  Mesh.Vertices.Count:= numFaces * 3;

  for I := 0 to numFaces - 1 do
  begin
    for J := 0 to 2 do
    begin
      if theFaces[I].vIndex[J] > 0 then
      begin
        Mesh.Vertices.List^[I * 3 + J].Position.X:= theVerticies[ theFaces[I].vIndex[J] - 1].X;
        Mesh.Vertices.List^[I * 3 + J].Position.Y:= theVerticies[ theFaces[I].vIndex[J] - 1].Y;
        Mesh.Vertices.List^[I * 3 + J].Position.Z:= theVerticies[ theFaces[I].vIndex[J] - 1].Z;
      end;

      if theFaces[I].nIndex[J] > 0 then
      begin
        Mesh.Vertices.List^[I * 3 + J].Normal.X:= theNormals[ theFaces[I].nIndex[J] - 1].X;
        Mesh.Vertices.List^[I * 3 + J].Normal.Y:= theNormals[ theFaces[I].nIndex[J] - 1].Y;
        Mesh.Vertices.List^[I * 3 + J].Normal.Z:= theNormals[ theFaces[I].nIndex[J] - 1].Z;
      end;
      if theFaces[I].tIndex[J] > 0 then
      begin
        Mesh.Vertices.List^[I * 3 + J].TexCoord0.X:= theTexCoords[ theFaces[I].tIndex[J] - 1].U;
        Mesh.Vertices.List^[I * 3 + J].TexCoord0.Y:= theTexCoords[ theFaces[I].tIndex[J] - 1].V;
      end;
      Mesh.Vertices.List^[I * 3 + J].Joint:= 0;
    end;
  end;
//  Mesh.Groups.Add();

end;



//------------------------------------------------------------------------------
procedure TImporterOBJ.LoadFromStream(Stream: TStream);
var Lines: TStrings;
var Index: Integer;
var Line : String;
begin
  Lines:= TStringList.Create;
  try
    Lines.LoadFromStream(Stream);

    for Index := 0 to Lines.Count - 1 do
    begin
      Line:= Lines[Index];

      // Empty line
      if Line = '' then Continue;
      // Lines beginning with a hash character (#) are comments.
      if Line[1] = '#' then Continue;

      case UpperCase(Line)[1] of
        // Vertex
        'V': ParseVertex(Line);
        // Face
        'F': ParseFace(Line);
        // Group

      end;




    end;

  finally
    Lines.Free;
  end;
end;

(*
function GetCoords(S : String) : TCoord;
var P, P2 : Integer;
    C : TCoord;
begin
  S :=Trim(Copy(S, 3, Length(S)));
  P :=Pos(' ', S);
  P2 :=PosEx(' ', S, P+1);
  S := StringReplace(S, '.', DecimalSeparator, [rfReplaceAll]);

  C.X :=StrToFloat(Copy(S, 1, P-1));
  C.Y :=StrToFloat(Copy(S, P+1, P2-P-1));
  C.Z :=StrToFloat(Copy(S, P2+1, Length(S)));
  Result :=C;
end;
*)

//------------------------------------------------------------------------------
procedure TImporterOBJ.ParseVertex(const Line: String);
var Tokens: TStrings;
begin
  Tokens := TStringList.Create;
  try
    Tokens.Delimiter    := ' ';
    Tokens.DelimitedText:= StringReplace(Line, '.', DecimalSeparator, [rfReplaceAll]);

    case UpperCase(Line)[2] of
      // # List of Vertices, with (x,y,z) coordinates.
      // v 0.123 0.234 0.345
      ' ' :
      begin
        Inc(numVerticies);

        SetLength(theVerticies, numVerticies);

        theVerticies[numVerticies-1].X:= StrToFloat(Tokens[1]);
        theVerticies[numVerticies-1].Y:= StrToFloat(Tokens[2]);
        theVerticies[numVerticies-1].Z:= StrToFloat(Tokens[3]);
      end;
      // # Normals in (x,y,z) form; normals might not be unit.
      //  vn 0.707 0.000 0.707
      //  vn ...
      //  ..
      'N' :
      begin
        Inc(numNormals);

        SetLength(theNormals, numNormals);

        theNormals[numNormals-1].X:= StrToFloat(Tokens[1]);
        theNormals[numNormals-1].Y:= StrToFloat(Tokens[2]);
        theNormals[numNormals-1].Z:= StrToFloat(Tokens[3]);
      end;
      //# Texture coordinates, in (u,v) coordinates.
      // vt 0.500 -1.352
      // vt ...
      'T' :
      begin
        Inc(numTexCoords);

        SetLength(theTexCoords, numTexCoords);

        theTexCoords[numTexCoords-1].U:= StrToFloat(Tokens[1]);
        theTexCoords[numTexCoords-1].V:= StrToFloat(Tokens[2]);
      end;
    end;

//  Mesh.Vertices.Add(Vertex);

  finally
    Tokens.Free;
  end;

end;
//f 1/1/ 2/2/ 65/65/
//f 2/2/ 66/66/ 65/65/

procedure TImporterOBJ.ParseFace(const Line: String);
var Tokens1: TStrings;
var Tokens2: TStrings;
var Index: Integer;
begin
  Tokens1:= TStringList.Create;
  Tokens2 := TStringList.Create;
  try
    Tokens1.Delimiter    := ' ';
    Tokens1.DelimitedText:= Copy(Line, 2, Maxint);

    Inc(numFaces);
    SetLength(theFaces, numFaces);

    Assert(Tokens1.Count = 3, 'Only supports triangular faces');

    for Index := 0 to Tokens1.Count - 1 do
    begin
      Tokens2.Delimiter    := '/';
      Tokens2.DelimitedText:= Tokens1[Index];

      case Tokens2.Count of
        // v v v
        1:
        begin
         // Tokens2[0];
         // Tokens2[1];
         // Tokens2[2];
          theFaces[numFaces-1].vIndex[Index]:= StrToIntDef(Tokens2[0], -1);
          theFaces[numFaces-1].tIndex[Index]:= -1;
          theFaces[numFaces-1].nIndex[Index]:= -1;
        end;
        // v/t v/t v/t
        2:
        begin
          theFaces[numFaces-1].vIndex[Index]:= StrToIntDef(Tokens2[0], -1);
          theFaces[numFaces-1].tIndex[Index]:= StrToIntDef(Tokens2[1], -1);
          theFaces[numFaces-1].nIndex[Index]:= -1;
        end;
        // v/t/n v/t/n v/t/n
        3:
        begin
          theFaces[numFaces-1].vIndex[Index]:= StrToIntDef(Tokens2[0], -1);
          theFaces[numFaces-1].tIndex[Index]:= StrToIntDef(Tokens2[1], -1);
          theFaces[numFaces-1].nIndex[Index]:= StrToIntDef(Tokens2[2], -1);
        end;
      end;

    end;

  finally
    Tokens1.Free;
    Tokens2.Free;
  end;


end;



initialization
  TImporterFactory.Add('Wavefront OBJ ', '.obj', TImporterOBJ);
end.
