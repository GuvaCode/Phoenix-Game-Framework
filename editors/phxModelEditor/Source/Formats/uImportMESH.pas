unit uImportMESH;

interface

uses
  Windows, Messages, SysUtils, Classes,

  StrUtils,

  uImport,

  phxTypes,
  phxMath,
  phxModel;

type

//
//----------------------------------------------------------------------------
TImporterMesh = class(TImporter)
  private
   // Verticies: TPHXMeshVertexList;

    function ReadVector(const Line: String): TVector3f;
    function ReadSingle(const Line: String): Single;
    function ReadInteger(const Line: String): Integer;
    function ReadString(const Line: String): String;
    function ReadColor(const Line: String): TColor4f;

    procedure LoadMaterial(Mesh: TPHXMesh; Lines: TStrings; var Index: Integer);
    procedure LoadPoint (Mesh: TPHXMesh; Lines: TStrings; var Index: Integer);
    procedure LoadVertex(Mesh: TPHXMesh; Lines: TStrings; var Index: Integer);
    procedure LoadTriangle(Mesh: TPHXMesh; Lines: TStrings; var Index: Integer);
  public
    constructor Create; override;
    destructor Destroy; override;

    Procedure ImportMesh(Stream: TStream); overload; override;
    Procedure ImportMesh(Lines: TStrings); reintroduce; overload;
  end;


implementation

//------------------------------------------------------------------------------
function LineStarts(const Line, Text: string): Boolean;
var Start: String;
begin
  Start:= Copy(Line, 1, Length(Text));

  Result:= SameText(Start, Text);
end;

function GetAValue(rgb: DWORD): Byte;
begin
  Result := Byte(rgb shr 24);
end;

//TImporterMesh
//==============================================================================
constructor TImporterMesh.Create;
begin
  //Verticies:= TPHXMeshVertexList.Create;
end;

//------------------------------------------------------------------------------
destructor TImporterMesh.Destroy;
begin
  //Verticies.Free;
  inherited;
end;

//------------------------------------------------------------------------------
procedure TImporterMesh.ImportMesh(Stream: TStream);
var Lines: TStrings;
begin
  Lines:= TStringList.Create;
  try
    Lines.LoadFromStream(Stream);

    ImportMesh(Lines);
  finally
    Lines.Free;
  end;
end;


//  TXT
//  MeshData
//    hasValidTangents TRUE
//    BoundingRadius 110.658546
//    MaxBoundingExtents [ 69.915894 68.869675 110.658546 ]
//    MinBoundingExtents [ -69.915894 -68.869675 -110.658546 ]
//    NumMaterials 1
//    Material
//      DiffuseTextureFileName ""
//      SelfIlluminationTextureFileName ""
//      NormalTextureFileName ""
//      DisplacementTextureFileName ""
//      TeamColorTextureFileName ""
//      Diffuse ffffffff
//      Ambient ffffffff
//      Specular ffffffff
//      Emissive ffffffff
//      Glossiness 50.000000
//    NumPoints 0
//    NumVertices 262
//    Vertex
//      Position [ 12.601639 -67.546364 8.261526 ]
//      Normal [ 0.178055 -0.982913 0.046669 ]
//      Tangent [ -0.988235 0.407843 0.498039 ]
//      Color 0
//      U0 0.409880
//      V0 0.537329
//      U1 0.409880
//      V1 0.537329
//
//------------------------------------------------------------------------------
procedure TImporterMesh.ImportMesh(Lines: TStrings);
var Index: Integer;
var Line : String;
var Group : TPHXMeshGroup;
begin
  Index:= 0;

  while (Index < Lines.Count) do
  begin
    Line:= Lines[Index] ;

    if SameText(Line, #9'Material') then
    begin
      LoadMaterial(Mesh, Lines, Index);
    end else
    if SameText(Line, #9'Point') then
    begin
      LoadPoint(Mesh, Lines, Index);
    end else
    if SameText(Line, #9'Vertex') then
    begin
      LoadVertex(Mesh, Lines, Index);
    end else
    if SameText(Line, #9'Triangle') then
    begin
      LoadTriangle(Mesh, Lines, Index);
    end else
    begin
      Inc(Index);
    end;
  end;

  Group.TriangleOffset:= 0;
  Group.TriangleCount := Mesh.Triangles.Count;

  Mesh.Groups.Add(Group);
end;

//
//	Material
//		DiffuseTextureFileName "AncientsBattlecruiser-cl.dds"
//		SelfIlluminationTextureFileName "AncientsBattlecruiser-da.dds"
//		NormalTextureFileName "AncientsBattlecruiser-nm.dds"
//		DisplacementTextureFileName ""
//		TeamColorTextureFileName ""
//		Diffuse ffffffff
//		Ambient ffffffff
//		Specular ffffffff
//		Emissive ffffffff
//		Glossiness 50.000000
//------------------------------------------------------------------------------
procedure TImporterMesh.LoadMaterial(Mesh: TPHXMesh; Lines: TStrings; var Index: Integer);
var Line: String;
var Name: String;
begin
  Inc(Index);

  while (Index < Lines.Count) do
  begin
    Line:= Lines[Index] ;

    // Done
    if Copy(Line, 1, 2) <> #9#9 then Break;

    // DiffuseTextureFileName "AncientsBattlecruiser-cl.dds"
    if LineStarts(Line, #9#9'DiffuseTextureFileName') then
    begin
      Name:= ReadString(Line);

      if Name <> '' then Mesh.Material.Textures.Add(Name, tkDiffuse);
    end else
    // SelfIlluminationTextureFileName "AncientsBattlecruiser-cl.dds"
    if LineStarts(Line, #9#9'SelfIlluminationTextureFileName') then
    begin
      Name:= ReadString(Line);

      if Name <> '' then Mesh.Material.Textures.Add(Name, tkCustom);
    end else
    // NormalTextureFileName "AncientsBattlecruiser-cl.dds"
    if LineStarts(Line, #9#9'NormalTextureFileName') then
    begin
      Name:= ReadString(Line);

      if Name <> '' then Mesh.Material.Textures.Add(Name, tkNormal);
    end else
    // DisplacementTextureFileName "AncientsBattlecruiser-cl.dds"
    if LineStarts(Line, #9#9'DisplacementTextureFileName') then
    begin
      Name:= ReadString(Line);

      if Name <> '' then Mesh.Material.Textures.Add(Name, tkDisplacement);
    end else
    // TeamColorTextureFileName "AncientsBattlecruiser-cl.dds"
    if LineStarts(Line, #9#9'TeamColorTextureFileName') then
    begin
      Name:= ReadString(Line);

      if Name <> '' then Mesh.Material.Textures.Add(Name, tkTeam);
    end else
    // Diffuse ffffffff
    if LineStarts(Line, #9#9'Diffuse') then
    begin

      Mesh.Material.Diffuse:= ReadColor(Line);
    end else
    begin
      // Unkown tag
    end;



    Inc(Index);
  end;
end;


//	Point
//		DataString "Weapon-2"
//		Position [ 147.093948 -14.969450 -31.450413 ]
//		Orientation
//			 [ -0.000000 0.000000 -1.000000 ]
//			 [ 0.000000 1.000000 0.000000 ]
//			 [ 1.000000 0.000000 -0.000000 ]
//------------------------------------------------------------------------------
procedure TImporterMesh.LoadPoint(Mesh: TPHXMesh; Lines: TStrings; var Index: Integer);
var Line : String;
var Tag : TPHXMeshTag;
var V1, V2, V3: TVector3f;
var Matrix: TMatrix4;
begin
  Inc(Index);

  Tag.Name      := '';
  Tag.Joint    := 0;
  Tag.JointName:= '';

  while (Index < Lines.Count) do
  begin
    Line:= Lines[Index] ;

    // Done
    if Copy(Line, 1, 2) <> #9#9 then Break;

    // DataString "Weapon-2"
    if LineStarts(Line, #9#9'DataString') then
    begin
      Tag.Name:= String(ReadString(Line));
    end else
    //  Position [ 147.093948 -14.969450 -31.450413 ]
    if LineStarts(Line, #9#9'Position') then
    begin
      Tag.Position:= ReadVector(Line);
    end;
    //  Position [ 147.093948 -14.969450 -31.450413 ]
    if LineStarts(Line, #9#9'Orientation') then
    begin
      Inc(Index);
      V1:= ReadVector(Lines[Index]);
      Inc(Index);
      V2:= ReadVector(Lines[Index]);
      Inc(Index);
      V3:= ReadVector(Lines[Index]);

      Matrix:= TMatrix4.Identity;
      Matrix.M11:= V1.X;
      Matrix.M12:= V1.Y;
      Matrix.M13:= V1.Z;

      Matrix.M21:= V2.X;
      Matrix.M22:= V2.Y;
      Matrix.M23:= V2.Z;

      Matrix.M31:= V3.X;
      Matrix.M32:= V3.Y;
      Matrix.M33:= V3.Z;

      Tag.Direction:= Matrix * Vector3f_AxisZ;

    end;
    Inc(Index);
  end;
  Mesh.Tags.Add(Tag);
end;


//	Vertex
//		Position [ 10.579029 27.377001 322.142212 ]
//		Normal [ -0.011029 0.992606 -0.120880 ]
//		Tangent [ -0.996078 0.494118 0.498039 ]
//		Color 0
//		U0 0.735369
//		V0 0.427716
//		U1 0.735369
//		V1 0.427716
//------------------------------------------------------------------------------
procedure TImporterMesh.LoadVertex(Mesh: TPHXMesh; Lines: TStrings; var Index: Integer);
var Line : String;
var Vertex: TPHXMeshVertex;
begin
  Vertex.Joint   := PHX_JOINT_NONE;

  Vertex.Position:= Vector3f_Zero;
  Vertex.Normal  := Vector3f_Zero;
  Vertex.Tangent := Vector3f_Zero;

  Vertex.TexCoord0:= Vector2f_Zero;
  Vertex.TexCoord1:= Vector2f_Zero;

  Inc(Index);

  while (Index < Lines.Count) do
  begin
    Line:= Lines[Index] ;

    // Done
    if Copy(Line, 1, 2) <> #9#9 then Break;

    // Position [ 10.579029 27.377001 322.142212 ]
    if LineStarts(Line, #9#9'Position') then
    begin
      Vertex.Position:= ReadVector(Line);
    end else
    // Position [ 10.579029 27.377001 322.142212 ]
    if LineStarts(Line,  #9#9'Normal') then
    begin
      Vertex.Normal:= ReadVector(Line);
    end else
    // Tangent [ -0.996078 0.494118 0.498039 ]
    if LineStarts(Line, #9#9'Tangent') then
    begin
      Vertex.Tangent:= ReadVector(Line);
    end else
    // U0 0.735369
    if LineStarts(Line, #9#9'U0') then
    begin
      Vertex.TexCoord0.X:= ReadSingle(Line);
    end else
    // V0 0.427716
    if LineStarts(Line, #9#9'V0') then
    begin
      Vertex.TexCoord0.Y:= ReadSingle(Line);
    end else
    // U1 0.735369
    if LineStarts(Line, #9#9'U1') then
    begin
      Vertex.TexCoord1.X:= ReadSingle(Line);
    end else
    // V1 0.427716
    if LineStarts(Line, #9#9'V1') then
    begin
      Vertex.TexCoord1.Y:= ReadSingle(Line);
    end;

    Inc(Index);
  end;

  Mesh.Vertices.Add(Vertex);
end;
//
//	Triangle
//		iVertex0 0
//		iVertex1 2
//		iVertex2 1
//		iMaterial 0
//------------------------------------------------------------------------------
procedure TImporterMesh.LoadTriangle(Mesh: TPHXMesh; Lines: TStrings; var Index: Integer);
var Line : String;
var Triangle: TPHXMeshTriangle;
begin
  Triangle.Vertex0:= -1;
  Triangle.Vertex1:= -1;
  Triangle.Vertex2:= -1;

  Inc(Index);

  while (Index < Lines.Count) do
  begin
    Line:= Lines[Index] ;

    // Done
    if Copy(Line, 1, 2) <> #9#9 then Break;


    // Vertex0 0
    if Copy(Line, 1, 10) = #9#9'iVertex0' then
    begin
      Triangle.Vertex0:= ReadInteger(Line);
    end;
    // iVertex1 2
    if Copy(Line, 1, 10) = #9#9'iVertex1' then
    begin
      Triangle.Vertex1:= ReadInteger(Line);
    end;
    // iVertex2 1
    if Copy(Line, 1, 10) = #9#9'iVertex2' then
    begin
      Triangle.Vertex2:= ReadInteger(Line);
    end;
    // iMaterial 0

    Inc(Index);
  end;

  if (Triangle.Vertex0 >= 0) and ( Triangle.Vertex1 >= 0) and (Triangle.Vertex2 >= 0) then
  begin
    Mesh.Triangles.Add(Triangle);

    //Mesh.Vertices.Add(Verticies.List^[Triangle.Vertex0]);
    //Mesh.Vertices.Add(Verticies.List^[Triangle.Vertex1]);
    //Mesh.Vertices.Add(Verticies.List^[Triangle.Vertex2]);
  end;
end;

// Position [ 10.579029 27.377001 322.142212 ]
//------------------------------------------------------------------------------
function TImporterMesh.ReadVector(const Line: String): TVector3f;
var P0: Integer;
var P1: Integer;
var P2: Integer;
var P3: Integer;
var SX: String;
var SY: String;
var SZ: String;
begin
  P0:= Pos('[', Line)+2;
  P1:= PosEx(' ', Line, P0+1)+1;
  P2:= PosEx(' ', Line, P1+1)+1;
  P3:= PosEx(']', Line, P1+1);

  SX:= Copy(Line, P0, P1-P0);
  SY:= Copy(Line, P1, P2-P1);
  SZ:= Copy(Line, P2, P3-P2);


  DecimalSeparator:= '.';

  Result.X:= StrToFloatDef(SX, 0);
  Result.Y:= StrToFloatDef(SY, 0);
  Result.Z:= StrToFloatDef(SZ, 0);
end;

// U0 0.735369
//------------------------------------------------------------------------------
function TImporterMesh.ReadSingle(const Line: String): Single;
var P0: Integer;
var SV: String;
begin
  P0:= Pos(' ', Line)+1;

  SV:= Copy(Line, P0, MaxInt);

  Result:= StrToFloatDef(SV, 0);
end;

// U0 0.735369
//------------------------------------------------------------------------------
function TImporterMesh.ReadInteger(const Line: String): Integer;
var P0: Integer;
var SV: String;
begin
  P0:= Pos(' ', Line)+1;

  SV:= Copy(Line, P0, MaxInt);

  Result:= StrToIntDef(SV, 0);
end;

// DataString "Weapon-2"
//------------------------------------------------------------------------------
function TImporterMesh.ReadString(const Line: String): String;
var P0: Integer;
var P1: Integer;
begin
  P0:= PosEx('"', Line, 1)+1;
  P1:= PosEx('"', Line, P0+1);

  Result:= Copy(Line, P0, P1-P0);
end;


// 	Diffuse ffffffff
//------------------------------------------------------------------------------
function TImporterMesh.ReadColor(const Line: String): TColor4f;
var Index: Integer;
var Value: String;
var Color: Cardinal;
begin
  Index:= Pos(' ', Line)+1;

  Value:= Copy(Line, Index, MaxInt);

  Color:= StrToInt('$' + Value);

  Result.Red  := GetRValue(Color) / 255;
  Result.Green:= GetGValue(Color) / 255;
  Result.Blue := GetBValue(Color) / 255;
  Result.Alpha:= GetAValue(Color) / 255;
end;


initialization
  TImporterFactory.Add('Sins of a Solar Empire', '.mesh', TImporterMesh);
end.


