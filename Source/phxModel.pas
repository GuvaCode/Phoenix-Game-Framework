////////////////////////////////////////////////////////////////////////////////
//                                                                            //
//    Phoenix Game Framework                                                  //
//                                                                            //
//    http://www.phoenixlib.net                                               //
//                                                                            //
//    The contents of this file are used with permission, subject to          //
//    the Mozilla Public License Version 1.1 (the "License"); you may         //
//    not use this file except in compliance with the License. You may        //
//    obtain a copy of the License at                                         //
//    http://www.mozilla.org/MPL/MPL-1.1.html                                 //
//                                                                            //
//    Software distributed under the License is distributed on an             //
//    "AS IS" basis, WITHOUT WARRANTY OF ANY KIND, either express or          //
//    implied. See the License for the specific language governing            //
//    rights and limitations under the License.                               //
//                                                                            //
////////////////////////////////////////////////////////////////////////////////
unit phxModel;
//< Model

interface

{$I 'phxConfig.inc'}

uses
  Classes, SysUtils,

  phxLogger,
  phxTypes,
  phxClasses,
  phxEffect,
  phxMath,

  phxDevice,
  phxGraphics;

{$MINENUMSIZE 4}

const

// Default file extension of meshes
PHXMESH_EXT   = '.phxmesh';
// Mesh file identifier
PHXMESH_IDENT = 'PHXMESH';
// Mesh file Version
PHXMESH_VERSION = 3;

PHX_JOINT_NONE = 255;

type

// Forward declarations
TPHXMesh = class;


{$REGION 'TPHXMeshVertex'}
(*
	Vertex
		Position [ -102.490776 -19.147852 -475.934540 ]
		Normal [ -0.997994 0.009958 -0.062517 ]
		Tangent [ 0.008178 -0.168346 0.985694 ]
		Color 0
		U0 0.951060
		V0 0.402023
		U1 0.000000
		V1 0.000000
*)

// A single vertex in the mesh
//------------------------------------------------------------------------------
TPHXMeshVertex = record
  // Index of the owning joint index or PHX_JOINT_NONE
  Joint: Word;
  // Vertex position
  Position: TVector3f;
  // The normal
  Normal: TVector3f;
  // The tangent vector
  Tangent: TVector3f;
  // Primary texture coordinates
  TexCoord0: TVector2f;
  // Secondary texture coordinates
  TexCoord1: TVector2f;
end;

PPHXMeshVertex = ^TPHXMeshVertex;

PMeshVertexList = ^TMeshVertexList;
TMeshVertexList = array[0.. $00FFFFFF] of TPHXMeshVertex;

// List of verticies for a mesh
//------------------------------------------------------------------------------
TPHXMeshVertices = class
  private
    FMesh    : TPHXMesh;
    FCount   : Integer;
    FCapacity: Integer;
    FList    : PMeshVertexList;

    procedure Grow;

    function  GetItem(const Index: Integer): TPHXMeshVertex;
    procedure SetItem(const Index: Integer; const Value: TPHXMeshVertex);

    procedure SetCapacity(const Value: Integer);
    procedure SetCount   (const Value: Integer);
  public
    constructor Create(AMesh: TPHXMesh);
    destructor Destroy; override;

    // Load the vertices from a stream
    procedure LoadVertices(Reader: TPHXReader);
    // Save the vertices to a stream
    procedure SaveVertices(Writer: TPHXWriter);

    // Remove all verticies
    procedure Clear;

    // Add a new vertex to the list
    procedure Add(const Value: TPHXMeshVertex);

    // Number of verticies
    property Count: Integer read FCount write SetCount;
    // Capacity of the list
    property Capacity: Integer read FCapacity write SetCapacity;
    // Pointer to the internal list
    property List: PMeshVertexList read FList;
    // The verticies in the list
    property Items[const Index: Integer]: TPHXMeshVertex read GetItem write SetItem; default;
  end;

{$ENDREGION}

{$REGION 'TPHXMeshTriangle'}

// A single tiangle in the mesh
//------------------------------------------------------------------------------
TPHXMeshTriangle = record
  // Index of the first vertex
  Vertex0: Integer;
  // Index of the second vertex
  Vertex1: Integer;
  // Index of the third vertex
  Vertex2: Integer;
  // Material
end;

PPHXMeshTriangle = ^TPHXMeshTriangle;

PMeshTriangleList = ^TMeshTriangleList;
TMeshTriangleList = array[0.. $00FFFFFF] of TPHXMeshTriangle;

// List of verticies for a mesh
//------------------------------------------------------------------------------
TPHXMeshTriangles = class
  private
    FMesh    : TPHXMesh;
    FCount   : Integer;
    FCapacity: Integer;
    FList    : PMeshTriangleList;

    procedure Grow;

    function  GetItem(const Index: Integer): TPHXMeshTriangle;
    procedure SetItem(const Index: Integer; const Value: TPHXMeshTriangle);

    procedure SetCapacity(const Value: Integer);
    procedure SetCount   (const Value: Integer);
  public
    constructor Create(AMesh: TPHXMesh);
    destructor Destroy; override;

    // Load the triangles from a stream
    procedure LoadTriangles(Reader: TPHXReader);
    // Save the triangles to a stream
    procedure SaveTriangles(Writer: TPHXWriter);

    // Remove all triangles
    procedure Clear;

    // Add a triangle to the list
    procedure Add(const Value: TPHXMeshTriangle); overload;
    // Add a triangle to the list
    procedure Add(const Vertex0, Vertex1, Vertex2: Integer); overload;

    // Number of triangles
    property Count: Integer read FCount write SetCount;
    // Capacity of the list
    property Capacity: Integer read FCapacity write SetCapacity;
    // Pointer to the internal list
    property List: PMeshTriangleList read FList;
    // The triangles in the list
    property Items[const Index: Integer]: TPHXMeshTriangle read GetItem write SetItem; default;
  end;



(*
	NumTriangles 2462
	Triangle
		iVertex0 0
		iVertex1 1
		iVertex2 2
		iMaterial 0
  *)

{$ENDREGION}

{$REGION 'TPHXMeshGroup'}

//------------------------------------------------------------------------------
TPHXMeshGroup = record
  // Name of the group
  Name: String[32];
  // Index of the first triangle
  TriangleOffset: Integer;
  // Number of triangles in the group
  TriangleCount: Integer;
end;

// Pointer to a group in the mesh
PPHXMeshGroup = ^TPHXMeshGroup;

PMeshGroupList = ^TMeshGroupList;
TMeshGroupList = array[0.. $002FFFFF] of TPHXMeshGroup;

//------------------------------------------------------------------------------
TPHXMeshGroups = class
  private
    FMesh    : TPHXMesh;
    FCount   : Integer;
    FCapacity: Integer;
    FList    : PMeshGroupList;

    procedure Grow;

    function  GetItem(const Index: Integer): TPHXMeshGroup;
    procedure SetItem(const Index: Integer; const Value: TPHXMeshGroup);

    procedure SetCapacity(const Value: Integer);
    procedure SetCount   (const Value: Integer);
  public
    constructor Create(AMesh: TPHXMesh);
    destructor Destroy; override;

    // Load the groups from a stream
    procedure LoadGroups(Reader: TPHXReader);
    // Save the groups to a stream
    procedure SaveGroups(Writer: TPHXWriter);

    // Remove all groups
    procedure Clear;

    // Add a new group to the list
    procedure Add(const Value: TPHXMeshGroup);

    // Number of groups
    property Count: Integer read FCount write SetCount;
    // Capacity of the list
    property Capacity: Integer read FCapacity write SetCapacity;
    // Pointer to the internal list
    property List: PMeshGroupList read FList;
    // The groups in the list
    property Items[const Index: Integer]: TPHXMeshGroup read GetItem write SetItem; default;
  end;

{$ENDREGION}

{$REGION 'TPHXMeshJoint'}

//------------------------------------------------------------------------------
TPHXMeshJoint = record
  // Name of the joint
  Name: String[32];
  // The ident of the parent joint
  Parent: Word;
  // Name of the parent joint
  ParentName: String[32];
  // Position of the joint relative to the parent joint
  Position: TVector3f;
  // Rotation of the joint relative to the parent joint
  Rotation: TVector3f;
end;

PPHXMeshJoint = ^TPHXMeshJoint;

PMeshJointList = ^TMeshJointList;
TMeshJointList = array[0.. $00FFFFFF] of TPHXMeshJoint;

//------------------------------------------------------------------------------
TPHXMeshJoints = class
  private
    FMesh    : TPHXMesh;
    FCount   : Integer;
    FCapacity: Integer;
    FList    : PMeshJointList;

    procedure Grow;

    function  GetItem(const Index: Integer): TPHXMeshJoint;
    procedure SetItem(const Index: Integer; const Value: TPHXMeshJoint);

    procedure SetCapacity(const Value: Integer);
    procedure SetCount   (const Value: Integer);
  public
    constructor Create(AMesh: TPHXMesh);
    destructor Destroy; override;

    // Load the joints from a stream
    procedure LoadJoints(Reader: TPHXReader);
    // Save the joints to a stream
    procedure SaveJoints(Writer: TPHXWriter);

    // Remove all joints
    procedure Clear;

    // Add a new joint to the list
    procedure Add(const Value: TPHXMeshJoint);

    // Number of joints
    property Count: Integer read FCount write SetCount;
    // Capacity of the list
    property Capacity: Integer read FCapacity write SetCapacity;
    // Pointer to the internal list
    property List: PMeshJointList read FList;
    // The joints in the list
    property Items[const Index: Integer]: TPHXMeshJoint read GetItem write SetItem; default;
  end;

{$ENDREGION}

{$REGION 'TPHXMeshTag'}

(*
	Point
		DataString "Exhaust"
		Position [ -30.340488 -19.197464 -447.104034 ]
		Orientation
			 [ -1.000000 0.000000 0.000000 ]
			 [ 0.000000 1.000000 0.000000 ]
			 [ 0.000000 0.000000 -1.000000 ]
 *)

//------------------------------------------------------------------------------
TPHXMeshTag = record
  // Name of the tag
  Name : String[32];
  // The parent joint
  Joint: Word;
  // Name of the parent joint
  JointName: String[32];
  // Position of the tag relative to the joint
  Position: TVector3f;
  // Direction of the tag relative to the joint
  Direction: TVector3f;
end;

PPHXMeshTag = ^TPHXMeshTag;

PMeshTagList = ^TMeshTagList;
TMeshTagList = array[0.. $00FFFFFF] of TPHXMeshTag;

//------------------------------------------------------------------------------
TPHXMeshTags = class
  private
    FMesh    : TPHXMesh;
    FCount   : Integer;
    FCapacity: Integer;
    FList    : PMeshTagList;

    procedure Grow;

    function  GetItem(const Index: Integer): TPHXMeshTag;
    procedure SetItem(const Index: Integer; const Value: TPHXMeshTag);

    procedure SetCapacity(const Value: Integer);
    procedure SetCount   (const Value: Integer);
  public
    constructor Create(AMesh: TPHXMesh);
    destructor Destroy; override;

    // Load the tags from a stream
    procedure LoadTags(Reader: TPHXReader);
    // Save the tags to a stream
    procedure SaveTags(Writer: TPHXWriter);

    // Remove all tags
    procedure Clear;

    // Add a new tag to the list
    procedure Add(const Value: TPHXMeshTag);

    // Number of tags
    property Count: Integer read FCount write SetCount;
    // Capacity of the list
    property Capacity: Integer read FCapacity write SetCapacity;
    // Pointer to the internal list
    property List: PMeshTagList read FList;
    // The tag in the list
    property Items[const Index: Integer]: TPHXMeshTag read GetItem write SetItem; default;
  end;

{$ENDREGION}

{$REGION 'TPHXMeshTexture'}
(*

Material
		DiffuseTextureFileName "CapitalAtlantianBattleship-cl.dds"
		SelfIlluminationTextureFileName "CapitalAtlantianBattleship-da.dds"
		NormalTextureFileName "CapitalAtlantianBattleship-nm.dds"
		DisplacementTextureFileName ""
		TeamColorTextureFileName ""
		Diffuse ffffffff
		Ambient ffffffff
		Specular ffffffff
		Emissive ffffffff
		Glossiness 9.999999
*)

//------------------------------------------------------------------------------
TPHXMeshTextureKind = (
  // Custom texture
  tkCustom,
  // Diffuse texture
  tkDiffuse,
  // Normal map
  tkNormal,
  // Team colors
  tkTeam,
  // Alpha map
  tkAlphamap,
  // Displacement map
  tkDisplacement
);

// A texture for the mesh
//------------------------------------------------------------------------------
TPHXMeshTexture = record
  // Name of the texture
  Name: String[64];
  // The type this texture is
  Kind: TPHXMeshTextureKind;
end;

// Pointer to a group in the mesh
PPHXMeshTexture = ^TPHXMeshTexture;

PMeshTextureList = ^TMeshTextureList;
TMeshTextureList = array[0.. $002FFFFF] of TPHXMeshTexture;

//------------------------------------------------------------------------------
TPHXMeshTextures = class
  private
    FMesh    : TPHXMesh;
    FCount   : Integer;
    FCapacity: Integer;
    FList    : PMeshTextureList;

    procedure Grow;

    function GetItem(const Index: Integer): TPHXMeshTexture;

    procedure SetCapacity(const Value: Integer);
    procedure SetCount   (const Value: Integer);
  public
    // Create a new texture list
    constructor Create(AMesh: TPHXMesh);
    // Free this list
    destructor Destroy; override;

    // Load the textures from a stream
    procedure LoadTextures(Reader: TPHXReader);
    // Save the textures to a stream
    procedure SaveTextures(Writer: TPHXWriter);

    // Remove all textures
    procedure Clear;

    // Add a new texture to the list
    procedure Add(const Value: TPHXMeshTexture); overload;
    // Add a new texture to the list
    procedure Add(const Name: String; const Kind: TPHXMeshTextureKind); overload;

    procedure Delete(const Index: Integer);

    // Find a texture by kind
    function Find(const Kind: TPHXMeshTextureKind): String;

    // Number of groups
    property Count: Integer read FCount write SetCount;
    // Capacity of the list
    property Capacity: Integer read FCapacity write SetCapacity;
    // Pointer to the internal list
    property List: PMeshTextureList read FList;
    // The groups in the list
    property Items[const Index: Integer]: TPHXMeshTexture read GetItem; default;
  end;

{$ENDREGION}

{$REGION 'TPHXMeshMaterial'}

//------------------------------------------------------------------------------
TPHXMeshMaterial = class
  private
    // Material name
    FName: String;
    // Material color
    FDiffuse: TColor4f;
    // Custom material flags
    FFlags: Cardinal;
    // Shader name
    FShader: String;
    // The textures for the material
    FTextures: TPHXMeshTextures;
  public
    constructor Create(AMesh: TPHXMesh);
    destructor Destroy; override;

    // Load the material from a stream
    procedure LoadMaterial(Reader: TPHXReader);
    // Save the material to a stream
    procedure SaveMaterial(Writer: TPHXWriter);

    // Material name
    property Name: String read FName write FName;
    // Shader name
    property Shader: String read FShader write FShader;
    // Material color
    property Diffuse: TColor4f read FDiffuse write FDiffuse;
    // The textures for the material
    property Textures: TPHXMeshTextures read FTextures;
  end;





{$ENDREGION}

{$REGION 'TPHXMesh'}


// The file header for meshes.
//------------------------------------------------------------------------------
TPHXMeshHeader  = record
  // The ident of the file, should always be PHXMESH.
  Ident: array[1..7] of AnsiChar;
  // The file version.
  Version: Integer;
end;


// Basic mesh class
//------------------------------------------------------------------------------
TPHXMesh = class
  private
    FName       : String;
    FAuthor     : String;
    FVersion    : String;
    FComment   : String;

    FBounds: TBoxf;

    FMaterial : TPHXMeshMaterial;
    FVertices : TPHXMeshVertices;
    FTriangles: TPHXMeshTriangles;
    FGroups   : TPHXMeshGroups;
    FJoints   : TPHXMeshJoints;
    FTags     : TPHXMeshTags;
  protected
    // Load the model from a stream
    procedure LoadModel(Reader: TPHXReader);
    // Save the model to a stream
    procedure SaveModel(Writer: TPHXWriter);
  public
    // Default constructor
    constructor Create;
    destructor Destroy; override;


    // Load the mesh from a file
    procedure LoadFromFile(const FileName: String);
    // Load the mesh from a stream
    procedure LoadFromStream(Stream: TStream);

    // Save the mesh to a file
    procedure SaveToFile(const FileName: String);
    // Save the mesh to a stream
    procedure SaveToStream(Stream: TStream);

    // Render the mesh
    procedure Render(Buffer: TPHXBuffer; Effect: TPHXEffect); overload;
    // Render a single group
    procedure RenderGroup(Buffer: TPHXBuffer; Effect: TPHXEffect; const Group: Integer); overload;
    // Render the normals of the mesh
    procedure RenderNormals(Buffer: TPHXBuffer; Effect: TPHXEffect; const Length: Single; const Color: TColor4f);

    // Upload the mesh
    procedure Upload(Buffer: TPHXBuffer); overload;
    // Upload a single group
    procedure UploadGroup(Buffer: TPHXBuffer; const Group: Integer); overload;
    // Upload the normals of the mesh
    procedure UploadNormals(Buffer: TPHXBuffer; const Length: Single; const Color: TColor4f);

    // Upload the mesh
    procedure Upload(const Buffer: Pointer; Declaration: TPHXVertexDeclaration); overload;

    // Transform the model by an matrix
    procedure Transform(const Matrix: TMatrix4f);

    // Calculate the bounding box from the vertices in the mesh
    function CalculateBounds: TBoxf;

    // Name of the mesh
    property Name: String read FName write FName;
    // Author of the mesh
    property Author: String read FAuthor write FAuthor;
    // Version of the mesh
    property Version: String read FVersion write FVersion;
    // Comment of the mesh
    property Comment: String read FComment write FComment;
    // The material for the mesh
    property Material: TPHXMeshMaterial read FMaterial write FMaterial;
    // Bounding box of the mesh
    property Bounds: TBoxf read FBounds write FBounds;

    // List of verticies in the mesh
    property Vertices: TPHXMeshVertices read FVertices;
    // List of triangles in the mesh
    property Triangles: TPHXMeshTriangles read FTriangles;
    // List of groups in the mesh
    property Groups: TPHXMeshGroups  read FGroups;
    // List of joints in the mesh
    property Joints: TPHXMeshJoints  read FJoints;
    // List of tags in the mesh
    property Tags : TPHXMeshTags    read FTags;
  end;

{$ENDREGION}

implementation

{$REGION 'TPHXMeshVertexList'}

// TPHXMeshVertexList
//==============================================================================
constructor TPHXMeshVertices.Create(AMesh: TPHXMesh);
begin
  FMesh    := AMesh;
  FCount   := 0;
  FCapacity:= 0;
  FList    := nil;
end;

//------------------------------------------------------------------------------
destructor TPHXMeshVertices.Destroy;
begin
  SetCapacity(0);
end;

//------------------------------------------------------------------------------
procedure TPHXMeshVertices.LoadVertices(Reader: TPHXReader);
begin
  FCount:= Reader.ReadInteger;

  SetCapacity(FCount);

  Reader.Read(FList^, FCount * SizeOf(TPHXMeshVertex));
end;

//------------------------------------------------------------------------------
procedure TPHXMeshVertices.SaveVertices(Writer: TPHXWriter);
begin
  Writer.WriteInteger(FCount);

  Writer.Write(FList^, FCount * SizeOf(TPHXMeshVertex));
end;

//------------------------------------------------------------------------------
procedure TPHXMeshVertices.Clear;
begin
  FCount:= 0;
end;

//------------------------------------------------------------------------------
procedure TPHXMeshVertices.Add(const Value: TPHXMeshVertex);
begin
  Inc(FCount);

  if FCount > Capacity then Grow;

  FList^[Count - 1]:= Value;
end;

//------------------------------------------------------------------------------
procedure TPHXMeshVertices.Grow;
var Delta: Integer;
begin
  if FCapacity > 64 then
  begin
    Delta := FCapacity div 4
  end else
  if FCapacity > 8 then
  begin
    Delta := 16
  end else
  begin
    Delta := 4;
  end;

  SetCapacity(FCapacity + Delta);
end;


//------------------------------------------------------------------------------
procedure TPHXMeshVertices.SetCapacity(const Value: Integer);
begin
  if FCapacity <> Value then
  begin
    FCapacity := Value;

    ReallocMem(FList, FCapacity * SizeOf(TPHXMeshVertex));
  end;
end;

//------------------------------------------------------------------------------
procedure TPHXMeshVertices.SetCount(const Value: Integer);
begin
  FCount := Value;

  if(FCount > FCapacity) then SetCapacity(FCount);
end;

//------------------------------------------------------------------------------
function TPHXMeshVertices.GetItem(const Index: Integer): TPHXMeshVertex;
begin
  Assert( (Index >= 0) and (Index < Count), 'List index out of bounds');

  Result:= FList^[Index];
end;

//------------------------------------------------------------------------------
procedure TPHXMeshVertices.SetItem(const Index: Integer; const Value: TPHXMeshVertex);
begin
  Assert( (Index >= 0) and (Index < Count), 'List index out of bounds');

  FList^[Index]:= Value;
end;

{$ENDREGION}

{$REGION 'TPHXMeshTriangleList'}

// TPHXMeshTriangleList
//==============================================================================
constructor TPHXMeshTriangles.Create(AMesh: TPHXMesh);
begin
  FMesh    := AMesh;
  FCount   := 0;
  FCapacity:= 0;
  FList    := nil;
end;

//------------------------------------------------------------------------------
destructor TPHXMeshTriangles.Destroy;
begin
  SetCapacity(0);
end;

//------------------------------------------------------------------------------
procedure TPHXMeshTriangles.LoadTriangles(Reader: TPHXReader);
begin
  FCount:= Reader.ReadInteger;

  SetCapacity(FCount);

  Reader.Read(FList^, FCount * SizeOf(TPHXMeshTriangle));
end;

//------------------------------------------------------------------------------
procedure TPHXMeshTriangles.SaveTriangles(Writer: TPHXWriter);
begin
  Writer.WriteInteger(FCount);

  Writer.Write(FList^, FCount * SizeOf(TPHXMeshTriangle));
end;

//------------------------------------------------------------------------------
procedure TPHXMeshTriangles.Clear;
begin
  FCount:= 0;
end;

//------------------------------------------------------------------------------
procedure TPHXMeshTriangles.Add(const Value: TPHXMeshTriangle);
begin
  Inc(FCount);

  if FCount > Capacity then Grow;

  FList^[Count - 1]:= Value;
end;

//------------------------------------------------------------------------------
procedure TPHXMeshTriangles.Add(const Vertex0, Vertex1, Vertex2: Integer);
begin
  Inc(FCount);

  if FCount > Capacity then Grow;

  FList^[Count - 1].Vertex0:= Vertex0;
  FList^[Count - 1].Vertex1:= Vertex1;
  FList^[Count - 1].Vertex2:= Vertex2;
end;

//------------------------------------------------------------------------------
procedure TPHXMeshTriangles.Grow;
var Delta: Integer;
begin
  if FCapacity > 64 then
  begin
    Delta := FCapacity div 4
  end else
  if FCapacity > 8 then
  begin
    Delta := 16
  end else
  begin
    Delta := 4;
  end;

  SetCapacity(FCapacity + Delta);
end;

//------------------------------------------------------------------------------
procedure TPHXMeshTriangles.SetCapacity(const Value: Integer);
begin
  if FCapacity <> Value then
  begin
    FCapacity := Value;

    ReallocMem(FList, FCapacity * SizeOf(TPHXMeshTriangle));
  end;
end;

//------------------------------------------------------------------------------
procedure TPHXMeshTriangles.SetCount(const Value: Integer);
begin
  FCount := Value;

  if(FCount > FCapacity) then SetCapacity(FCount);
end;

//------------------------------------------------------------------------------
function TPHXMeshTriangles.GetItem(const Index: Integer): TPHXMeshTriangle;
begin
  Assert( (Index >= 0) and (Index < Count), 'List index out of bounds');

  Result:= FList^[Index];
end;

//------------------------------------------------------------------------------
procedure TPHXMeshTriangles.SetItem(const Index: Integer; const Value: TPHXMeshTriangle);
begin
  Assert( (Index >= 0) and (Index < Count), 'List index out of bounds');

  FList^[Index]:= Value;
end;

{$ENDREGION}


{$REGION 'TPHXMeshGroup'}

// TPHXMeshGroupList
//==============================================================================
constructor TPHXMeshGroups.Create(AMesh: TPHXMesh);
begin
  FMesh    := AMesh;
  FCount   := 0;
  FCapacity:= 0;
  FList    := nil;
end;

//------------------------------------------------------------------------------
destructor TPHXMeshGroups.Destroy;
begin
  SetCapacity(0);
end;

//------------------------------------------------------------------------------
procedure TPHXMeshGroups.LoadGroups(Reader: TPHXReader);
var Index: Integer;
begin
  FCount:= Reader.ReadInteger;

  SetCapacity(FCount);

  for Index := 0 to FCount-1 do
  begin
    Reader.Read(FList^[Index].Name, SizeOf(FList^[Index].Name));

    Reader.Read(FList^[Index].TriangleOffset, SizeOf(FList^[Index].TriangleOffset));
    Reader.Read(FList^[Index].TriangleCount , SizeOf(FList^[Index].TriangleCount));
  end;
end;

//------------------------------------------------------------------------------
procedure TPHXMeshGroups.SaveGroups(Writer: TPHXWriter);
var Index: Integer;
begin
  Writer.WriteInteger(FCount);

  for Index := 0 to FCount-1 do
  begin
    Writer.Write(FList^[Index].Name, SizeOf(FList^[Index].Name));

    Writer.Write(FList^[Index].TriangleOffset, SizeOf(FList^[Index].TriangleOffset));
    Writer.Write(FList^[Index].TriangleCount , SizeOf(FList^[Index].TriangleCount));
  end;
end;


//------------------------------------------------------------------------------
procedure TPHXMeshGroups.Clear;
begin
  FCount:= 0;
end;

//------------------------------------------------------------------------------
procedure TPHXMeshGroups.Add(const Value: TPHXMeshGroup);
begin
  Inc(FCount);

  if FCount > Capacity then Grow;

  FList^[Count - 1]:= Value;
end;

//------------------------------------------------------------------------------
procedure TPHXMeshGroups.Grow;
var Delta: Integer;
begin
  if FCapacity > 64 then
  begin
    Delta := FCapacity div 4
  end else
  if FCapacity > 8 then
  begin
    Delta := 16
  end else
  begin
    Delta := 4;
  end;

  SetCapacity(FCapacity + Delta);
end;

//------------------------------------------------------------------------------
procedure TPHXMeshGroups.SetCapacity(const Value: Integer);
begin
  if FCapacity <> Value then
  begin
    FCapacity := Value;

    ReallocMem(FList, FCapacity * SizeOf(TPHXMeshGroup));
  end;
end;

//------------------------------------------------------------------------------
procedure TPHXMeshGroups.SetCount(const Value: Integer);
begin
  FCount := Value;

  if(FCount > FCapacity) then SetCapacity(FCount);
end;

//------------------------------------------------------------------------------
function TPHXMeshGroups.GetItem(const Index: Integer): TPHXMeshGroup;
begin
  Assert( (Index >= 0) and (Index < Count), 'List index out of bounds');

  Result:= FList^[Index];
end;

//------------------------------------------------------------------------------
procedure TPHXMeshGroups.SetItem(const Index: Integer; const Value: TPHXMeshGroup);
begin
  Assert( (Index >= 0) and (Index < Count), 'List index out of bounds');

  FList^[Index]:= Value;
end;

{$ENDREGION}

{$REGION 'TPHXMeshJoint'}

// TPHXMeshJointList
//==============================================================================
constructor TPHXMeshJoints.Create(AMesh: TPHXMesh);
begin
  FMesh    := AMesh;
  FCount   := 0;
  FCapacity:= 0;
  FList    := nil;
end;

//------------------------------------------------------------------------------
destructor TPHXMeshJoints.Destroy;
begin
  SetCapacity(0);
end;

//------------------------------------------------------------------------------
procedure TPHXMeshJoints.LoadJoints(Reader: TPHXReader);
var Index: Integer;
begin
  FCount:= Reader.ReadInteger;

  SetCapacity(FCount);

  for Index := 0 to FCount-1 do
  begin
    Reader.Read(FList^[Index].Name       , SizeOf(FList^[Index].Name));
    Reader.Read(FList^[Index].Parent     , SizeOf(FList^[Index].Parent));
    Reader.Read(FList^[Index].ParentName , SizeOf(FList^[Index].ParentName));
    Reader.Read(FList^[Index].Position   , SizeOf(FList^[Index].Position));
    Reader.Read(FList^[Index].Rotation   , SizeOf(FList^[Index].Rotation));
  end;
end;

//------------------------------------------------------------------------------
procedure TPHXMeshJoints.SaveJoints(Writer: TPHXWriter);
var Index: Integer;
begin
  Writer.WriteInteger(FCount);

  for Index := 0 to FCount-1 do
  begin
    Writer.Write(FList^[Index].Name       , SizeOf(FList^[Index].Name));
    Writer.Write(FList^[Index].Parent     , SizeOf(FList^[Index].Parent));
    Writer.Write(FList^[Index].ParentName , SizeOf(FList^[Index].ParentName));
    Writer.Write(FList^[Index].Position   , SizeOf(FList^[Index].Position));
    Writer.Write(FList^[Index].Rotation   , SizeOf(FList^[Index].Rotation));
  end;
end;

//------------------------------------------------------------------------------
procedure TPHXMeshJoints.Clear;
begin
  FCount:= 0;
end;

//------------------------------------------------------------------------------
procedure TPHXMeshJoints.Add(const Value: TPHXMeshJoint);
begin
  Inc(FCount);

  if FCount > Capacity then Grow;

  FList^[Count - 1]:= Value;
end;

//------------------------------------------------------------------------------
procedure TPHXMeshJoints.Grow;
var Delta: Integer;
begin
  if FCapacity > 64 then
  begin
    Delta := FCapacity div 4
  end else
  if FCapacity > 8 then
  begin
    Delta := 16
  end else
  begin
    Delta := 4;
  end;

  SetCapacity(FCapacity + Delta);
end;

//------------------------------------------------------------------------------
procedure TPHXMeshJoints.SetCapacity(const Value: Integer);
begin
  if FCapacity <> Value then
  begin
    FCapacity := Value;

    ReallocMem(FList, FCapacity * SizeOf(TPHXMeshJoint));
  end;
end;

//------------------------------------------------------------------------------
procedure TPHXMeshJoints.SetCount(const Value: Integer);
begin
  FCount := Value;

  if(FCount > FCapacity) then SetCapacity(FCount);
end;

//------------------------------------------------------------------------------
function TPHXMeshJoints.GetItem(const Index: Integer): TPHXMeshJoint;
begin
  Assert( (Index >= 0) and (Index < Count), 'List index out of bounds');

  Result:= FList^[Index];
end;

//------------------------------------------------------------------------------
procedure TPHXMeshJoints.SetItem(const Index: Integer; const Value: TPHXMeshJoint);
begin
  Assert( (Index >= 0) and (Index < Count), 'List index out of bounds');

  FList^[Index]:= Value;
end;

{$ENDREGION}

{$REGION 'TPHXMeshTag'}

// TPHXMeshTagList
//==============================================================================
constructor TPHXMeshTags.Create(AMesh: TPHXMesh);
begin
  FMesh    := AMesh;
  FCount   := 0;
  FCapacity:= 0;
  FList    := nil;
end;

//------------------------------------------------------------------------------
destructor TPHXMeshTags.Destroy;
begin
  SetCapacity(0);
end;

//------------------------------------------------------------------------------
procedure TPHXMeshTags.LoadTags(Reader: TPHXReader);
var Index: Integer;
begin
  FCount:= Reader.ReadInteger;

  SetCapacity(FCount);

  for Index := 0 to FCount-1 do
  begin
    Reader.Read( FList^[Index].Name       , SizeOf(FList^[Index].Name));
    Reader.Read( FList^[Index].Joint      , SizeOf(FList^[Index].Joint));
    Reader.Read( FList^[Index].JointName  , SizeOf(FList^[Index].JointName));
    Reader.Read( FList^[Index].Position   , SizeOf(FList^[Index].Position));
    Reader.Read( FList^[Index].Direction  , SizeOf(FList^[Index].Direction));
  end;
end;

//------------------------------------------------------------------------------
procedure TPHXMeshTags.SaveTags(Writer: TPHXWriter);
var Index: Integer;
begin
  Writer.WriteInteger(FCount);

  for Index := 0 to FCount-1 do
  begin
    Writer.Write(FList^[Index].Name       , SizeOf(FList^[Index].Name));
    Writer.Write(FList^[Index].Joint      , SizeOf(FList^[Index].Joint));
    Writer.Write(FList^[Index].JointName  , SizeOf(FList^[Index].JointName));
    Writer.Write(FList^[Index].Position   , SizeOf(FList^[Index].Position));
    Writer.Write(FList^[Index].Direction  , SizeOf(FList^[Index].Direction));
  end;
end;

//------------------------------------------------------------------------------
procedure TPHXMeshTags.Clear;
begin
  FCount:= 0;
end;

//------------------------------------------------------------------------------
procedure TPHXMeshTags.Add(const Value: TPHXMeshTag);
begin
  Inc(FCount);

  if FCount > Capacity then Grow;

  FList^[Count - 1]:= Value;
end;

//------------------------------------------------------------------------------
procedure TPHXMeshTags.Grow;
var Delta: Integer;
begin
  if FCapacity > 64 then
  begin
    Delta := FCapacity div 4
  end else
  if FCapacity > 8 then
  begin
    Delta := 16
  end else
  begin
    Delta := 4;
  end;

  SetCapacity(FCapacity + Delta);
end;

//------------------------------------------------------------------------------
procedure TPHXMeshTags.SetCapacity(const Value: Integer);
begin
  if FCapacity <> Value then
  begin
    FCapacity := Value;

    ReallocMem(FList, FCapacity * SizeOf(TPHXMeshTag));
  end;
end;

//------------------------------------------------------------------------------
procedure TPHXMeshTags.SetCount(const Value: Integer);
begin
  FCount := Value;

  if(FCount > FCapacity) then SetCapacity(FCount);
end;

//------------------------------------------------------------------------------
function TPHXMeshTags.GetItem(const Index: Integer): TPHXMeshTag;
begin
  Assert( (Index >= 0) and (Index < Count), 'List index out of bounds');

  Result:= FList^[Index];
end;

//------------------------------------------------------------------------------
procedure TPHXMeshTags.SetItem(const Index: Integer; const Value: TPHXMeshTag);
begin
  Assert( (Index >= 0) and (Index < Count), 'List index out of bounds');

  FList^[Index]:= Value;
end;

{$ENDREGION}


{$REGION 'TPHXMeshTextures'}

// TPHXMeshTextures
//==============================================================================
constructor TPHXMeshTextures.Create(AMesh: TPHXMesh);
begin
  FMesh    := AMesh;
  FCount   := 0;
  FCapacity:= 0;
  FList    := nil;
end;

//------------------------------------------------------------------------------
destructor TPHXMeshTextures.Destroy;
begin
  SetCapacity(0);
end;

//------------------------------------------------------------------------------
procedure TPHXMeshTextures.Grow;
var Delta: Integer;
begin
  if FCapacity > 64 then
  begin
    Delta := FCapacity div 4
  end else
  if FCapacity > 8 then
  begin
    Delta := 16
  end else
  begin
    Delta := 4;
  end;

  SetCapacity(FCapacity + Delta);
end;

//------------------------------------------------------------------------------
procedure TPHXMeshTextures.LoadTextures(Reader: TPHXReader);
var Index: Integer;
begin
  FCount:= Reader.ReadInteger;

  SetCapacity(FCount);

  for Index := 0 to FCount-1 do
  begin
    Reader.Read(FList^[Index].Name, SizeOf(FList^[Index].Name));
    Reader.Read(FList^[Index].Kind, SizeOf(FList^[Index].Kind));
  end;
end;

//------------------------------------------------------------------------------
procedure TPHXMeshTextures.SaveTextures(Writer: TPHXWriter);
var Index: Integer;
begin
  Writer.WriteInteger(FCount);

  for Index := 0 to FCount-1 do
  begin
    Writer.Write(FList^[Index].Name, SizeOf(FList^[Index].Name));
    Writer.Write(FList^[Index].Kind, SizeOf(FList^[Index].Kind));
  end;
end;

//------------------------------------------------------------------------------
procedure TPHXMeshTextures.Clear;
begin
  FCount:= 0;

  SetCapacity(0);
end;

//------------------------------------------------------------------------------
procedure TPHXMeshTextures.Add(const Value: TPHXMeshTexture);
begin
  Inc(FCount);

  if FCount > Capacity then Grow;

  FList^[Count - 1]:= Value;
end;

//------------------------------------------------------------------------------
procedure TPHXMeshTextures.Add(const Name: String; const Kind: TPHXMeshTextureKind);
var Texture: TPHXMeshTexture;
begin
  Texture.Name:= ShortString(Name);
  Texture.Kind:= Kind;

  Add(Texture);
end;

//------------------------------------------------------------------------------
procedure TPHXMeshTextures.Delete(const Index: Integer);
begin
  if (Index < 0) or (Index >= FCount) then Exit;

  FCount:= FCount-1;

  System.Move(FList^[Index+1], FList^[Index], (FCount - Index) * SizeOf(TPHXMeshTexture));
end;

//------------------------------------------------------------------------------
function TPHXMeshTextures.Find(const Kind: TPHXMeshTextureKind): String;
var Index: Integer;
begin
  for Index := 0 to FCount-1 do
  begin
    if FList^[Index].Kind = Kind then
    begin
      Result:= String(FList^[Index].Name);
      Exit;
    end;
  end;
  Result:= '';
end;



//------------------------------------------------------------------------------
procedure TPHXMeshTextures.SetCapacity(const Value: Integer);
begin
  if FCapacity <> Value then
  begin
    FCapacity := Value;

    ReallocMem(FList, FCapacity * SizeOf(TPHXMeshTexture));
  end;
end;

//------------------------------------------------------------------------------
procedure TPHXMeshTextures.SetCount(const Value: Integer);
begin
  FCount := Value;

  if(FCount > FCapacity) then SetCapacity(FCount);
end;

//------------------------------------------------------------------------------
function TPHXMeshTextures.GetItem(const Index: Integer): TPHXMeshTexture;
begin
  Assert( (Index >= 0) and (Index < Count), 'List index out of bounds');

  Result:= FList^[Index];
end;


{$ENDREGION}

{$REGION 'TPHXMeshMaterial'}

// Material for a mesh
//=============================================================================
constructor TPHXMeshMaterial.Create(AMesh: TPHXMesh);
begin
  FTextures:= TPHXMeshTextures.Create(AMesh);

  FName    := 'Material';
  FDiffuse := clrWhite;
  FFlags   := 0;
  FShader  := '';
end;

//------------------------------------------------------------------------------
destructor TPHXMeshMaterial.Destroy;
begin
  FTextures.Free;
  inherited;
end;

//------------------------------------------------------------------------------
procedure TPHXMeshMaterial.LoadMaterial(Reader: TPHXReader);
begin
  // Material name
  FName:= Reader.ReadString;
  // Shader name
  FShader:= Reader.ReadString;;

  // Material color
  FDiffuse:= Reader.ReadColor;

  FTextures.LoadTextures(Reader);
end;

//------------------------------------------------------------------------------
procedure TPHXMeshMaterial.SaveMaterial(Writer: TPHXWriter);
begin
  Writer.WriteString(FName);
  Writer.WriteString(FShader);

  Writer.WriteColor(FDiffuse);

  FTextures.SaveTextures(Writer);
end;

{$ENDREGION}

{$REGION 'TPHXMesh'}

//=============================================================================
constructor TPHXMesh.Create;
begin
  FName   := 'Mesh';
  FAuthor := '';
  FVersion:= '';
  FComment:= '';

  FMaterial := TPHXMeshMaterial.Create(Self);
  FGroups   := TPHXMeshGroups.Create(Self);
  FVertices := TPHXMeshVertices.Create(Self);
  FTriangles:= TPHXMeshTriangles.Create(Self);

  FJoints  := TPHXMeshJoints.Create(Self);
  FTags    := TPHXMeshTags.Create(Self);
end;

//------------------------------------------------------------------------------
destructor TPHXMesh.Destroy;
begin
  FMaterial.Free;
  FGroups.Free;
  FVertices.Free;
  FTriangles.Free;
  FJoints.Free;
  FTags.Free;

  inherited;
end;

//------------------------------------------------------------------------------
procedure TPHXMesh.LoadModel(Reader: TPHXReader);
begin
  FName   := Reader.ReadString;
  FAuthor := Reader.ReadString;
  FVersion:= Reader.ReadString;
  FComment:= Reader.ReadString;

  Reader.Read(FBounds, SizeOf(FBounds));

  FMaterial.LoadMaterial(Reader);
  FVertices.LoadVertices(Reader);
  FTriangles.LoadTriangles(Reader);
  FGroups.LoadGroups(Reader);
  FJoints.LoadJoints(Reader);
  FTags.LoadTags(Reader);
end;

//------------------------------------------------------------------------------
procedure TPHXMesh.SaveModel(Writer: TPHXWriter);
begin
  Writer.WriteString(FName);
  Writer.WriteString(FAuthor);
  Writer.WriteString(FVersion);
  Writer.WriteString(FComment);

  Writer.Write(FBounds, SizeOf(FBounds));

  FMaterial.SaveMaterial(Writer);
  FVertices.SaveVertices(Writer);
  FTriangles.SaveTriangles(Writer);
  FGroups.SaveGroups(Writer);
  FJoints.SaveJoints(Writer);
  FTags.SaveTags(Writer);
end;

//------------------------------------------------------------------------------
procedure TPHXMesh.LoadFromFile(const FileName: String);
var Stream: TStream;
begin
  Stream:= TPHXContentManager.CreateStream(FileName, ckModel, fmOpenRead or fmShareDenyNone);
  try
    LoadFromStream(Stream);
  finally
    Stream.Free;
  end;
end;

//------------------------------------------------------------------------------
procedure TPHXMesh.LoadFromStream(Stream: TStream);
var Header: TPHXMeshHeader;
var Reader: TPHXReader;
begin
  Header.Ident  := #0#0#0#0#0#0#0;
  Header.Version:= 0;

  Stream.Read(Header.Ident  , SizeOf(Header.Ident));
  Stream.Read(Header.Version, SizeOf(Header.Version));

  if (Header.Ident <> PHXMESH_IDENT) then
  begin
    TPHXLogger.Error('TPHXMesh.LoadFromStream', 'Invalid mesh file.');

    raise Exception.Create('Invalid mesh file.');
  end;

  if (Header.Version <> PHXMESH_VERSION) then
  begin
    TPHXLogger.Error('TPHXMesh.LoadFromStream', 'Mesh version mismatch [File: %d Code: %d].', [Header.Version, PHXMESH_VERSION]);

    raise Exception.CreateFmt('Mesh version mismatch [File: %d Code: %d].', [Header.Version, PHXMESH_VERSION]);
  end;

  Reader:= TPHXReader.Create(Stream);
  try
    LoadModel(Reader);
  finally
    Reader.Free;
  end;
end;

//------------------------------------------------------------------------------
procedure TPHXMesh.SaveToFile(const FileName: String);
var Stream: TStream;
begin
  Stream:= TPHXContentManager.CreateStream(FileName, ckModel, fmCreate);
  try
    SaveToStream(Stream);
  finally
    Stream.Free;
  end;
end;

//------------------------------------------------------------------------------
procedure TPHXMesh.SaveToStream(Stream: TStream);
var Header: TPHXMeshHeader;
var Writer: TPHXWriter;
begin
  Header.Ident  := PHXMESH_IDENT;
  Header.Version:= PHXMESH_VERSION;

  Stream.Write(Header.Ident  , SizeOf(Header.Ident));
  Stream.Write(Header.Version, SizeOf(Header.Version));

  Writer:= TPHXWriter.Create(Stream);
  try
    SaveModel(Writer);
  finally
    Writer.Free;
  end;
end;








//------------------------------------------------------------------------------
procedure TPHXMesh.Transform(const Matrix: TMatrix4f);
var Index: Integer;
begin
  for Index := 0 to Vertices.Count - 1 do
  begin
    Vertices.List^[Index].Position:= Matrix_Transform(Matrix, Vertices.List^[Index].Position);
    Vertices.List^[Index].Normal  := Matrix_Rotate   (Matrix, Vertices.List^[Index].Normal  );
    Vertices.List^[Index].Tangent := Matrix_Rotate   (Matrix, Vertices.List^[Index].Tangent  );
  end;

  for Index := 0 to Joints.Count - 1 do
  begin
    Joints.List^[Index].Position:= Matrix_Transform(Matrix, Joints.List^[Index].Position);
    Joints.List^[Index].Rotation:= Matrix_Rotate   (Matrix, Joints.List^[Index].Rotation);
  end;

  for Index := 0 to Tags.Count - 1 do
  begin
    Tags.List^[Index].Position := Matrix_Transform(Matrix, Tags.List^[Index].Position);
    Tags.List^[Index].Direction:= Matrix_Rotate   (Matrix, Tags.List^[Index].Direction);
  end;
end;

//------------------------------------------------------------------------------
function TPHXMesh.CalculateBounds: TBoxf;
var Index : Integer;
begin
  if Vertices.Count = 0 then
  begin
    Result.MinX:= 0;
    Result.MaxX:= 0;

    Result.MinY:= 0;
    Result.MaxY:= 0;

    Result.MinZ:= 0;
    Result.MaxZ:= 0;
  end else
  begin
    Result.MinX:= Vertices.List^[0].Position.X;
    Result.MaxX:= Vertices.List^[0].Position.X;

    Result.MinY:= Vertices.List^[0].Position.Y;
    Result.MaxY:= Vertices.List^[0].Position.Y;

    Result.MinZ:= Vertices.List^[0].Position.Z;
    Result.MaxZ:= Vertices.List^[0].Position.Z;

    for Index := 1 to Vertices.Count - 1 do
    begin
      if Vertices.List^[Index].Position.X < Result.MinX then Result.MinX:=Vertices.List^[Index].Position.X;
      if Vertices.List^[Index].Position.Y < Result.MinY then Result.MinY:=Vertices.List^[Index].Position.Y;
      if Vertices.List^[Index].Position.Z < Result.MinZ then Result.MinZ:=Vertices.List^[Index].Position.Z;

      if Vertices.List^[Index].Position.X > Result.MaxX then Result.MaxX:=Vertices.List^[Index].Position.X;
      if Vertices.List^[Index].Position.Y > Result.MaxY then Result.MaxY:=Vertices.List^[Index].Position.Y;
      if Vertices.List^[Index].Position.Z > Result.MaxZ then Result.MaxZ:=Vertices.List^[Index].Position.Z;
    end;
  end;
end;

//------------------------------------------------------------------------------
procedure TPHXMesh.Upload(Buffer: TPHXBuffer);
var Index: Integer;
var IV   : Integer;
var II   : Integer;
begin
  Buffer.Primitive:= PHX_TRIANGLES;

  IV:= Buffer.Vertices.Alloc(Vertices.Count);
  for Index:=0 to Vertices.Count -1 do
  begin
    Buffer.Vertices.List^[IV + Index].Color    := clrWhite;
    Buffer.Vertices.List^[IV + Index].TexCoord := FVertices.List^[Index].TexCoord0;
    Buffer.Vertices.List^[IV + Index].Normal   := FVertices.List^[Index].Normal;
    Buffer.Vertices.List^[IV + Index].Position := FVertices.List^[Index].Position;
  end;

  II:= Buffer.Indices.Alloc(Triangles.Count * 3);
  for Index:=0 to Triangles.Count -1 do
  begin
    Buffer.Indices.List^[II + Index*3+0]:= IV + Triangles.List[Index].Vertex0;
    Buffer.Indices.List^[II + Index*3+1]:= IV + Triangles.List[Index].Vertex1;
    Buffer.Indices.List^[II + Index*3+2]:= IV + Triangles.List[Index].Vertex2;
  end;

  Buffer.Upload;
end;

//------------------------------------------------------------------------------
procedure TPHXMesh.Upload(const Buffer: Pointer; Declaration: TPHXVertexDeclaration);
begin    (*
  IV:= Buffer.Vertices.Alloc(Vertices.Count);
  for Index:=0 to Vertices.Count -1 do
  begin
    Buffer.Vertices.List^[IV + Index].Color    := clrWhite;
    Buffer.Vertices.List^[IV + Index].TexCoord := FVertices.List^[Index].TexCoord0;
    Buffer.Vertices.List^[IV + Index].Normal   := FVertices.List^[Index].Normal;
    Buffer.Vertices.List^[IV + Index].Position := FVertices.List^[Index].Position;
  end;  *)

end;


//------------------------------------------------------------------------------
procedure TPHXMesh.UploadGroup(Buffer: TPHXBuffer; const Group: Integer);
var Index: Integer;
var IV   : Integer;
var II   : Integer;
begin
  Buffer.Primitive:= PHX_TRIANGLES;

  IV:= Buffer.Vertices.Alloc(Vertices.Count);
  for Index:=0 to Vertices.Count -1 do
  begin
    Buffer.Vertices.List^[IV + Index].Color    := clrWhite;
    Buffer.Vertices.List^[IV + Index].TexCoord := FVertices.List^[Index].TexCoord0;
    Buffer.Vertices.List^[IV + Index].Normal   := FVertices.List^[Index].Normal;
    Buffer.Vertices.List^[IV + Index].Position := FVertices.List^[Index].Position;
  end;

  II:= Buffer.Indices.Alloc(Groups.List^[Group].TriangleCount * 3);
  for Index:=0 to Groups.List^[Group].TriangleCount - 1 do
  begin
    Buffer.Indices.List^[II + Index*3+0]:= IV + Triangles.List[Groups.List^[Group].TriangleOffset + Index].Vertex0;
    Buffer.Indices.List^[II + Index*3+1]:= IV + Triangles.List[Groups.List^[Group].TriangleOffset + Index].Vertex1;
    Buffer.Indices.List^[II + Index*3+2]:= IV + Triangles.List[Groups.List^[Group].TriangleOffset + Index].Vertex2;
  end;

  Buffer.Upload;
end;

//------------------------------------------------------------------------------
procedure TPHXMesh.UploadNormals(Buffer: TPHXBuffer; const Length: Single; const Color: TColor4f);
var Index: Integer;
var IV   : Integer;
var II   : Integer;
begin
  Buffer.Primitive:= PHX_LINES;

  IV:= Buffer.Vertices.Alloc(Vertices.Count * 2 );
  II:= Buffer.Indices.Alloc(Vertices.Count * 2 );

  for Index:=0 to Vertices.Count -1 do
  begin
    Buffer.Vertices.List^[IV + Index*2+0].Color    := Color;
    Buffer.Vertices.List^[IV + Index*2+0].TexCoord := Vector2f_Zero;
    Buffer.Vertices.List^[IV + Index*2+0].Normal   := Vector3f_Zero;
    Buffer.Vertices.List^[IV + Index*2+0].Position := FVertices.List^[Index].Position;

    Buffer.Vertices.List^[IV + Index*2+1].Color     := Color;
    Buffer.Vertices.List^[IV + Index*2+1].TexCoord  := Vector2f_Zero;
    Buffer.Vertices.List^[IV + Index*2+1].Normal    := Vector3f_Zero;
    Buffer.Vertices.List^[IV + Index*2+1].Position.X:= FVertices.List^[Index].Position.X + FVertices.List^[Index].Normal.X * Length;
    Buffer.Vertices.List^[IV + Index*2+1].Position.Y:= FVertices.List^[Index].Position.Y + FVertices.List^[Index].Normal.Y * Length;
    Buffer.Vertices.List^[IV + Index*2+1].Position.Z:= FVertices.List^[Index].Position.Z + FVertices.List^[Index].Normal.Z * Length;

    Buffer.Indices.List^[II + Index*2+0]:= IV + Index*2+0;
    Buffer.Indices.List^[II + Index*2+1]:= IV + Index*2+1;
  end;

  Buffer.Upload;
end;

//------------------------------------------------------------------------------
procedure TPHXMesh.Render(Buffer: TPHXBuffer; Effect: TPHXEffect);
begin
  Upload(Buffer);

  Effect.Render(Buffer);
end;

//------------------------------------------------------------------------------
procedure TPHXMesh.RenderGroup(Buffer: TPHXBuffer; Effect: TPHXEffect; const Group: Integer);
begin
  UploadGroup(Buffer, Group);

  Effect.Render(Buffer);
end;

//------------------------------------------------------------------------------
procedure TPHXMesh.RenderNormals(Buffer: TPHXBuffer; Effect: TPHXEffect; const Length: Single; const Color: TColor4f);
begin
  UploadNormals(Buffer, Length, Color);

  Effect.Render(Buffer);
end;


(*
//------------------------------------------------------------------------------
procedure TPHXMesh.Render(Buffer: TPHXDrawBuffer);
var Index: Integer;
var IV   : Integer;
var II   : Integer;
begin
  Buffer.Primitive:= PHX_TRIANGLES;

  IV:= Buffer.Vertices.Alloc(Vertices.Count);
  for Index:=0 to Vertices.Count -1 do
  begin
    Buffer.Vertices.List^[IV + Index].Color    := clrWhite;
    Buffer.Vertices.List^[IV + Index].TexCoord := FVertices.List^[Index].TexCoord0;
    Buffer.Vertices.List^[IV + Index].Normal   := FVertices.List^[Index].Normal;
    Buffer.Vertices.List^[IV + Index].Position := FVertices.List^[Index].Position;
  end;
      {
  II:= Buffer.IndexList.Alloc(Vertices.Count);
  for Index:=0 to Vertices.Count -1 do
  begin
    Buffer.IndexList.List^[II + Index]:= IV + Index;
  end;
   }

  II:= Buffer.Indices.Alloc(Triangles.Count * 3);
  for Index:=0 to Triangles.Count -1 do
  begin
    Buffer.Indices.List^[II + Index*3+0]:= IV + Triangles.List[Index].Vertex0;
    Buffer.Indices.List^[II + Index*3+1]:= IV + Triangles.List[Index].Vertex1;
    Buffer.Indices.List^[II + Index*3+2]:= IV + Triangles.List[Index].Vertex2;
  end;

  Buffer.Flush;
end;


*)


 (*
//------------------------------------------------------------------------------
procedure TPHXMesh.Render(Device: TPHXDevice);
var Buffer: TPHXVertexList;
//var Joint: Word;
//var JointPosition: TVector3f;
var SrcIndex : Integer;
var DstIndex : Integer;
begin
  Buffer:= TPHXVertexList.Create;

  DstIndex:= Buffer.Alloc( FVertices.Count );
  For SrcIndex:=0 to Vertices.Count -1 do
  begin
   // Joint:= FVertices.List^[SrcIndex].Joint;

    Buffer.List^[DstIndex].Color    := clrWhite;
    Buffer.List^[DstIndex].TexCoord := FVertices.List^[SrcIndex].TexCoord;
    Buffer.List^[DstIndex].Normal   := FVertices.List^[SrcIndex].Normal;
    Buffer.List^[DstIndex].Position := FVertices.List^[SrcIndex].Position;

    Inc(DstIndex);
  end;
  Device.Render(PHX_TRIANGLES, Buffer, Vertices.Count);

  Buffer.Free;
end;

//------------------------------------------------------------------------------
procedure TPHXMesh.Render(Device: TPHXDevice; Group: Integer);
var Buffer: TPHXVertexList;
var SrcIndex : Integer;
var DstIndex : Integer;
begin
   Assert( (Group >= 0) and (Group < Groups.Count) );

  Buffer:= TPHXVertexList.Create;

  DstIndex:= Buffer.Alloc( Groups.List^[Group].VertexCount );

  For SrcIndex:= Groups.List^[Group].VertexOffset to Groups.List^[Group].VertexOffset + Groups.List^[Group].VertexCount - 1 do
  begin
    Buffer.List^[DstIndex].Color    := clrWhite;
    Buffer.List^[DstIndex].TexCoord := FVertices.List^[SrcIndex].TexCoord;
    Buffer.List^[DstIndex].Normal   := FVertices.List^[SrcIndex].Normal;
    Buffer.List^[DstIndex].Position := FVertices.List^[SrcIndex].Position;

    Inc(DstIndex);
  end;
  Device.Render(PHX_TRIANGLES, Buffer, Vertices.Count);

  Buffer.Free;


end;


//------------------------------------------------------------------------------
procedure TPHXMesh.RenderNormals(Device: TPHXDevice; const Length: Single; const Color: TColor4f);
var Buffer: TPHXVertexList;
var SrcIndex : Integer;
var DstIndex : Integer;
begin
  Buffer:= TPHXVertexList.Create;

  DstIndex:= Buffer.Alloc( FVertices.Count * 2 );
  For SrcIndex:=0 to Vertices.Count -1 do
  begin
    Buffer.List^[DstIndex].Color    := Color;
    Buffer.List^[DstIndex].TexCoord := Vector2f_Zero;
    Buffer.List^[DstIndex].Normal   := Vector3f_Zero;
    Buffer.List^[DstIndex].Position := FVertices.List^[SrcIndex].Position;

    Inc(DstIndex);

    Buffer.List^[DstIndex].Color     := Color;
    Buffer.List^[DstIndex].TexCoord  := Vector2f_Zero;
    Buffer.List^[DstIndex].Normal    := Vector3f_Zero;
    Buffer.List^[DstIndex].Position.X:= FVertices.List^[SrcIndex].Position.X + FVertices.List^[SrcIndex].Normal.X * Length;
    Buffer.List^[DstIndex].Position.Y:= FVertices.List^[SrcIndex].Position.Y + FVertices.List^[SrcIndex].Normal.Y * Length;
    Buffer.List^[DstIndex].Position.Z:= FVertices.List^[SrcIndex].Position.Z + FVertices.List^[SrcIndex].Normal.Z * Length;

    Inc(DstIndex);
  end;
  Device.Render(PHX_LINES, Buffer, Vertices.Count);

  Buffer.Free;
end;



// Render the static mesh
//------------------------------------------------------------------------------
procedure TPHXMesh.Render;
var Index: Integer;
begin
  {
  glEnableClientState(GL_TEXTURE_COORD_ARRAY);
  glEnableClientState(GL_VERTEX_ARRAY);
  glEnableClientState(GL_NORMAL_ARRAY);

  glTexCoordPointer(2, GL_FLOAT, SizeOf(TMeshVertex), @FVertices[0].TexCoord);
  glVertexPointer  (3, GL_FLOAT, SizeOf(TMeshVertex), @FVertices[0].Position);
  glNormalPointer  (   GL_FLOAT, SizeOf(TMeshVertex), @FVertices[0].Normal  );

//  glClientActiveTextureARB(GL_TEXTURE0_ARB);
    glTexCoordPointer(2, GL_FLOAT, SizeOf(TMeshVertex), @FVertices[0].TexCoord);
//    glEnableClientState(GL_TEXTURE_COORD_ARRAY);

//  glClientActiveTextureARB(GL_TEXTURE1_ARB);
//    glTexCoordPointer(2, GL_FLOAT, SizeOf(TMeshVertex), @FVertices[0].TexCoord);
//    glEnableClientState(GL_TEXTURE_COORD_ARRAY);

  glDrawArrays(GL_TRIANGLES, 0, numVerts);

//  glClientActiveTextureARB(GL_TEXTURE1_ARB);
//    glDisableClientState(GL_TEXTURE_COORD_ARRAY);
//  glClientActiveTextureARB(GL_TEXTURE0_ARB);


 // glDisableClientState(GL_NORMAL_ARRAY);
//  glDisableClientState(GL_VERTEX_ARRAY);
//  glDisableClientState(GL_TEXTURE_COORD_ARRAY);
}
  if FVertices.Count = 0 then Exit;

  glColor4f(1,1,1,1);

  glEnableClientState(GL_VERTEX_ARRAY);
  glDisableClientState(GL_COLOR_ARRAY);
  glEnableClientState(GL_NORMAL_ARRAY);
  glEnableClientState(GL_TEXTURE_COORD_ARRAY);

  glVertexPointer  (3, GL_FLOAT, SizeOf(TPHXMeshVertex), @FVertices.List^[0].Position);

 // glColorPointer   (4, GL_FLOAT, SizeOf(TMeshVertex), @FVertices[0].Color);
  glNormalPointer  (   GL_FLOAT, SizeOf(TPHXMeshVertex ), @FVertices.List^[0].Normal);
  glTexCoordPointer(2, GL_FLOAT, SizeOf(TPHXMeshVertex), @FVertices.List^[0].TexCoord );
  glDrawArrays(GL_TRIANGLES, 0, FVertices.Count);



end;

//------------------------------------------------------------------------------
procedure TPHXMesh.Render(const Buffer: PVertexList);
var Index : Integer;
var DstVertex: PPHXVertex;
begin
  DstVertex:= @Buffer^[0];
  For Index:=0 to Vertices.Count -1 do
  begin
    Buffer^[Index].Color    := clrWhite;
    Buffer^[Index].TexCoord := FVertices.List^[Index].TexCoord;
    Buffer^[Index].Normal   := FVertices.List^[Index].Normal;
    Buffer^[Index].Position := FVertices.List^[Index].Position;

    Inc(DstVertex);
  end;
end;


//------------------------------------------------------------------------------
procedure TPHXMesh.BuildVBO(var VBOObject: glUInt);
//var Index: Integer;
begin
 // glGenBuffersARB(1, @VBOObject);

  //glBindBufferARB(GL_ARRAY_BUFFER_ARB, VBOObject);
  // Push the
  //glBufferDataARB(GL_ARRAY_BUFFER_ARB, numVerts * SizeOf(TMeshVertex), @FVertices[0], GL_STATIC_DRAW_ARB);

//  glBindBufferARB(GL_ARRAY_BUFFER_ARB, 0);
end;
*)

{$ENDREGION}



end.
