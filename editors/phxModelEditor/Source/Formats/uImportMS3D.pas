unit uImportMS3D;

interface

uses
  Messages, SysUtils, Classes,

  uImport,

  phxTypes,
  phxMath,
  phxModel;

Const

MAX_KEYFRAMES  = 65536;     // increase when needed

type

// id - Should always be MS3D000000
// version - Should always be 3 or 4
//------------------------------------------------------------------------------
TMS3D_HEADER = Record
  Id     : Array[1..10] of AnsiChar;
  Version: Integer;
end;

// The vertex structure:
//------------------------------------------------------------------------------
TMS3D_VERTEX = Record
   Flags   : AnsiChar;
   RefCount: AnsiChar;
   BoneID  : AnsiChar ;
   Vertex  : Array[0..2] of Single;
end;

//------------------------------------------------------------------------------
TMS3D_TRIANGLE = Record
   Flags         : Smallint;
   VertexIndices : Array[0..2] of Smallint;
   VertexNormals : Array[0..2, 0..2] of Single;
   VertexTexCoord: Array[0..2, 0..1] of Single;
   SmoothingGroup: AnsiChar;
   GroupIndex    : AnsiChar;
end;

//------------------------------------------------------------------------------
TMS3D_GROUP = Record
  Flags          : AnsiChar;
  Name           : Array[1..32] of AnsiChar;
  NumTriangles   : SmallInt;
  TriangleIndices: Array of SmallInt;
  MaterialIndex  : Byte;
end;

//------------------------------------------------------------------------------
TMS3D_Material = Record
  Name            : Array[1..32] of AnsiChar;
  Ambient         : Array[0.. 3] of Single;
  Diffuse         : Array[0.. 3] of Single;
  Specular        : Array[0.. 3] of Single;
  Emissive        : Array[0.. 3] of Single;
  Shininess       : Single;
  Transparency    : Single;
  Mode            : AnsiChar;
  Texture         : Cardinal;
  TextureFilename : Array[1..128] of AnsiChar;
  TextureAlphamap : Array[1..128] of AnsiChar;
end;

//------------------------------------------------------------------------------
TMS3D_KEYFRAME = Record
  Time     : Single;                  // time in seconds
  Parameter: Array[0.. 2] of Single;
end;

//------------------------------------------------------------------------------
TMS3D_JOINT = Record
  Name                   : Array[1..32] of AnsiChar;
  ParentName             : Array[1..32] of AnsiChar;
  Parent                 : Integer;
  Flags                  : Byte;
	Rotation               : Array[0.. 2] of Single;
	Translation            : Array[0.. 2] of Single;
	numKeyFramesRot        : Word		;
	numKeyFramesTrans      : Word		;

  theKeyFramesRot   : Array of TMS3D_Keyframe;
  theKeyFramesTrans : Array of TMS3D_Keyframe;
end;

TVertices  = Array of TMS3D_VERTEX;
TTriangles = Array of TMS3D_TRIANGLE;
TGroups    = Array of TMS3D_GROUP;
TMaterials = Array of TMS3D_Material;
TJoints    = Array of TMS3D_JOINT;



//----------------------------------------------------------------------------
TImporterMS3D = class(TImporter)
  private
    numVertices : SmallInt;
    numTriangles: SmallInt;
    numGroups   : Smallint;
    numMaterials: Smallint;
    numJoints   : Smallint;

    theVertices : TVertices;
    theTriangles: TTriangles;
    theGroups   : TGroups;
    theMaterials: TMaterials;
    theJoints   : TJoints;

    JointAbsoluteMatrix: array of TMatrix4f;
    JointRelativeMatrix: array of TMatrix4f;

    procedure SetupJointMatrices;

    procedure CopyMaterials(Mesh: TPHXMesh);
    procedure CopyGroups(Mesh: TPHXMesh);
    procedure CopyJoints(Mesh: TPHXMesh);
    procedure CopyVerticies(Mesh: TPHXMesh; Group: TMS3D_GROUP);
    procedure CopyTags(Mesh: TPHXMesh);
  public
    constructor Create; override;
    destructor Destroy; override;

    procedure LoadFromFile(const FileName: String);
    Procedure LoadFromStream(Stream : TStream);

    Procedure ImportMesh(Stream: TStream); override;
  end;

implementation

//------------------------------------------------------------------------------
function isTag(Joint : TMS3D_JOINT): Boolean;
begin
  Result:= Copy(Joint.Name, 1, 5) = '[TAG]';
end;

//------------------------------------------------------------------------------
function IndexOfJoint(Mesh: TPHXMesh; Name: String): Word;
var Index: Integer;
begin
  for Index:= 0 to Mesh.Joints.Count - 1 do
  begin
     if Trim(String(Mesh.Joints[Index].Name)) = Trim(Name) then
     begin
       Result:= Index;
       Exit;
     end;
  end;
  Result:= PHX_JOINT_NONE;
end;

//------------------------------------------------------------------------------
constructor TImporterMS3D.Create;
begin

end;

//------------------------------------------------------------------------------
destructor TImporterMS3D.Destroy;
begin

  inherited;
end;


//------------------------------------------------------------------------------
procedure TImporterMS3D.LoadFromFile(const FileName: String);
var Stream: TStream;
begin
  Stream:= TFileStream.Create(FileName, fmOpenRead or fmShareDenyNone);
  try
    LoadFromStream(Stream);
  finally
    Stream.Free;
  end;
end;

//------------------------------------------------------------------------------
procedure TImporterMS3D.LoadFromStream(Stream: TStream);
var Header  : TMS3D_HEADER;
var Counter : Integer;
var Counter2: Integer;

var animFPS    : Single;
var currentTime: Single;
var totalFrames: Integer;
begin
  Stream.Read(Header.ID     , SizeOf(Header.ID));
  Stream.Read(Header.Version, SizeOf(Header.Version));

  If (Header.ID <> 'MS3D000000') or ((Header.version <> 3) and (Header.version <> 4)) then
  begin
    raise Exception.Create('Not a valid Milkshape3D Model !');
  end;


  Stream.Read(NumVertices, SizeOf(numVertices));
  SetLength(theVertices, numVertices);
  For Counter:=0 to numVertices-1 do
  begin
     Stream.Read(theVertices[Counter].Flags   , SizeOf(theVertices[Counter].Flags  ));
     Stream.Read(theVertices[Counter].Vertex  , SizeOf(theVertices[Counter].Vertex  ));
     Stream.Read(theVertices[Counter].BoneID  , SizeOf(theVertices[Counter].BoneID  ));
     Stream.Read(theVertices[Counter].RefCount, SizeOf(theVertices[Counter].RefCount));
  end;

  Stream.Read(numTriangles     , SizeOf(numTriangles));
  SetLength(theTriangles, numTriangles);
  For Counter:=0 to numTriangles-1 do
  begin
     Stream.Read(theTriangles[Counter].Flags           , SizeOf(theTriangles[Counter].Flags  ));
     Stream.Read(theTriangles[Counter].vertexIndices   , SizeOf(theTriangles[Counter].vertexIndices  ));
     Stream.Read(theTriangles[Counter].vertexNormals[0], SizeOf(theTriangles[Counter].vertexNormals[0]));
     Stream.Read(theTriangles[Counter].vertexNormals[1], SizeOf(theTriangles[Counter].vertexNormals[1]));
     Stream.Read(theTriangles[Counter].vertexNormals[2], SizeOf(theTriangles[Counter].vertexNormals[2]));

     Stream.Read(theTriangles[Counter].VertexTexCoord[0][0] , SizeOf(theTriangles[Counter].VertexTexCoord[0][0]));
     Stream.Read(theTriangles[Counter].VertexTexCoord[1][0] , SizeOf(theTriangles[Counter].VertexTexCoord[1][0]));
     Stream.Read(theTriangles[Counter].VertexTexCoord[2][0] , SizeOf(theTriangles[Counter].VertexTexCoord[2][0]));
     Stream.Read(theTriangles[Counter].VertexTexCoord[0][1] , SizeOf(theTriangles[Counter].VertexTexCoord[0][1]));
     Stream.Read(theTriangles[Counter].VertexTexCoord[1][1] , SizeOf(theTriangles[Counter].VertexTexCoord[1][1]));
     Stream.Read(theTriangles[Counter].VertexTexCoord[2][1] , SizeOf(theTriangles[Counter].VertexTexCoord[2][1]));

     Stream.Read(theTriangles[Counter].smoothingGroup  , SizeOf(theTriangles[Counter].smoothingGroup  ));
     Stream.Read(theTriangles[Counter].groupIndex      , SizeOf(theTriangles[Counter].groupIndex  ));
  end;

  Stream.Read(numGroups, SizeOf(numGroups));
  SetLength(theGroups, numGroups);
  For Counter:=0 to numGroups-1 do
  begin
     Stream.Read(theGroups[Counter].Flags           , SizeOf(theGroups[Counter].Flags       ));
     Stream.Read(theGroups[Counter].Name            , SizeOf(theGroups[Counter].Name        ));
     Stream.Read(theGroups[Counter].numTriangles    , SizeOf(theGroups[Counter].numTriangles));

     SetLength(theGroups[Counter].TriangleIndices, theGroups[Counter].numTriangles);
     For Counter2:=0 to theGroups[Counter].numTriangles-1 do begin
        Stream.Read(theGroups[Counter].TriangleIndices[Counter2]  , SizeOf(SmallInt));
     end;
     Stream.Read(theGroups[Counter].materialIndex , SizeOf(theGroups[Counter].materialIndex));
   end;

   Stream.Read(NumMaterials  , SizeOf(NumMaterials));
   SetLength(theMaterials, NumMaterials);
   For Counter:=0 to NumMaterials-1 do
   begin
     Stream.Read(theMaterials[Counter].Name            , SizeOf(theMaterials[Counter].Name));
     Stream.Read(theMaterials[Counter].Ambient         , SizeOf(theMaterials[Counter].Ambient));
     Stream.Read(theMaterials[Counter].Diffuse         , SizeOf(theMaterials[Counter].Diffuse));
     Stream.Read(theMaterials[Counter].Specular        , SizeOf(theMaterials[Counter].Specular));
     Stream.Read(theMaterials[Counter].Emissive        , SizeOf(theMaterials[Counter].Emissive));
     Stream.Read(theMaterials[Counter].Shininess       , SizeOf(theMaterials[Counter].Shininess));
     Stream.Read(theMaterials[Counter].Transparency    , SizeOf(theMaterials[Counter].Transparency));
     Stream.Read(theMaterials[Counter].Mode            , SizeOf(theMaterials[Counter].Mode));
     Stream.Read(theMaterials[Counter].TextureFileName , SizeOf(theMaterials[Counter].TextureFileName));
     Stream.Read(theMaterials[Counter].TextureAlphamap , SizeOf(theMaterials[Counter].TextureAlphamap));
   end;
	// Load Skeletal Animation Stuff

  Stream.Read(animFPS , SizeOf(animFPS));
 	// Skip currentTime
  Stream.Read(currentTime , SizeOf(currentTime));
  // Read the number of frames
  Stream.Read(totalFrames , SizeOf(totalFrames));

  Stream.Read(numJoints, SizeOf(numJoints));
  SetLength(theJoints, numJoints);
  For Counter:=0 to numJoints-1 do
  begin
     Stream.Read(theJoints[Counter].Flags                  , SizeOf(theJoints[Counter].Flags));
     Stream.Read(theJoints[Counter].Name                   , SizeOf(theJoints[Counter].Name));
     Stream.Read(theJoints[Counter].ParentName             , SizeOf(theJoints[Counter].ParentName));
     Stream.Read(theJoints[Counter].Rotation               , SizeOf(theJoints[Counter].Rotation));
     Stream.Read(theJoints[Counter].Translation            , SizeOf(theJoints[Counter].Translation));

     Stream.Read(theJoints[Counter].numKeyFramesRot        , SizeOf(theJoints[Counter].numKeyFramesRot));
     Stream.Read(theJoints[Counter].numKeyFramesTrans      , SizeOf(theJoints[Counter].numKeyFramesTrans));

     SetLength(theJoints[Counter].theKeyFramesRot, theJoints[Counter].numKeyFramesRot);
     For Counter2:=0 to theJoints[Counter].numKeyFramesRot-1 do
     begin
       Stream.Read(theJoints[Counter].theKeyFramesRot[Counter2].Time     , SizeOf(theJoints[Counter].theKeyFramesRot[Counter].Time));
       Stream.Read(theJoints[Counter].theKeyFramesRot[Counter2].Parameter, SizeOf(theJoints[Counter].theKeyFramesRot[Counter].Parameter));

       theJoints[Counter].theKeyFramesRot[Counter2].Time:=theJoints[Counter].theKeyFramesRot[Counter2].Time * 1000;
     end;

     SetLength(theJoints[Counter].theKeyFramesTrans, theJoints[Counter].numKeyFramesTrans);
     For Counter2:=0 to theJoints[Counter].numKeyFramesTrans-1 do
     begin
       Stream.Read(theJoints[Counter].theKeyFramesTrans[Counter2].Time     , SizeOf(theJoints[Counter].theKeyFramesTrans[Counter].Time));
       Stream.Read(theJoints[Counter].theKeyFramesTrans[Counter2].Parameter, SizeOf(theJoints[Counter].theKeyFramesTrans[Counter].Parameter));

       theJoints[Counter].theKeyFramesTrans[Counter2].Time:=theJoints[Counter].theKeyFramesTrans[Counter2].Time * 1000;
     end;

     theJoints[Counter].Parent:= PHX_JOINT_NONE;
     For Counter2:=0 to Counter do
     begin
       if( Trim(theJoints[Counter].ParentName) = Trim(theJoints[Counter2].Name)) then theJoints[Counter].Parent:= Counter2;
     end;

  end;
end;



/// The function goes through all the joints setting up the matrices as follows.
/// Relative: Creates it from the joint's rotation and translation key-frames.
/// Absolute: Creates it by catenating the relative matrix of a given joint to its parent absolute matrix.
//------------------------------------------------------------------------------
procedure TImporterMS3D.SetupJointMatrices();
var Index   : Integer;
var Joint   : TMS3D_JOINT;
var MatAbsolute: TMatrix4f;
var MatRelative: TMatrix4f;
begin
  SetLength(JointAbsoluteMatrix, numJoints);
  SetLength(JointRelativeMatrix, numJoints);

  for Index:=0 to numJoints-1 do
  begin
    Joint:= theJoints[Index];

   // MatRelative:= Matrix_Identity;
    MatRelative:= Matrix_CreateRotation( Joint.Rotation[0], Joint.Rotation[1], Joint.Rotation[2] );
    MatRelative.v[12]:= Joint.Translation[0];
    MatRelative.v[13]:= Joint.Translation[1];
    MatRelative.v[14]:= Joint.Translation[2];


    if (Joint.Parent <> PHX_JOINT_NONE) then
    begin
      MatAbsolute:= Matrix_Multiply(JointAbsoluteMatrix[ Joint.Parent ], MatRelative);
    end else begin
      MatAbsolute:= MatRelative;
    end;

    JointRelativeMatrix[Index]:= MatRelative;
    JointAbsoluteMatrix[Index]:= MatAbsolute;
  end;

end;

//------------------------------------------------------------------------------
procedure TImporterMS3D.ImportMesh(Stream: TStream);
begin
  LoadFromStream(Stream);

  // if Hierarchical then begin
  SetupJointMatrices();
 // SetupVertices();
  // end;

  CopyMaterials(Mesh);
  CopyJoints(Mesh);
  CopyTags  (Mesh);
  CopyGroups(Mesh);

  SetLength(theVertices , 0);
  SetLength(theTriangles, 0);
  SetLength(theGroups   , 0);
  SetLength(theMaterials, 0);
  SetLength(theJoints   , 0);

  SetLength(JointAbsoluteMatrix, 0);
  SetLength(JointRelativeMatrix, 0);
end;

//  Name            : Array[1..32] of AnsiChar;
//  Ambient         : Array[0.. 3] of Single;
//  Diffuse         : Array[0.. 3] of Single;
//  Specular        : Array[0.. 3] of Single;
//  Emissive        : Array[0.. 3] of Single;
//  Shininess       : Single;
//  Transparency    : Single;
//  Mode            : AnsiChar;
//  Texture         : Cardinal;
//  TextureFilename : Array[1..128] of AnsiChar;
//  TextureAlphamap : Array[1..128] of AnsiChar;
//------------------------------------------------------------------------------
Procedure TImporterMS3D.CopyMaterials(Mesh: TPHXMesh);
var NameDiffuse: String;
var NameAlpha  : String;
begin
  if numMaterials > 0 then
  begin
    Mesh.Material.Name   := AnsiString(theMaterials[0 ].Name);
    Mesh.Material.Diffuse:= clrWhite;

    NameDiffuse:= StrPas(PAnsiChar(@theMaterials[ 0 ].TextureFilename));
    NameAlpha  := StrPas(PAnsiChar(@theMaterials[ 0 ].TextureAlphamap));

    if Copy(NameDiffuse, 1, 2) = '.\' then  NameDiffuse:= Copy(NameDiffuse, 3, MaxInt);
    if Copy(NameAlpha  , 1, 2) = '.\' then  NameAlpha  := Copy(NameAlpha  , 3, MaxInt);

    if NameDiffuse <> '' then Mesh.Material.Textures.Add( NameDiffuse, tkDiffuse);
    if NameAlpha   <> '' then Mesh.Material.Textures.Add( NameAlpha  , tkAlphamap);
  end;
end;

//------------------------------------------------------------------------------
Procedure TImporterMS3D.CopyJoints(Mesh: TPHXMesh);
var Counter: Integer;
var Index : Integer;
var Joint  : TMS3D_JOINT;
begin
  Index:=0;
  for Counter:=0 to numJoints - 1 do
  begin
    Joint:= theJoints[Counter];

    if not isTag(Joint) then
    begin
      Mesh.Joints.Count:= Mesh.Joints.Count + 1;

      Mesh.Joints.List^[Index].Name      := Trim(Joint.Name);
      Mesh.Joints.List^[Index].Parent    := IndexOfJoint(Mesh, Joint.ParentName);
      Mesh.Joints.List^[Index].ParentName:= Joint.ParentName;

      Mesh.Joints.List^[Index].Position.X:= Joint.Translation[0];
      Mesh.Joints.List^[Index].Position.Y:= Joint.Translation[1];
      Mesh.Joints.List^[Index].Position.Z:= Joint.Translation[2];

      Mesh.Joints.List^[Index].Rotation.X:= Joint.Rotation[0];
      Mesh.Joints.List^[Index].Rotation.Y:= Joint.Rotation[1];
      Mesh.Joints.List^[Index].Rotation.Z:= Joint.Rotation[2];

      Inc(Index);
    end;

  end;
end;

//------------------------------------------------------------------------------
Procedure TImporterMS3D.CopyTags(Mesh: TPHXMesh);
var Counter: Integer;
var Index : Integer;
var Joint  : TMS3D_JOINT;
begin
  Index:=0;
  for Counter:=0 to numJoints - 1 do
  begin
    Joint:= theJoints[Counter];

    // Joints named [TAG]#### is considered a tag with the name ####
    if isTag(Joint) then
    begin
      Mesh.Tags.Count:= Mesh.Tags.Count + 1;

      Mesh.Tags.List^[Index].Name     := Copy( Trim(Joint.Name), 6, MaxInt);
      Mesh.Tags.List^[Index].Joint    := IndexOfJoint(Mesh, Joint.ParentName);
      Mesh.Tags.List^[Index].JointName:= Joint.ParentName;

      Mesh.Tags.List^[Index].Position.X:= Joint.Translation[0];
      Mesh.Tags.List^[Index].Position.Y:= Joint.Translation[1];
      Mesh.Tags.List^[Index].Position.Z:= Joint.Translation[2];

      Mesh.Tags.List^[Index].Direction.X:= Joint.Rotation[0];
      Mesh.Tags.List^[Index].Direction.Y:= Joint.Rotation[1];
      Mesh.Tags.List^[Index].Direction.Z:= Joint.Rotation[2];

      Inc(Index);
    end;

  end;
end;


//------------------------------------------------------------------------------
Procedure TImporterMS3D.CopyVerticies(Mesh: TPHXMesh; Group : TMS3D_GROUP);
var I,J: Integer;
//var Index  : Integer;
var MS3DTriangle: TMS3D_TRIANGLE;
var MS3DVertex  : TMS3D_VERTEX;

var Vertex  : TPHXMeshVertex;
begin
  //Index:= Mesh.Vertices.Count;

 // Mesh.Vertices.Count:= Mesh.Vertices.Count + Group.numTriangles * 3;
  for I:=0 to Group.numTriangles-1 do
  begin
    MS3DTriangle:= theTriangles[ Group.triangleIndices[I] ];

    Mesh.Triangles.Add(Mesh.Vertices.Count, Mesh.Vertices.Count + 1, Mesh.Vertices.Count + 2);

    for J:=0 to 2 do
    begin
      MS3DVertex:= theVertices[ MS3DTriangle.VertexIndices[J] ];

      Vertex.Position.X:= MS3DVertex.Vertex[0];
      Vertex.Position.Y:= MS3DVertex.Vertex[1];
      Vertex.Position.Z:= MS3DVertex.Vertex[2];

      Vertex.Normal.X  := MS3DTriangle.VertexNormals [J][0];
      Vertex.Normal.Y  := MS3DTriangle.VertexNormals [J][1];
      Vertex.Normal.Z  := MS3DTriangle.VertexNormals [J][2];

      Vertex.TexCoord0.X:= MS3DTriangle.VertexTexCoord[J][0];
      Vertex.TexCoord0.Y:= MS3DTriangle.VertexTexCoord[J][1];

      if MS3DVertex.BoneID = AnsiChar(255) then
      begin
        Vertex.Joint := PHX_JOINT_NONE;
      end else
      begin
        // Compensate for tags
        Vertex.Joint  := Word(MS3DVertex.BoneID);
        Vertex.Position:=Matrix_TransformInv( JointAbsoluteMatrix[ Integer(MS3DVertex.BoneID) ], Vertex.Position)
      end;
      Mesh.Vertices.Add(Vertex);
    end;


  end;
end;

//------------------------------------------------------------------------------
Procedure TImporterMS3D.CopyGroups(Mesh: TPHXMesh);
var Index : Integer;
var Group : TMS3D_GROUP;
begin
  Mesh.Groups.Count:= numGroups;

  For Index:= 0 to numGroups-1 do
  begin
    Group:= theGroups[Index];

    // Group.MaterialIndex

     ;
    Mesh.Groups.List^[Index].Name          := Shortstring( Trim(Group.Name) );
    Mesh.Groups.List^[Index].TriangleCount := Group.numTriangles;
    Mesh.Groups.List^[Index].TriangleOffset:= Mesh.Triangles.Count;

    CopyVerticies(Mesh, Group);
  end;
end;


initialization
  TImporterFactory.Add('Milkshape 3D', '.ms3d' , TImporterMS3D);
end.
