unit phxMilkshape3D;

interface

Uses SysUtils, Classes, Types,

  dglOpenGL,

  phxTypes,
  phxMath,

  phxModels;

Const MAX_KEYFRAMES  = 65536;     // increase when needed

// id - Should always be MS3D000000
// version - Should always be 3 or 4
//------------------------------------------------------------------------------
Type TMS3D_HEADER = Record
  Id     : Array[1..10] of AnsiChar;
  Version: Integer;
end;

// The vertex structure:
//------------------------------------------------------------------------------
Type TMS3D_VERTEX = Record
   Flags   : AnsiChar;
   RefCount: AnsiChar;
   BoneID  : AnsiChar ;
   Vertex  : Array[0..2] of Single;
end;

//------------------------------------------------------------------------------
Type TMS3D_TRIANGLE = Record
   Flags         : Smallint;
   VertexIndices : Array[0..2] of Smallint;
   VertexNormals : Array[0..2, 0..2] of Single;
   VertexTexCoord: Array[0..2, 0..1] of Single;
   SmoothingGroup: AnsiChar;
   GroupIndex    : AnsiChar;
end;

//------------------------------------------------------------------------------
Type TMS3D_GROUP = Record
  Flags          : AnsiChar;
  Name           : Array[1..32] of AnsiChar;
  NumTriangles   : SmallInt;
  TriangleIndices: Array of SmallInt;
  MaterialIndex  : Byte;
end;

//------------------------------------------------------------------------------
Type TMS3D_Material = Record
  Name            : Array[1..32] of AnsiChar;
  Ambient         : Array[0.. 3] of Single;
  Diffuse         : Array[0.. 3] of Single;
  Specular        : Array[0.. 3] of Single;
  Emissive        : Array[0.. 3] of Single;
  Shininess       : Single;
  Transparency    : Single;
  Mode            : AnsiChar;
  Texture         : glUint;
  TextureFilename : Array[1..128] of AnsiChar;
  TextureAlphamap : Array[1..128] of AnsiChar;
end;

//------------------------------------------------------------------------------
Type TMS3D_KEYFRAME = Record
  Time     : Single;                  // time in seconds
  Parameter: Array[0.. 2] of Single;
end;

//------------------------------------------------------------------------------
Type TMS3D_JOINT = Record
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

Type TVertices  = Array of TMS3D_VERTEX;
Type TTriangles = Array of TMS3D_TRIANGLE;
Type TGroups    = Array of TMS3D_GROUP;
Type TMaterials = Array of TMS3D_Material;
Type TJoints    = Array of TMS3D_JOINT;



Function MS3D_TO_PHXMDL(FileName: String): TPHXMesh;

implementation

var numVertices : SmallInt;
var numTriangles: SmallInt;
var numGroups   : Smallint;
var numMaterials: Smallint;
var numJoints   : Smallint;

var theVertices : TVertices;
var theTriangles: TTriangles;
var theGroups   : TGroups;
var theMaterials: TMaterials;
var theJoints   : TJoints;

var JointAbsoluteMatrix: array of TMatrix4f;
var JointRelativeMatrix: array of TMatrix4f;


procedure LoadMS3DFromStream(Stream: TStream); forward;

//------------------------------------------------------------------------------
procedure LoadMS3DFromFile(FileName: String);
var Stream: TStream;
begin
  Stream:= TFileStream.Create(FileName, fmOpenRead or fmShareDenyNone);
  try
    LoadMS3DFromStream(Stream);
  finally
    Stream.Free;
  end;
end;
    
//------------------------------------------------------------------------------
procedure LoadMS3DFromStream(Stream: TStream);
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
    raise Exception.Create('Not a valid Milkshape3D Model !');


  Stream.Read(NumVertices, SizeOf(numVertices));
  SetLength(theVertices, numVertices);
  For Counter:=0 to numVertices-1 do begin
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
  For Counter:=0 to numJoints-1 do begin
     Stream.Read(theJoints[Counter].Flags                  , SizeOf(theJoints[Counter].Flags));
     Stream.Read(theJoints[Counter].Name                   , SizeOf(theJoints[Counter].Name));
     Stream.Read(theJoints[Counter].ParentName             , SizeOf(theJoints[Counter].ParentName));
     Stream.Read(theJoints[Counter].Rotation               , SizeOf(theJoints[Counter].Rotation));
     Stream.Read(theJoints[Counter].Translation            , SizeOf(theJoints[Counter].Translation));

     Stream.Read(theJoints[Counter].numKeyFramesRot        , SizeOf(theJoints[Counter].numKeyFramesRot));
     Stream.Read(theJoints[Counter].numKeyFramesTrans      , SizeOf(theJoints[Counter].numKeyFramesTrans));

     SetLength(theJoints[Counter].theKeyFramesRot, theJoints[Counter].numKeyFramesRot); 
     For Counter2:=0 to theJoints[Counter].numKeyFramesRot-1 do begin
       Stream.Read(theJoints[Counter].theKeyFramesRot[Counter2].Time     , SizeOf(theJoints[Counter].theKeyFramesRot[Counter].Time));
       Stream.Read(theJoints[Counter].theKeyFramesRot[Counter2].Parameter, SizeOf(theJoints[Counter].theKeyFramesRot[Counter].Parameter));

       theJoints[Counter].theKeyFramesRot[Counter2].Time:=theJoints[Counter].theKeyFramesRot[Counter2].Time * 1000;
     end;

     SetLength(theJoints[Counter].theKeyFramesTrans, theJoints[Counter].numKeyFramesTrans);
     For Counter2:=0 to theJoints[Counter].numKeyFramesTrans-1 do begin
       Stream.Read(theJoints[Counter].theKeyFramesTrans[Counter2].Time     , SizeOf(theJoints[Counter].theKeyFramesTrans[Counter].Time));
       Stream.Read(theJoints[Counter].theKeyFramesTrans[Counter2].Parameter, SizeOf(theJoints[Counter].theKeyFramesTrans[Counter].Parameter));

       theJoints[Counter].theKeyFramesTrans[Counter2].Time:=theJoints[Counter].theKeyFramesTrans[Counter2].Time * 1000;
     end;

     theJoints[Counter].Parent:= PHX_JOINT_NONE;
     For Counter2:=0 to Counter do begin
       if( Trim(theJoints[Counter].ParentName) = Trim(theJoints[Counter2].Name)) then theJoints[Counter].Parent:= Counter2;

     end;

  end;
end;

//------------------------------------------------------------------------------
Procedure CopyMaterials(Mesh: TPHXMesh);
begin
  if numMaterials > 0 then
  begin
      Mesh.Material.Name:= AnsiString(theMaterials[0 ].Name);
      Mesh.Material.Color:= clrWhite;
      Mesh.Material.Flags:= 0;

      Mesh.Material.Textures[0]:= StrPas(PAnsiChar(@theMaterials[ 0 ].TextureFilename));
      Mesh.Material.Textures[1]:= StrPas(PAnsiChar(@theMaterials[ 0 ].TextureAlphamap));
      Mesh.Material.Textures[2]:= '';
      Mesh.Material.Textures[3]:= '';
      Mesh.Material.Textures[4]:= '';
      Mesh.Material.Textures[5]:= '';
      Mesh.Material.Textures[6]:= '';
      Mesh.Material.Textures[7]:= '';
  end;
end;

//------------------------------------------------------------------------------
function isTag( Joint  : TMS3D_JOINT): Boolean;
begin
  Result:= Copy(Joint.Name, 1, 5) = '[TAG]';
end;

//------------------------------------------------------------------------------
function IndexOfJoint(Mesh: TPHXMesh; Name: String): Word;
var Index: Integer;
begin
  for Index:= 0 to Mesh.Joints.Count - 1 do begin
     if Trim(Mesh.Joints[Index].Name) = Trim(Name) then begin
       Result:= Index;
       Exit;
     end;
  end;
  Result:= PHX_JOINT_NONE;
end;

//------------------------------------------------------------------------------
Procedure CopyTags(Mesh: TPHXMesh);
var Counter: Integer;
var Index : Integer;
var Joint  : TMS3D_JOINT;
begin
  Index:=0;
  for Counter:=0 to numJoints - 1 do begin
    Joint:= theJoints[Counter];
    
    // Joints named [TAG]#### is considered a tag with the name ####
    if isTag(Joint) then begin
      Mesh.Tags.Count:= Mesh.Tags.Count + 1;

      Mesh.Tags.List^[Index].Name     := Copy( Trim(Joint.Name), 6, MaxInt);
      Mesh.Tags.List^[Index].Joint    := IndexOfJoint(Mesh, Joint.ParentName);
      Mesh.Tags.List^[Index].JointName:= Joint.ParentName;

      Mesh.Tags.List^[Index].Position.X:= Joint.Translation[0];
      Mesh.Tags.List^[Index].Position.Y:= Joint.Translation[1];
      Mesh.Tags.List^[Index].Position.Z:= Joint.Translation[2];

      Mesh.Tags.List^[Index].Rotation.X:= Joint.Rotation[0];
      Mesh.Tags.List^[Index].Rotation.Y:= Joint.Rotation[1];
      Mesh.Tags.List^[Index].Rotation.Z:= Joint.Rotation[2];
      
      Inc(Index);
    end;
    
  end;
end;

//------------------------------------------------------------------------------
Procedure CopyJoints(Mesh: TPHXMesh);
var Counter: Integer;
var Index : Integer;
var Joint  : TMS3D_JOINT;
begin
  Index:=0;
  for Counter:=0 to numJoints - 1 do begin
    Joint:= theJoints[Counter];

    if not isTag(Joint) then begin
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
Procedure CopyVerticies(Mesh: TPHXMesh; Group : TMS3D_GROUP);
var Counter: Integer;
var k       : Integer;
var Index  : Integer;
var Triangle: TMS3D_TRIANGLE; 
var Vertex : TMS3D_VERTEX;
begin

  Index:= Mesh.Vertices.Count;

  Mesh.Vertices.Count:= Mesh.Vertices.Count + Group.numTriangles * 3;
  For Counter:=0 to Group.numTriangles-1 do
  begin
    Triangle:= theTriangles[ Group.triangleIndices[Counter] ];

    For k:=0 to 2 do
    begin
      Vertex:= theVertices[ Triangle.VertexIndices[k] ];

      Mesh.Vertices.List^[Index].Position.X:=  Vertex.Vertex[0];
      Mesh.Vertices.List^[Index].Position.Y:=  Vertex.Vertex[1];
      Mesh.Vertices.List^[Index].Position.Z:=  Vertex.Vertex[2];

      Mesh.Vertices.List^[Index].Normal.X  :=  Triangle.VertexNormals [K][0];
      Mesh.Vertices.List^[Index].Normal.Y  :=  Triangle.VertexNormals [K][1];
      Mesh.Vertices.List^[Index].Normal.Z   :=  Triangle.VertexNormals [K][2];

      Mesh.Vertices.List^[Index].TexCoord.X:=  Triangle.VertexTexCoord[K][0];
      Mesh.Vertices.List^[Index].TexCoord.Y:=  Triangle.VertexTexCoord[K][1];

      if Vertex.BoneID = AnsiChar(255) then
      begin
        Mesh.Vertices.List^[Index].Joint := PHX_JOINT_NONE;
      end else begin
        // Compensate for tags
        Mesh.Vertices.List^[Index].Joint := Word(Vertex.BoneID);

        Mesh.Vertices.List^[Index].Position:=Matrix_TransformInv( JointAbsoluteMatrix[ Integer(Vertex.BoneID) ], Mesh.Vertices[Index].Position)

         // Vector:=Matrix.InverseTransform(Vector);
         // Vector:=  matrix.inverseTranslateVect( Vector );
	     //  Vector:=	matrix.inverseRotateVect( Vector );

        //  theVertices[ Triangle.VertexIndices[k] ].Vertex[0]:= Vector.X;
       //   theVertices[ Triangle.VertexIndices[k] ].Vertex[1]:= Vector.Y;


      end;



      Inc(Index);
    end;
  end;
end;

//------------------------------------------------------------------------------
Procedure CopyGroups(Mesh: TPHXMesh);
var Counter: Integer;
var Index : Integer;
var Group : TMS3D_GROUP;
begin
  Mesh.Groups.Count:= numGroups;

  For Counter:= 0 to numGroups-1 do
  begin
    Group:= theGroups[Counter];

    // Group.MaterialIndex

      ;


    Mesh.Groups.List^[Counter].Name        := Trim(Group.Name);
    Mesh.Groups.List^[Counter].VertexCount := Group.numTriangles * 3;
    Mesh.Groups.List^[Counter].VertexOffset:= Mesh.Vertices.Count;

    CopyVerticies(Mesh, Group);
  end;
end; 


/// The function goes through all the joints setting up the matrices as follows.
/// Relative: Creates it from the joint's rotation and translation key-frames.
/// Absolute: Creates it by catenating the relative matrix of a given joint to its parent absolute matrix.
//------------------------------------------------------------------------------
procedure SetupJointMatrices();
var Index   : Integer;
var Joint   : TMS3D_JOINT;
var Absolute: TMatrix4f;
var Relative: TMatrix4f;
begin
  SetLength(JointAbsoluteMatrix, numJoints);
  SetLength(JointRelativeMatrix, numJoints);

  for Index:=0 to numJoints-1 do begin
    Joint:= theJoints[Index];

    Relative:= Matrix_Identity;
    Relative:= Matrix_Rotation( Joint.Rotation   [0], Joint.Rotation   [1], Joint.Rotation   [2] );
    Relative[12]:= Joint.Translation[0];
    Relative[13]:= Joint.Translation[1];
    Relative[14]:= Joint.Translation[2];


    if (Joint.Parent <> PHX_JOINT_NONE) then begin
      Absolute:= Matrix_Multiply(JointAbsoluteMatrix[ Joint.Parent ], Relative);
    end else begin
      Absolute:= Relative;
    end;

    JointRelativeMatrix[Index]:= Relative;
    JointAbsoluteMatrix[Index]:= Absolute;
  end;
  
end;

/// MS3D store the vertices in world-space, it seems. However, animation is described in
/// joint-space (e.g. relative to the joint). It is necessary to transform all vertices
/// to their joint space initially, using the inverse of their absolute matrix.
//------------------------------------------------------------------------------
procedure SetupVertices();
var I,J, K: Integer;
var Group   : TMS3D_GROUP;
var Triangle: TMS3D_TRIANGLE; 
var Vertex  : TMS3D_VERTEX;
var Matrix  : TMatrix4f;
var Vector  : TVector3f;
begin

  For I:= 0 to numGroups-1 do begin
    Group:= theGroups[I];

    For J:=0 to Group.numTriangles-1 do begin
      Triangle:= theTriangles[ Group.triangleIndices[J] ];

      For k:=0 to 2 do begin
        Vertex:= theVertices[ Triangle.VertexIndices[k] ];
        
        if(Vertex.BoneID <> AnsiChar(255)) then begin
          Matrix:= JointAbsoluteMatrix[ Integer(Vertex.BoneID) ];

          Vector.X:= Vertex.Vertex[0];
          Vector.Y:= Vertex.Vertex[1];
          Vector.Z:= Vertex.Vertex[2];

         // Vector:=Matrix.InverseTransform(Vector);
         // Matrix_TranslateInv
          Vector:= Matrix_TransformInv(Matrix, Vector );
	     //  Vector:=	matrix.inverseRotateVect( Vector );

        //  theVertices[ Triangle.VertexIndices[k] ].Vertex[0]:= Vector.X;
       //   theVertices[ Triangle.VertexIndices[k] ].Vertex[1]:= Vector.Y;
        //  theVertices[ Triangle.VertexIndices[k] ].Vertex[2]:= Vector.Z;

         // theVertices[ Triangle.VertexIndices[k] ].Vertex[0]:= theVertices[ Triangle.VertexIndices[k] ].Vertex[0] -5 ;
        //  theVertices[ Triangle.VertexIndices[k] ].Vertex[1]:= theVertices[ Triangle.VertexIndices[k] ].Vertex[1] -5 ;
         // theVertices[ Triangle.VertexIndices[k] ].Vertex[2]:= theVertices[ Triangle.VertexIndices[k] ].Vertex[2] -5 ;
        end;
      end;
    end;
  end;
 
end;



//------------------------------------------------------------------------------
Function MS3D_TO_PHXMDL(FileName: String): TPHXMesh ;
var i, j, k: Integer;
var Index: Integer;

var materialIndex: Integer;
var theVertex    : TMS3D_VERTEX;
var Vector: TVector3f;

var Group   : TMS3D_GROUP;
var Triangle: TMS3D_TRIANGLE; 
var Vertex  : TMS3D_VERTEX; 
begin
  LoadMS3DFromFile(FileName);

  // if Hierarchical then begin
  SetupJointMatrices();
 // SetupVertices();
  // end;

  Result:= TPHXMesh.Create;
  Result.Vertices.Count:= 0;

  CopyMaterials(Result);
  CopyJoints(Result);
  CopyTags  (Result);
  CopyGroups(Result);

  SetLength(theVertices , 0);
  SetLength(theTriangles, 0);
  SetLength(theGroups   , 0);
  SetLength(theMaterials, 0);
  SetLength(theJoints   , 0);

  SetLength(JointAbsoluteMatrix, 0);
  SetLength(JointRelativeMatrix, 0);
  
 {

  SetLength( Result.theGroups, numGroups);
  
  Index:=0;
  For I:= 0 to numGroups-1 do begin
    Group:= theGroups[I];
    
    Result.theGroups[I].Name    := Trim(Group.Name);
    Result.theGroups[I].Material:= Group.MaterialIndex;

    Result.theGroups[I].VertexCount := Group.numTriangles * 3;

    SetLength( Result.theGroups[I].VertexIndicies, Group.numTriangles * 3);


    Result.numVerts:= Result.numVerts + Group.numTriangles * 3;

    SetLength(Result.FVertices, Result.numVerts);

   For J:=0 to Group.numTriangles-1 do begin
      Triangle:= theTriangles[ Group.triangleIndices[J] ];

      For k:=0 to 2 do begin
        Vertex:= theVertices[ Triangle.VertexIndices[k] ];

        Result.FVertices[Index].Position.X:=  Vertex.Vertex[0];
        Result.FVertices[Index].Position.Y:=  Vertex.Vertex[1];
        Result.FVertices[Index].Position.Z:=  Vertex.Vertex[2];

        Result.FVertices[Index].Normal.X  :=  Triangle.VertexNormals [K][0];
        Result.FVertices[Index].Normal.Y  :=  Triangle.VertexNormals [K][1];
        Result.FVertices[Index].Normal.Z  :=  Triangle.VertexNormals [K][2];

        Result.FVertices[Index].TexCoord.X:=  Triangle.VertexTexCoord[K][0];
        Result.FVertices[Index].TexCoord.Y:=  Triangle.VertexTexCoord[K][1];
          
        Result.FVertices[Index].Joint := Word(Vertex.BoneID);

        Result.theGroups[I].VertexIndicies[J * 3 + K]:= J * 3 + K; 

        Inc(Index);
      end;
    end;

  end;
 
 }
end;



end.
