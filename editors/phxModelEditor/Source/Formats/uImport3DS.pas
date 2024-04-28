unit uImport3DS;

interface

uses
  Messages, SysUtils, Classes,

  uImport,

  phxTypes,
  phxMath,
  phxModel;

const
// Primary Chunk, at the beginning of each file
//------------------------------------------------------------------------------
CHUNK_PRIMARY     = $4D4D;
// Main Chunks
//------------------------------------------------------------------------------
CHUNK_OBJECTINFO      = $3D3D; // This gives the version of the mesh and is found right before the material and object information
CHUNK_VERSION         = $0002; // This gives the version of the .3ds file
CHUNK_EDITKEYFRAME    = $B000; // This is the header for all of the key frame info
// Sub defines of OBJECTINFO
//------------------------------------------------------------------------------
CHUNK_MATERIAL	      = $AFFF; // This stored the texture info
CHUNK_OBJECT		      = $4000; // This stores the faces, vertices, etc...
// Sub defines of MATERIAL
//------------------------------------------------------------------------------
CHUNK_MATNAME         = $A000; // This holds the material name
CHUNK_MATLUMINANCE    = $A010; // This holds the color of the object/material
CHUNK_MATDIFFUSE      = $A020; // This holds the color of the object/material
CHUNK_MATSPECULAR     = $A030; // This holds the color of the object/material
CHUNK_MATSHININESS    = $A040; // This holds the color of the object/material
CHUNK_MATMAP          = $A200; // This is a header for a new material
CHUNK_MATBUMPMAP      = $A300; // This is a header for a new material
CHUNK_MATMAPFILE      = $A300; // This holds the file name of the texture


CHUNK_OBJECT_MESH     = $4100; // This lets us know that we are reading a new object
// Sub defines of OBJECT_MESH
//------------------------------------------------------------------------------
CHUNK_OBJECT_VERTICES = $4110; // The objects vertices
CHUNK_OBJECT_FACES		= $4120; // The objects faces
CHUNK_OBJECT_MATERIAL = $4130; // This is found if the object has a material, either texture map or color
CHUNK_OBJECT_UV			  = $4140; // The UV texture coordinates
CHUNK_OBJECT_LMAT		  = $4160; // Local transformation matrix

Type

// Here is our structure for our 3DS indicies (since .3DS stores 4 unsigned shorts)
//------------------------------------------------------------------------------
TIndices = Record
  A      : Word; //Point 1 index's into the vertex array
  B      : Word; //Point 2 index's into the vertex array
  C      : Word; //Point 3 index's into the vertex array
  Visible: Word; //Visible flag
end;

// This holds the chunk info
//------------------------------------------------------------------------------
TChunk = Record
  ID       : Word	;    // The chunk's ID
  Length   : LongWord; // The length of the chunk
  BytesRead: LongWord; // The amount of bytes read within that chunk
end;

// This is our face structure.  This is is used for indexing into the vertex
// and texture coordinate arrays.  From this information we know which vertices
// from our vertex array go to which face, along with the correct texture coordinates.
//------------------------------------------------------------------------------
TFace = Record
  VertIndex : Array[0..2] of Integer;	// indicies for the verts that make up this triangle
  CoordIndex: Array[0..2] of Integer;	// indicies for the tex coords to texture this face
  MaterialID: Integer;
  HasTexture: Boolean;
end;

TMeshMatrix = array[0..11] of Single;

// This holds the information for a material.  It may be a texture map of a color.
// Some of these are not used, but I left them because you will want to eventually
// read in the UV tile ratio and the UV tile offset for some models.
//------------------------------------------------------------------------------
TMaterialInfo = Record
  Name           : AnsiString;                 // The texture name
  TextureFilename: AnsiString;                 // The texture file name (If this is set it's a texture map)
  Color          : Array[0..  2] of Byte; // The color of the object (R, G, B)
//  Texture        : Car;                // the texture ID
  uTile          : Single;                // u tiling of texture  (Currently not used)
  vTile          : Single;                // v tiling of texture	(Currently not used)
  uOffset        : Single;                // u offset of texture	(Currently not used)
  vOffset        : Single;                // v offset of texture	(Currently not used)
end ;

//------------------------------------------------------------------------------
TVector2 = Record
  X: Single;
  Y: Single;
end;

//------------------------------------------------------------------------------
TVector3 = Record
  X: Single;
  Y: Single;
  Z: Single;
end;

// This holds all the information for our model/scene.
// You should eventually turn into a robust class that
// has loading/drawing/querying functions like:
// LoadModel(...); DrawObject(...); DrawModel(...); DestroyModel(...);
//------------------------------------------------------------------------------
TGroup = Record
  numVertices : Word;                         // The number of verts in the model
  numFaces    : Word;                	        // The number of faces in the model
  numTexVertex: Word;                         // The number of texture coordinates
  materialID  : Integer;                      // The texture ID to use, which is the index into our texture array
  HasTexture  : Boolean;                      // This is TRUE if there is a texture map for this object
  strName     : AnsiString;                       // The name of the object
  theVertices : Array of TVector3;           // The object's vertices
  theNormals  : Array of TVector3;           // The object's vertices
  theFaces    : Array of TFace;               // The object's normals
  theTexVerts : Array of TVector2;           // The texture's UV coordinates
  LocalMatrix : Array[0..3, 0..3] of Single;  // Local transformation matrix
end;

TGroups = Array of TGroup;

//------------------------------------------------------------------------------
TImporter3DS = class(TImporter)
 private
    numGroups   : Integer;
    numMaterials: Integer;

    theGroups   : TGroups;
    theMaterials: Array of TMaterialInfo;

    Procedure LoadFromStream(Stream  : TStream);

    procedure CopyModel(Mesh: TPHXMesh);
    procedure CopyMaterials;

    function Seek(Stream: TStream; Offset: Integer): Integer;
    Function GetString (Stream: TStream; var S: AnsiString): Integer;

    procedure ProcessNextChunk        (Stream: TStream; var PreviousChunk: TChunk);
    procedure ProcessNextMaterialChunk(Stream: TStream; var PreviousChunk: TChunk);
    procedure ProcessNextObjectChunk  (Stream: TStream; var Group        : TGroup; var PreviousChunk: TChunk);

    procedure ReadChunk               (Stream: TStream; var Chunk        : TChunk);
    procedure ReadColorChunk          (Stream: TStream; var Material     : TMaterialInfo; var Chunk: TChunk);
    procedure ReadVertices            (Stream: TStream; var Group        : TGroup; var PreviousChunk: TChunk);
    procedure ReadVertexIndices       (Stream: TStream; var Group        : TGroup; var PreviousChunk: TChunk);
    procedure ReadLocalMatrix         (Stream: TStream; var Group        : TGroup; var PreviousChunk: TChunk);

    procedure ReadObjectMaterial(Stream: TStream; var Group: TGroup; var PreviousChunk: TChunk);
    procedure ReadUVCoordinates (Stream: TStream; var Group: TGroup; var PreviousChunk: TChunk);

    procedure ComputeNormals;
  public
    constructor Create; override;
    destructor Destroy; override;

    Procedure ImportMesh(Stream: TStream); overload; override;
  end;

implementation

//------------------------------------------------------------------------------
function TrimName(Const Name: AnsiString): AnsiString;
begin
   Result:= Copy(Name ,1, Pos(#0, Name )-1);
end;

//------------------------------------------------------------------------------
function Max(const A, B: Single): Single;
begin
  if A > B then
    Result := A
  else
    Result := B;
end;

//------------------------------------------------------------------------------
function Min(const A, B: Single): Single;
begin
  if A < B then
    Result := A
  else
    Result := B;
end;




// Class TImporter3DS
//==============================================================================
constructor TImporter3DS.Create;
begin
  inherited;
end;

//------------------------------------------------------------------------------
destructor TImporter3DS.Destroy;
begin

  inherited;
end;

//------------------------------------------------------------------------------
procedure TImporter3DS.ImportMesh(Stream: TStream);
begin
  LoadFromStream(Stream);

  // Clear the vertex data
//  Clear;

  CopyMaterials;
  CopyModel(Mesh);
end;

//------------------------------------------------------------------------------
procedure TImporter3DS.CopyMaterials;
begin
(*
  For Counter:=0 to NumMaterials-1 do
  begin
//    newMaterial(theMaterials[Counter].Name );

    Color.X:= theMaterials[Counter].Color[0] / 255;
    Color.Y:= theMaterials[Counter].Color[1] / 255;
    Color.Z:= theMaterials[Counter].Color[2] / 255;
    Color.W:= 1.0;

  //  matTexture( theMaterials[Counter].TextureFilename);
  //  matDiffuse( Color);
  end;
  *)
end;

//------------------------------------------------------------------------------
procedure TImporter3DS.CopyModel(Mesh: TPHXMesh);
var Counter1: Integer;
var Counter2: Integer;
var WhichVertex: Integer;
var Index      : Integer;
var Group  : TGroup;

var phxGroup: TPHXMeshGroup;
begin

  // Since we know how many objects our model has, go through each of them.
  for Counter1:=0 to numGroups-1 do
  begin
    // Get the current object that we are displaying
    Group:= theGroups[Counter1];

    phxGroup.Name          := Group.strName;
    phxGroup.TriangleCount := Group.numFaces;
    phxGroup.TriangleOffset:= Mesh.Triangles.Count;

    (*
    if (Group.materialID >= 0) and (Group.materialID <  NumMaterials) then
    begin
      phxGroup.Material.Name := theMaterials[Group.materialID ].Name;
      phxGroup.Material.Color:= clrWhite;
      phxGroup.Material.Flags:= 0;

      phxGroup.Material.Textures[0]:= theMaterials[Group.materialID ].TextureFilename;
      phxGroup.Material.Textures[1]:= '';
      phxGroup.Material.Textures[2]:= '';
      phxGroup.Material.Textures[3]:= '';
      phxGroup.Material.Textures[4]:= '';
      phxGroup.Material.Textures[5]:= '';
      phxGroup.Material.Textures[6]:= '';
      phxGroup.Material.Textures[7]:= '';
    end else
    begin
      phxGroup.Material.Name:='';
      phxGroup.Material.Color:= clrWhite;
      phxGroup.Material.Flags:= 0;

      phxGroup.Material.Textures[0]:= '';
      phxGroup.Material.Textures[1]:= '';
      phxGroup.Material.Textures[2]:= '';
      phxGroup.Material.Textures[3]:= '';
      phxGroup.Material.Textures[4]:= '';
      phxGroup.Material.Textures[5]:= '';
      phxGroup.Material.Textures[6]:= '';
      phxGroup.Material.Textures[7]:= '';
   end;
   *)

    (*
     // Go through all of the vertices  of the object
    for Counter2:=0 to Group.numVertices-1 do
    begin
      Mesh.Vertices.list^[Index].Position.X:= Group.theVertices[ Counter2 ].X;
      Mesh.Vertices.list^[Index].Position.Y:= Group.theVertices[ Counter2 ].Y;
      Mesh.Vertices.list^[Index].Position.Z:= Group.theVertices[ Counter2 ].Z;

      Mesh.Vertices.list^[Index].Normal.X:= Group.theNormals[ Counter2 ].X;
      Mesh.Vertices.list^[Index].Normal.Y:= Group.theNormals[ Counter2 ].Y;
      Mesh.Vertices.list^[Index].Normal.Z:= Group.theNormals[ Counter2 ].Z;

      Mesh.Vertices.list^[Index].Position.X:= Group.theTexVerts[ Counter2 ].X;
      Mesh.Vertices.list^[Index].Position.Y:= Group.theTexVerts[ Counter2 ].Y;
      // Pass in the current vertex of the object (Corner of current face)
//      Vertex(Group.theVertices[ Counter2 ].x, Group.theVertices[ Counter2 ].y, Group.theVertices[ Counter2 ].z);

      Inc(Index);
    end;
    *)

    Mesh.Vertices.Capacity:= Mesh.Vertices.Capacity + phxGroup.TriangleCount * 3;
    // Go through all of the faces (polygons) of the object and draw them
    for Counter2:=0 to Group.numFaces-1 do
    begin

     // newTriangle();
      // Go through each corner of the triangle and draw it.
      for WhichVertex:= 0 to 2 do
      begin
        // Get the index for each point of the face
        Index:= Group.theFaces[Counter2].vertIndex[WhichVertex];

        Mesh.Vertices.List^[Mesh.Vertices.Count].Joint:= PHX_JOINT_NONE;

        Mesh.Vertices.List^[Mesh.Vertices.Count].Position.X:= Group.theVertices[ Index ].X;
        Mesh.Vertices.List^[Mesh.Vertices.Count].Position.Y:= Group.theVertices[ Index ].Y;
        Mesh.Vertices.List^[Mesh.Vertices.Count].Position.Z:= Group.theVertices[ Index ].Z;
     // Give OpenGL the normal for this vertex.
      //  Normal(Group.theNormals[ index ].x, Group.theNormals[ index ].y, Group.theNormals[ index ].z);
        Mesh.Vertices.List^[Mesh.Vertices.Count].Normal.X:= Group.theNormals[ Index ].X;
        Mesh.Vertices.List^[Mesh.Vertices.Count].Normal.Y:= Group.theNormals[ Index ].Y;
        Mesh.Vertices.List^[Mesh.Vertices.Count].Normal.Z:= Group.theNormals[ Index ].Z;

        Mesh.Vertices.List^[Mesh.Vertices.Count].Normal:= VectorNormalize(Mesh.Vertices.List^[Mesh.Vertices.Count].Normal);

        // Make sure there was a UVW map applied to the object or else it won't have tex coords.
        if(Group.numTexVertex > 0) then
        begin
          Mesh.Vertices.List^[Mesh.Vertices.Count].TexCoord0.X:=   Group.theTexVerts[ Index ].X;
          Mesh.Vertices.List^[Mesh.Vertices.Count].TexCoord0.Y:= 1-Group.theTexVerts[ Index ].Y;
        end else
        begin
          Mesh.Vertices.List^[Mesh.Vertices.Count].TexCoord0.X:= 0;
          Mesh.Vertices.List^[Mesh.Vertices.Count].TexCoord0.Y:= 0;
        end;

      //   Self.Index( index  );
        Mesh.Vertices.Count:= Mesh.Vertices.Count + 1;
      end;

    end;
    Mesh.Groups.Add(phxGroup);
  end;

end;


{
//------------------------------------------------------------------------------
procedure TGLXLoaderStudioMesh.DrawGroupTexture;
var Counter1: Integer;
var Counter2: Integer;
var WhichVertex: Integer;
var Index      : Integer;
var Group  : TGroup;
var Color    : Array[0..  2] of Byte;
begin
  // Since we know how many objects our model has, go through each of them.
  for Counter1:=0 to numGroups-1 do begin

    // Get the current object that we are displaying
    Group:= theGroups[Counter1];

    IF Texturing then
    // Check to see if this object has a texture map, if so bind the texture to it.
    if(Group.HasTexture) then begin

      // Turn on texture mapping and turn off color
      glEnable(GL_TEXTURE_2D);

      // Reset the color to normal again
      glColor3ub(255, 255, 255);

      // Bind the texture map to the object by it's materialID
      glBindTexture(GL_TEXTURE_2D, theMaterials[Group.materialID].Texture);
    end else begin

      // Turn off texture mapping and turn on color
      glDisable(GL_TEXTURE_2D);

      // Reset the color to normal again
      glColor3ub(255, 255, 255);
    end;

    // Begin drawing with triangles
    glBegin(GL_TRIANGLES);
    // Go through all of the faces (polygons) of the object and draw them
    for Counter2:=0 to Group.numFaces-1 do begin
      // Go through each corner of the triangle and draw it.
      for WhichVertex:= 0 to 2 do begin
        // Get the index for each point of the face
        Index:= Group.theFaces[Counter2].vertIndex[WhichVertex];

        // Give OpenGL the normal for this vertex.
        glNormal3f(Group.theNormals[ index ].x, Group.theNormals[ index ].y, Group.theNormals[ index ].z);

        // If the object has a texture associated with it, give it a texture coordinate.
        if(Group.HasTexture) then begin

          // Make sure there was a UVW map applied to the object or else it won't have tex coords.
          if(Group.numTexVertex > 0) then begin
            glTexCoord2f(Group.theTexVerts[ Index ].X, Group.theTexVerts[ Index ].Y);
          end;

        end else begin

          // Make sure there is a valid material/color assigned to this object.
          // You should always at least assign a material color to an object,
          // but just in case we want to check the size of the material list.
          // if the size is at least one, and the material ID != -1,
          // then we have a valid material.
          if((numMaterials > 0)  and (Group.materialID >= 0)) then begin

            // Get and set the color that the object is, since it must not have a texture
            Color[0]:=theMaterials[Group.materialID].Color[0];
            Color[1]:=theMaterials[Group.materialID].Color[1];
            Color[2]:=theMaterials[Group.materialID].Color[2];

            // Assign the current color to this model
            glColor3ub(Color[0], Color[1], Color[2]);
          end ;
        end;

        // Pass in the current vertex of the object (Corner of current face)
        glVertex3f(Group.theVertices[ index ].x, Group.theVertices[ index ].y, Group.theVertices[ index ].z);
      end;
    end;
    glEnd();// End the drawing
  end;
end;

       {
//------------------------------------------------------------------------------
procedure TGLXLoaderStudioMesh.LoadTextures;
var Counter    : Integer;
var Name       : AnsiString;
var Filename   : AnsiString;
//var Texture    : TGLXGraphic;
begin
//  Texture:= TGLXGraphic.Create;
  For Counter:=0 to NumMaterials-1 do begin
    Name     :=theMaterials[Counter].Name           ;
    Filename :=theMaterials[Counter].TextureFilename;

    IF FileExists(BasePath + Filename) then begin
      Texture.LoadTexture(BasePath + Filename);
      Texture.BuildTexture(theMaterials[Counter].Texture);
    end else
    IF FileExists(BasePath + ChangeFileExt(Filename, '.bmp')) then begin
      Texture.LoadTexture(BasePath + ChangeFileExt(Filename, '.bmp'));
      Texture.BuildTexture(theMaterials[Counter].Texture);
    end else begin
      theMaterials[Counter].Texture:=0;
    end;
  end;
  Texture.Free;
end;

   }


//------------------------------------------------------------------------------
procedure TImporter3DS.LoadFromStream(Stream: TStream);
var currentChunk: TChunk;
begin
  // Once we have the file open, we need to read the very first data chunk
  // to see if it's a 3DS file.  That way we don't read an invalid file.
  // If it is a 3DS file, then the first chunk ID will be equal to PRIMARY (some hex num)

  // Read the first chuck of the file to see if it's a 3DS file
  ReadChunk(Stream, CurrentChunk);

  // Make sure this is a 3DS file
  if (CurrentChunk.ID <> CHUNK_PRIMARY) then
  begin
    raise Exception.Create('Unable to load PRIMARY chuck from file:');
    Exit;
  end;
  // Now we actually start reading in the data.  ProcessNextChunk() is recursive
  try
    // Begin loading objects, by calling this recursive function
    ProcessNextChunk(Stream, CurrentChunk);
  finally
  end;

  // After we have read the whole 3DS file, we want to calculate our own vertex normals.
  ComputeNormals;

  // Load all the textures for the model
 // LoadTextures;
end;


//------------------------------------------------------------------------------
procedure TImporter3DS.ProcessNextChunk(Stream: TStream; var PreviousChunk: TChunk);
var newTexture   : TMaterialInfo;// This is used to add to our material list
var newGroup     : TGroup;
var currentChunk : TChunk;       // The current chunk to load
var tempChunk    : TChunk;       // A temp chunk for holding data
var Version      : Cardinal;    // This will hold the file version
begin
  version := 0;

  FillChar(newTexture, SizeOf(newTexture), 0);
  FillChar(newGroup  , SizeOf(newGroup  ), 0);

  // Below we check our chunk ID each time we read a new chunk.  Then, if
  // we want to extract the information from that chunk, we do so.
  // If we don't want a chunk, we just read past it.

  // Continue to read the sub chunks until we have reached the length.
  // After we read ANYTHING we add the bytes read to the chunk and then check
  // check against the length.
  while (PreviousChunk.BytesRead < PreviousChunk.Length) do
  begin

    // Read next Chunk
    ReadChunk(Stream, CurrentChunk);

    // Check the chunk ID
    Case (CurrentChunk.ID) of

      // This holds the version of the file
      //--------------------------------------------------------------------------
      CHUNK_VERSION:
      begin
        // If the file was made in 3D Studio Max, this chunk has an int that
        // holds the file version.  Since there might be new additions to the 3DS file
        // format in 4.0, we give a warning to that problem.
        // However, if the file wasn't made by 3D Studio Max, we don't 100% what the
        // version length will be so we'll simply ignore the value

        Inc(CurrentChunk.BytesRead, Stream.Read(version, CurrentChunk.Length - CurrentChunk.BytesRead));

        // If the file version is over 3, give a warning that there could be a problem
        if ( version > $03 ) then
        begin
          raise Exception.Create('This 3DS file is over version 3 so it may load incorrectly');
        end;
      end;

      // This holds the version of the mesh
      //--------------------------------------------------------------------------
      CHUNK_OBJECTINFO:
      begin
        // This chunk holds the version of the mesh.  It is also the head of the MATERIAL
        // and OBJECT chunks.  From here on we start reading in the material and object info.

        // Read the next chunk
        ReadChunk(Stream, TempChunk);

        // Get the version of the mesh
        Inc(TempChunk.BytesRead, Stream.Read(Version, TempChunk.Length - TempChunk.BytesRead));


        // Increase the bytesRead by the bytes read from the last chunk
        Inc(CurrentChunk.bytesRead, TempChunk.bytesRead);

        // Go to the next chunk, which is the object has a texture, it should be MATERIAL, then OBJECT.
        ProcessNextChunk(Stream, CurrentChunk);
      end;

      // This holds the material information
      //--------------------------------------------------------------------------
      CHUNK_MATERIAL:
      begin
        // This chunk is the header for the material info chunks

        // Increase the number of materials
        Inc(numMaterials);

        // Add a empty texture structure to our texture list.
        // If you are unfamiliar with STL's "vector" class, all push_back()
        // does is add a new node onto the list.  I used the vector class
        // so I didn't need to write my own link list functions.
        SetLength(theMaterials, numMaterials);

        theMaterials[numMaterials-1]:=newTexture;

        // Proceed to the material loading function
        ProcessNextMaterialChunk(Stream, CurrentChunk);
      end;

      // This holds the name of the object being read
      //--------------------------------------------------------------------------
      CHUNK_OBJECT:
      begin
        // This chunk is the header for the object info chunks.  It also
        // holds the name of the object.

        // Increase the object count
        Inc(numGroups);

        // Add a new tObject node to our list of objects (like a link list)
        SetLength(theGroups, numGroups);
        theGroups[numGroups- 1]:= newGroup;

        // Initialize the object and all it's data members
        //  memset(&(pModel->pObject[pModel->numOfObjects - 1]), 0, sizeof(t3DObject));

        // Get the name of the object and store it, then add the read bytes to our byte counter.
        Inc(CurrentChunk.BytesRead, GetString(Stream, theGroups[numGroups - 1].strName));

        // Now proceed to read in the rest of the object information
        ProcessNextObjectChunk(Stream, theGroups[numGroups - 1], currentChunk);
      end;

      // Because I wanted to make this a SIMPLE tutorial as possible, I did not include
      // the key frame information.  This chunk is the header for all the animation info.
      // In a later tutorial this will be the subject and explained thoroughly.
      //--------------------------------------------------------------------------
      CHUNK_EDITKEYFRAME:
      begin
        //ProcessNextKeyFrameChunk(pModel, currentChunk);

        // Read past this chunk and add the bytes read to the byte counter
        Inc(CurrentChunk.bytesRead, Seek(Stream, CurrentChunk.Length - CurrentChunk.BytesRead));

      end;

      // If we didn't care about a chunk, then we get here.  We still need
      // to read past the unknown or ignored chunk and add the bytes read to the byte counter.
      //--------------------------------------------------------------------------
      else
      begin
        Inc(CurrentChunk.bytesRead, Seek(Stream, CurrentChunk.length - CurrentChunk.bytesRead));
      end;
    end;

    // Add the bytes read from the last chunk to the previous chunk passed in.
    Inc(PreviousChunk.BytesRead, CurrentChunk.BytesRead);
  end;
end;


//------------------------------------------------------------------------------
Function TImporter3DS.Seek(Stream: TStream; Offset: Integer): Integer;
var Start: Integer;
begin
  Start:=Stream.Position;
  Stream.Seek(Offset, soFromCurrent);
  Result:=Stream.Position - Start;
end;


//------------------------------------------------------------------------------
procedure TImporter3DS.ProcessNextMaterialChunk(Stream: TStream; var PreviousChunk: TChunk);
var CurrentChunk: TChunk;
begin

  // Continue to read these chunks until we read the end of this sub chunk
  while (PreviousChunk.BytesRead < PreviousChunk.Length) do
  begin

    // Read the next chunk
    ReadChunk(Stream, CurrentChunk);

    // Check which chunk we just read in
    Case (CurrentChunk.ID) of
      // This chunk holds the name of the material
      //--------------------------------------------------------------------------
      CHUNK_MATNAME:
      begin
        // Here we read in the material name
        SetLength(theMaterials[numMaterials - 1].Name, 256);
        Inc(CurrentChunk.BytesRead, Stream.Read(theMaterials[numMaterials - 1].Name[1], CurrentChunk.Length - CurrentChunk.BytesRead));
        // Trim the name
        theMaterials[numMaterials - 1].Name:=TrimName(theMaterials[numMaterials - 1].Name);
      end;

      // This holds the R G B color of our object
      //--------------------------------------------------------------------------
      CHUNK_MATDIFFUSE:
      begin
        ReadColorChunk(Stream, theMaterials[numMaterials - 1], CurrentChunk);
      end;


{		 CHUNK_MATLUMINANCE:
      ReadColorChunk(Stream, theMaterials[numMaterials - 1], CurrentChunk);

			readColorChunk(file, &data, CurrentMaterial.Material.EmissiveColor);
			break;
		case C3DS_MATDIFFUSE:
			readColorChunk(file, &data, CurrentMaterial.Material.DiffuseColor);
			break;
		case C3DS_MATSPECULAR:
			readColorChunk(file, &data, CurrentMaterial.Material.SpecularColor);
			break;
		case C3DS_MATREFLMAP:
		case C3DS_MATMAP:
		case C3DS_MATOPACMAP:
		case C3DS_MATSPECMAP:
		case C3DS_MATBUMPMAP:
			readMaterialChunk(file, &data);
			break;  }


      // This is the header for the texture info
      //--------------------------------------------------------------------------
      CHUNK_MATMAP:
      begin
        // Proceed to read in the material information
        ProcessNextMaterialChunk(Stream, currentChunk);
      end;

      // This stores the file name of the material
      //--------------------------------------------------------------------------
      CHUNK_MATMAPFILE:
      begin
        // Here we read in the material's file name
        SetLength(theMaterials[numMaterials - 1].TextureFilename, 256);
        Inc(CurrentChunk.BytesRead, Stream.Read(theMaterials[numMaterials - 1].TextureFilename[1], CurrentChunk.Length - CurrentChunk.BytesRead));
        // Trim the name
        theMaterials[numMaterials - 1].TextureFilename:=TrimName(theMaterials[numMaterials - 1].TextureFilename);
      end;

      // Read past the ignored or unknown chunks
      //--------------------------------------------------------------------------
      else
      begin
        // Read past the ignored or unknown chunks
        Inc(CurrentChunk.bytesRead, Seek(Stream, CurrentChunk.length - CurrentChunk.bytesRead));
      end;
    end;

    // Add the bytes read from the last chunk to the previous chunk passed in.
    Inc(PreviousChunk.BytesRead, CurrentChunk.BytesRead);
  end;
end;




// This function handles all the information about the objects in the file
//------------------------------------------------------------------------------
procedure TImporter3DS.ProcessNextObjectChunk(Stream: TStream; var Group: TGroup; var PreviousChunk: TChunk);
var CurrentChunk: TChunk;
begin

  // Continue to read these chunks until we read the end of this sub chunk
  while (PreviousChunk.BytesRead < PreviousChunk.Length) do
  begin

    // Read the next chunk
    ReadChunk(Stream, CurrentChunk);

    // Check which chunk we just read
    Case (currentChunk.ID) of

      // This lets us know that we are reading a new object
      //--------------------------------------------------------------------------
      CHUNK_OBJECT_MESH:
      begin
        // We found a new object, so let's read in it's info using recursion
        ProcessNextObjectChunk(Stream, Group, CurrentChunk);
      end;

      // This is the objects vertices
      //--------------------------------------------------------------------------
      CHUNK_OBJECT_VERTICES:
      begin
        ReadVertices(Stream, Group, CurrentChunk);
      end;

      // This is the objects face information
      //--------------------------------------------------------------------------
      CHUNK_OBJECT_FACES:
      begin // 0x4120
        ReadVertexIndices(Stream, Group, CurrentChunk);
      end;


      // This holds the material name that the object has
      //--------------------------------------------------------------------------
      CHUNK_OBJECT_MATERIAL:
      begin
        // This chunk holds the name of the material that the object has assigned to it.
        // This could either be just a color or a texture map.  This chunk also holds
        // the faces that the texture is assigned to (In the case that there is multiple
        // textures assigned to one object, or it just has a texture on a part of the object.
        // Since most of my game objects just have the texture around the whole object, and
        // they aren't multitextured, I just want the material name.

        // We now will read the name of the material assigned to this object
        ReadObjectMaterial(Stream, Group, CurrentChunk);
      end;

      // This holds the UV texture coordinates for the object
      //--------------------------------------------------------------------------
      CHUNK_OBJECT_UV:
      begin
        // This chunk holds all of the UV coordinates for our object.  Let's read them in.
        ReadUVCoordinates(Stream, Group, CurrentChunk);
      end;

      // Local transformation matrix
      //--------------------------------------------------------------------------
      CHUNK_OBJECT_LMAT:
      begin
        // This chunk holds the local transformation matrix
        ReadLocalMatrix(Stream, Group, CurrentChunk);
      end;
      // Read past the ignored or unknown chunks
      //--------------------------------------------------------------------------
      else
      begin
        Inc(CurrentChunk.BytesRead, Seek(Stream, CurrentChunk.length - CurrentChunk.BytesRead));
      end;
    end;
    // Add the bytes read from the last chunk to the previous chunk passed in.
    Inc(PreviousChunk.BytesRead, CurrentChunk.bytesRead);
  end;
end;



//------------------------------------------------------------------------------
procedure TImporter3DS.ReadChunk(Stream: TStream; var Chunk: TChunk);
begin
  Chunk.BytesRead:=0;

  // This reads the chunk ID which is 2 bytes.
  // The chunk ID is like OBJECT or MATERIAL.  It tells what data is
  // able to be read in within the chunks section.
  Inc(Chunk.BytesRead, Stream.Read(Chunk.ID    , 2));
  // Then, we read the length of the chunk which is 4 bytes.
  // This is how we know how much to read in, or read past.
  Inc(Chunk.BytesRead, Stream.Read(Chunk.Length, 4));
end;



//  This function reads in a AnsiString of characters
//------------------------------------------------------------------------------
Function TImporter3DS.GetString(Stream: TStream; var S: AnsiString): Integer;
var Index: Integer;
var Value: AnsiChar;
begin
  Index:=0;
  SetLength(S, 0);

  // Read 1 byte of data which is the first letter of the AnsiString
  Stream.Read(Value, 1);

  // Loop until we get NULL
  while(Value <> #0) do
  begin
    Inc(Index);

    IF Value <> #0 then S:=S + Value;

    Stream.Read(Value, 1);
  end;
//  Index:=Length(s);
  Result:= Index + 1;
  S:=S+'';
end;




// This function reads in the RGB color data
//------------------------------------------------------------------------------
procedure TImporter3DS.ReadColorChunk(Stream: TStream; var Material: TMaterialInfo; var Chunk: TChunk);
var TempChunk: TChunk;
begin
  // Read the color chunk info
  ReadChunk(Stream, TempChunk);

  // Read in the R G B color (3 bytes - 0 through 255)
  Inc(TempChunk.BytesRead, Stream.Read(Material.Color[0], TempChunk.Length - TempChunk.BytesRead));

  // Add the bytes read to our chunk
  Inc(Chunk.BytesRead, TempChunk.BytesRead);
end;


// This function reads in the indices for the vertex array
//------------------------------------------------------------------------------
procedure TImporter3DS.ReadVertexIndices(Stream: TStream; var Group: TGroup; var PreviousChunk: TChunk);
var Index  : Word;
var Counter1: Integer;
var Counter2: Integer;
begin
  // In order to read in the vertex indices for the object, we need to first
  // read in the number of them, then read them in.  Remember,
  // we only want 3 of the 4 values read in for each face.  The fourth is
  // a visibility flag for 3D Studio Max that doesn't mean anything to us.

  // Read in the number of faces that are in this object (int)
  Inc(PreviousChunk.BytesRead, Stream.Read(Group.numFaces, SizeOf(Group.numFaces)));

  // Alloc enough memory for the faces and initialize the structure
  SetLength(Group.theFaces, Group.numFaces);

  FillChar(Group.theFaces[0], SizeOf(TFace) * Group.numFaces, 0);

  // Go through all of the faces in this object
  for Counter1:= 0 to Group.numFaces-1 do
  begin
    // Next, we read in the A then B then C index for the face, but ignore the 4th value.
    // The fourth value is a visibility flag for 3D Studio Max, we don't care about this.
    for Counter2:= 0 to 3 do
    begin
      // Read the first vertice index for the current face
      Inc(PreviousChunk.BytesRead, Stream.Read(Index, SizeOf(Index)));
      // Store the index in our face structure.
      if(Counter2 < 3) then
      begin
        Group.theFaces[Counter1].vertIndex[Counter2]:= Index;
      end;
    end;
  end;
end;


// This function reads in the UV coordinates for the object
//------------------------------------------------------------------------------
procedure TImporter3DS.ReadUVCoordinates(Stream: TStream; var Group: TGroup; var PreviousChunk: TChunk);
begin
  // In order to read in the UV indices for the object, we need to first
  // read in the amount there are, then read them in.

  // Read in the number of UV coordinates there are (int)
  Inc(PreviousChunk.BytesRead, Stream.Read(Group.numTexVertex, 2));

  // Allocate memory to hold the UV coordinates
  SetLength(Group.theTexVerts, Group.numTexVertex);

  Inc(PreviousChunk.BytesRead, Stream.Read(Group.theTexVerts[0], PreviousChunk.Length - PreviousChunk.BytesRead));
end;


// his function reads in the local transformation matrix
//------------------------------------------------------------------------------
procedure TImporter3DS.ReadLocalMatrix(Stream: TStream; var Group: TGroup; var PreviousChunk: TChunk);
var i, j: Integer;
begin
  For j:=0 to 3 do
  begin
    For i:=0 to 2 do
    begin
      // Read in the local transformation matrix
      Inc(PreviousChunk.BytesRead, Stream.Read(Group.LocalMatrix[j,i], Sizeof(Single) ));
    end;
  end;

  Group.LocalMatrix[0,3]:=0;
  Group.LocalMatrix[1,3]:=0;
  Group.LocalMatrix[2,3]:=0;
  Group.LocalMatrix[3,3]:=1;
end;




// This function reads in the vertices for the object
//------------------------------------------------------------------------------
procedure TImporter3DS.ReadVertices(Stream: TStream; var Group: TGroup; var PreviousChunk: TChunk);
var Counter: Integer;
var TempY  : Single;
begin
  // Like most chunks, before we read in the actual vertices, we need
  // to find out how many there are to read in.  Once we have that number
  // we then fread() them into our vertice array.

  // Read in the number of vertices (int)
  Inc(PreviousChunk.BytesRead, Stream.Read(Group.numVertices, SizeOf(Group.numVertices)));

  // Allocate the memory for the verts and initialize the structure
  SetLength(Group.theVertices, Group.numVertices);
  FillChar(Group.theVertices[0], SizeOf(TVector3) * Group.numVertices, 0);// todo

  // Read in the array of vertices (an array of 3 floats)
  Inc(PreviousChunk.BytesRead, Stream.Read(Group.theVertices[0], PreviousChunk.Length - PreviousChunk.BytesRead));

  // Now we should have all of the vertices read in.  Because 3D Studio Max
  // Models with the Z-Axis pointing up (strange and ugly I know!), we need
  // to flip the y values with the z values in our vertices.  That way it
  // will be normal, with Y pointing up.  If you prefer to work with Z pointing
  // up, then just delete this next loop.  Also, because we swap the Y and Z
  // we need to negate the Z to make it come out correctly.


  // Go through all of the vertices that we just read and swap the Y and Z values
  for Counter:=0 to Group.numVertices -1 do
  begin
    // Store off the Y value
    TempY := Group.theVertices[Counter].Y;

    // Set the Y value to the Z value
    Group.theVertices[Counter].Y:= Group.theVertices[Counter].Z;

    // Set the Z value to the Y value,
    // but negative Z because 3D Studio max does the opposite.
    Group.theVertices[Counter].Z:= -TempY;
  end;

end;



//------------------------------------------------------------------------------
Function InArray(Value: Word; List: Array of Word): Boolean;
var Counter: Integer;
begin
  Result:=False;

  For Counter:=Low(List) to High(List) do
   begin
    Result:=Result or (List[Counter] = Value);
  end;
end;


// This function reads in the material name assigned to the object and sets the materialID
//------------------------------------------------------------------------------
procedure TImporter3DS.ReadObjectMaterial(Stream: TStream; var Group: TGroup; var PreviousChunk: TChunk);
var strMaterial: AnsiString;
var Counter    : Integer;
var materialID : Integer;
var HasTexture : Boolean;
var numFaces   : Word;
var theFaces   : Array of Word;
begin
  strMaterial := '';
  // *What is a material?*  - A material is either the color or the texture map of the object.
  // It can also hold other information like the brightness, shine, etc... Stuff we don't
  // really care about.  We just want the color, or the texture map file name really.

  // Here we read the material name that is assigned to the current object.
  // strMaterial should now have a AnsiString of the material name, like "Material #2" etc..
  Inc(PreviousChunk.bytesRead,  GetString(Stream, strMaterial));

  // Now that we have a material name, we need to go through all of the materials
  // and check the name against each material.  When we find a material in our material
  // list that matches this name we just read in, then we assign the materialID
  // of the object to that material index.  You will notice that we passed in the
  // model to this function.  This is because we need the number of textures.
  // Yes though, we could have just passed in the model and not the object too.

  // Set the ID to -1 to show there is no material for this object
  materialID:=-1;
  HasTexture:=False;
  // Go through all of the textures
  for Counter:=0 to numMaterials-1 do
  begin

    // If the material we just read in matches the current texture name
    if  strMaterial = theMaterials[Counter].Name then
    begin
      // Set the material ID to the current index 'i' and stop checking
       materialID:=Counter;
      // Now that we found the material, check if it's a texture map.
      // If the strFile has a AnsiString length of 1 and over it's a texture
      IF  Length(theMaterials[Counter].TextureFilename) > 0 then
      begin
        HasTexture:=True;
        Group.HasTexture:= HasTexture;
      end;
    end;
  end;
  // Set the groups materialID
  Group.materialID:= materialID;


  // Read the number of faces this material applies to
  Inc(PreviousChunk.BytesRead, Stream.Read(NumFaces, SizeOf(NumFaces)));
  // Set the face arrays length
  SetLength(TheFaces, NumFaces);
  // Read in the array of face materialID's
  Inc(PreviousChunk.BytesRead, Stream.Read(TheFaces[0],  SizeOf(Word) * NumFaces));

  // Loop trough all the faces
  For Counter:=0 to Group.numFaces-1 do
  begin
    // Does this material affect the face ?
    IF InArray(Counter, theFaces) then
    begin
      // Set the face materialID
      Group.theFaces[Counter].MaterialID:=materialID;
      Group.theFaces[Counter].HasTexture:=HasTexture;
    end;
  end;

  // Read past the rest of the chunk
  // You will notice we subtract the bytes already read in this chunk from the total length.
  Inc(PreviousChunk.bytesRead, Seek(Stream, PreviousChunk.length - PreviousChunk.BytesRead));
end;









//------------------------------------------------------------------------------
function VectorDiv(const destVector: TVector3f; const Scale: Single): TVector3; register;
var
  fScale: Single;
begin
  fScale := 1 / Scale;
  with Result do
  begin
    x := destVector.x * fScale;
    y := destVector.y * fScale;
    z := destVector.z * fScale;
  end;
end;

// Normalize a vector
//------------------------------------------------------------------------------
function VectorNormalise(const Vector: TVector3 ): TVector3 ;
var AMagnitude: Single;
begin
  AMagnitude:= Sqrt( Sqr(Vector.X) + Sqr(Vector.Y)+ Sqr(Vector.Z) );

  if AMagnitude <> 0 then
  begin
   Result.X:=Trunc(Vector.X / AMagnitude);
   Result.Y:=Trunc(Vector.Y / AMagnitude);
  end;

end;

//  This function computes the normals and vertex normals of the objects
//------------------------------------------------------------------------------
procedure TImporter3DS.ComputeNormals();
var
  vVector1: TVector3f;
  vVector2: TVector3f;
  vNormal: TVector3f;
  vPoly: array[0..2] of TVector3f;
  pNormals: Array of TVector3f;
  PTempNormals:  Array of TVector3f;
  vSum: TVector3f;
  vZero: TVector3f;
  i: Integer;
  j: Integer;
  index: Integer;
  shared: Integer;
  Group: ^TGroup;
begin
  // If there are no objects, we can skip this part
  if(numGroups <= 0) then Exit;


  // What are vertex normals?  And how are they different from other normals?
  // Well, if you find the normal to a triangle, you are finding a "Face Normal".
  // If you give OpenGL a face normal for lighting, it will make your object look
  // really flat and not very round.  If we find the normal for each vertex, it makes
  // the smooth lighting look.  This also covers up blocky looking objects and they appear
  // to have more polygons than they do.    Basically, what you do is first
  // calculate the face normals, then you take the average of all the normals around each
  // vertex.  It's just averaging.  That way you get a better approximation for that vertex.

  // Go through each of the objects to calculate their normals
  for index := 0 to numGroups - 1 do
  begin
    // Get the current object
    Group := @theGroups[index];

    // Here we allocate all the memory we need to calculate the normals
    SetLength(pNormals        , Group.numFaces);
    SetLength(pTempNormals    , Group.numFaces);
    SetLength(Group.theNormals, Group.numVertices);

    // Go though all of the faces of this object
    for i := 0 to Group.numFaces - 1 do
    begin
      // To cut down LARGE code, we extract the 3 points of this face
      vPoly[0].X:= Group.theVertices[Group.theFaces[i].VertIndex[0]].X;
      vPoly[0].Y:= Group.theVertices[Group.theFaces[i].VertIndex[0]].Y;
      vPoly[0].Z:= Group.theVertices[Group.theFaces[i].VertIndex[0]].Z;

      vPoly[1].X:= Group.theVertices[Group.theFaces[i].VertIndex[1]].X;
      vPoly[1].Y:= Group.theVertices[Group.theFaces[i].VertIndex[1]].Y;
      vPoly[1].Z:= Group.theVertices[Group.theFaces[i].VertIndex[1]].Z;

      vPoly[2].X:= Group.theVertices[Group.theFaces[i].VertIndex[2]].X;
      vPoly[2].Y:= Group.theVertices[Group.theFaces[i].VertIndex[2]].Y;
      vPoly[2].Z:= Group.theVertices[Group.theFaces[i].VertIndex[2]].Z;

      // Now let's calculate the face normals (Get 2 vectors and find the cross product of those 2)

      vVector1:= VectorSub(vPoly[0], vPoly[2]);				// Get the vector of the polygon (we just need 2 sides for the normal)
      vVector2:= VectorSub(vPoly[2], vPoly[1]);				// Get a second vector of the polygon

      vNormal:=VectorCross(vVector1, vVector2);		// Return the cross product of the 2 vectors (normalize vector, but not a unit vector)

      pTempNormals[i] := vNormal;					// Save the un-normalized normal for the vertex normals
      	// Normalize the cross product to give us the polygons normal

      pNormals[i] := VectorNormalize(vNormal);						// Assign the normal to the list of normals
    end;

    //////////////// Now Get The Vertex Normals /////////////////

    vSum:= Vector3f_Zero;
    vZero := vSum;
    shared := 0;

    for i := 0 to Group.numVertices - 1 do
    begin
      for j := 0 to Group.numFaces - 1 do
      begin										// Check if the vertex is shared by another face
        if (Group.theFaces[j].VertIndex[0] = i) or
          (Group.theFaces[j].VertIndex[1] = i) or
          (Group.theFaces[j].VertIndex[2] = i) then
        begin
          vSum := VectorAdd(vSum, pTempNormals[j]);			// Add the un-normalized normal of the shared face
          Inc(shared);								// Increase the number of shared triangles
        end;
      end;

      // Get the normal by dividing the sum by the shared.  We negate the shared so it has the normals pointing out.
      Group.theNormals[i] := VectorDiv(vSum, -shared);

      // Normalize the normal for the final vertex normal
      Group.theNormals[i]:= VectorNormalise(Group.theNormals[i]);

      vSum := vZero;									// Reset the sum
      shared := 0;										// Reset the shared
    end;

    // Free our memory and start over on the next object
    SetLength(pTempNormals, 0);
    SetLength(pNormals, 0);
  end;

end;


initialization
  TImporterFactory.Add('3D Studio File Format', '.3ds' , TImporter3DS);
end.
