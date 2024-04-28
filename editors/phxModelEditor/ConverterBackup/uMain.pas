unit uMain;

interface

uses
  Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, ComCtrls, ToolWin, ImgList, Menus, StdCtrls, Types,

  uActions, ExtCtrls,

  dglOpenGL,

  phxCamera,

  phxTypes,
  phxClasses,
  phxApplication,
  phxGraphics,
  phxGraphicsEx,
  phxDevice,
  phxTexture,
  phxModel,

  uViewer,
  uTextures,

  uModel.Properties,
  uModel.Material,
  uModel.Groups,
  uModel.Tags,
  uModel.Joints

  ;

type
  TFrmMain = class(TForm)
    StatusBar1: TStatusBar;
    MainMenu1: TMainMenu;
    File1: TMenuItem;
    Open1: TMenuItem;
    Open2: TMenuItem;
    Save1: TMenuItem;
    Save2: TMenuItem;
    menuRecent: TMenuItem;
    N2: TMenuItem;
    Exit1: TMenuItem;
    Edit1: TMenuItem;
    N5: TMenuItem;
    ools1: TMenuItem;
    MenuImport: TMenuItem;
    N3: TMenuItem;
    About1: TMenuItem;
    PhoenixImageEditor1: TMenuItem;
    N6: TMenuItem;
    lblVersion: TMenuItem;
    View1: TMenuItem;
    pnlClient: TPanel;
    PanelDraw: TPanel;
    PageControl1: TPageControl;
    TabModel: TTabSheet;
    TabGroups: TTabSheet;
    TabJoints: TTabSheet;
    TabTags: TTabSheet;
    N31: TMenuItem;
    btnLoadTexture: TMenuItem;
    Splitter1: TSplitter;
    btnditCenter: TMenuItem;
    btnEditScale: TMenuItem;
    btnEditRotate: TMenuItem;
    TabMaterial: TTabSheet;
    N4: TMenuItem;
    Shader1: TMenuItem;
    btnShaderTextured: TMenuItem;
    btnViewTextures: TMenuItem;
    btnShowGrid: TMenuItem;
    btnShowWireframe: TMenuItem;
    btnShowNormals: TMenuItem;
    btnShowBounds: TMenuItem;
    ControlBar2: TControlBar;
    ToolBarStandard: TToolBar;
    ToolButton11: TToolButton;
    ToolButton12: TToolButton;
    ToolButton13: TToolButton;
    ToolButton14: TToolButton;
    ToolButton15: TToolButton;
    ToolButton16: TToolButton;
    ToolButton17: TToolButton;
    ToolButton18: TToolButton;
    ToolButton1: TToolButton;
    ImageList1: TImageList;
    extureMapper1: TMenuItem;
    ToolButton2: TToolButton;
    btnViewMode: TToolButton;
    PopupMenu1: TPopupMenu;
    actViewPerspective1: TMenuItem;
    actViewOrthoX1: TMenuItem;
    actViewOrthoY1: TMenuItem;
    actViewOrthoZ1: TMenuItem;
    N1: TMenuItem;
    actViewOrthoBottom1: TMenuItem;
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);

    procedure PageControl1Change(Sender: TObject);


    procedure btnViewModeClick(Sender: TObject);
  private
    function GetSettings: TSettings;

    procedure SetSelected(Document: TDocument);
  public
    FrmViewer  : TFrmViewer;
    FrmModel   : TFrmModelProperties;
    FrmMaterial: TFrmModelMaterial;
    FrmGroups  : TFrmModelGroups;
    FrmTags    : TFrmModelTags;
    FrmJoints  : TFrmModelJoints;


    // List of recent documents
    property Settings: TSettings read GetSettings;
  end;

var
  FrmMain: TFrmMain;

implementation

{$R *.dfm}

Uses phxGraphics_FreeImage;
// Uses  phxFreeImage,  phxMilkshape3D;


//------------------------------------------------------------------------------
procedure TFrmMain.FormCreate(Sender: TObject);
begin
  ModActions:= TModActions.Create(Self);
  ModActions.Settings.Recent.Menu:= menuRecent;

  FrmModel:= TFrmModelProperties.Create(Self);
  FrmModel.Parent:= TabModel;
  FrmModel.Align := alClient;

  FrmMaterial:= TFrmModelMaterial.Create(Self);
  FrmMaterial.Parent:= TabMaterial;
  FrmMaterial.Align := alClient;

  FrmGroups:= TFrmModelGroups.Create(Self);
  FrmGroups.Parent:= TabGroups;
  FrmGroups.Align := alClient;


  FrmJoints:= TFrmModelJoints.Create(Self);
  FrmJoints.Parent:= TabJoints;
  FrmJoints.Align := alClient;

  FrmTags:= TFrmModelTags.Create(Self);
  FrmTags.Parent:= TabTags;
  FrmTags.Align := alClient;

  FrmViewer:= TFrmViewer.Create(Self);
  FrmViewer.Parent:= PanelDraw;
  FrmViewer.Align := alClient;

 // FrmTextures:= TFrmTextures.Create(Self);
 // FrmTextures.Parent:= TabTextures;
 // FrmTextures.Align := alClient;
 // FrmTextures.Device:= PHXDraw1.Device;
//  FrmTextures.LoadTexture('Models/fighter.jpg');

  ModActions.OnDocumentChange:= SetSelected;

  ModActions.CreateImportMenu(MenuImport);

  PageControl1.ActivePage:= TabModel;


//  Editor.Image:= nil;
  if ParamCount > 0 then
  begin
    ModActions.Open( ExpandFileName( ParamStr(1)) );
  end else
  begin
    SetSelected(nil);
  end;

  lblVersion.Caption:= 'Mesh version: ' + IntToStr( PHXMESH_VERSION );
end;

//------------------------------------------------------------------------------
procedure TFrmMain.PageControl1Change(Sender: TObject);
begin
end;

//------------------------------------------------------------------------------
procedure TFrmMain.SetSelected(Document: TDocument);
var Size  : TVector3f;
var Center:  TVector3f;
begin
  if Assigned(Document) then
  begin
    StatusBar1.Panels[0].Text:= '';

    if dsChanged in Document.State then
    begin
      Caption:= 'phxModelEditor - ' + Document.Filename + '*';

      StatusBar1.Panels[1].Text:= 'Modified';
    end else
    begin
      Caption:= 'phxModelEditor - ' + Document.Filename;

      StatusBar1.Panels[1].Text:= '';
    end;
    FrmViewer  .Mesh:= Document.Mesh;
    FrmModel   .Mesh:= Document.Mesh;
    FrmMaterial.Mesh:= Document.Mesh;
    FrmGroups  .Mesh:= Document.Mesh;
    FrmTags    .Mesh:= Document.Mesh;
    FrmJoints  .Mesh:= Document.Mesh;

    Size  := Document.Mesh.MeasureSize;
    Center:= Document.Mesh.MeasureCenter;

    StatusBar1.Panels[2].Text:= Format('Size: (%.3f, %.3f, %.3f)' , [Size.X, Size.Y, Size.Z]);
    StatusBar1.Panels[3].Text:= Format('Center: (%.3f, %.3f, %.3f)', [Center.X, Center.Y, Center.Z]);

  end else
  begin
    Caption:= 'phxModelEditor';

    StatusBar1.Panels[0].Text:= '';
    StatusBar1.Panels[1].Text:= '';
    StatusBar1.Panels[2].Text:= '';
    StatusBar1.Panels[3].Text:= '';

    FrmViewer  .Mesh:= nil;
    FrmModel   .Mesh:= nil;
    FrmGroups  .Mesh:= nil;
    FrmMaterial.Mesh:= nil;
    FrmTags    .Mesh:= nil;
    FrmJoints  .Mesh:= nil;
  end;

 // if Visible then PHXDraw1.SetFocus;
end;

//------------------------------------------------------------------------------
procedure TFrmMain.FormShow(Sender: TObject);
begin
  FrmViewer.PHXDraw1.SetFocus;
end;
   (*
//------------------------------------------------------------------------------
var Keys: array[Word] of Boolean;

var MouseX, MouseY: Integer;



var
  LightAmbient: array [0..3] of Single = ( 0.5, 0.5, 0.5, 1.0 );       // Okolní svìtlo
  LightDiffuse: array [0..3] of Single = ( 1.0, 1.0, 1.0, 1.0 );       // Pøímé svìtlo
  LightPosition: array [0..3] of Single = ( 0.0, 0.0, 2.0, 1.0 );      // Pozice svìtla

//var Ray: TRay;

//------------------------------------------------------------------------------
procedure TFrmMain.PHXDraw1Render(Sender: TObject);
//var Matrix: TMatrix;
//var View: TMatrix;
var Mesh: TPHXMesh;
//var Buffer: TPHXDrawBuffer;
//var WorldPosition: TVector3f;
var Zoom: Single;
begin
//  Device.SetViewMatrix(FMatrix);
  //TPHXCamera.PerspectiveMode(PHXDraw1.Device);
  PHXDraw1Resize(Sender);

  PHXDraw1.Device.SetClearColor( TColor4f.Create(28 / 255, 36 / 255, 45 / 255, 1.0) );
  PHXDraw1.Device.Clear;

  Zoom:= Camera.Distance / PHXDraw1.Width;

  // setup the projection matrix
  if Settings.ViewMode = vmPerspective then
  begin
    PHXDraw1.Device.SetProjectionMatrix(Camera.Projection);
  end else
  begin
    Matrix:= TMatrix.CreateOrthographic(- (PHXDraw1.Width * Zoom), +(PHXDraw1.Width * Zoom), - (PHXDraw1.Height * Zoom), +(PHXDraw1.Height * Zoom), 0.1, 5000);

    PHXDraw1.Device.SetProjectionMatrix(Matrix);
  end;

  View:= TMatrix.Identity;

  // Setup the view matrix
  case Settings.ViewMode of
    vmPerspective:
    begin
      View:= Camera.View;

      Camera.Update(0);
    end;
    vmOrthoTop:
    begin
      View:= TMatrix.CreateRotationX(90);

      PHXDraw1.Device.SetViewMatrix(View);
    end;
    vmOrthoBottom:
    begin
      View:= TMatrix.CreateRotationY(180) * TMatrix.CreateRotationX(90) ;
      PHXDraw1.Device.SetViewMatrix(View);
    end;

    vmOrthoY:
    begin
      Matrix:= TMatrix.CreateOrthographic(- (PHXDraw1.Width * Zoom), +(PHXDraw1.Width * Zoom), - (PHXDraw1.Height * Zoom), +(PHXDraw1.Height * Zoom), 0.1, 5000);

      PHXDraw1.Device.SetProjectionMatrix(Matrix);

      PHXDraw1.Device.SetViewMatrix(TMatrix.CreateRotationY(90));
    end;
    vmOrthoZ:
    begin
      Matrix:= TMatrix.CreateOrthographic(- (PHXDraw1.Width * Zoom), +(PHXDraw1.Width * Zoom), - (PHXDraw1.Height * Zoom), +(PHXDraw1.Height * Zoom), 0.1, 5000);

      PHXDraw1.Device.SetProjectionMatrix(Matrix);

      PHXDraw1.Device.SetViewMatrix(TMatrix.CreateRotationX(-90) );
    end;
  end;




  //  glEnable(GL_LIGHTING);
 // glEnable(GL_LIGHT0);
 // glLightfv(GL_LIGHT0, GL_AMBIENT, @LightAmbient);  // Nastavení okolního svìtla
//	glLightfv(GL_LIGHT0, GL_DIFFUSE, @LightDiffuse);  // Nastavení pøímého svìtla
//	glLightfv(GL_LIGHT0, GL_POSITION,@LightPosition); // Nastavení pozice svìtla


 // PHXDraw1.Device.SetViewMatrix(Camera.ModelViewMatrix);
  if ModActions.Settings.ShowGrid then
  begin
    PHXDraw1.Device.SetTexture(nil);

    case Settings.ViewMode of
      vmPerspective            :  Geometry.GridY(Vector3f_Zero, 100, 1, 10);
      vmOrthoTop, vmOrthoBottom:  Geometry.GridX(Vector3f_Zero, 100, 1, 10);
      vmOrthoY: ;
      vmOrthoZ: ;
    end;

  end;
  Geometry.Origin(TVector3f.Create(0, 10, 0), 1);

  if Assigned(ModActions.Document) then
  begin
    Mesh:= ModActions.Document.Mesh;

    Buffer:= TPHXDrawBuffer.Create(PHXDraw1.Device);
    try
      RenderMesh(Buffer, Mesh);
    finally
        Buffer.Free;
    end;
  end;

  WorldPosition:= ScreenToWorld(TVector2f.Create(MouseX, MouseY));
  StatusBar1.Panels[0].Text:= Format('X: %.2f, Y: %.2f, Z: %.2f', [WorldPosition.X, WorldPosition.Y, WorldPosition.Z]);

  Geometry.Color:= clrFuchsia;
  Geometry.Line(Vector3f_Zero, Vector3f_Zero + Ray.Direction * 100);

  PHXDraw1.Device.SetProjectionMatrix(TMatrix.CreateOrthographic(0, PHXDraw1.Width, 0, PHXDraw1.Height, -100.0, 100.0));

  View.Translation:= TVector3f.Create(0,0,0);
  PHXDraw1.Device.SetViewMatrix(View);

  Geometry.Origin(TVector3f.Create(32, 32, 0), 32);


  PHXDraw1.Device.Flip;

//  StatusBar1.Panels[0].Text:= Format('RotationX: %.2f, RotationY: %.2f, Distance: %.2f', [Camera.RotationX, Camera.RotationY, Camera.Distance]);
end;

//------------------------------------------------------------------------------
procedure TFrmMain.RenderMesh(Buffer: TPHXDrawBuffer; Mesh: TPHXMesh);
begin
  PHXDraw1.Device.SetTexture( ModActions.FindTexture(Mesh.Material.Textures[0]) );

  if ModActions.Settings.ShowWireframe then
  begin
    PHXDraw1.Device.SetRenderState(PHX_WIREFRAME, PHX_WIREFRAME_LINES);
    Mesh.Render(Buffer);
    PHXDraw1.Device.SetRenderState(PHX_WIREFRAME, PHX_WIREFRAME_FILL);
  end else
  begin
    Mesh.Render(Buffer);
  end;

  // Draw normals
  if ModActions.Settings.ShowNormals then
  begin
    RenderNormals(Mesh);
  end;
  // Draw bounding box
  if ModActions.Settings.ShowBounds then
  begin
    PHXDraw1.Device.SetTexture(nil);
    Geometry.Color:= clrMaroon;
    Geometry.BoundingBox(Mesh.Bounds);
  end;

      // Mesh.RenderNormals;
  if PageControl1.ActivePage = TabTags then RenderTags(Mesh);
end;


//------------------------------------------------------------------------------
procedure TFrmMain.RenderNormals(Mesh: TPHXMesh);
begin
  PHXDraw1.Device.SetTexture(nil);

  Mesh.RenderNormals(Geometry, 1, clrYellow);
end;

//------------------------------------------------------------------------------
procedure TFrmMain.RenderTags(Mesh: TPHXMesh);
var Index: Integer;
var Tag  : TPHXMeshTag;
var Normal: TVector3f;
begin

//  PHXDraw1.Device.SetRenderState(PHX_BLENDMODE, PHX_BLENDMODE_ALPHABLEND);

  for Index := 0 to Mesh.Tags.Count - 1 do
  begin
    Tag:= Mesh.Tags.List^[Index];

    //PHXDraw1.Device.SetTexture(nil);

    //Geometry.Color:= clrMaroon;
   // Geometry.Line( TVector3f.Create(Tag.Position.X - 1.0, Tag.Position.Y      , Tag.Position.Z      ), TVector3f.Create(Tag.Position.X + 1.0, Tag.Position.Y      , Tag.Position.Z      ));
   // Geometry.Line( TVector3f.Create(Tag.Position.X      , Tag.Position.Y - 1.0, Tag.Position.Z      ), TVector3f.Create(Tag.Position.X      , Tag.Position.Y + 1.0, Tag.Position.Z      ));
   // Geometry.Line( TVector3f.Create(Tag.Position.X      , Tag.Position.Y      , Tag.Position.Z - 1.0), TVector3f.Create(Tag.Position.X      , Tag.Position.Y + 1.0, Tag.Position.Z + 1.0));

    if Index = FrmTags.Selected then
    begin
      Geometry.Color:= clrWhite;
    end else
    begin
      Geometry.Color:= clrGray;
    end;
    PHXDraw1.Device.SetTexture(TextureJoint);
   // PHXDraw1.Device.SetTexture(nil);

    Geometry.Cube(Tag.Position, TVector3f.Create(0.5, 0.5, 0.5));


//    PHXDraw1.Device.SetTexture(nil);

   // Geometry.Color:= clrYellow;

   // Geometry.Line( Tag.Position, Tag.Position + (Tag.Direction * 2));

    Normal:= VectorCross(Tag.Direction, Vector3f_One);

    //Geometry.Color:= clrSilver;
   //// Geometry.Line( Tag.Position, Tag.Position + (Normal * 2));

    Geometry.Cube(Tag.Position + Tag.Direction * 0.5, TVector3f.Create(Tag.Direction.X + Normal.X * 0.1, Tag.Direction.Y + Normal.Y * 0.1, Tag.Direction.Z + Normal.Z * 0.1));


  //  Geometry.Line( TVector3f.Create(Mesh.Bounds.MinX, Tag.Position.Y  , Tag.Position.Z), TVector3f.Create(Mesh.Bounds.MaxX, Tag.Position.Y  , Tag.Position.Z));
 //   Geometry.Line( TVector3f.Create( Tag.Position.X , Mesh.Bounds.MinY, Tag.Position.Z), TVector3f.Create(Tag.Position.X  , Mesh.Bounds.MaxY,  Tag.Position.Z));

  //  Geometry.Line( TVector3f.Create(Mesh.Bounds.MinX, Tag.Position.Y  , Tag.Position.Z), TVector3f.Create(Mesh.Bounds.MaxX, Tag.Position.Y  , Tag.Position.Z));
   // Geometry.Line( TVector3f.Create( Tag.Position.X , Mesh.Bounds.MinY, Tag.Position.Z), TVector3f.Create(Tag.Position.X  , Mesh.Bounds.MaxY,  Tag.Position.Z));


  end;

//  PHXDraw1.Device.SetRenderState(PHX_BLENDMODE, PHX_BLENDMODE_NORMAL);
end;

//var ProjectionMatrix: TMatrix4f;

//------------------------------------------------------------------------------
procedure TFrmMain.PHXDraw1Resize(Sender: TObject);
begin
  //ProjectionMatrix:= Matrix_LoadPerspective(45.0, PHXDraw1.Device.Width / PHXDraw1.Device.Height, 0.1, 5000.0);
//  PHXDraw1.Device.SetProjectionMatrix(Camera.Projection);

end;



{$REGION 'Input handling'}

(*
This problem is called picking.
Search for mouse picking and you get lots and lots of hits.

Basic theory is this:

   1. Get x,y coords from the mouse click.
   2. Convert these to x,y,z coordinates in eye coordinates (i.e -1 <= x <= 1, -1 <= y <= 1, z=near/far clip distance, if you have a normal projection).
   3. Transform these coordinates by the inverse of the projection matrix to get world coordinates.
   4. You now have a ray from the camera position, with the direction towards the world coordinates you just got.
   5. Make a ray-object intersection test with the objects you want to consider. Choose the object that intersects the ray that is closest to the ray origin (camera position).










common use for Unproject is determining if the current cursor location intersects
with an object in 3D world space. First, calculate two Vector3 values that differ
only by their Z value. For instance, assume that the cursor location is currently (100, 100).
Therefore, the first vector (located at the near clip plane) becomes (100, 100, 0)
and the second (located at the far clip plane) becomes (100, 100, 1).

Call Unproject for each point, and store the result. For example, minPointSource
stores the result of "unprojecting" (100,100,0), and maxPointSource stores the
result of "unprojecting" (100, 100, 1). Determine the direction vector by
subtracting maxPointSource from minPointSource.

Finally, normalize the direction vector, and create a Ray with minPointSource
and the now-normalized direction vector. You can now use this ray in a simple
intersect test case (for example, Intersects) with the model.

http://www.3dkingdoms.com/selection.html


*)  (*

function GLMatrixd4(m: TMatrix4f): TGLMatrixd4;
var i,j: integer;
begin
  for i := 0 to 3 do
    for j := 0 to 3 do
    Result[i,j]:= m[i*3 +j];
end;

//------------------------------------------------------------------------------
function TFrmMain.ScreenToWorld(const MousePosition: TVector2f): TVector3f;
var minPointSource: TVector3f;
var maxPointSource: TVector3f;
//var ViewPosition: TVector3f;
var MatProjection: TMatrix4f;
var MatView     : TMatrix4f;


var Viewport: dglOpenGL.TVector4i;
var model: dglOpenGL.TGLMatrixd4;
var proj: dglOpenGL.TGLMatrixd4;
var objx, objy, objz: GLdouble;
var depth: GLfloat;
begin
//  MatProjection:= ProjectionMatrix;
 // MatProjection:= ProjectionMatrix;
  MatProjection:= Camera.Projection.v;
  MatView      := Camera.View.v;

  // 1. Get x,y coords from the mouse click.
  // 2. Convert these to x,y,z coordinates in eye coordinates (i.e -1 <= x <= 1, -1 <= y <= 1, z=near/far clip distance, if you have a normal projection).
  //Result.X:= MousePosition.X / PHXDraw1.Device.Width;
  //Result.Y:= MousePosition.Y / PHXDraw1.Device.Height;
  //Result.Z:= 0.1 / 100.0;

 // Result:= Matrix_TransformInv(MatProjection, Result);
 // Result:= Matrix_TransformInv(MatView      , Result);

  minPointSource.X:= MousePosition.X;
  minPointSource.Y:= MousePosition.Y;
  minPointSource.Z:= 0.0;

  maxPointSource.X:= MousePosition.X;
  maxPointSource.Y:= MousePosition.Y;
  maxPointSource.Z:= 1.0;

  minPointSource:= PHXDraw1.Device.Viewport.Unproject(minPointSource, MatProjection, MatView, TMatrix.Identity);
  maxPointSource:= PHXDraw1.Device.Viewport.Unproject(maxPointSource, MatProjection, MatView, TMatrix.Identity);

 // StatusBar1.Panels[4].Text:= Format('X: %.2f, Y: %.2f, Z: %.2f', [minPointSource.X, minPointSource.Y, minPointSource.Z]);

  Ray.Origin   := minPointSource;
  Ray.Direction:= (maxPointSource - minPointSource).Normalize;

  Ray.PlaneCollision(Vector3f_Zero, Vector3f_AxisZ, Result);

  StatusBar1.Panels[4].Text:= Format('X: %.2f, Y: %.2f, Z: %.2f', [Result.X, Result.Y, Result.Z]);

  Viewport[0]:= 0;
  Viewport[1]:= 0;
  Viewport[2]:=  PHXDraw1.Device.Width;
  Viewport[3]:=  PHXDraw1.Device.Height;

  minPointSource.X:= MousePosition.X;
  minPointSource.Y:= MousePosition.Y;
  minPointSource.Z:= 0.0;

  maxPointSource.X:= MousePosition.X;
  maxPointSource.Y:= MousePosition.Y;
  maxPointSource.Z:= 1.0;

  glGetDoublev(GL_PROJECTION_MATRIX, @proj);
  glGetDoublev(GL_MODELVIEW_MATRIX, @model);
  glGetIntegerv(GL_VIEWPORT, @viewport);

//  gluUnProject(MouseX, Viewport[3]-MouseY, 0, model, proj, Viewport, @objx, @objy, @objz); minPointSource:= TVector3f.Create(objx, objy, objz);
//  gluUnProject(MouseX, Viewport[3]-MouseY, 1, model, proj, Viewport, @objx, @objy, @objz); maxPointSource:= TVector3f.Create(objx, objy, objz);
  gluUnProject(MouseX, Viewport[3]-MouseY, 0, model, proj, Viewport, @objx, @objy, @objz); minPointSource:= TVector3f.Create(objx, objy, objz);
  gluUnProject(MouseX, Viewport[3]-MouseY, 1, model, proj, Viewport, @objx, @objy, @objz); maxPointSource:= TVector3f.Create(objx, objy, objz);

  //  StatusBar1.Panels[5].Text:= Format('X: %.2f, Y: %.2f, Z: %.2f', [minPointSource.X, minPointSource.Y, minPointSource.Z]);


  Ray.Origin   := minPointSource;
  Ray.Direction:= (maxPointSource - minPointSource).Normalize;

//  Ray.PlaneCollision(Vector3f_Zero, Vector3f_AxisZ, Result);
  Ray.Intersect(TPlane.Create(Vector3f_Zero, Vector3f_AxisZ), Result);

  StatusBar1.Panels[5].Text:= Format('X: %.2f, Y: %.2f, Z: %.2f', [Result.X, Result.Y, Result.Z]);


  glReadPixels(Round(MousePosition.x), Round(MousePosition.y), 1, 1, GL_DEPTH_COMPONENT, GL_FLOAT, @depth);

  gluUnProject(MouseX, MouseY, depth, model, proj, Viewport, @objx, @objy, @objz);
  Result:= TVector3f.Create(objx, objy, objz);

  StatusBar1.Panels[6].Text:= Format('X: %.2f, Y: %.2f, Z: %.2f', [Result.X, Result.Y, Result.Z]);
end;



//------------------------------------------------------------------------------
procedure TFrmMain.PHXDraw1KeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
  Keys[Key]:= True;
end;

//------------------------------------------------------------------------------
procedure TFrmMain.PHXDraw1KeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
  Keys[Key]:= False;
end;

//var DownPosition: TVector3f;

//------------------------------------------------------------------------------
procedure TFrmMain.PHXDraw1MouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  MouseX:= X;
  MouseY:= Y;

//  DownPosition:= ScreenToWorld(TVector2f.Create(X,Y));
end;

//------------------------------------------------------------------------------
procedure TFrmMain.PHXDraw1MouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
//var WorldPosition: TVector3f;
//var WorldDelta   : TVector3f;
var Mesh: TPHXMesh;
begin

 // WorldDelta.X:= DownPosition.X - WorldPosition.X;
 // WorldDelta.Y:= DownPosition.Y - WorldPosition.Y;
 // WorldDelta.Z:= DownPosition.Z - WorldPosition.Z;


  if ssLeft in Shift then
  begin
    Camera.RotationX:= Camera.RotationX + MouseY - Y;
    Camera.RotationY:= Camera.RotationY + MouseX - X;
  end else
  if ssRight in Shift then
  begin

    //StatusBar1.Panels[2].Text:= Format('X: %.2f, Y: %.2f, Z: %.2f', [WorldDelta   .X, WorldDelta   .Y, WorldDelta   .Z]);
  end else
  begin
  end;

  if Assigned(ModActions.Document) then
  begin
    Mesh:= ModActions.Document.Mesh;

    if PageControl1.ActivePage = TabTags then
    begin

      if ssRight in Shift then
      begin
        if (FrmTags.Selected >= 0) and (FrmTags.Selected < Mesh.Tags.Count)  then
        begin
         // Mesh.Tags.List^[FrmTags.Selected].Position.X:= Mesh.Tags.List^[FrmTags.Selected].Position.X - WorldDelta.X;
         // Mesh.Tags.List^[FrmTags.Selected].Position.Y:= Mesh.Tags.List^[FrmTags.Selected].Position.Y - WorldDelta.Y;
         // Mesh.Tags.List^[FrmTags.Selected].Position.Z:= Mesh.Tags.List^[FrmTags.Selected].Position.Z - WorldDelta.Z;
        end;
      end;


    end;

  end;
  // DownPosition:= WorldPosition;


  MouseX:= X;
  MouseY:= Y;

end;

//------------------------------------------------------------------------------
procedure TFrmMain.PHXDraw1MouseWheel(Sender: TObject; Shift: TShiftState; WheelDelta: Integer; MousePos: TPoint; var Handled: Boolean);
begin
  Camera.Distance:= Camera.Distance + WheelDelta / 100;
end;

{$ENDREGION}


   *)

//------------------------------------------------------------------------------
function TFrmMain.GetSettings: TSettings;
begin
  Result:= ModActions.Settings;
end;

//------------------------------------------------------------------------------
procedure TFrmMain.btnViewModeClick(Sender: TObject);
var P: TPoint;
begin
  P:= btnViewMode.ClientToScreen( Point(0, btnViewMode.Height));

  PopupMenu1.Popup(P.X, P.Y);
end;

end.

