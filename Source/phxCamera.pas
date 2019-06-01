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
unit phxCamera;
//< Camera classes

interface

{$I phxConfig.inc}

uses Classes,

  phxTypes,
  phxClasses,
  phxEvents,
  phxMath,

  phxDevice;

Type

// Abstract camera class
//------------------------------------------------------------------------------
TPHXCamera = class(TObject)
  private
    FName      : String;
    FDevice    : TPHXDevice;
  protected
    // Return the projection matrix for the camera
    function GetProjectionMatrix: TMatrix4f; virtual;
    // Return the view matrix for the camera
    function GetViewMatrix: TMatrix4f; virtual;
  public
    // Default constructor
    constructor Create(ADevice: TPHXDevice); virtual;
    destructor Destroy; override;

    // Update the camera
    procedure Update(FrameTime: Single); virtual; abstract;
    // Handle a phoenix event
    procedure HandleEvent(const Event: TPHXEvent); virtual;


    // Owning device
    Property Device: TPHXDevice read FDevice;
    // Name of the camera
    property Name: String read FName write FName;
    // Return the projection matrix for the camera
    property Projection: TMatrix4f read GetProjectionMatrix;
    // Return the view matrix for the camera
    property View: TMatrix4f read GetViewMatrix;
  end;

//------------------------------------------------------------------------------
TPHXCameraManager = class
  private
    // List of cameras
    FCameras: TList;
    // Active camera
    FActive: TPHXCamera;
  public
    constructor Create(ADevice: TPHXDevice); virtual;
    destructor Destroy; override;

    // Update the active camera
    procedure Update(FrameTime: Single); virtual;

    // The active camera
    property Active: TPHXCamera read FActive;
    // List of cameras
    property Cameras: TList read FCameras;
  end;

// 2D camera with a orthographic projection matrix
//------------------------------------------------------------------------------
TPHXCamera2D = class(TPHXCamera)
  private
    FPosition : TVector2f;
    FRotation : Single;
    FZoom     : Single;
    FView     : TMatrix4f;
  protected
    function GetProjectionMatrix: TMatrix4f; override;
    function GetViewMatrix: TMatrix4f; override;
  public
    constructor Create(ADevice: TPHXDevice); override;
    destructor Destroy; override;

    // Update the camera
    procedure Update(FrameTime: Single); override;

    // Attach the camera to a matrix
    procedure Attatch(const Matrix: TMatrix4f; Width, Height: Integer); overload;
    // Attach the camera to a position
    procedure Attatch(const Position: TVector2f; Width, Height: Integer); overload;

    // Move the camera right
    procedure MoveRight(Distance: Single);
    // Move the camera left
    procedure MoveLeft(Distance: Single);
    // Move the camera up
    procedure MoveUp(Distance: Single);
    // Move the camera down
    procedure MoveDown(Distance: Single);
    // Rotate the camera to the left
    procedure RotateLeft(Distance: Single);
    // Rotate the camera to the right
    procedure RotateRight(Distance: Single);

    // Convert a screen coordinate to a world coordinate
    function ScreenToWorld(const Vector: TVector3f): TVector3f; overload;
    // Convert a screen coordinate to a world coordinate
    function ScreenToWorld(const Vector: TVector2i): TVector2f; overload;
    // Convert a screen coordinate to a world coordinate
    function ScreenToWorld(const Vector: TVector2f): TVector2f; overload;

    // Current position of the camera
    property Position: TVector2f read FPosition write FPosition;
    // Current rotation of the camera
    property Rotation: Single read FRotation write FRotation;
    // The zoom of the camrea
    property Zoom: Single read FZoom write FZoom;
  end;

// The editor is a camera that rotates around a point in the scene
//------------------------------------------------------------------------------
TPHXEditorCamera = class(TPHXCamera)
  private
    FCenter   : TVector3f;
    FRotationX: Single;
    FRotationY: Single;
    FDistance : Single;
    FView     : TMatrix4f;

    procedure UpdateMatrix;

    procedure SetCenter(const Value: TVector3f);
    procedure SetDistance(const Value: Single);
    procedure SetRotationX(const Value: Single);
    procedure SetRotationY(const Value: Single);
  protected
    function GetViewMatrix: TMatrix4f; override;
    function GetProjectionMatrix: TMatrix4f; override;
  public
    constructor Create(ADevice: TPHXDevice); override;

    // Reset the camera
    procedure Reset;

    // Update the camera
    procedure Update(FrameTime: Single); override;

    // Rotate the camera to the left
    procedure RotateLeft(const Delta: Single);
    // Rotate the camera to the right
    procedure RotateRight(const Delta: Single);

    // Center positon
    property Center: TVector3f read FCenter write SetCenter;
    // Camera rotation
    property RotationX: Single read FRotationX write SetRotationX;
    // Camera rotation
    property RotationY: Single read FRotationY write SetRotationY;
    // Distance from the center position
    property Distance: Single read FDistance write SetDistance;
 end;













// A camera that can be panned using the right mouse button
//------------------------------------------------------------------------------
TPHXDefaultCamera2D = class(TPHXCamera2D)
  private
  //  constructor Create(ADevice: TPHXDevice); override;

//    Property Input: TPHXInput;
  end;


// 3D camera with a perspective projection matrix
//------------------------------------------------------------------------------
TPHXCamera3D = class(TPHXCamera)
  private
    FPosition: TVector3f;
    
    FVecForward: TVector3f;
    FVecRight  : TVector3f;
    FVecUp     : TVector3f;

    // The the matrix needs to be recalculated
    FChanged   : Boolean;

    FView     : TMatrix4f;
    procedure BuildViewMatrix;
  protected
    function GetViewMatrix: TMatrix4f; override;
    function GetProjectionMatrix: TMatrix4f; override;

    property Changed: Boolean read FChanged write FChanged;
  public
    constructor Create(ADevice: TPHXDevice); override;

    // Update the camera
    procedure Update(FrameTime: Single); override;

    // Reset the camera to the default position
    procedure LoadIdentity;

    // Translate the camera
    procedure Translate(DX, DY, DZ: Single);

    // Define a viewing transformation
    procedure LookAt(const Eye, Target, Up: TVector3f);


    procedure MoveForward (const Distance: Single);
    procedure MoveBackward(const Distance: Single);
    procedure MoveRight   (const Distance: Single);
    procedure MoveLeft    (const Distance: Single);
    procedure MoveUp      (const Distance: Single);
    procedure MoveDown    (const Distance: Single);

    procedure RotateX(Distance: Single; WorldAxis: Boolean = False);
    procedure RotateY(Distance: Single; WorldAxis: Boolean = True);
    procedure RotateZ(Distance: Single; WorldAxis: Boolean = False);

    procedure RotateFlight(Heading, Pitch, Roll: Single);

    procedure SetTarget(Target: TVector3f);

    property Position: TVector3f read FPosition;
  end;






(*

glPushMatrix();
  glMatrixMode(GL_PROJECTION);
  glLoadIdentity();
  glOrtho(-(width/2)/zoom, (width/2)/zoom, -(height/2)/zoom, (height/2)/zoom, nearZ, farZ);
  glMatrixMode(GL_MODELVIEW);
  glLoadIdentity();
  glRotated(-60,1,0,0);
  glRotated(45,0,0,1);
glPopMatrix();


*)
TPHXCameraISO = class(TPHXCamera)

  end;


implementation


const DEFAULT_CAMERA_NAME = 'Camera';


// TPHXCamera
//==============================================================================
constructor TPHXCamera.Create(ADevice: TPHXDevice);
begin
  FDevice:= ADevice;
  FName  := DEFAULT_CAMERA_NAME;
end;

//------------------------------------------------------------------------------
destructor TPHXCamera.Destroy;
begin

  inherited;
end;


//------------------------------------------------------------------------------
procedure TPHXCamera.HandleEvent(const Event: TPHXEvent);
begin

end;

//------------------------------------------------------------------------------
function TPHXCamera.GetProjectionMatrix: TMatrix4f;
begin
  Result:= Matrix_CreateOrthographicLH(Device.Width, Device.Height, -1000, 1000);;
end;

//------------------------------------------------------------------------------
function TPHXCamera.GetViewMatrix: TMatrix4f;
begin
  Result:= TMatrix4f.Identity;
end;



// TPHXCameraManager
//==============================================================================
constructor TPHXCameraManager.Create(ADevice: TPHXDevice);
begin

end;

//------------------------------------------------------------------------------
destructor TPHXCameraManager.Destroy;
begin

  inherited;
end;

//------------------------------------------------------------------------------
procedure TPHXCameraManager.Update(FrameTime: Single);
begin
  if Assigned(Active) then
  begin
    Active.Update(FrameTime);
  end;
end;






// TPHXCamera2D
//==============================================================================
constructor TPHXCamera2D.Create(ADevice: TPHXDevice);
begin
  inherited Create(ADevice);

  FPosition:= Vector2f_Zero;
  FRotation:= 0;
  FZoom    := 1.0;
  FView    := TMatrix4f.Identity;
end;

//------------------------------------------------------------------------------
destructor TPHXCamera2D.Destroy;
begin

  inherited;
end;

//------------------------------------------------------------------------------
procedure TPHXCamera2D.MoveLeft(Distance: Single);
begin
  FPosition.X:= FPosition.X - Distance;
end;

//------------------------------------------------------------------------------
procedure TPHXCamera2D.MoveRight(Distance: Single);
begin
  FPosition.X:= FPosition.X + Distance;
end;

//------------------------------------------------------------------------------
procedure TPHXCamera2D.MoveUp(Distance: Single);
begin
  FPosition.Y:= FPosition.Y - Distance;
end;

//------------------------------------------------------------------------------
procedure TPHXCamera2D.RotateLeft(Distance: Single);
begin
  FRotation:= FRotation - Distance;
end;

//------------------------------------------------------------------------------
procedure TPHXCamera2D.RotateRight(Distance: Single);
begin
  FRotation:= FRotation + Distance;
end;  

//------------------------------------------------------------------------------
function TPHXCamera2D.ScreenToWorld(const Vector: TVector3f): TVector3f;
begin
  Result:= Matrix_TransformInv(FView, Vector);
end;

//------------------------------------------------------------------------------
function TPHXCamera2D.ScreenToWorld(const Vector: TVector2f): TVector2f;
begin
  Result:= Matrix_TransformInv(FView, Vector);
end;

//------------------------------------------------------------------------------
function TPHXCamera2D.ScreenToWorld(const Vector: TVector2i): TVector2f;
begin
  Result:= Matrix_TransformInv(FView, TVector2f.Create(Vector.X, Vector.Y));
end;

//------------------------------------------------------------------------------
procedure TPHXCamera2D.MoveDown(Distance: Single);
begin
  FPosition.Y:= FPosition.Y + Distance;
end;

//------------------------------------------------------------------------------
procedure TPHXCamera2D.Attatch(const Matrix: TMatrix4f; Width, Height: Integer);
begin
  FPosition:= Matrix_Transform(Matrix, TVector2f.Create(0, 0));
  FPosition.X:= FPosition.X - Width  * 0.5;
  FPosition.Y:= FPosition.Y - Height * 0.5;
end;

//------------------------------------------------------------------------------
procedure TPHXCamera2D.Attatch(const Position: TVector2f; Width, Height: Integer);
begin
  FPosition.X:= Position.X - Width  * 0.5;
  FPosition.Y:= Position.Y - Height * 0.5;
end;

//------------------------------------------------------------------------------
procedure TPHXCamera2D.Update(FrameTime: Single);
var MatTranslate: TMatrix4f;
var MatRotate   : TMatrix4f;
var MatScale    : TMatrix4f;
begin
  MatTranslate:= Matrix_CreateTranslation(-Trunc(FPosition.X), -Trunc(FPosition.Y), 0);
  MatRotate   := Matrix_CreateRotationZ(-Rotation);
  MatScale    := Matrix_CreateScale(FZoom, FZoom, 1.0);

  FView:= Matrix_Multiply(MatScale, MatTranslate, MatRotate );
end;

//------------------------------------------------------------------------------
function TPHXCamera2D.GetProjectionMatrix: TMatrix4f;
begin
  Result:= Matrix_CreateOrthographicLH(Device.Width, Device.Height, -10, 10);
end;

//------------------------------------------------------------------------------
function TPHXCamera2D.GetViewMatrix: TMatrix4f;
begin
  Result:= FView;
end;





// TPHXEditorCamera
//==============================================================================
constructor TPHXEditorCamera.Create(ADevice: TPHXDevice);
begin
  inherited Create(ADevice);

  Reset;
end;

//------------------------------------------------------------------------------
procedure TPHXEditorCamera.Reset;
begin
  FView     := TMatrix4f.Identity;
  FCenter   := TVector3f.Zero;
  FRotationX:= -30;
  FRotationY:=   0;
  FDistance :=  50;

  UpdateMatrix;
end;


//------------------------------------------------------------------------------
procedure TPHXEditorCamera.Update(FrameTime: Single);
begin
 // Device.SetViewMatrix(FMatrix);
end;

//------------------------------------------------------------------------------
procedure TPHXEditorCamera.RotateLeft(const Delta: Single);
begin
  FRotationY := FRotationY - Delta;

  UpdateMatrix;
end;

//------------------------------------------------------------------------------
procedure TPHXEditorCamera.RotateRight(const Delta: Single);
begin
  FRotationY := FRotationY + Delta;

  UpdateMatrix;
end;

//------------------------------------------------------------------------------
procedure TPHXEditorCamera.UpdateMatrix;
var MatCenter     : TMatrix4f;
var MatRotationX  : TMatrix4f;
var MatRotationY  : TMatrix4f;
var MatTranslation: TMatrix4f;
begin
  MatCenter     := Matrix_CreateTranslation(Center);
  MatRotationX  := Matrix_CreateRotationX(-FRotationX);
  MatRotationY  := Matrix_CreateRotationY(-FRotationY);
  MatTranslation:= Matrix_CreateTranslation(0, 0, -Distance);

  FView:= Matrix_Multiply(MatTranslation, MatRotationX, MatRotationY, MatCenter);
end;

//------------------------------------------------------------------------------
procedure TPHXEditorCamera.SetCenter(const Value: TVector3f);
begin
  FCenter := Value;

  UpdateMatrix;
end;

//------------------------------------------------------------------------------
procedure TPHXEditorCamera.SetRotationX(const Value: Single);
begin
  if FRotationX <> Value then
  begin
    FRotationX := Value;

  //  while FRotationX > 360 do FRotationX:= FRotationX - 360;
  //  while FRotationX < 0   do FRotationX:= FRotationX + 360;

    if FRotationX >  90 then FRotationX:=  90;
    if FRotationX < -90 then FRotationX:= -90;


    UpdateMatrix;
  end;
end;

//------------------------------------------------------------------------------
procedure TPHXEditorCamera.SetRotationY(const Value: Single);
begin
  if FRotationY <> Value then
  begin
    FRotationY := Value;

    while FRotationY > 360 do FRotationY:= FRotationY - 360;
    while FRotationY < 0   do FRotationY:= FRotationY + 360;

   UpdateMatrix;
  end;
end;

//------------------------------------------------------------------------------
procedure TPHXEditorCamera.SetDistance(const Value: Single);
begin
  if FDistance <> Value then
  begin
    FDistance := Value;

    UpdateMatrix;
  end;
end;

//------------------------------------------------------------------------------
function TPHXEditorCamera.GetProjectionMatrix: TMatrix4f;
begin
  Result:= Matrix_CreatePerspective(45.0, Device.Width/Device.Height, 0.1, 1000.0);
end;

//------------------------------------------------------------------------------
function TPHXEditorCamera.GetViewMatrix: TMatrix4f;
begin
  Result:= FView;
end;












//
//==============================================================================
constructor TPHXCamera3D.Create(ADevice: TPHXDevice);
begin
  inherited Create(ADevice);

  FPosition  := TVector3f.Create( 0.0,  0.0,  0.0);
  FVecForward:= TVector3f.Create( 0.0,  0.0, -1.0);
  FVecRight  := TVector3f.Create(-1.0,  0.0,  0.0);
  FVecUp     := TVector3f.Create( 0.0,  1.0,  0.0);
  FChanged   := False;
  FView      := TMatrix4f.Identity;

  BuildViewMatrix;
end;


//------------------------------------------------------------------------------
procedure TPHXCamera3D.BuildViewMatrix;
begin
  FView.v[0 ]:= FVecRight  .x;
  FView.v[1 ]:= FVecUp     .x;
  FView.v[2 ]:= FVecForward.x;
  FView.v[3 ]:= 0.0;

  FView.v[4 ]:= FVecRight  .y;
  FView.v[5 ]:= FVecUp     .y;
  FView.v[6 ]:= FVecForward.y;
  FView.v[7 ]:= 0.0;

  FView.v[8 ]:= FVecRight  .z;
  FView.v[9 ]:= FVecUp     .z;
  FView.v[10]:= FVecForward.z;
  FView.v[11]:= 0.0;

  FView.v[12]:= - VectorDot(FPosition, FVecRight);
  FView.v[13]:= - VectorDot(FPosition, FVecUp   );
  FView.v[14]:= - VectorDot(FPosition, FVecForward );
  FView.v[15]:= 1.0;
end;

//------------------------------------------------------------------------------
procedure TPHXCamera3D.Update(FrameTime: Single);
begin
  if Changed then BuildViewMatrix;

//  Device.SetViewMatrix(FMatrix);
end;
    {
//------------------------------------------------------------------------------
function TPHXCamera3D.GetMatrix: TMatrix4f;
begin
  if Changed then BuildViewMatrix;

  Result:= FView;
end;


     }
//------------------------------------------------------------------------------
Procedure TPHXCamera3D.LookAt(const Eye, Target, Up: TVector3f);
begin
  FPosition:= eye;

  FVecForward:= VectorNormalize( VectorSub(Eye, Target));
  FVecRight  := VectorNormalize( VectorCross(Up         , FVecForward));
  FVecUp     := VectorNormalize( VectorCross(FVecForward, FVecRight));
  // Extract the pitch angle from the view matrix.
  //    m_accumPitchDegrees = Math::radiansToDegrees(-asinf(m_viewMatrix[1][2]));

  Changed:= True;
end;

//------------------------------------------------------------------------------
procedure TPHXCamera3D.LoadIdentity;
begin
  FPosition  := TVector3f.Create( 0.0,  0.0,  0.0);
  FVecForward:= TVector3f.Create( 0.0,  0.0, -1.0);
  FVecRight  := TVector3f.Create(-1.0,  0.0,  0.0);
  FVecUp     := TVector3f.Create( 0.0,  1.0,  0.0);
  FView      := TMatrix4f.Identity;
  FChanged   := False;
end;

//------------------------------------------------------------------------------
Procedure TPHXCamera3D.Translate(DX, DY, DZ: Single);
begin
  FPosition.X:= FPosition.X + FVecForward.X * DZ;
  FPosition.Y:= FPosition.Y + FVecForward.Y * DZ;
  FPosition.Z:= FPosition.Z + FVecForward.Z * DZ;

  FPosition.X:= FPosition.X + FVecUp.X * DY;
  FPosition.Y:= FPosition.Y + FVecUp.Y * DY;
  FPosition.Z:= FPosition.Z + FVecUp.Z * DY;

  FPosition.X:= FPosition.X + FVecRight.X * DX;
  FPosition.Y:= FPosition.Y + FVecRight.Y * DX;
  FPosition.Z:= FPosition.Z + FVecRight.Z * DX;
end;



//------------------------------------------------------------------------------
procedure TPHXCamera3D.MoveForward(const Distance: Single);
begin
  FPosition.X:= FPosition.X - FVecForward.X * Distance;
  FPosition.Y:= FPosition.Y - FVecForward.Y * Distance;
  FPosition.Z:= FPosition.Z - FVecForward.Z * Distance;

  FChanged:= True;
end;

//------------------------------------------------------------------------------
procedure TPHXCamera3D.MoveBackward(const Distance: Single);
begin
  FPosition.X:= FPosition.X + FVecForward.X * Distance;
  FPosition.Y:= FPosition.Y + FVecForward.Y * Distance;
  FPosition.Z:= FPosition.Z + FVecForward.Z * Distance;

  FChanged:= True;
end;

//------------------------------------------------------------------------------
procedure TPHXCamera3D.MoveRight(const Distance: Single);
begin
  FPosition.X:= FPosition.X + FVecRight.X * Distance;
  FPosition.Y:= FPosition.Y + FVecRight.Y * Distance;
  FPosition.Z:= FPosition.Z + FVecRight.Z * Distance;

  FChanged:= True;
end;

//------------------------------------------------------------------------------
procedure TPHXCamera3D.MoveLeft(const Distance: Single);
begin
  FPosition.X:= FPosition.X - FVecRight.X * Distance;
  FPosition.Y:= FPosition.Y - FVecRight.Y * Distance;
  FPosition.Z:= FPosition.Z - FVecRight.Z * Distance;

  FChanged:= True;
end;

//------------------------------------------------------------------------------
procedure TPHXCamera3D.MoveUp(const Distance: Single);
begin
  FPosition.X:= FPosition.X + FVecUp.X * Distance;
  FPosition.Y:= FPosition.Y + FVecUp.Y * Distance;
  FPosition.Z:= FPosition.Z + FVecUp.Z * Distance;

  FChanged:= True;
end;

//------------------------------------------------------------------------------
procedure TPHXCamera3D.MoveDown(const Distance: Single);
begin
  FPosition.X:= FPosition.X - FVecUp.X * Distance;
  FPosition.Y:= FPosition.Y - FVecUp.Y * Distance;
  FPosition.Z:= FPosition.Z - FVecUp.Z * Distance;

  FChanged:= True;
end;


const WORLD_XAXIS: TVector3f = (X: 1.0; Y: 0.0; Z: 0.0);
const WORLD_YAXIS: TVector3f = (X: 0.0; Y: 1.0; Z: 0.0);
const WORLD_ZAXIS: TVector3f = (X: 0.0; Y: 0.0; Z: 1.0);


var RotationMatrix: TMatrix4f;

//------------------------------------------------------------------------------
procedure TPHXCamera3D.RotateX(Distance: Single; WorldAxis: Boolean = False);
begin
  if WorldAxis then
  begin
    RotationMatrix:= Matrix_CreateRotation(Distance, WORLD_XAXIS);
  end else
  begin
    RotationMatrix:= Matrix_CreateRotation(Distance, FVecRight);
  end;

  FVecForward:= Matrix_Transform(RotationMatrix, FVecForward);
  FVecUp     := Matrix_Transform(RotationMatrix, FVecUp);
  FVecRight  := Matrix_Transform(RotationMatrix, FVecRight);

  FChanged:= True;
end;


//------------------------------------------------------------------------------
procedure TPHXCamera3D.RotateY(Distance: Single; WorldAxis: Boolean = True);
begin
  if WorldAxis then
  begin
    RotationMatrix:= Matrix_CreateRotation(Distance, WORLD_YAXIS);
  end else
  begin
    RotationMatrix:= Matrix_CreateRotation(Distance, FVecUp);
  end;

  FVecForward:= Matrix_Transform(RotationMatrix, FVecForward);
  FVecUp     := Matrix_Transform(RotationMatrix, FVecUp);
  FVecRight  := Matrix_Transform(RotationMatrix, FVecRight);

  FChanged:= True;
end;

//------------------------------------------------------------------------------
procedure TPHXCamera3D.RotateZ(Distance: Single; WorldAxis: Boolean = False);
begin
  if WorldAxis then
  begin
    RotationMatrix:= Matrix_CreateRotation(Distance, WORLD_ZAXIS);
  end else
  begin
    RotationMatrix:= Matrix_CreateRotation(Distance, FVecForward);
  end;

  FVecForward:= Matrix_Transform(RotationMatrix, FVecForward);
  FVecUp     := Matrix_Transform(RotationMatrix, FVecUp);
  FVecRight  := Matrix_Transform(RotationMatrix, FVecRight);

  FChanged:= True;
end;


//------------------------------------------------------------------------------
procedure TPHXCamera3D.RotateFlight(Heading, Pitch, Roll: Single);
begin
  RotationMatrix:= Matrix_CreateRotation(Heading, WORLD_YAXIS );

  FVecForward:= Matrix_Transform(RotationMatrix, FVecForward);
  FVecUp     := Matrix_Transform(RotationMatrix, FVecUp);
  FVecRight  := Matrix_Transform(RotationMatrix, FVecRight);

  FChanged:= True;
end;

//------------------------------------------------------------------------------
procedure TPHXCamera3D.SetTarget(Target: TVector3f);
var projectedTarget: TVector3f;
begin
  Target.X:= Target.X - FPosition.X;
  Target.Y:= Target.Y - FPosition.Y;
  Target.Z:= Target.Z - FPosition.Z;
  
  projectedTarget:= Target;
              {
  if( abs(target.X) < 0.00001) and (fabs(target.Z) < 0.00001) then begin // YZ plane
    projectedTarget.X:= 0.0;
    projectedTarget:= normalize(projectedTarget);

    FVecRight:= Vector3f(1.0f, 0.0f, 0.0f);
    FVecUp   := cross(projectedTarget, m_right);

    FVecForward:= target;
    FVecRight  := -cross(m_target, m_up);
  end else begin // XZ plane

    projectedTarget.m_xyzw[1] = 0.0f;
    projectedTarget.normalize();

    m_up = Vector(0.0f, 1.0f, 0.0f);
    m_right = -cross(projectedTarget, m_up);

    m_target = target;
    m_up = cross(m_target, m_right);
        end;

m_target.normalize();
m_right.normalize();
m_up.normalize();  }
end;
 
(*
{
	Vector projectedTarget;

	target = target - m_position;
	projectedTarget = target;

	if(fabs(target.m_xyzw[0]) < 0.00001f && fabs(target.m_xyzw[2]) < 0.00001f) {	// YZ plane

		projectedTarget.m_xyzw[0] = 0.0f;
		projectedTarget.normalize();

		m_right = Vector(1.0f, 0.0f, 0.0f);
		m_up = cross(projectedTarget, m_right);

		m_target = target;
		m_right = -cross(m_target, m_up);
	}

	else {										// XZ plane

		projectedTarget.m_xyzw[1] = 0.0f;
		projectedTarget.normalize();

		m_up = Vector(0.0f, 1.0f, 0.0f);
		m_right = -cross(projectedTarget, m_up);

		m_target = target;
		m_up = cross(m_target, m_right);
	}

	m_target.normalize();
	m_right.normalize();
	m_up.normalize();
}
*)


//------------------------------------------------------------------------------
function TPHXCamera3D.GetProjectionMatrix: TMatrix4f;
begin
  Result:= Matrix_CreatePerspective(45.0, Device.Width/Device.Height, 0.1, 10000.0);
end;

//------------------------------------------------------------------------------
function TPHXCamera3D.GetViewMatrix: TMatrix4f;
begin
  if Changed then
  begin
    BuildViewMatrix;
  end;

  Result:= FView;
end;




end.
