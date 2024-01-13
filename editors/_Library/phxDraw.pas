unit phxDraw;

interface

uses
  SysUtils, Classes, Controls, Messages, Graphics, LCLIntf, LCLType,

  //dglOpenGl,
 // GLU,GL,
  phxOpenGL,

  phxEvents,
  phxDevice,
  phxCanvas,
  phxTypes,
  phxClasses,
  phxGraphics,
  phxMath;

type

TPHXDraw = class;
TPHXDrawRenderer = class;

//------------------------------------------------------------------------------

{ TPHXDrawProvider }

TPHXDrawProvider = class(TPHXProvider)
  private
    FRenderer: TPHXDrawRenderer;
  protected
    function GetName: String; override;
    function GetTarget: TPHXProviderTarget; override;
   // function GetRenderer: String; override;
   // function GetVendor: String; override;
   // function GetVersion: String; override;
  public
    constructor Create(Renderer: TPHXDrawRenderer);

    function CreateRenderer: IPHXDevice; override;
    //function CreateRenderer(Device: TPHXDevice): IPHXDevice; override;

    property Renderer: TPHXDrawRenderer read FRenderer write FRenderer;
  end;

//------------------------------------------------------------------------------

{ TPHXDrawRenderer }

TPHXDrawRenderer = class(TPHXOpenGL_Renderer)
  private
    FOwner: TPHXDraw;
    // Device Context
    FDC    : HDC;
    // Rendering Context
    FRC    : Pointer;//HGLRC;
  public
    constructor Create(Owner: TPHXDraw);
    destructor Destroy; override;

    // Initialize the renderer
    //Procedure Initialize(const Title: String; const Mode: TPHXDisplayMode); override;
    procedure Initialize(const Parameters: TPHXDeviceParameters);  override;
    // Finalize the renderer, clears all memory
    procedure Finalize; override;
    // Reinitializes the renderer using a new display mode

    procedure Reinitialize(const Parameters: TPHXDeviceParameters); override;
    // Change the window title
  //  procedure SetWindowTitle(const Title: String); override;
    // Change the window size
  //  procedure SetWindowSize(const Size: TSizei); override;
    // Set the window flags
    //procedure SetWindowFlags(const Flags: TPHXWindowFlags); override;
    //procedure SetWindowIcon(const IconFile: string); override;

    // Get the current window size
   // function GetWindowSize: TSizei; override;

    // Update the device, call once per frame
    procedure Update; override;
    // Clear the back buffers
    //procedure Clear(const Color: TColor4f); override;
  //  // Flip the front and back buffers
    procedure Flip; override;


    property Owner: TPHXDraw read FOwner;
    // Rendering context.
    property RC: Pointer{HGLRC} read FRC;
    // Device context.
    property DC: HDC read FDC;
  end;

//------------------------------------------------------------------------------
TPHXDraw = class(TCustomControl)
  private

    FDevice  : TPHXDevice;
    FProvider: TPHXDrawProvider;
    FRenderer: TPHXDrawRenderer;
    FGeneratePhoenixEvents: Boolean;
    FOnRender: TNotifyEvent;

    function GetOpenGLAvailable: Boolean;
    function GetDC: HDC;
  //  function GetRC: HGLRC;             // Device Context
  protected
    Initialized: Boolean;
    procedure Paint; override;
    procedure Loaded; override;
    procedure Resize; override;


    procedure WMEraseBkgnd(var Message: TWMEraseBkgnd); Message WM_ERASEBKGND;
    procedure WMPaint(var Message: TWMPaint); Message WM_PAINT;
    procedure WMGetDlgCode(var Message: TWMGetDlgCode); message WM_GETDLGCODE;
    procedure Click; override;

    property Renderer: TPHXDrawRenderer read FRenderer;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    procedure SetBounds(ALeft: Integer; ATop: Integer; AWidth: Integer; AHeight: Integer); override;

    procedure MakeCurrent;

    procedure Render;

    property IsOpenGLAvailable: Boolean read GetOpenGLAvailable;

    // Rendering context.
   // property RC: HGLRC read GetRC;
    // Device context.
    property DC: HDC read GetDC;

    property Device: TPHXDevice read FDevice;
  published
    property OnRender: TNotifyEvent read FOnRender write FOnRender;

    property OnDblClick;

    property OnResize;
    // If the draw should generate phoenix events
    property GeneratePhoenixEvents: Boolean read FGeneratePhoenixEvents write FGeneratePhoenixEvents;

    property OnDragOver;
    property OnDragDrop;

    property Popupmenu;
    property Anchors;
    property Align;
    property Constraints;
    property Cursor;
    property Enabled;
    property Visible;
    property OnClick;
    property OnMouseWheel;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
    property OnKeyDown;
    property OnKeyPress;
    property OnKeyUp;
  end;

//------------------------------------------------------------------------------
TPHXDrawEvents = class
  private
  public
    class procedure MouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    class procedure MouseMove(Sender: TObject; Shift: TShiftState; X,  Y: Integer);
    class procedure MouseUp(Sender: TObject; Button: TMouseButton;  Shift: TShiftState; X, Y: Integer);

    class procedure KeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    class procedure KeyPress(Sender: TObject; var Key: Char);
    class procedure KeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);

    class procedure Resize(Sender: TObject);
  end;
function ConvertVirtualKey(Const Key: Word): Integer;

procedure Register;

implementation


//------------------------------------------------------------------------------
function ConvertShiftState(Shift: TShiftState): TPHXShiftStates;
begin
  Result:= [];
  if Classes.ssCtrl in Shift then
  begin
    Result:= Result + [phxEvents.ssCtrl];
  end;
  if Classes.ssAlt in Shift then
  begin
    Result:= Result + [phxEvents.ssAlt];
  end;
  if Classes.ssShift in Shift then
  begin
    Result:= Result + [phxEvents.ssShift];
  end;
end;

(*
  VK_ESC          = (VK_SPECIAL+1);
  VK_F1           = (VK_SPECIAL+2);
  VK_F2           = (VK_SPECIAL+3);
  VK_F3           = (VK_SPECIAL+4);
  VK_F4           = (VK_SPECIAL+5);
  VK_F5           = (VK_SPECIAL+6);
  VK_F6           = (VK_SPECIAL+7);
  VK_F7           = (VK_SPECIAL+8);
  VK_F8           = (VK_SPECIAL+9);
  VK_F9           = (VK_SPECIAL+10);
  VK_F10          = (VK_SPECIAL+11);
  VK_F11          = (VK_SPECIAL+12);
  VK_F12          = (VK_SPECIAL+13);
  VK_F13          = (VK_SPECIAL+14);
  VK_F14          = (VK_SPECIAL+15);
  VK_F15          = (VK_SPECIAL+16);
  VK_F16          = (VK_SPECIAL+17);
  VK_F17          = (VK_SPECIAL+18);
  VK_F18          = (VK_SPECIAL+19);
  VK_F19          = (VK_SPECIAL+20);
  VK_F20          = (VK_SPECIAL+21);
  VK_F21          = (VK_SPECIAL+22);
  VK_F22          = (VK_SPECIAL+23);
  VK_F23          = (VK_SPECIAL+24);
  VK_F24          = (VK_SPECIAL+25);
  VK_F25          = (VK_SPECIAL+26);
  VK_UP           = (VK_SPECIAL+27);
  VK_DOWN         = (VK_SPECIAL+28);
  VK_LEFT         = (VK_SPECIAL+29);
  VK_RIGHT        = (VK_SPECIAL+30);
  VK_LSHIFT       = (VK_SPECIAL+31);
  VK_RSHIFT       = (VK_SPECIAL+32);
  VK_LCTRL        = (VK_SPECIAL+33);
  VK_RCTRL        = (VK_SPECIAL+34);
  VK_LALT         = (VK_SPECIAL+35);
  VK_RALT         = (VK_SPECIAL+36);
  VK_TAB          = (VK_SPECIAL+37);
  VK_ENTER        = (VK_SPECIAL+38);
  VK_RETURN       = (VK_SPECIAL+38);
  VK_BACKSPACE    = (VK_SPECIAL+39);
  VK_INSERT       = (VK_SPECIAL+40);
  VK_DEL          = (VK_SPECIAL+41);
  VK_PAGEUP       = (VK_SPECIAL+42);
  VK_PAGEDOWN     = (VK_SPECIAL+43);
  VK_HOME         = (VK_SPECIAL+44);
  VK_END          = (VK_SPECIAL+45);
  VK_NUM_0        = (VK_SPECIAL+46);
  VK_NUM_1        = (VK_SPECIAL+47);
  VK_NUM_2        = (VK_SPECIAL+48);
  VK_NUM_3        = (VK_SPECIAL+49);
  VK_NUM_4        = (VK_SPECIAL+50);
  VK_NUM_5        = (VK_SPECIAL+51);
  VK_NUM_6        = (VK_SPECIAL+52);
  VK_NUM_7        = (VK_SPECIAL+53);
  VK_NUM_8        = (VK_SPECIAL+54);
  VK_NUM_9        = (VK_SPECIAL+55);
  VK_NUM_DIVIDE   = (VK_SPECIAL+56);
  VK_NUM_MULTIPLY = (VK_SPECIAL+57);
  VK_NUM_SUBTRACT = (VK_SPECIAL+58);
  VK_NUM_ADD      = (VK_SPECIAL+59);
  VK_NUM_DECIMAL  = (VK_SPECIAL+60);
  VK_NUM_EQUAL    = (VK_SPECIAL+61);
  VK_NUM_ENTER    = (VK_SPECIAL+62);
  *)
//------------------------------------------------------------------------------
function ConvertVirtualKey(Const Key: Word): Integer;
begin
  Result:= Key;
  case Key of
    Windows.VK_ESCAPE        : Result:= phxEvents.VK_ESC          ;
    Windows.VK_F1            : Result:= phxEvents.VK_F1           ;
    Windows.VK_F2            : Result:= phxEvents.VK_F2           ;
    Windows.VK_F3            : Result:= phxEvents.VK_F3           ;
    Windows.VK_F4            : Result:= phxEvents.VK_F4           ;
    Windows.VK_F5            : Result:= phxEvents.VK_F5           ;
    Windows.VK_F6            : Result:= phxEvents.VK_F6           ;
    Windows.VK_F7            : Result:= phxEvents.VK_F7           ;
    Windows.VK_F8            : Result:= phxEvents.VK_F8           ;
    Windows.VK_F9            : Result:= phxEvents.VK_F9           ;
    Windows.VK_F10           : Result:= phxEvents.VK_F10          ;
    Windows.VK_F11           : Result:= phxEvents.VK_F11          ;
    Windows.VK_F12           : Result:= phxEvents.VK_F12          ;
    Windows.VK_F13           : Result:= phxEvents.VK_F13          ;
    Windows.VK_F14           : Result:= phxEvents.VK_F14          ;
    Windows.VK_F15           : Result:= phxEvents.VK_F15          ;
    Windows.VK_F16           : Result:= phxEvents.VK_F16          ;
    Windows.VK_F17           : Result:= phxEvents.VK_F17          ;
    Windows.VK_F18           : Result:= phxEvents.VK_F18          ;
    Windows.VK_F19           : Result:= phxEvents.VK_F19          ;
    Windows.VK_F20           : Result:= phxEvents.VK_F20          ;
    Windows.VK_F21           : Result:= phxEvents.VK_F21          ;
    Windows.VK_F22           : Result:= phxEvents.VK_F22          ;
    Windows.VK_F23           : Result:= phxEvents.VK_F23          ;
    Windows.VK_F24           : Result:= phxEvents.VK_F24          ;
//    Windows.VK_F25           : Result:= phxEvents.VK_F25          ;
    Windows.VK_UP            : Result:= phxEvents.VK_UP           ;
    Windows.VK_DOWN          : Result:= phxEvents.VK_DOWN         ;
    Windows.VK_LEFT          : Result:= phxEvents.VK_LEFT         ;
    Windows.VK_RIGHT         : Result:= phxEvents.VK_RIGHT        ;
    Windows.VK_LSHIFT        : Result:= phxEvents.VK_LSHIFT       ;
    Windows.VK_RSHIFT        : Result:= phxEvents.VK_RSHIFT       ;
    Windows.VK_LCONTROL      : Result:= phxEvents.VK_LCTRL        ;
    Windows.VK_RCONTROL      : Result:= phxEvents.VK_RCTRL        ;
//    Windows.VK_LALT          : Result:= phxEvents.VK_LALT         ;
//    Windows.VK_RALT          : Result:= phxEvents.VK_RALT         ;
    Windows.VK_TAB           : Result:= phxEvents.VK_TAB          ;
    Windows.VK_RETURN        : Result:= phxEvents.VK_RETURN       ;
    Windows.VK_BACK          : Result:= phxEvents.VK_BACKSPACE    ;
    Windows.VK_INSERT        : Result:= phxEvents.VK_INSERT       ;
    Windows.VK_DELETE        : Result:= phxEvents.VK_DEL          ;
   // Windows.VK_PAGEUP        : Result:= phxEvents.VK_PAGEUP       ;
  //  Windows.VK_PAGEDOWN      : Result:= phxEvents.VK_PAGEDOWN     ;
    Windows.VK_HOME          : Result:= phxEvents.VK_HOME         ;
    Windows.VK_END           : Result:= phxEvents.VK_END          ;
    Windows.VK_NUMPAD0         : Result:= phxEvents.VK_NUM_0        ;
    Windows.VK_NUMPAD1         : Result:= phxEvents.VK_NUM_1        ;
    Windows.VK_NUMPAD2         : Result:= phxEvents.VK_NUM_2        ;
    Windows.VK_NUMPAD3         : Result:= phxEvents.VK_NUM_3        ;
    Windows.VK_NUMPAD4         : Result:= phxEvents.VK_NUM_4        ;
    Windows.VK_NUMPAD5         : Result:= phxEvents.VK_NUM_5        ;
    Windows.VK_NUMPAD6         : Result:= phxEvents.VK_NUM_6        ;
    Windows.VK_NUMPAD7         : Result:= phxEvents.VK_NUM_7        ;
    Windows.VK_NUMPAD8         : Result:= phxEvents.VK_NUM_8        ;
    Windows.VK_NUMPAD9         : Result:= phxEvents.VK_NUM_9        ;
    Windows.VK_ADD             : Result:= phxEvents.VK_NUM_ADD   ;
    Windows.VK_MULTIPLY         : Result:= phxEvents.VK_NUM_MULTIPLY ;
    Windows.VK_SUBTRACT         : Result:= phxEvents.VK_NUM_SUBTRACT ;
    Windows.VK_DIVIDE         : Result:= phxEvents.VK_NUM_DIVIDE  ;
//    Windows.VK_ret     : Result:= phxEvents.VK_NUM_EQUAL    ;
//    Windows.VK_NUMPAD_ENTER     : Result:= phxEvents.VK_NUM_ENTER    ;
 end;
end;

//------------------------------------------------------------------------------
class procedure TPHXDrawEvents.MouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var Event: TPHXEvent;
begin
  Event.Mouse.Event:= PHX_MOUSE_PRESSED;
  Event.Mouse.X:= X;
  Event.Mouse.Y:= Y;
  Event.Mouse.Shift:= ConvertShiftState(Shift);
  Event.Mouse.Button:= Integer(Button);

  Events.Push(Event);
end;

//------------------------------------------------------------------------------
class procedure TPHXDrawEvents.MouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
var Event: TPHXEvent;
begin
  Event.Mouse.Event:= PHX_MOUSE_MOVED;
  Event.Mouse.X:= X;
  Event.Mouse.Y:= Y;
  Event.Mouse.Shift:= ConvertShiftState(Shift);
  Event.Mouse.Button:= 0;

  Events.Push(Event);
end;

//------------------------------------------------------------------------------
class procedure TPHXDrawEvents.MouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var Event: TPHXEvent;
begin
  Event.Mouse.Event:= PHX_MOUSE_RELEASED;
  Event.Mouse.X:= X;
  Event.Mouse.Y:= Y;
  Event.Mouse.Shift:= ConvertShiftState(Shift);
  Event.Mouse.Button:= Integer(Button);

  Events.Push(Event);
end;

//------------------------------------------------------------------------------
class procedure TPHXDrawEvents.KeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
var Event: TPHXEvent;
begin
  Event.Keyboard.Event:= PHX_KEY_PRESSED;
  Event.Keyboard.Key  := ConvertVirtualKey(Key);
  Event.Keyboard.Shift:= ConvertShiftState(Shift);

  Events.Push(Event);
end;

//------------------------------------------------------------------------------
class procedure TPHXDrawEvents.KeyPress(Sender: TObject; var Key: Char);
var Event: TPHXEvent;
begin
  Event.Keyboard.Event:= PHX_KEY_TEXT;
  Event.Keyboard.Key  := Ord(Key);
  Event.Keyboard.Shift:= [];

  Events.Push(Event);
end;

//------------------------------------------------------------------------------
class procedure TPHXDrawEvents.KeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
var Event: TPHXEvent;
begin
  Event.Keyboard.Event:= PHX_KEY_RELEASED;
  Event.Keyboard.Key  := ConvertVirtualKey(Key);
  Event.Keyboard.Shift:= ConvertShiftState(Shift);

  Events.Push(Event);
end;

//------------------------------------------------------------------------------
class procedure TPHXDrawEvents.Resize(Sender: TObject);
var Event: TPHXEvent;
begin
  Event.Device.Event:= PHX_DEVICE_RESIZED;
 // Event.Device.Width:= ;

  if Sender is TControl then
  begin
    Event.Device.Width := TControl(Sender).Width;
    Event.Device.Height:= TControl(Sender).Height;
  end else
  begin
    Event.Device.Width := TControl(Sender).Width;
    Event.Device.Height:= TControl(Sender).Height;
  end;

  Events.Push(Event);
end;




procedure Register;
begin
  RegisterComponents('Phoenix', [TPHXDraw]);
end;

{$REGION 'TPHXDrawProvider'}

//  TPHXDrawProvider
//==============================================================================
constructor TPHXDrawProvider.Create(Renderer: TPHXDrawRenderer);
begin
  FRenderer:= Renderer;
end;

function TPHXDrawProvider.CreateRenderer: IPHXDevice;
begin
 Result:= FRenderer;
end;

//------------------------------------------------------------------------------
function TPHXDrawProvider.CreateRenderer(Device: TPHXDevice): IPHXDevice;
begin
  Result:= FRenderer;
end;

//------------------------------------------------------------------------------
function TPHXDrawProvider.GetName: String;
begin
  Result:= 'TPHXDraw';
end;

function TPHXDrawProvider.GetTarget: TPHXProviderTarget;
begin
  Result:=inherited GetTarget;
end;

//------------------------------------------------------------------------------
function TPHXDrawProvider.GetRenderer: String;
begin

end;

//------------------------------------------------------------------------------
function TPHXDrawProvider.GetVendor: String;
begin

end;

//------------------------------------------------------------------------------
function TPHXDrawProvider.GetVersion: String;
begin

end;
{$ENDREGION}

{$REGION 'TPHXDrawRenderer'}

// TPHXDrawRenderer
//==============================================================================
constructor TPHXDrawRenderer.Create(Owner: TPHXDraw);
begin
  FOwner:= Owner;

  MatProjection:= Matrix_Identity;
  MatWorld     := Matrix_Identity;;
  MatView      := Matrix_Identity;;
end;

//-----------------------------------------------------------------------------
destructor TPHXDrawRenderer.Destroy;
begin
  inherited;
end;

//------------------------------------------------------------------------------
procedure TPHXDrawRenderer.Initialize(const Parameters: TPHXDeviceParameters);
begin
  if RC <> 0 then Finalize;

  // Get the device context
  FDC := GetDC(Owner.Handle);

  if FDC = 0 then raise Exception.Create('Could not get device context!');

//  FRenderer.DeviceContext:= FDC;

  // Create the rendering context
  FRC := CreateRenderingContext(FDC, [opDoubleBuffered], 32, 16, 16, 0, 0,0);

  if FRC = 0 then raise Exception.Create('Could not create rendering context!');
  // Activate the rendering context
  ActivateRenderingContext(DC, RC);

//  Device.Init('TPHXDraw', 100, 100);

  Owner.SetBounds(Owner.Left, Owner.Top, Owner.Width, Owner.Height);

//  if Assigned(FOnInit) then FOnInit(Self);
end;

//-----------------------------------------------------------------------------
procedure TPHXDrawRenderer.Finalize;
begin
  wglMakeCurrent(0,0);

  if FRC <> 0 then DestroyRenderingContext(FRC);

  FRC:=0;
end;

//-----------------------------------------------------------------------------
procedure TPHXDrawRenderer.Reinitialize(const Mode: TPHXDisplayMode);
begin
//  TPHXEventFactory.DeviceReset(Mode.Width, Mode.Height) ;
end;

procedure TPHXDrawRenderer.Reinitialize(const Parameters: TPHXDeviceParameters);
begin
  //
end;


//------------------------------------------------------------------------------
{function TPHXDrawRenderer.GetWindowSize: TSizei;
begin
  Result.Width  := FOwner.Width;
  Result.Height := FOwner.Height;
end;  }

//-----------------------------------------------------------------------------
procedure TPHXDrawRenderer.Update;
begin
end;

//-----------------------------------------------------------------------------
procedure TPHXDrawRenderer.Clear(const Color: TColor4f);
begin
  if not Assigned(glClear) then Exit;

  glClearColor(Color.Red, Color.Green, Color.Blue, Color.Alpha);

  glClear( GL_COLOR_BUFFER_BIT or GL_DEPTH_BUFFER_BIT);
end;

//-----------------------------------------------------------------------------
procedure TPHXDrawRenderer.Flip;
begin
  glFlush();
  // Display the scene
  SwapBuffers(DC);
end;



{$ENDREGION}


{$REGION 'TPHXDraw'}

//==============================================================================

constructor TPHXDraw.Create(AOwner: TComponent);
begin
  inherited;
  FRenderer:= TPHXDrawRenderer.Create(Self);
  FProvider:= TPHXDrawProvider.Create(FRenderer);

  FDevice:= TPHXDevice.Create(FProvider);

  Initialized:= False;
end;

//------------------------------------------------------------------------------
destructor TPHXDraw.Destroy;
begin

  inherited;
end;

//------------------------------------------------------------------------------
procedure TPHXDraw.Loaded;
begin
  inherited Loaded;

  IF (ComponentState = []) then
  begin
    Initialized:= True;

    Device.Init('', Width, Height);
  end;

end;

//------------------------------------------------------------------------------
procedure TPHXDraw.MakeCurrent;
begin
  ActivateRenderingContext(DC, RC);

  SetBounds(Left, Top, Width, Height);
end;

//------------------------------------------------------------------------------
procedure TPHXDraw.Render;
begin
  if Assigned(OnRender) then OnRender(Self);
end;

//------------------------------------------------------------------------------
procedure TPHXDraw.Resize;
var Event: TPHXEvent;
begin

  if GeneratePhoenixEvents then
  begin
    Event.Event:= PHX_DEVICE_RESIZED;
    Event.Device.Width:= Width;
    Event.Device.Height:= Height;

    phxEvents.Events.Push(Event);
  end;

  IF (ComponentState = []) then
  begin
  //  inherited Resize;
//    if Initialized and Assigned(OnResize) then OnResize(Self);
  end;
end;

//------------------------------------------------------------------------------
procedure TPHXDraw.Click;
begin
  inherited;
  SetFocus;
end;

// WMEraseBkgnd
//------------------------------------------------------------------------------
procedure TPHXDraw.WMEraseBkgnd(var Message: TWMEraseBkgnd);
begin
   if GetOpenGLAvailable then
   begin
     Message.Result:=1
   end else
   begin
     inherited;
   end;
end;

// WMPaint
//------------------------------------------------------------------------------
procedure TPHXDraw.WMPaint(var Message: TWMPaint);
var PS : TPaintStruct;
begin
  BeginPaint(Handle, PS);
  try
    if GetOpenGLAvailable and (Width>0) and (Height>0) then
    begin
      Render;
    end else
    begin
      Paint;
    end;
     //    FBuffer.Render;
 finally
   EndPaint(Handle, PS);

   Message.Result:=0;
 end;
end;


//------------------------------------------------------------------------------
procedure TPHXDraw.WMGetDlgCode(var Message: TWMGetDlgCode);
begin
  inherited;
  Message.Result := Message.Result or DLGC_WANTARROWS or DLGC_WANTALLKEYS or DLGC_WANTCHARS or DLGC_WANTTAB;
//DLGC_WANTARROWS
//eturn DLGC_WANTARROWS|DLGC_WANTALLKEYS|DLGC_WANTCHARS;

end;

// Paint
//------------------------------------------------------------------------------
Procedure TPHXDraw.Paint;
var S   : String;
var w, h: Integer;
var Canvas: TCanvas;
begin
 // inherited Paint;

  Canvas:= inherited Canvas;

  If (csDesigning in ComponentState) then
  begin
    Canvas.Brush.Style := Graphics.bsSolid;
    Canvas.Brush.Color := inherited Color;
    Canvas.Pen.Color   := clBlack;
    Canvas.Pen.Style   := Graphics.psDash;
    Canvas.Rectangle(0, 0, Width, Height);

    Canvas.Pen.Style := Graphics.psSolid;
    Canvas.Pen.Color := clGray;
    Canvas.MoveTo(0, 0);
    Canvas.LineTo(Width, Height);

    Canvas.MoveTo(0, Height);
    Canvas.LineTo(Width, 0);

    s := Format('(%s)', [ClassName]);

    w := Canvas.TextWidth(s);
    h := Canvas.TextHeight(s);

    Canvas.Brush.Style := Graphics.bsSolid;
    Canvas.Brush.Color := clBtnFace;
    Canvas.TextOut(Width div 2-w div 2, Height div 2-h div 2, s);
  end else
  begin
    if (ComponentState = []) and Assigned(FOnRender) then FOnRender(Self);
  end;
end;


//------------------------------------------------------------------------------
procedure TPHXDraw.SetBounds(ALeft, ATop, AWidth, AHeight: Integer);
var Matrix: TMatrix4f;
begin
  if GetOpenGLAvailable then
  begin
    Matrix:= Matrix_LoadOrthographic(0, AWidth, AHeight, 0, -1000, 1000);

    Device.SetProjectionMatrix(Matrix );

    Device.SetViewport(0,0, AWidth, AHeight);
  end;
  inherited;
end;

//------------------------------------------------------------------------------
function TPHXDraw.GetOpenGLAvailable: Boolean;
begin
  Result:= dglOpenGl.GL_LibHandle <> nil;
end;

//------------------------------------------------------------------------------
function TPHXDraw.GetDC: HDC;
begin
  Result:= Renderer.DC;
end;

//------------------------------------------------------------------------------
function TPHXDraw.GetRC: HGLRC;
begin
  Result:= Renderer.RC;
end;

{$ENDREGION}

end.
