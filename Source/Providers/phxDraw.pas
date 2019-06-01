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
unit phxDraw;
//< OpenGL provider using a VCL control

interface

{$I ../phxConfig.inc}

uses
  SysUtils, Classes,

  {$IFDEF FPC}
  GL,
  {$ELSE}
  dglOpenGL,
  {$ENDIF}

  Windows, Messages,

  Vcl.Controls, Vcl.Graphics, Vcl.ExtCtrls,

  phxTypes,
  phxClasses,
  phxEvents,
  phxDevice,
  phxGraphics,

  phxOpenGL;

type

{$R phxIcons.dcr}

TPHXDevice = phxDevice.TPHXDevice;

TPHXDraw = class;

// Renderer for the draw
//-----------------------------------------------------------------------------
TPHXDrawRenderer = class(TPHXOpenGL_Renderer)
  private
    FOwner: TPHXDraw;
    // Device Context
    FDC    : HDC;
    // Rendering Context
    FRC    : HGLRC;
  protected
    // Get the current window width
    function GetWidth: Integer; override;
    // Get the current window height
    function GetHeight: Integer; override;
    // Get the window flags
    function GetFlags: TPHXWindowFlags; override;

    // Set the window title
    procedure SetTitle(const Title: String); override;
    // Set the window flags
    procedure SetFlags(const Flags: TPHXWindowFlags); override;
    // Load a new window icon
    procedure SetIcon(const Icon: String); override;
  public
    constructor Create(AOwner: TPHXDraw);
    destructor Destroy; override;

    // Enumerate all supported display modes
    procedure EnumDisplayModes(const Modes: TPHXDisplayModes); override;

    // Initialize the renderer
    procedure Initialize(const Parameters: TPHXDeviceParameters); override;
    // Reinitializes the renderer using a new display mode
    procedure Reinitialize(const Parameters: TPHXDeviceParameters); override;
    // Finalize the renderer
    procedure Finalize; override;

    // Update the device, call once per frame
    procedure Update; override;
    // Clear the back buffers
    procedure Clear; override;
    // Flip the front and back buffers
    procedure Flip; override;

    // Owning TPHXDraw
    property Owner: TPHXDraw read FOwner;
    // Rendering context.
    property RC: HGLRC read FRC;
    // Device context.
    property DC: HDC read FDC;
  end;

TPHXDeviceEvent = procedure(Sender: TObject; Device: TPHXDevice) of object;
TPHXResizeEvent = procedure(Sender: TObject; Device: TPHXDevice; const Width: Integer; const Height: Integer) of object;

// The draw is a visual component that acts as a OpenGL provider
//------------------------------------------------------------------------------
TPHXDraw = class(TCustomControl)
  private
    FDevice  : TPHXDevice;
    FRenderer: TPHXDrawRenderer;
    FInterval: Cardinal;
    FTimer   : TTimer;

    FOnInit    : TPHXDeviceEvent;
    FOnFinalize: TPHXDeviceEvent;
    FOnUpdate  : TPHXDeviceEvent;
    FOnRender  : TPHXDeviceEvent;
    FOnResize  : TPHXResizeEvent;

    FGenerateEvents: Boolean;

    procedure PaintDesign;


    procedure TimerEvent(Sender: TObject);

    function GetDC: HDC;
    function GetRC: HGLRC;
  protected
    Initialized: Boolean;

    procedure Paint; override;
    procedure Loaded; override;
    procedure Resize; override;
    procedure Click; override;

    procedure WMEraseBkgnd(var Message: TWMEraseBkgnd); Message WM_ERASEBKGND;
    procedure WMPaint(var Message: TWMPaint); Message WM_PAINT;
    procedure WMGetDlgCode(var Message: TWMGetDlgCode); message WM_GETDLGCODE;

    procedure SetInterval(const Value: Cardinal);
    procedure SetEnabled(Value: Boolean); override;

    procedure MouseMove(Shift: TShiftState; X: Integer; Y: Integer); override;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X: Integer; Y: Integer); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X: Integer; Y: Integer); override;

    procedure KeyDown(var Key: Word; Shift: TShiftState); override;
    procedure KeyPress(var Key: Char); override;
    procedure KeyUp(var Key: Word; Shift: TShiftState); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    procedure Initialize;

    procedure SetBounds(ALeft: Integer; ATop: Integer; AWidth: Integer; AHeight: Integer); override;

    // Makes this specified OpenGL rendering context the calling thread's current rendering context
    procedure MakeCurrent;

    // Checks if OpenGL is initialized
    function IsOpenGLAvailable: Boolean;

    // Rendering context.
    property RC: HGLRC read GetRC;
    // Device context.
    property DC: HDC read GetDC;
    // The renderer for the device
    property Renderer: TPHXDrawRenderer read FRenderer;
    // The current device
    property Device: TPHXDevice read FDevice;
  published
    // Generate phoenix events?
    property GenerateEvents: Boolean read FGenerateEvents write FGenerateEvents default False;

    // The interval in ms to call the update and render events
    property Interval: Cardinal read FInterval write SetInterval default 16;
    // Intitialization event
    property OnInit: TPHXDeviceEvent read FOnInit write FOnInit;
    // Finalization event
    property OnFinalize: TPHXDeviceEvent read FOnFinalize write FOnFinalize;

    // Update event
    property OnUpdate: TPHXDeviceEvent read FOnUpdate write FOnUpdate;
    // Render event
    property OnRender: TPHXDeviceEvent read FOnRender write FOnRender;
    // Render event
    property OnResize: TPHXResizeEvent read FOnResize write FOnResize;

    property Popupmenu;
    property Anchors;
    property Align;
    property Cursor;
    property Enabled;
    property Visible;

    property OnClick;
    property OnDblClick;
    property OnMouseWheel;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
    property OnKeyDown;
    property OnKeyPress;
    property OnKeyUp;

//    property OnResize;
    property OnDragOver;
    property OnDragDrop;

  end;

procedure Register;

implementation

//------------------------------------------------------------------------------
procedure Register;
begin
  RegisterComponents('Phoenix', [TPHXDraw]);
end;


//------------------------------------------------------------------------------
function ConvertShiftState(Shift: TShiftState): TPHXShiftStates;
begin
  Result:= [];
  if Classes.ssCtrl in Shift then
  begin
    Result:= Result + [phxTypes.ssCtrl];
  end;
  if Classes.ssAlt in Shift then
  begin
    Result:= Result + [phxTypes.ssAlt];
  end;
  if Classes.ssShift in Shift then
  begin
    Result:= Result + [phxTypes.ssShift];
  end;
end;

//------------------------------------------------------------------------------
function ConvertVirtualKey(Const Key: Word): TPHXVirtualKey;
begin
  Result:= TPHXVirtualKey(Key);
  case Key of
    Windows.VK_ESCAPE        : Result:= TPHXVirtualKey.VK_ESC          ;
    Windows.VK_F1            : Result:= TPHXVirtualKey.VK_F1           ;
    Windows.VK_F2            : Result:= TPHXVirtualKey.VK_F2           ;
    Windows.VK_F3            : Result:= TPHXVirtualKey.VK_F3           ;
    Windows.VK_F4            : Result:= TPHXVirtualKey.VK_F4           ;
    Windows.VK_F5            : Result:= TPHXVirtualKey.VK_F5           ;
    Windows.VK_F6            : Result:= TPHXVirtualKey.VK_F6           ;
    Windows.VK_F7            : Result:= TPHXVirtualKey.VK_F7           ;
    Windows.VK_F8            : Result:= TPHXVirtualKey.VK_F8           ;
    Windows.VK_F9            : Result:= TPHXVirtualKey.VK_F9           ;
    Windows.VK_F10           : Result:= TPHXVirtualKey.VK_F10          ;
    Windows.VK_F11           : Result:= TPHXVirtualKey.VK_F11          ;
    Windows.VK_F12           : Result:= TPHXVirtualKey.VK_F12          ;
    Windows.VK_F13           : Result:= TPHXVirtualKey.VK_F13          ;
    Windows.VK_F14           : Result:= TPHXVirtualKey.VK_F14          ;
    Windows.VK_F15           : Result:= TPHXVirtualKey.VK_F15          ;
    Windows.VK_F16           : Result:= TPHXVirtualKey.VK_F16          ;
    Windows.VK_F17           : Result:= TPHXVirtualKey.VK_F17          ;
    Windows.VK_F18           : Result:= TPHXVirtualKey.VK_F18          ;
    Windows.VK_F19           : Result:= TPHXVirtualKey.VK_F19          ;
    Windows.VK_F20           : Result:= TPHXVirtualKey.VK_F20          ;
    Windows.VK_F21           : Result:= TPHXVirtualKey.VK_F21          ;
    Windows.VK_F22           : Result:= TPHXVirtualKey.VK_F22          ;
    Windows.VK_F23           : Result:= TPHXVirtualKey.VK_F23          ;
    Windows.VK_F24           : Result:= TPHXVirtualKey.VK_F24          ;
//    Windows.VK_F25           : Result:= TPHXVirtualKey.VK_F25          ;
    Windows.VK_UP            : Result:= TPHXVirtualKey.VK_UP           ;
    Windows.VK_DOWN          : Result:= TPHXVirtualKey.VK_DOWN         ;
    Windows.VK_LEFT          : Result:= TPHXVirtualKey.VK_LEFT         ;
    Windows.VK_RIGHT         : Result:= TPHXVirtualKey.VK_RIGHT        ;
    Windows.VK_LSHIFT        : Result:= TPHXVirtualKey.VK_LSHIFT       ;
    Windows.VK_RSHIFT        : Result:= TPHXVirtualKey.VK_RSHIFT       ;
    Windows.VK_LCONTROL      : Result:= TPHXVirtualKey.VK_LCTRL        ;
    Windows.VK_RCONTROL      : Result:= TPHXVirtualKey.VK_RCTRL        ;
//    Windows.VK_LALT          : Result:= TPHXVirtualKey.VK_LALT         ;
//    Windows.VK_RALT          : Result:= TPHXVirtualKey.VK_RALT         ;
    Windows.VK_TAB           : Result:= TPHXVirtualKey.VK_TAB          ;
    Windows.VK_RETURN        : Result:= TPHXVirtualKey.VK_RETURN       ;
    Windows.VK_BACK          : Result:= TPHXVirtualKey.VK_BACKSPACE    ;
    Windows.VK_INSERT        : Result:= TPHXVirtualKey.VK_INSERT       ;
    Windows.VK_DELETE        : Result:= TPHXVirtualKey.VK_DEL          ;
   // Windows.VK_PAGEUP        : Result:= TPHXVirtualKey.VK_PAGEUP       ;
  //  Windows.VK_PAGEDOWN      : Result:= TPHXVirtualKey.VK_PAGEDOWN     ;
    Windows.VK_HOME          : Result:= TPHXVirtualKey.VK_HOME         ;
    Windows.VK_END           : Result:= TPHXVirtualKey.VK_END          ;
    Windows.VK_NUMPAD0         : Result:= TPHXVirtualKey.VK_NUM_0        ;
    Windows.VK_NUMPAD1         : Result:= TPHXVirtualKey.VK_NUM_1        ;
    Windows.VK_NUMPAD2         : Result:= TPHXVirtualKey.VK_NUM_2        ;
    Windows.VK_NUMPAD3         : Result:= TPHXVirtualKey.VK_NUM_3        ;
    Windows.VK_NUMPAD4         : Result:= TPHXVirtualKey.VK_NUM_4        ;
    Windows.VK_NUMPAD5         : Result:= TPHXVirtualKey.VK_NUM_5        ;
    Windows.VK_NUMPAD6         : Result:= TPHXVirtualKey.VK_NUM_6        ;
    Windows.VK_NUMPAD7         : Result:= TPHXVirtualKey.VK_NUM_7        ;
    Windows.VK_NUMPAD8         : Result:= TPHXVirtualKey.VK_NUM_8        ;
    Windows.VK_NUMPAD9         : Result:= TPHXVirtualKey.VK_NUM_9        ;
    Windows.VK_ADD             : Result:= TPHXVirtualKey.VK_NUM_ADD   ;
    Windows.VK_MULTIPLY         : Result:= TPHXVirtualKey.VK_NUM_MULTIPLY ;
    Windows.VK_SUBTRACT         : Result:= TPHXVirtualKey.VK_NUM_SUBTRACT ;
    Windows.VK_DIVIDE         : Result:= TPHXVirtualKey.VK_NUM_DIVIDE  ;
//    Windows.VK_ret     : Result:= phxEvents.VK_NUM_EQUAL    ;
//    Windows.VK_NUMPAD_ENTER     : Result:= phxEvents.VK_NUM_ENTER    ;
 end;
end;



{$REGION 'TPHXDrawRenderer'}

// TPHXDrawRenderer
//==============================================================================
constructor TPHXDrawRenderer.Create(AOwner: TPHXDraw);
begin
  inherited Create;

  FOwner:= AOwner;
  //MatProjection:= Matrix_Identity;
  //MatWorld     := Matrix_Identity;;
  //MatView      := Matrix_Identity;;
end;

//-----------------------------------------------------------------------------
destructor TPHXDrawRenderer.Destroy;
begin
  inherited;
end;

//------------------------------------------------------------------------------
procedure TPHXDrawRenderer.EnumDisplayModes(const Modes: TPHXDisplayModes);
begin

end;

//------------------------------------------------------------------------------
procedure TPHXDrawRenderer.Initialize(const Parameters: TPHXDeviceParameters);
begin
  if RC <> 0 then
  begin
    Finalize;
  end;

  if (dglOpenGL.InitOpenGL <> True) then
  begin
    raise Exception.Create('OpenGL Initialization failed.');
  end;
  dglOpenGL.ReadExtensions;


  FDC:= GetDC(Owner.Handle);
  if FDC = 0 then
  begin
    raise Exception.Create('Could not get device context!');
  end;

//  SetDCPixelFormat(FDC);

//  FRC := wglCreateContext(DC);
  FRC := CreateRenderingContext(FDC, [opDoubleBuffered], 32, 16, 16, 0, 0,0);
  if FRC = 0 then
  begin
    raise Exception.Create('Could not create rendering context!');
  end;
  ActivateRenderingContext(DC, RC);

 // if not ActivateRenderingContext(DC, RC) then
 // begin
 //   raise Exception.Create('error');
 // end;

      {

  // Get the device context
  FDC := GetDC(Owner.Handle);

  if FDC = 0 then
  begin
    raise Exception.Create('Could not get device context!');
  end;

  // Initialize opengl
  if (dglOpenGL.InitOpenGL <> True) then
  begin
    raise Exception.Create('OpenGL Initialization failed.');
  end;
  dglOpenGL.ReadExtensions;

//  FRenderer.DeviceContext:= FDC;

  // Create the rendering context
  FRC := CreateRenderingContext(FDC, [opDoubleBuffered], 32, 16, 16, 0, 0,0);

  if FRC = 0 then
  begin
    raise Exception.Create('Could not create rendering context!');
  end;

  // Activate the rendering context
  ActivateRenderingContext(DC, RC);

//  Device.Init('TPHXDraw', 100, 100);

  Owner.SetBounds(Owner.Left, Owner.Top, Owner.Width, Owner.Height);



//  if Assigned(FOnInit) then FOnInit(Self);
}
end;


//-----------------------------------------------------------------------------
procedure TPHXDrawRenderer.Reinitialize(const Parameters: TPHXDeviceParameters);
begin
//  TPHXEventFactory.DeviceReset(Mode.Width, Mode.Height) ;
end;

//-----------------------------------------------------------------------------
procedure TPHXDrawRenderer.Finalize;
begin
  wglMakeCurrent(0,0);

  if FRC <> 0 then DestroyRenderingContext(FRC);

  FRC:=0;
end;


//------------------------------------------------------------------------------
function TPHXDrawRenderer.GetWidth: Integer;
begin
  Result:= Owner.Width;
end;

//------------------------------------------------------------------------------
function TPHXDrawRenderer.GetHeight: Integer;
begin
  Result:= Owner.Height;
end;

//-----------------------------------------------------------------------------
function TPHXDrawRenderer.GetFlags: TPHXWindowFlags;
begin
  Result:= [];
end;

//-----------------------------------------------------------------------------
procedure TPHXDrawRenderer.SetTitle(const Title: string);
begin
end;

//------------------------------------------------------------------------------
procedure TPHXDrawRenderer.SetFlags(const Flags: TPHXWindowFlags);
begin

end;

//------------------------------------------------------------------------------
procedure TPHXDrawRenderer.SetIcon(const Icon: String);
begin

end;

//-----------------------------------------------------------------------------
procedure TPHXDrawRenderer.Update;
begin
end;

//-----------------------------------------------------------------------------
procedure TPHXDrawRenderer.Clear;
begin
  if not Assigned(glClear) then Exit;

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

  FRenderer      := TPHXDrawRenderer.Create(Self);
  FDevice        := TPHXDevice.Create(FRenderer);
  FInterval      := 16;
  FTimer         := TTimer.Create(Self);
  FTimer.Interval:= FInterval;
  FTimer.Enabled := False;
  FTimer.OnTimer := TimerEvent;

  FGenerateEvents:= False;

  Initialized:= False;
end;

//------------------------------------------------------------------------------
destructor TPHXDraw.Destroy;
begin
  if Initialized and Assigned(OnFinalize) then
  begin
    OnFinalize(Self, FDevice);
  end;

  inherited;
end;

//------------------------------------------------------------------------------
procedure TPHXDraw.Loaded;
begin
  inherited Loaded;

  if (ComponentState = []) then
  begin
   // Initialize;
  end;
end;

//------------------------------------------------------------------------------
procedure TPHXDraw.Initialize;
begin
  // Already initialized
  if Initialized then Exit;

  if Parent = nil then
  begin
    raise Exception.Create('No parent assigned');
  end;

  Device.Initialize('', Width, Height);

  if Assigned(FOnInit) then  FOnInit(Self, Device);

  FTimer.Interval:= Interval;
  FTimer.Enabled := Enabled;

  Initialized:= True;
end;

//------------------------------------------------------------------------------
procedure TPHXDraw.MakeCurrent;
begin
  ActivateRenderingContext(DC, RC);

  SetBounds(Left, Top, Width, Height);
end;

//------------------------------------------------------------------------------
procedure TPHXDraw.TimerEvent(Sender: TObject);
//var Event  : TPHXEvent;
begin
  if not IsOpenGLAvailable then Exit;

  if not Enabled then Exit;

  ActivateRenderingContext(DC, RC);

  if Assigned(OnUpdate) then OnUpdate(Self, Device);
  if Assigned(OnRender) then OnRender(Self, Device);

  // Poll and handle events
 // while Events.Poll(Event) do
 // begin
 //   HandleEvent(Event);
 // end;
end;

//------------------------------------------------------------------------------
procedure TPHXDraw.Resize;
var Event: TPHXEvent;
begin
  if Initialized then
  begin
    Event.Event:= PHX_DEVICE_RESIZED;
    Event.Device.Width:= Width;
    Event.Device.Height:= Height;

    TPHXEvents.Notify(Self,Event);

    Device.SetViewport(0, 0, Width, Height);

    if Assigned(FOnResize) then
    begin
      FOnResize(Self, Device, Width, Height);
    end;
  end;

  if (ComponentState = []) then
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

var MoveX, MoveY: Integer;

//------------------------------------------------------------------------------
procedure TPHXDraw.MouseMove(Shift: TShiftState; X, Y: Integer);
var Event: TPHXEvent;
begin
  inherited;

  if GenerateEvents and ((MoveX <> X) or (MoveY <> Y)) then
  begin
    Event.Mouse.Event:= PHX_MOUSE_MOVED;
    Event.Mouse.X:= X;
    Event.Mouse.Y:= Y;
    Event.Mouse.Shift:= ConvertShiftState(Shift);
    Event.Mouse.Button:= mbNone;

    TPHXEvents.Notify(Self, Event);
  end;

  MoveX:= X;
  MoveY:= Y;
end;

//------------------------------------------------------------------------------
procedure TPHXDraw.MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var Event: TPHXEvent;
begin
  inherited;

  if GenerateEvents then
  begin
    Event.Mouse.Event:= PHX_MOUSE_PRESSED;
    Event.Mouse.X:= X;
    Event.Mouse.Y:= Y;
    Event.Mouse.Shift:= ConvertShiftState(Shift);
    Event.Mouse.Button:= TPHXMouseButton(Ord(Button)+1);

    TPHXEvents.Notify(Self, Event);
  end;
end;

//------------------------------------------------------------------------------
procedure TPHXDraw.MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var Event: TPHXEvent;
begin
  inherited;

  if GenerateEvents then
  begin
    Event.Mouse.Event:= PHX_MOUSE_RELEASED;
    Event.Mouse.X:= X;
    Event.Mouse.Y:= Y;
    Event.Mouse.Shift:= ConvertShiftState(Shift);
    Event.Mouse.Button:= TPHXMouseButton(Ord(Button)+1);

    TPHXEvents.Notify(Self, Event);
  end;
end;

//------------------------------------------------------------------------------
procedure TPHXDraw.KeyDown(var Key: Word; Shift: TShiftState);
var Event: TPHXEvent;
begin
  inherited;

  if GenerateEvents then
  begin
    Event.Keyboard.Event:= PHX_KEY_PRESSED;
    Event.Keyboard.Key  := ConvertVirtualKey(Key);
    Event.Keyboard.Shift:= ConvertShiftState(Shift);

    TPHXEvents.Notify(Self, Event);
  end;
end;

//------------------------------------------------------------------------------
procedure TPHXDraw.KeyPress(var Key: Char);
var Event: TPHXEvent;
begin
  inherited;

  if GenerateEvents then
  begin
    Event.Keyboard.Event:= PHX_KEY_CHARACTER;
    Event.Keyboard.Char := Key;
    Event.Keyboard.Shift:= [];

    TPHXEvents.Notify(Self, Event);
  end;
end;

//------------------------------------------------------------------------------
procedure TPHXDraw.KeyUp(var Key: Word; Shift: TShiftState);
var Event: TPHXEvent;
begin
  inherited;

  if GenerateEvents then
  begin
    Event.Keyboard.Event:= PHX_KEY_RELEASED;
    Event.Keyboard.Key  := ConvertVirtualKey(Key);
    Event.Keyboard.Shift:= ConvertShiftState(Shift);

    TPHXEvents.Notify(Self, Event);
  end;
end;


// WMEraseBkgnd
//------------------------------------------------------------------------------
procedure TPHXDraw.WMEraseBkgnd(var Message: TWMEraseBkgnd);
begin
   if IsOpenGLAvailable then
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
    if IsOpenGLAvailable and Initialized and (Width > 0) and (Height > 0) then
    begin
      // Force a redraw
      if Assigned(OnRender) then
      begin
        OnRender(Self, Device);
      end;
    end else
    begin
      Paint;
    end;
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
end;

//------------------------------------------------------------------------------
procedure TPHXDraw.Paint;
begin
  if (csDesigning in ComponentState) then
  begin
    PaintDesign;
  end else
  if Assigned(FOnRender) and Initialized then
  begin
    FOnRender(Self, Device);
  end;
end;

//------------------------------------------------------------------------------
procedure TPHXDraw.PaintDesign;
var S   : String;
var w, h: Integer;
var Canvas: TCanvas;
begin
  Canvas:= inherited Canvas;
  Canvas.Brush.Style := Vcl.Graphics.bsSolid;
  Canvas.Brush.Color := inherited Color;

  Canvas.Pen.Color   := clBlack;
  Canvas.Pen.Style   := Vcl.Graphics.psDash;
  Canvas.Rectangle(0, 0, Width, Height);

  Canvas.Pen.Style := Vcl.Graphics.psSolid;
  Canvas.Pen.Color := clGray;
  Canvas.MoveTo(0, 0);
  Canvas.LineTo(Width, Height);

  Canvas.MoveTo(0, Height);
  Canvas.LineTo(Width, 0);

  s := Format('(%s)', [ClassName]);

  w := Canvas.TextWidth(s);
  h := Canvas.TextHeight(s);

  Canvas.Brush.Style := Vcl.Graphics.bsClear;
  Canvas.Brush.Color := clBtnFace;
  Canvas.TextOut(Width div 2-w div 2, Height div 2-h div 2, s);
end;

//------------------------------------------------------------------------------
procedure TPHXDraw.SetBounds(ALeft, ATop, AWidth, AHeight: Integer);
begin
  if Initialized and (AWidth > 0) and (AHeight > 0) then
  begin
    Device.SetViewport(0, 0, AWidth, AHeight);

    if Assigned(FOnResize) then FOnResize(Self, Device, AWidth, AHeight);
  end;
  inherited;
end;

//------------------------------------------------------------------------------
procedure TPHXDraw.SetEnabled(Value: Boolean);
begin
  inherited;
  FTimer.Enabled:= Value;
end;

//------------------------------------------------------------------------------
procedure TPHXDraw.SetInterval(const Value: Cardinal);
begin
  if Value <> FInterval then
  begin
    FInterval := Value;

    FTimer.Interval:= Value;
  end;
end;

//------------------------------------------------------------------------------
function TPHXDraw.IsOpenGLAvailable: Boolean;
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
