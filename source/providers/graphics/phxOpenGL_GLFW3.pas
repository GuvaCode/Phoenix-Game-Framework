unit phxOpenGL_GLFW3;

interface

{$I ../source/phxConfig.inc}

uses
  GLFW3, SysUtils, Classes, dglOpenGL, phxLogger, phxTypes, phxClasses, phxEvents, phxDevice, phxGraphics, phxOpenGL;

const
  ProviderName = 'OpenGL.GLFW3';

type
  TPHXOpenGL_Provider_GLFW3 = class(TPHXProvider)
  protected
    function GetName: String; override;
    function GetTarget: TPHXProviderTarget; override;
  public
    constructor Create; override;
    function CreateRenderer: IPHXDevice; override;
  end;

  TPHXOpenGLRendererGLFW3 = class(TPHXOpenGL_Renderer)
  private
    FInitialized: Boolean;
    FWindow: PGLFWWindow;
    FTitle: String;
    FWidth: Integer;
    FHeight: Integer;
    FFullscreen: Boolean;
    FWindowFlags: TPHXWindowFlags;
  protected
    function GetWidth: Integer; override;
    function GetHeight: Integer; override;
    function GetFlags: TPHXWindowFlags; override;
    procedure SetTitle(const Title: String); override;
    procedure SetFlags(const Flags: TPHXWindowFlags); override;
    procedure SetIcon(const Icon: String); override;
  public
    constructor Create;
    destructor Destroy; override;
    procedure EnumDisplayModes(const Modes: TPHXDisplayModes); override;
    procedure Initialize(const Parameters: TPHXDeviceParameters); override;
    procedure Reinitialize(const Parameters: TPHXDeviceParameters); override;
    procedure Finalize; override;
    procedure Update; override;
    procedure Clear; override;
    procedure Flip; override;
  end;

implementation

{$REGION 'GLFW event callbacks'}

var
  ShiftStates: TPHXShiftStates;
  MousePos: TVector2i = (X: 0; Y: 0);
  Major, Minor, Rev: Integer;

function GLFWKeyToPHXKey(const Key: Integer): TPHXVirtualKey; inline;
begin
  case Key of
    GLFW_KEY_UP: Result := VK_UP;
    GLFW_KEY_DOWN: Result := VK_DOWN;
    GLFW_KEY_RIGHT: Result := VK_RIGHT;
    GLFW_KEY_LEFT: Result := VK_LEFT;
    GLFW_KEY_SPACE: Result := VK_SPACE;
    GLFW_KEY_INSERT: Result := VK_INSERT;
    GLFW_KEY_HOME: Result := VK_HOME;
    GLFW_KEY_END: Result := VK_END;
    GLFW_KEY_PAGE_UP: Result := VK_PAGEUP;
    GLFW_KEY_PAGE_DOWN: Result := VK_PAGEDOWN;
    GLFW_KEY_0: Result := VK_0;
    GLFW_KEY_1: Result := VK_1;
    GLFW_KEY_2: Result := VK_2;
    GLFW_KEY_3: Result := VK_3;
    GLFW_KEY_4: Result := VK_4;
    GLFW_KEY_5: Result := VK_5;
    GLFW_KEY_6: Result := VK_6;
    GLFW_KEY_7: Result := VK_7;
    GLFW_KEY_8: Result := VK_8;
    GLFW_KEY_9: Result := VK_9;
    GLFW_KEY_KP_0: Result := VK_NUM_0;
    GLFW_KEY_KP_1: Result := VK_NUM_1;
    GLFW_KEY_KP_2: Result := VK_NUM_2;
    GLFW_KEY_KP_3: Result := VK_NUM_3;
    GLFW_KEY_KP_4: Result := VK_NUM_4;
    GLFW_KEY_KP_5: Result := VK_NUM_5;
    GLFW_KEY_KP_6: Result := VK_NUM_6;
    GLFW_KEY_KP_7: Result := VK_NUM_7;
    GLFW_KEY_KP_8: Result := VK_NUM_8;
    GLFW_KEY_KP_9: Result := VK_NUM_9;
    GLFW_KEY_A: Result := VK_A;
    GLFW_KEY_B: Result := VK_B;
    GLFW_KEY_C: Result := VK_C;
    GLFW_KEY_D: Result := VK_D;
    GLFW_KEY_E: Result := VK_E;
    GLFW_KEY_F: Result := VK_F;
    GLFW_KEY_G: Result := VK_G;
    GLFW_KEY_H: Result := VK_H;
    GLFW_KEY_I: Result := VK_I;
    GLFW_KEY_J: Result := VK_J;
    GLFW_KEY_K: Result := VK_K;
    GLFW_KEY_L: Result := VK_L;
    GLFW_KEY_M: Result := VK_M;
    GLFW_KEY_N: Result := VK_N;
    GLFW_KEY_O: Result := VK_O;
    GLFW_KEY_P: Result := VK_P;
    GLFW_KEY_Q: Result := VK_Q;
    GLFW_KEY_R: Result := VK_R;
    GLFW_KEY_S: Result := VK_S;
    GLFW_KEY_T: Result := VK_T;
    GLFW_KEY_U: Result := VK_U;
    GLFW_KEY_V: Result := VK_V;
    GLFW_KEY_W: Result := VK_W;
    GLFW_KEY_X: Result := VK_X;
    GLFW_KEY_Y: Result := VK_Y;
    GLFW_KEY_Z: Result := VK_Z;
    GLFW_KEY_KP_DECIMAL: Result := VK_NUM_DECIMAL;
    GLFW_KEY_KP_DIVIDE: Result := VK_NUM_DIVIDE;
    GLFW_KEY_KP_MULTIPLY: Result := VK_NUM_MULTIPLY;
    GLFW_KEY_KP_SUBTRACT: Result := VK_NUM_SUBTRACT;
    GLFW_KEY_KP_ADD: Result := VK_NUM_ADD;
    GLFW_KEY_KP_ENTER: Result := VK_NUM_ENTER;
    GLFW_KEY_KP_EQUAL: Result := VK_NUM_EQUAL;
    GLFW_KEY_RIGHT_SHIFT: Result := VK_RSHIFT;
    GLFW_KEY_LEFT_SHIFT: Result := VK_LSHIFT;
    GLFW_KEY_RIGHT_CONTROL: Result := VK_RCTRL;
    GLFW_KEY_LEFT_CONTROL: Result := VK_LCTRL;
    GLFW_KEY_RIGHT_ALT: Result := VK_RALT;
    GLFW_KEY_LEFT_ALT: Result := VK_LALT;
    GLFW_KEY_F1: Result := VK_F1;
    GLFW_KEY_F2: Result := VK_F2;
    GLFW_KEY_F3: Result := VK_F3;
    GLFW_KEY_F4: Result := VK_F4;
    GLFW_KEY_F5: Result := VK_F5;
    GLFW_KEY_F6: Result := VK_F6;
    GLFW_KEY_F7: Result := VK_F7;
    GLFW_KEY_F8: Result := VK_F8;
    GLFW_KEY_F9: Result := VK_F9;
    GLFW_KEY_F10: Result := VK_F10;
    GLFW_KEY_F11: Result := VK_F11;
    GLFW_KEY_F12: Result := VK_F12;
    GLFW_KEY_F13: Result := VK_F13;
    GLFW_KEY_F14: Result := VK_F14;
    GLFW_KEY_F15: Result := VK_F15;
    GLFW_KEY_ESCAPE: Result := VK_ESC;
    GLFW_KEY_TAB: Result := VK_TAB;
    GLFW_KEY_ENTER: Result := VK_RETURN;
    GLFW_KEY_BACKSPACE: Result := VK_BACKSPACE;
    GLFW_KEY_DELETE: Result := VK_DEL;
    GLFW_KEY_GRAVE_ACCENT: Result := VK_TILDE;
    else
      Result := VK_UNKNOWN;
  end;
end;

procedure WindowCloseEvent(window: PGLFWwindow); cdecl;
var
  Event: TPHXEvent;
begin
  Event.Event := PHX_EVENT_QUIT;
  TPHXEvents.Notify(nil, Event);
end;

procedure WindowSizeEvent(window: PGLFWwindow; Width, Height: Integer); cdecl;
begin
  TPHXEvents.NotifyDeviceResized(nil, Width, Height);
end;

procedure KeyCallback(window: PGLFWwindow; key, scancode, action, mods: Integer); cdecl;
var
  Event: TPHXEvent;
begin
  case Action of
    GLFW_PRESS:
    begin
      case Key of
        GLFW_KEY_RIGHT_SHIFT: Include(ShiftStates, ssShift);
        GLFW_KEY_LEFT_SHIFT: Include(ShiftStates, ssShift);
        GLFW_KEY_LEFT_CONTROL: Include(ShiftStates, ssCtrl);
        GLFW_KEY_RIGHT_CONTROL: Include(ShiftStates, ssCtrl);
        GLFW_KEY_LEFT_ALT: Include(ShiftStates, ssAlt);
        GLFW_KEY_RIGHT_ALT: Include(ShiftStates, ssAlt);
      end;
      Event.Keyboard.Event := PHX_KEY_PRESSED;
      Event.Keyboard.Key := GLFWKeyToPHXKey(Key);
      Event.Keyboard.Shift := ShiftStates;
      TPHXEvents.Notify(nil, Event);
    end;
    GLFW_RELEASE:
    begin
      case Key of
        GLFW_KEY_RIGHT_SHIFT: Exclude(ShiftStates, ssShift);
        GLFW_KEY_LEFT_SHIFT: Exclude(ShiftStates, ssShift);
        GLFW_KEY_LEFT_CONTROL: Exclude(ShiftStates, ssCtrl);
        GLFW_KEY_RIGHT_CONTROL: Exclude(ShiftStates, ssCtrl);
        GLFW_KEY_LEFT_ALT: Exclude(ShiftStates, ssAlt);
        GLFW_KEY_RIGHT_ALT: Exclude(ShiftStates, ssAlt);
      end;
      Event.Keyboard.Event := PHX_KEY_RELEASED;
      Event.Keyboard.Key := GLFWKeyToPHXKey(Key);
      Event.Keyboard.Shift := ShiftStates;
      TPHXEvents.Notify(nil, Event);
    end;
  end;
end;

procedure CharCallback(window: PGLFWwindow; codepoint: Cardinal); cdecl;
var
  Event: TPHXEvent;
begin
  Event.Keyboard.Event := PHX_KEY_CHARACTER;
  Event.Keyboard.Char := Chr(codepoint);
  TPHXEvents.Notify(nil, Event);
end;

procedure MousePosCallback(window: PGLFWwindow; xpos, ypos: Double); cdecl;
var
  Event: TPHXEvent;
  IX, IY: Integer;
begin
  IX := Trunc(xpos);
  IY := Trunc(ypos);
  MousePos.X := IX;
  MousePos.Y := IY;
  Event.Mouse.Event := PHX_MOUSE_MOVED;
  Event.Mouse.X := IX;
  Event.Mouse.Y := IY;
  TPHXEvents.Notify(nil, Event);
end;

procedure MouseButtonCallback(window: PGLFWwindow; button, action, mods: Integer); cdecl;
var
  Event: TPHXEvent;
begin
  case Action of
    GLFW_PRESS:
    begin
      Event.Mouse.Event := PHX_MOUSE_PRESSED;
      Event.Mouse.Button := TPHXMouseButton(Button + 1);
      Event.Mouse.X := MousePos.X;
      Event.Mouse.Y := MousePos.Y;
      Event.Mouse.Shift := ShiftStates;
      TPHXEvents.Notify(nil, Event);
    end;
    GLFW_RELEASE:
    begin
      Event.Mouse.Event := PHX_MOUSE_RELEASED;
      Event.Mouse.Button := TPHXMouseButton(Button + 1);
      Event.Mouse.X := MousePos.X;
      Event.Mouse.Y := MousePos.Y;
      Event.Mouse.Shift := ShiftStates;
      TPHXEvents.Notify(nil, Event);
    end;
  end;
end;

{$ENDREGION}
{$REGION 'TPHXOpenGL_Provider_GLFW3'}

constructor TPHXOpenGL_Provider_GLFW3.Create;
begin
  inherited Create;
end;

function TPHXOpenGL_Provider_GLFW3.GetName: String;
begin
  Result := ProviderName;
end;

function TPHXOpenGL_Provider_GLFW3.GetTarget: TPHXProviderTarget;
begin
  Result := ptOpenGL_3;
end;

function TPHXOpenGL_Provider_GLFW3.CreateRenderer: IPHXDevice;
begin
  Result := TPHXOpenGLRendererGLFW3.Create;
end;

{$ENDREGION}
{$REGION 'TPHXOpenGLRendererGLFW3'}

constructor TPHXOpenGLRendererGLFW3.Create;
begin
  inherited Create;
  FInitialized := False;
  FWindow := nil;
  FTitle := 'Phoenix Framework';
  FWidth := 800;
  FHeight := 600;
  FFullscreen := False;
  FWindowFlags := DefaultWindowFlags;
end;

destructor TPHXOpenGLRendererGLFW3.Destroy;
begin
  glfwDestroyWindow(FWindow);
  glfwTerminate;
  inherited Destroy;
end;

procedure TPHXOpenGLRendererGLFW3.EnumDisplayModes(const Modes: TPHXDisplayModes);
begin

end;

procedure TPHXOpenGLRendererGLFW3.Initialize(const Parameters: TPHXDeviceParameters);
var
  IX: Integer = 0;
  IY: Integer = 0;
begin
  FTitle := Parameters.Title;
  FWidth := Parameters.Width;
  FHeight := Parameters.Height;

  if glfwInit() <> 1 then raise Exception.Create('Failed to initialize GLFW.');

  glfwGetVersion(@major, @minor, @rev);
  glfwWindowHint(GLFW_RESIZABLE, 0);

  FWindow := glfwCreateWindow(FWidth, FHeight, PAnsiChar(FTitle), nil, nil);

  if FWindow = nil then
  begin
    glfwTerminate;
    raise Exception.Create('Failed to initialize GLFW.');
  end;

  glfwSetWindowSizeCallback(FWindow, @WindowSizeEvent);
  glfwSetWindowCloseCallback(FWindow, @WindowCloseEvent);
  glfwSetKeyCallback(FWindow, @KeyCallback);
  glfwSetCharCallback(FWindow, @CharCallback);
  glfwSetMouseButtonCallback(FWindow, @MouseButtonCallback);
  glfwSetCursorPosCallback(FWindow, @MousePosCallback);
  glfwMakeContextCurrent(FWindow);
  glfwGetMonitorPhysicalSize(glfwGetPrimaryMonitor, @IX, @IY);
  if Parameters.FullScreen then
    glfwSetWindowMonitor(FWindow, glfwGetPrimaryMonitor, IX div 2, IY div 2, FWidth, FHeight, 0)
  else
  begin
    glfwSetWindowPos(FWindow, IX div 2, IY div 2);
    glfwSetWindowSize(FWindow, FWidth, FHeight);
  end;

  if wfVerticalSync in FWindowFlags then
  begin
  glfwSwapInterval(1);
  end else
  begin
  glfwSwapInterval(0);
  end;

  glfwSetInputMode(FWindow, GLFW_STICKY_MOUSE_BUTTONS, 1);
  if (dglOpenGL.InitOpenGL <> True) then
    raise Exception.Create('OpenGL Initialization failed.');
  dglOpenGL.ReadExtensions;
  InitializeOpenGL;
  FInitialized := True;
end;

procedure TPHXOpenGLRendererGLFW3.Reinitialize(const Parameters: TPHXDeviceParameters);
begin
end;

procedure TPHXOpenGLRendererGLFW3.Finalize;
begin
end;

procedure TPHXOpenGLRendererGLFW3.Update;
begin
  glfwPollEvents;
end;

procedure TPHXOpenGLRendererGLFW3.Clear;
begin
  glClear(GL_COLOR_BUFFER_BIT or GL_DEPTH_BUFFER_BIT);
end;

procedure TPHXOpenGLRendererGLFW3.Flip;
begin
  glfwSwapBuffers(FWindow);
end;

function TPHXOpenGLRendererGLFW3.GetWidth: Integer;
begin
  Result := FWidth;
end;

function TPHXOpenGLRendererGLFW3.GetHeight: Integer;
begin
  Result := FHeight;
end;

function TPHXOpenGLRendererGLFW3.GetFlags: TPHXWindowFlags;
begin
  Result := FWindowFlags;
end;

procedure TPHXOpenGLRendererGLFW3.SetTitle(const Title: String);
begin
  FTitle := Title;
end;

procedure TPHXOpenGLRendererGLFW3.SetFlags(const Flags: TPHXWindowFlags);
begin
  FWindowFlags := Flags;
end;

procedure TPHXOpenGLRendererGLFW3.SetIcon(const Icon: String);
begin
end;

{$ENDREGION}

initialization
  RegisterProvider(ProviderName, TPHXOpenGL_Provider_GLFW3);
end.
