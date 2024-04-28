unit GLFW3;

{$I ../phxConfig.inc}


interface
uses CTypes{$IFDEF VK_VERSION_1_0}, Vulkan{$ENDIF};

const
{$IFDEF WINDOWS}
  {$IFDEF GLFW3_STATIC}
  {$IFDEF GLFW3_STATIC_LINKDLL}
    {$LINKLIB libglfw3dll.a}
    {$ELSE}
      {$LINKLIB libglfw3.a}
  {$ENDIF}
    {$ELSE}
     cDllName = 'glfw3.dll';
  {$ENDIF}
{$ENDIF}

{$IFDEF DARWIN}
  cDllName = 'libglfw3.dylib';
  {$ENDIF}
{$IFDEF LINUX}
  {$IFDEF GLFW3_STATIC}
  {$LINKLIB libglfw}
  {$ELSE}
  cDllName = 'libglfw.so.3';
  {$ENDIF}
{$ENDIF}

  GLFW_VERSION_MAJOR = 3;
  {$IFDEF GLFW3_LASTEST}
  GLFW_VERSION_MINOR = 3;
  GLFW_VERSION_REVISION = 7;
  {$ELSE}
  GLFW_VERSION_MINOR = 4;
  GLFW_VERSION_REVISION = 0;
  {$ENDIF}
  GLFW_TRUE = 1;
  GLFW_FALSE = 0;
  GLFW_RELEASE = 0;
  GLFW_PRESS = 1;
  GLFW_REPEAT = 2;

  GLFW_HAT_CENTERED = 0;
  GLFW_HAT_UP = 1;
  GLFW_HAT_RIGHT = 2;
  GLFW_HAT_DOWN = 4;
  GLFW_HAT_LEFT = 8;
  GLFW_HAT_RIGHT_UP = (GLFW_HAT_RIGHT or GLFW_HAT_UP);
  GLFW_HAT_RIGHT_DOWN = (GLFW_HAT_RIGHT or GLFW_HAT_DOWN);
  GLFW_HAT_LEFT_UP = (GLFW_HAT_LEFT or GLFW_HAT_UP);
  GLFW_HAT_LEFT_DOWN = (GLFW_HAT_LEFT or GLFW_HAT_DOWN);

  GLFW_KEY_UNKNOWN = -1;

  GLFW_KEY_SPACE = 32;
  GLFW_KEY_APOSTROPHE = 39;
  GLFW_KEY_COMMA = 44;
  GLFW_KEY_MINUS = 45;
  GLFW_KEY_PERIOD = 46;
  GLFW_KEY_SLASH = 47;
  GLFW_KEY_0 = 48;
  GLFW_KEY_1 = 49;
  GLFW_KEY_2 = 50;
  GLFW_KEY_3 = 51;
  GLFW_KEY_4 = 52;
  GLFW_KEY_5 = 53;
  GLFW_KEY_6 = 54;
  GLFW_KEY_7 = 55;
  GLFW_KEY_8 = 56;
  GLFW_KEY_9 = 57;
  GLFW_KEY_SEMICOLON = 59;
  GLFW_KEY_EQUAL = 61;
  GLFW_KEY_A = 65;
  GLFW_KEY_B = 66;
  GLFW_KEY_C = 67;
  GLFW_KEY_D = 68;
  GLFW_KEY_E = 69;
  GLFW_KEY_F = 70;
  GLFW_KEY_G = 71;
  GLFW_KEY_H = 72;
  GLFW_KEY_I = 73;
  GLFW_KEY_J = 74;
  GLFW_KEY_K = 75;
  GLFW_KEY_L = 76;
  GLFW_KEY_M = 77;
  GLFW_KEY_N = 78;
  GLFW_KEY_O = 79;
  GLFW_KEY_P = 80;
  GLFW_KEY_Q = 81;
  GLFW_KEY_R = 82;
  GLFW_KEY_S = 83;
  GLFW_KEY_T = 84;
  GLFW_KEY_U = 85;
  GLFW_KEY_V = 86;
  GLFW_KEY_W = 87;
  GLFW_KEY_X = 88;
  GLFW_KEY_Y = 89;
  GLFW_KEY_Z = 90;
  GLFW_KEY_LEFT_BRACKET = 91;
  GLFW_KEY_BACKSLASH = 92;
  GLFW_KEY_RIGHT_BRACKET = 93;
  GLFW_KEY_GRAVE_ACCENT = 96;
  GLFW_KEY_WORLD_1 = 161;
  GLFW_KEY_WORLD_2 = 162;

  GLFW_KEY_ESCAPE = 256;
  GLFW_KEY_ENTER = 257;
  GLFW_KEY_TAB = 258;
  GLFW_KEY_BACKSPACE = 259;
  GLFW_KEY_INSERT = 260;
  GLFW_KEY_DELETE = 261;
  GLFW_KEY_RIGHT = 262;
  GLFW_KEY_LEFT = 263;
  GLFW_KEY_DOWN = 264;
  GLFW_KEY_UP = 265;
  GLFW_KEY_PAGE_UP = 266;
  GLFW_KEY_PAGE_DOWN = 267;
  GLFW_KEY_HOME = 268;
  GLFW_KEY_END = 269;
  GLFW_KEY_CAPS_LOCK = 280;
  GLFW_KEY_SCROLL_LOCK = 281;
  GLFW_KEY_NUM_LOCK = 282;
  GLFW_KEY_PRINT_SCREEN = 283;
  GLFW_KEY_PAUSE = 284;
  GLFW_KEY_F1 = 290;
  GLFW_KEY_F2 = 291;
  GLFW_KEY_F3 = 292;
  GLFW_KEY_F4 = 293;
  GLFW_KEY_F5 = 294;
  GLFW_KEY_F6 = 295;
  GLFW_KEY_F7 = 296;
  GLFW_KEY_F8 = 297;
  GLFW_KEY_F9 = 298;
  GLFW_KEY_F10 = 299;
  GLFW_KEY_F11 = 300;
  GLFW_KEY_F12 = 301;
  GLFW_KEY_F13 = 302;
  GLFW_KEY_F14 = 303;
  GLFW_KEY_F15 = 304;
  GLFW_KEY_F16 = 305;
  GLFW_KEY_F17 = 306;
  GLFW_KEY_F18 = 307;
  GLFW_KEY_F19 = 308;
  GLFW_KEY_F20 = 309;
  GLFW_KEY_F21 = 310;
  GLFW_KEY_F22 = 311;
  GLFW_KEY_F23 = 312;
  GLFW_KEY_F24 = 313;
  GLFW_KEY_F25 = 314;
  GLFW_KEY_KP_0 = 320;
  GLFW_KEY_KP_1 = 321;
  GLFW_KEY_KP_2 = 322;
  GLFW_KEY_KP_3 = 323;
  GLFW_KEY_KP_4 = 324;
  GLFW_KEY_KP_5 = 325;
  GLFW_KEY_KP_6 = 326;
  GLFW_KEY_KP_7 = 327;
  GLFW_KEY_KP_8 = 328;
  GLFW_KEY_KP_9 = 329;
  GLFW_KEY_KP_DECIMAL = 330;
  GLFW_KEY_KP_DIVIDE = 331;
  GLFW_KEY_KP_MULTIPLY= 332;
  GLFW_KEY_KP_SUBTRACT = 333;
  GLFW_KEY_KP_ADD = 334;
  GLFW_KEY_KP_ENTER = 335;
  GLFW_KEY_KP_EQUAL = 336;
  GLFW_KEY_LEFT_SHIFT = 340;
  GLFW_KEY_LEFT_CONTROL = 341;
  GLFW_KEY_LEFT_ALT = 342;
  GLFW_KEY_LEFT_SUPER = 343;
  GLFW_KEY_RIGHT_SHIFT = 344;
  GLFW_KEY_RIGHT_CONTROL = 345;
  GLFW_KEY_RIGHT_ALT = 346;
  GLFW_KEY_RIGHT_SUPER = 347;
  GLFW_KEY_MENU = 348;

  GLFW_KEY_LAST = GLFW_KEY_MENU;
  
  GLFW_MOD_SHIFT = $0001;
  GLFW_MOD_CONTROL = $0002;
  GLFW_MOD_ALT = $0004;
  GLFW_MOD_CAPS_LOCK = $0010;
  GLFW_MOD_NUM_LOCK = $0020;

  GLFW_MOUSE_BUTTON_1 = 0;
  GLFW_MOUSE_BUTTON_2 = 1;
  GLFW_MOUSE_BUTTON_3 = 2;
  GLFW_MOUSE_BUTTON_4 = 3;
  GLFW_MOUSE_BUTTON_5 = 4;
  GLFW_MOUSE_BUTTON_6 = 5;
  GLFW_MOUSE_BUTTON_7 = 6;
  GLFW_MOUSE_BUTTON_8 = 7;
  GLFW_MOUSE_BUTTON_LAST = GLFW_MOUSE_BUTTON_8;
  GLFW_MOUSE_BUTTON_LEFT = GLFW_MOUSE_BUTTON_1;
  GLFW_MOUSE_BUTTON_RIGHT = GLFW_MOUSE_BUTTON_2;
  GLFW_MOUSE_BUTTON_MIDDLE = GLFW_MOUSE_BUTTON_3;

  GLFW_JOYSTICK_1 = 0;
  GLFW_JOYSTICK_2 = 1;
  GLFW_JOYSTICK_3 = 2;
  GLFW_JOYSTICK_4 = 3;
  GLFW_JOYSTICK_5 = 4;
  GLFW_JOYSTICK_6 = 5;
  GLFW_JOYSTICK_7 = 6;
  GLFW_JOYSTICK_8 = 7;
  GLFW_JOYSTICK_9 = 8;
  GLFW_JOYSTICK_10 = 9;
  GLFW_JOYSTICK_11 = 10;
  GLFW_JOYSTICK_12 = 11;
  GLFW_JOYSTICK_13 = 12;
  GLFW_JOYSTICK_14 = 13;
  GLFW_JOYSTICK_15 = 14;
  GLFW_JOYSTICK_16 = 15;
  GLFW_JOYSTICK_LAST = GLFW_JOYSTICK_16;

  GLFW_GAMEPAD_BUTTON_A = 0;
  GLFW_GAMEPAD_BUTTON_B = 1;
  GLFW_GAMEPAD_BUTTON_X = 2;
  GLFW_GAMEPAD_BUTTON_Y = 3;
  GLFW_GAMEPAD_BUTTON_LEFT_BUMPER = 4;
  GLFW_GAMEPAD_BUTTON_RIGHT_BUMPER = 5;
  GLFW_GAMEPAD_BUTTON_BACK = 6;
  GLFW_GAMEPAD_BUTTON_START = 7;
  GLFW_GAMEPAD_BUTTON_GUIDE = 8;
  GLFW_GAMEPAD_BUTTON_LEFT_THUMB = 9;
  GLFW_GAMEPAD_BUTTON_RIGHT_THUMB = 10;
  GLFW_GAMEPAD_BUTTON_DPAD_UP = 11;
  GLFW_GAMEPAD_BUTTON_DPAD_RIGHT = 12;
  GLFW_GAMEPAD_BUTTON_DPAD_DOWN = 13;
  GLFW_GAMEPAD_BUTTON_DPAD_LEFT = 14;
  GLFW_GAMEPAD_BUTTON_LAST = GLFW_GAMEPAD_BUTTON_DPAD_LEFT;

  GLFW_GAMEPAD_BUTTON_CROSS = GLFW_GAMEPAD_BUTTON_A;
  GLFW_GAMEPAD_BUTTON_CIRCLE = GLFW_GAMEPAD_BUTTON_B;
  GLFW_GAMEPAD_BUTTON_SQUARE = GLFW_GAMEPAD_BUTTON_X;
  GLFW_GAMEPAD_BUTTON_TRIANGLE = GLFW_GAMEPAD_BUTTON_Y;

  GLFW_GAMEPAD_AXIS_LEFT_X = 0;
  GLFW_GAMEPAD_AXIS_LEFT_Y = 1;
  GLFW_GAMEPAD_AXIS_RIGHT_X = 2;
  GLFW_GAMEPAD_AXIS_RIGHT_Y = 3;
  GLFW_GAMEPAD_AXIS_LEFT_TRIGGER = 4;
  GLFW_GAMEPAD_AXIS_RIGHT_TRIGGER = 5;
  GLFW_GAMEPAD_AXIS_LAST = GLFW_GAMEPAD_AXIS_RIGHT_TRIGGER;

  GLFW_NO_ERROR = 0;
  GLFW_NOT_INITIALIZED = $00010001;
  GLFW_NO_CURRENT_CONTEXT = $00010002;
  GLFW_INVALID_ENUM = $00010003;
  GLFW_INVALID_VALUE = $00010004;
  GLFW_OUT_OF_MEMORY = $00010005;
  GLFW_API_UNAVAILABLE = $00010006;
  GLFW_VERSION_UNAVAILABLE = $00010007;
  GLFW_PLATFORM_ERROR = $00010008;
  GLFW_FORMAT_UNAVAILABLE = $00010009;
  GLFW_NO_WINDOW_CONTEXT = $0001000A;
  GLFW_CURSOR_UNAVAILABLE = $0001000B;
  GLFW_FEATURE_UNAVAILABLE = $0001000C;
  GLFW_FEATURE_UNIMPLEMENTED = $0001000D;
  GLFW_PLATFORM_UNAVAILABLE = $0001000E;
  GLFW_FOCUSED = $00020001;

  GLFW_ICONIFIED = $00020002;
  GLFW_RESIZABLE = $00020003;
  GLFW_VISIBLE = $00020004;
  GLFW_DECORATED = $00020005;

  GLFW_AUTO_ICONIFY = $00020006;
  GLFW_FLOATING = $00020007;
  GLFW_MAXIMIZED = $00020008;
  GLFW_CENTER_CURSOR = $00020009;
  GLFW_TRANSPARENT_FRAMEBUFFER  = $0002000A;
  GLFW_HOVERED = $0002000B;
  GLFW_FOCUS_ON_SHOW = $0002000C;
  GLFW_MOUSE_PASSTHROUGH = $0002000D;
  GLFW_POSITION_X = $0002000E;
  GLFW_POSITION_Y = $0002000F;
  GLFW_RED_BITS = $00021001;
  GLFW_GREEN_BITS = $00021002;
  GLFW_BLUE_BITS = $00021003;
  GLFW_ALPHA_BITS = $00021004;
  GLFW_DEPTH_BITS = $00021005;
  GLFW_STENCIL_BITS = $00021006;
  GLFW_ACCUM_RED_BITS = $00021007;
  GLFW_ACCUM_GREEN_BITS = $00021008;
  GLFW_ACCUM_BLUE_BITS = $00021009;
  GLFW_ACCUM_ALPHA_BITS = $0002100A;
  GLFW_AUX_BUFFERS = $0002100B;
  GLFW_STEREO = $0002100C;
  GLFW_SAMPLES = $0002100D;
  GLFW_SRGB_CAPABLE = $0002100E;
  GLFW_REFRESH_RATE = $0002100F;
  GLFW_DOUBLEBUFFER = $00021010;
  GLFW_CLIENT_API = $00022001;
  GLFW_CONTEXT_VERSION_MAJOR = $00022002;
  GLFW_CONTEXT_VERSION_MINOR = $00022003;
  GLFW_CONTEXT_REVISION = $00022004;
  GLFW_CONTEXT_ROBUSTNESS = $00022005;
  GLFW_OPENGL_FORWARD_COMPAT = $00022006;
  GLFW_CONTEXT_DEBUG = $00022007;

  GLFW_OPENGL_DEBUG_CONTEXT = GLFW_CONTEXT_DEBUG;
  GLFW_OPENGL_PROFILE = $00022008;
  GLFW_CONTEXT_RELEASE_BEHAVIOR = $00022009;
  GLFW_CONTEXT_NO_ERROR = $0002200A;
  GLFW_CONTEXT_CREATION_API = $0002200B;
  GLFW_SCALE_TO_MONITOR = $0002200C;
  GLFW_SCALE_FRAMEBUFFER = $0002200D;
  GLFW_COCOA_RETINA_FRAMEBUFFER = $00023001;
  GLFW_COCOA_FRAME_NAME = $00023002;
  GLFW_COCOA_GRAPHICS_SWITCHING  = $00023003;
  GLFW_X11_CLASS_NAME = $00024001;
  GLFW_X11_INSTANCE_NAME = $00024002;
  GLFW_WIN32_KEYBOARD_MENU = $00025001;
  GLFW_WIN32_SHOWDEFAULT = $00025002;
  GLFW_WAYLAND_APP_ID = $00026001;
  GLFW_NO_API = 0;
  GLFW_OPENGL_API = $00030001;
  GLFW_OPENGL_ES_API = $00030002;

  GLFW_NO_ROBUSTNESS = 0;
  GLFW_NO_RESET_NOTIFICATION  = $00031001;
  GLFW_LOSE_CONTEXT_ON_RESET  = $00031002;

  GLFW_OPENGL_ANY_PROFILE = 0;
  GLFW_OPENGL_CORE_PROFILE = $00032001;
  GLFW_OPENGL_COMPAT_PROFILE = $00032002;

  GLFW_CURSOR = $00033001;
  GLFW_STICKY_KEYS = $00033002;
  GLFW_STICKY_MOUSE_BUTTONS = $00033003;
  GLFW_LOCK_KEY_MODS = $00033004;
  GLFW_RAW_MOUSE_MOTION = $00033005;

  GLFW_CURSOR_NORMAL = $00034001;
  GLFW_CURSOR_HIDDEN = $00034002;
  GLFW_CURSOR_DISABLED = $00034003;
  GLFW_CURSOR_CAPTURED = $00034004;

  GLFW_ANY_RELEASE_BEHAVIOR = 0;
  GLFW_RELEASE_BEHAVIOR_FLUSH = $00035001;
  GLFW_RELEASE_BEHAVIOR_NONE = $00035002;

  GLFW_NATIVE_CONTEXT_API = $00036001;
  GLFW_EGL_CONTEXT_API = $00036002;
  GLFW_OSMESA_CONTEXT_API = $00036003;

  GLFW_ANGLE_PLATFORM_TYPE_NONE = $00037001;
  GLFW_ANGLE_PLATFORM_TYPE_OPENGL = $00037002;
  GLFW_ANGLE_PLATFORM_TYPE_OPENGLES = $00037003;
  GLFW_ANGLE_PLATFORM_TYPE_D3D9 = $00037004;
  GLFW_ANGLE_PLATFORM_TYPE_D3D11 = $00037005;
  GLFW_ANGLE_PLATFORM_TYPE_VULKAN = $00037007;
  GLFW_ANGLE_PLATFORM_TYPE_METAL = $00037008;

  GLFW_WAYLAND_PREFER_LIBDECOR = $00038001;
  GLFW_WAYLAND_DISABLE_LIBDECOR = $00038002;

  GLFW_ANY_POSITION = $80000000;

  GLFW_ARROW_CURSOR = $00036001;
  GLFW_IBEAM_CURSOR = $00036002;
  GLFW_CROSSHAIR_CURSOR = $00036003;
  GLFW_POINTING_HAND_CURSOR = $00036004;
  GLFW_RESIZE_EW_CURSOR = $00036005;
  GLFW_RESIZE_NS_CURSOR = $00036006;
  GLFW_RESIZE_NWSE_CURSOR = $00036007;
  GLFW_RESIZE_NESW_CURSOR = $00036008;
  GLFW_RESIZE_ALL_CURSOR = $00036009;
  GLFW_NOT_ALLOWED_CURSOR = $0003600A;
  GLFW_HRESIZE_CURSOR = GLFW_RESIZE_EW_CURSOR;
  GLFW_VRESIZE_CURSOR = GLFW_RESIZE_NS_CURSOR;
  GLFW_HAND_CURSOR = GLFW_POINTING_HAND_CURSOR;
  GLFW_CONNECTED = $00040001;
  GLFW_DISCONNECTED = $00040002;
  GLFW_JOYSTICK_HAT_BUTTONS = $00050001;
  GLFW_ANGLE_PLATFORM_TYPE = $00050002;
  GLFW_PLATFORM = $00050003;
  GLFW_COCOA_CHDIR_RESOURCES = $00051001;
  GLFW_COCOA_MENUBAR = $00051002;
  GLFW_X11_XCB_VULKAN_SURFACE = $00052001;
  GLFW_WAYLAND_LIBDECOR = $00053001;
  GLFW_ANY_PLATFORM = $00060000;
  GLFW_PLATFORM_WIN32 = $00060001;
  GLFW_PLATFORM_COCOA = $00060002;
  GLFW_PLATFORM_WAYLAND = $00060003;
  GLFW_PLATFORM_X11 = $00060004;
  GLFW_PLATFORM_NULL = $00060005;

  GLFW_DONT_CARE = -1;

type
  TGLFWglproc = procedure(); cdecl;
  TGLFWvkproc = procedure(); cdecl;

  PGLFWmonitor = Pointer;
  PPGLFWmonitor = ^PGLFWmonitor;

  PGLFWwindow = Pointer;
  PPGLFWwindow = ^PGLFWwindow;

  PGLFWcursor = Pointer;
  PPGLFWcursor = ^PGLFWcursor;

 {$IFDEF GLFW3_LASTEST}
  TGLFWallocatefun = procedure(size: cSize_t; user: Pointer); cdecl;
  TGLFWreallocatefun = procedure (block: Pointer; size: cSize_t; user: Pointer); cdecl;
  TGLFWdeallocatefun = procedure(block: Pointer; size: cSize_t; user: Pointer); cdecl;
 {$ENDIF}

  TGLFWerrorfun = procedure(error_code: Integer; const description: PChar); cdecl;
  TGLFWwindowposfun = procedure(window: PGLFWwindow; xpos, ypos: Integer); cdecl;
  TGLFWwindowsizefun = procedure(window: PGLFWwindow; width, height: Integer); cdecl;
  TGLFWwindowclosefun = procedure(window: PGLFWwindow); cdecl;
  TGLFWwindowrefreshfun = procedure(window: PGLFWwindow); cdecl;
  TGLFWwindowfocusfun = procedure(window: PGLFWwindow; focused: Integer); cdecl;
  TGLFWwindowiconifyfun = procedure(window: PGLFWwindow; iconified: Integer); cdecl;
  TGLFWwindowmaximizefun = procedure(window: PGLFWwindow; maximized: Integer); cdecl;
  TGLFWframebuffersizefun = procedure(window: PGLFWwindow; width, height: Integer); cdecl;
  TGLFWwindowcontentscalefun = procedure(window: PGLFWwindow; xscale, yscale: single); cdecl;
  TGLFWmousebuttonfun = procedure(window: PGLFWwindow; button, action, mods: Integer); cdecl;
  TGLFWcursorposfun = procedure(window: PGLFWwindow; xpos, ypos: Double); cdecl;
  TGLFWcursorenterfun = procedure(window: PGLFWwindow; entered: Integer); cdecl;
  TGLFWscrollfun = procedure(window: PGLFWwindow; xoffset, yoffset: Double); cdecl;
  TGLFWkeyfun = procedure(window: PGLFWwindow; key, scancode, action, mods: Integer); cdecl;
  TGLFWcharfun = procedure(window: PGLFWwindow; codepoint: Cardinal); cdecl;
  TGLFWcharmodsfun = procedure(window: PGLFWwindow; codepoint: Cardinal; mods: Integer); cdecl;
  TGLFWdropfun = procedure(window: PGLFWwindow; path_count: Integer; const paths: PPChar); cdecl;
  TGLFWmonitorfun = procedure(monitor: PGLFWmonitor; event: Integer); cdecl;
  TGLFWjoystickfun = procedure(joy, event: Integer); cdecl;

  TGLFWvidmode = record
    width: Integer;
    height: Integer;
    redBits: Integer;
    greenBits: Integer;
    blueBits: Integer;
    refreshRate: Integer;
  end;
  PGLFWvidmode = ^TGLFWvidmode;
  PPGLFWvidmode = ^PGLFWvidmode;

  TGLFWgammaramp = record
    red: PWord;
    green: PWord;
    blue: PWord;
    size: Cardinal;
  end;
  PGLFWgammaramp = ^TGLFWgammaramp;
  PPGLFWgammaramp = ^PGLFWgammaramp;

  TGLFWimage = record
    width: Integer;
    height: Integer;
    pixels: PByte;
  end;
  PGLFWimage = ^TGLFWimage;
  PPGLFWimage = ^PGLFWimage;

  TGLFWgamepadstate = record
  buttons: array[0..14] of Byte;
  axes: array[0..5] of Single;
  end;
  PGLFWgamepadstate = ^TGLFWgamepadstate;
  PPGLFWgamepadstate = ^PGLFWgamepadstate;

  {$IFDEF GLFW3_LASTEST}
  TGLFWallocator = record
    allocate: TGLFWallocatefun;
    reallocate: TGLFWreallocatefun;
    deallocate: TGLFWdeallocatefun;
    user: Pointer;
  end;
  PGLFWallocator = ^TGLFWallocator;
  PPGLFWallocator = PGLFWallocator;
  {$ENDIF}

function glfwInit(): Integer; cdecl; external {$IFNDEF GLFW3_STATIC}cDllName{$ENDIF} name 'glfwInit';
procedure glfwTerminate(); cdecl; external {$IFNDEF GLFW3_STATIC}cDllName{$ENDIF} name 'glfwTerminate';
procedure glfwInitHint(hint, value: Integer); cdecl; external {$IFNDEF GLFW3_STATIC}cDllName{$ENDIF} name 'glfwInitHint';
{$IFDEF GLFW3_LASTEST}
procedure glfwInitAllocator(allocator: PGLFWallocator);  cdecl; external {$IFNDEF GLFW3_STATIC}cDllName{$ENDIF} name 'glfwInitAllocator';
{$IFDEF VK_VERSION_1_0}
procedure glfwInitVulkanLoader(loader: TGLFWvkproc); cdecl; external {$IFNDEF GLFW3_STATIC}cDllName{$ENDIF} name 'glfwInitVulkanLoader';
{$ENDIF} {$ENDIF}
procedure glfwGetVersion(major, minor, rev: PInteger); cdecl; external {$IFNDEF GLFW3_STATIC}cDllName{$ENDIF} name 'glfwGetVersion';
function glfwGetVersionString(): PChar; cdecl; external {$IFNDEF GLFW3_STATIC}cDllName{$ENDIF} name 'glfwGetVersionString';
function glfwError(const description: PPChar): integer; cdecl; external {$IFNDEF GLFW3_STATIC}cDllName{$ENDIF} name 'glfwError';
function glfwSetErrorCallback(cbfun: TGLFWerrorfun): TGLFWerrorfun; cdecl; external {$IFNDEF GLFW3_STATIC}cDllName{$ENDIF} name 'glfwSetErrorCallback';
{$IFDEF GLFW3_LASTEST}
function glfwGetPlatform(): Integer; cdecl; external {$IFNDEF GLFW3_STATIC}cDllName{$ENDIF} name 'glfwGetPlatform';
function glfwPlatformSupported(platform: Integer): Integer; cdecl; external {$IFNDEF GLFW3_STATIC}cDllName{$ENDIF} name 'glfwPlatformSupported';
{$ENDIF}
function glfwGetMonitors(out count: Integer): PPGLFWmonitor; cdecl; external {$IFNDEF GLFW3_STATIC}cDllName{$ENDIF} name 'glfwGetMonitors';
function glfwGetPrimaryMonitor(): PGLFWmonitor; cdecl; external {$IFNDEF GLFW3_STATIC}cDllName{$ENDIF} name 'glfwGetPrimaryMonitor';
procedure glfwGetMonitorPos(monitor: PGLFWmonitor; xpos, ypos: PInteger); cdecl; external {$IFNDEF GLFW3_STATIC}cDllName{$ENDIF} name 'glfwGetMonitorPos';
procedure glfwGetMonitorWorkarea(monitor: PGLFWmonitor; xpos, ypos, width, height: PInteger); cdecl; external {$IFNDEF GLFW3_STATIC}cDllName{$ENDIF} name 'glfwGetMonitorWorkarea';
procedure glfwGetMonitorPhysicalSize(monitor: PGLFWmonitor; widthMM, heightMM: PInteger); cdecl; external {$IFNDEF GLFW3_STATIC}cDllName{$ENDIF} name 'glfwGetMonitorPhysicalSize';
procedure glfwGetMonitorContentScale(monitor: PGLFWmonitor; xscale, yscale: PSingle); cdecl; external {$IFNDEF GLFW3_STATIC}cDllName{$ENDIF} name 'glfwGetMonitorContentScale';
function glfwGetMonitorName(monitor: PGLFWmonitor): PChar; cdecl; external {$IFNDEF GLFW3_STATIC}cDllName{$ENDIF} name 'glfwGetMonitorName';
procedure glfwSetMonitorUserPointer(monitor: PGLFWmonitor; user: Pointer); cdecl; external {$IFNDEF GLFW3_STATIC}cDllName{$ENDIF} name 'glfwSetMonitorUserPointer';
procedure glfwGetMonitorUserPointer(monitor: PGLFWmonitor); cdecl; external {$IFNDEF GLFW3_STATIC}cDllName{$ENDIF} name 'glfwGetMonitorUserPointer';
function glfwSetMonitorCallback(cbfun: TGLFWmonitorfun): TGLFWmonitorfun; cdecl; external {$IFNDEF GLFW3_STATIC}cDllName{$ENDIF} name 'glfwSetMonitorCallback';
function glfwGetVideoModes(monitor: PGLFWmonitor; out count: PInteger): PGLFWvidmode; cdecl; external {$IFNDEF GLFW3_STATIC}cDllName{$ENDIF} name 'glfwGetVideoModes';
function glfwGetVideoMode(monitor: PGLFWmonitor): PGLFWvidmode; cdecl; external {$IFNDEF GLFW3_STATIC}cDllName{$ENDIF} name 'glfwGetVideoMode';
procedure glfwSetGamma(monitor: PGLFWmonitor; gamma: Single); cdecl; external {$IFNDEF GLFW3_STATIC}cDllName{$ENDIF} name 'glfwSetGamma';
function glfwGetGammaRamp(monitor: PGLFWmonitor): PGLFWgammaramp; cdecl; external {$IFNDEF GLFW3_STATIC}cDllName{$ENDIF} name 'glfwGetGammaRamp';
procedure glfwSetGammaRamp(monitor: PGLFWmonitor; const ramp: PGLFWgammaramp); cdecl; external {$IFNDEF GLFW3_STATIC}cDllName{$ENDIF} name 'glfwSetGammaRamp';
procedure glfwDefaultWindowHints(); cdecl; external {$IFNDEF GLFW3_STATIC}cDllName{$ENDIF} name 'glfwDefaultWindowHints';
procedure glfwWindowHint(hint, value: Integer); cdecl; external {$IFNDEF GLFW3_STATIC}cDllName{$ENDIF} name 'glfwWindowHint';
procedure glfwWindowHintString(hint: Integer; value: PChar); cdecl; external {$IFNDEF GLFW3_STATIC}cDllName{$ENDIF} name 'glfwWindowHintString';
function glfwCreateWindow(width, height: Integer; const title: PChar; monitor: PGLFWmonitor; share: PGLFWwindow): PGLFWwindow; cdecl; external {$IFNDEF GLFW3_STATIC}cDllName{$ENDIF} name 'glfwCreateWindow';
procedure glfwDestroyWindow(window: PGLFWwindow); cdecl; external {$IFNDEF GLFW3_STATIC}cDllName{$ENDIF} name 'glfwDestroyWindow';
function glfwWindowShouldClose(window: PGLFWwindow): Integer; cdecl; external {$IFNDEF GLFW3_STATIC}cDllName{$ENDIF} name 'glfwWindowShouldClose';
procedure glfwSetWindowShouldClose(window: PGLFWwindow; value: Integer); cdecl; external {$IFNDEF GLFW3_STATIC}cDllName{$ENDIF} name 'glfwSetWindowShouldClose';
{$IFDEF GLFW3_LASTEST}
function glfwGetWindowTitle(window: PGLFWwindow): PChar; cdecl; external {$IFNDEF GLFW3_STATIC}cDllName{$ENDIF} name 'glfwGetWindowTitle';
{$ENDIF}
procedure glfwSetWindowTitle(window: PGLFWwindow; const title: PChar); cdecl; external {$IFNDEF GLFW3_STATIC}cDllName{$ENDIF} name 'glfwSetWindowTitle';
procedure glfwSetWindowIcon(window: PGLFWwindow; count: Integer; const images: PGLFWimage); cdecl; external {$IFNDEF GLFW3_STATIC}cDllName{$ENDIF} name 'glfwSetWindowIcon';
procedure glfwGetWindowPos(window: PGLFWwindow; xpos, ypos: PInteger); cdecl; external {$IFNDEF GLFW3_STATIC}cDllName{$ENDIF} name 'glfwGetWindowPos';
procedure glfwSetWindowPos(window: PGLFWwindow; xpos, ypos: Integer); cdecl; external {$IFNDEF GLFW3_STATIC}cDllName{$ENDIF} name 'glfwSetWindowPos';
procedure glfwGetWindowSize(window: PGLFWwindow; width, height: PInteger); cdecl; external {$IFNDEF GLFW3_STATIC}cDllName{$ENDIF} name 'glfwGetWindowSize';
procedure glfwSetWindowSizeLimits(window: PGLFWwindow; minwidth, minheight, maxwidth, maxheight: Integer); cdecl; external {$IFNDEF GLFW3_STATIC}cDllName{$ENDIF} name 'glfwSetWindowSizeLimits';
procedure glfwSetWindowAspectRatio(window: PGLFWwindow; numer, denom: Integer); cdecl; external {$IFNDEF GLFW3_STATIC}cDllName{$ENDIF} name 'glfwSetWindowAspectRatio';
procedure glfwSetWindowSize(window: PGLFWwindow; width, height: Integer); cdecl; external {$IFNDEF GLFW3_STATIC}cDllName{$ENDIF} name 'glfwSetWindowSize';
procedure glfwGetFramebufferSize(window: PGLFWwindow; width, height: PInteger); cdecl; external {$IFNDEF GLFW3_STATIC}cDllName{$ENDIF} name 'glfwGetFramebufferSize';
procedure glfwGetWindowFrameSize(window: PGLFWwindow; left, top, right, bottom: PInteger); cdecl; external {$IFNDEF GLFW3_STATIC}cDllName{$ENDIF} name 'glfwGetWindowFrameSize';
procedure glfwGetWindowContentScale(window: PGLFWwindow; xscale, yscale: PSingle); cdecl; external {$IFNDEF GLFW3_STATIC}cDllName{$ENDIF} name 'glfwGetWindowContentScale';
function glfwGetWindowOpacity(window: PGLFWwindow): Single; cdecl; external {$IFNDEF GLFW3_STATIC}cDllName{$ENDIF} name 'glfwGetWindowOpacity';
procedure glfwSetWindowOpacity(window: PGLFWwindow; opacity: Single); cdecl; external {$IFNDEF GLFW3_STATIC}cDllName{$ENDIF} name 'glfwSetWindowOpacity';
procedure glfwIconifyWindow(window: PGLFWwindow); cdecl; external {$IFNDEF GLFW3_STATIC}cDllName{$ENDIF} name 'glfwIconifyWindow';
procedure glfwRestoreWindow(window: PGLFWwindow); cdecl; external {$IFNDEF GLFW3_STATIC}cDllName{$ENDIF} name 'glfwRestoreWindow';
procedure glfwMaximizeWindow(window: PGLFWwindow); cdecl; external {$IFNDEF GLFW3_STATIC}cDllName{$ENDIF} name 'glfwMaximizeWindow';
procedure glfwShowWindow(window: PGLFWwindow); cdecl; external {$IFNDEF GLFW3_STATIC}cDllName{$ENDIF} name 'glfwShowWindow';
procedure glfwHideWindow(window: PGLFWwindow); cdecl; external {$IFNDEF GLFW3_STATIC}cDllName{$ENDIF} name 'glfwHideWindow';
procedure glfwFocusWindow(window: PGLFWwindow); cdecl; external {$IFNDEF GLFW3_STATIC}cDllName{$ENDIF} name 'glfwFocusWindow';
procedure glfwRequestWindowAttention(window: PGLFWwindow); cdecl; external {$IFNDEF GLFW3_STATIC}cDllName{$ENDIF} name 'glfwRequestWindowAttention';
function glfwGetWindowMonitor(window: PGLFWwindow): PGLFWmonitor; cdecl; external {$IFNDEF GLFW3_STATIC}cDllName{$ENDIF} name 'glfwGetWindowMonitor';
procedure glfwSetWindowMonitor(window: PGLFWwindow; monitor: PGLFWmonitor; xpos, ypos, width, height, refreshRate: Integer); cdecl; external {$IFNDEF GLFW3_STATIC}cDllName{$ENDIF} name 'glfwSetWindowMonitor';
function glfwGetWindowAttrib(window: PGLFWwindow; attrib: Integer): Integer; cdecl; external {$IFNDEF GLFW3_STATIC}cDllName{$ENDIF} name 'glfwGetWindowAttrib';
procedure glfwSetWindowAttrib(window: PGLFWwindow; attrib, value: Integer); cdecl; external {$IFNDEF GLFW3_STATIC}cDllName{$ENDIF} name 'glfwSetWindowAttrib';
procedure glfwSetWindowUserPointer(window: PGLFWwindow; userpointer: Pointer); cdecl; external {$IFNDEF GLFW3_STATIC}cDllName{$ENDIF} name 'glfwSetWindowUserPointer';
function glfwGetWindowUserPointer(window: PGLFWwindow): Pointer; cdecl; external {$IFNDEF GLFW3_STATIC}cDllName{$ENDIF} name 'glfwGetWindowUserPointer';
function glfwSetWindowPosCallback(window: PGLFWwindow; callback: TGLFWwindowposfun): TGLFWwindowposfun; cdecl; external {$IFNDEF GLFW3_STATIC}cDllName{$ENDIF} name 'glfwSetWindowPosCallback';
function glfwSetWindowSizeCallback(window: PGLFWwindow; callback: TGLFWwindowsizefun): TGLFWwindowsizefun; cdecl; external {$IFNDEF GLFW3_STATIC}cDllName{$ENDIF} name 'glfwSetWindowSizeCallback';
function glfwSetWindowCloseCallback(window: PGLFWwindow; callback: TGLFWwindowclosefun): TGLFWwindowclosefun; cdecl; external {$IFNDEF GLFW3_STATIC}cDllName{$ENDIF} name 'glfwSetWindowCloseCallback';
function glfwSetWindowRefreshCallback(window: PGLFWwindow; callback: TGLFWwindowrefreshfun): TGLFWwindowrefreshfun; cdecl; external {$IFNDEF GLFW3_STATIC}cDllName{$ENDIF} name 'glfwSetWindowRefreshCallback';
function glfwSetWindowFocusCallback(window: PGLFWwindow; callback: TGLFWwindowfocusfun): TGLFWwindowfocusfun; cdecl; external {$IFNDEF GLFW3_STATIC}cDllName{$ENDIF} name 'glfwSetWindowFocusCallback';
function glfwSetWindowIconifyCallback(window: PGLFWwindow; callback: TGLFWwindowiconifyfun): TGLFWwindowiconifyfun; cdecl; external {$IFNDEF GLFW3_STATIC}cDllName{$ENDIF} name 'glfwSetWindowIconifyCallback';
function glfwSetWindowMaximizeCallback(window: PGLFWwindow; callback: TGLFWwindowmaximizefun): TGLFWwindowmaximizefun; cdecl; external {$IFNDEF GLFW3_STATIC}cDllName{$ENDIF} name 'glfwSetWindowMaximizeCallback';
function glfwSetFramebufferSizeCallback(window: PGLFWwindow; callback: TGLFWframebuffersizefun): TGLFWframebuffersizefun; cdecl; external {$IFNDEF GLFW3_STATIC}cDllName{$ENDIF} name 'glfwSetFramebufferSizeCallback';
function glfwSetWindowContentScaleCallback(window: PGLFWwindow; callback: TGLFWwindowcontentscalefun): TGLFWwindowcontentscalefun; cdecl; external {$IFNDEF GLFW3_STATIC}cDllName{$ENDIF} name 'glfwSetWindowContentScaleCallback';
procedure glfwPollEvents(); cdecl; external {$IFNDEF GLFW3_STATIC}cDllName{$ENDIF} name 'glfwPollEvents';
procedure glfwWaitEvents(); cdecl; external {$IFNDEF GLFW3_STATIC}cDllName{$ENDIF} name 'glfwWaitEvents';
procedure glfwWaitEventsTimeout(timeout: Double); cdecl; external {$IFNDEF GLFW3_STATIC}cDllName{$ENDIF} name 'glfwWaitEventsTimeout';
procedure glfwPostEmptyEvent(); cdecl; external {$IFNDEF GLFW3_STATIC}cDllName{$ENDIF} name 'glfwPostEmptyEvent';
function glfwGetInputMode(window: PGLFWwindow; mode: Integer): Integer; cdecl; external {$IFNDEF GLFW3_STATIC}cDllName{$ENDIF} name 'glfwGetInputMode';
procedure glfwSetInputMode(window: PGLFWwindow; mode, value: Integer); cdecl; external {$IFNDEF GLFW3_STATIC}cDllName{$ENDIF} name 'glfwSetInputMode';
function glfwRawMouseMotionSupported(): Integer; cdecl; external {$IFNDEF GLFW3_STATIC}cDllName{$ENDIF} name 'glfwRawMouseMotionSupported';
function glfwGetKeyName(key, scancode: Integer): PChar; cdecl; external {$IFNDEF GLFW3_STATIC}cDllName{$ENDIF} name 'glfwGetKeyName';
function glfwGetKeyScancode(key: Integer): Integer; cdecl; external {$IFNDEF GLFW3_STATIC}cDllName{$ENDIF} name 'glfwGetKeyScancode';
function glfwGetKey(window: PGLFWwindow; key: Integer): Integer; cdecl; external {$IFNDEF GLFW3_STATIC}cDllName{$ENDIF} name 'glfwGetKey';
function glfwGetMouseButton(window: PGLFWwindow; button: Integer): Integer; cdecl; external {$IFNDEF GLFW3_STATIC}cDllName{$ENDIF} name 'glfwGetMouseButton';
procedure glfwGetCursorPos(window: PGLFWwindow; xpos, ypos: PDouble); cdecl; external {$IFNDEF GLFW3_STATIC}cDllName{$ENDIF} name 'glfwGetCursorPos';
procedure glfwSetCursorPos(window: PGLFWwindow; xpos, ypos: Double); cdecl; external {$IFNDEF GLFW3_STATIC}cDllName{$ENDIF} name 'glfwSetCursorPos';
function glfwCreateCursor(const image: PGLFWimage; xhot, yhot: Integer): PGLFWcursor; cdecl; external {$IFNDEF GLFW3_STATIC}cDllName{$ENDIF} name 'glfwCreateCursor';
function glfwCreateStandardCursor(shape: Integer): PGLFWcursor; cdecl; external {$IFNDEF GLFW3_STATIC}cDllName{$ENDIF} name 'glfwCreateStandardCursor';
procedure glfwDestroyCursor(cursor: PGLFWcursor); cdecl; external {$IFNDEF GLFW3_STATIC}cDllName{$ENDIF} name 'glfwDestroyCursor';
procedure glfwSetCursor(window: PGLFWwindow; cursor: PGLFWcursor); cdecl; external {$IFNDEF GLFW3_STATIC}cDllName{$ENDIF} name 'glfwSetCursor';
function glfwSetKeyCallback(window: PGLFWwindow; callback: TGLFWkeyfun): TGLFWkeyfun; cdecl; external {$IFNDEF GLFW3_STATIC}cDllName{$ENDIF} name 'glfwSetKeyCallback';
function glfwSetCharCallback(window: PGLFWwindow; callback: TGLFWcharfun): TGLFWcharfun; cdecl; external {$IFNDEF GLFW3_STATIC}cDllName{$ENDIF} name 'glfwSetCharCallback';
function glfwSetCharModsCallback(window: PGLFWwindow; callback: TGLFWcharmodsfun): TGLFWcharmodsfun; cdecl; external {$IFNDEF GLFW3_STATIC}cDllName{$ENDIF} name 'glfwSetCharModsCallback';
function glfwSetMouseButtonCallback(window: PGLFWwindow; callback: TGLFWmousebuttonfun): TGLFWmousebuttonfun; cdecl; external {$IFNDEF GLFW3_STATIC}cDllName{$ENDIF} name 'glfwSetMouseButtonCallback';
function glfwSetCursorPosCallback(window: PGLFWwindow; callback: TGLFWcursorposfun): TGLFWcursorposfun; cdecl; external {$IFNDEF GLFW3_STATIC}cDllName{$ENDIF} name 'glfwSetCursorPosCallback';
function glfwSetCursorEnterCallback(window: PGLFWwindow; callback: TGLFWcursorenterfun): TGLFWcursorenterfun; cdecl; external {$IFNDEF GLFW3_STATIC}cDllName{$ENDIF} name 'glfwSetCursorEnterCallback';
function glfwSetScrollCallback(window: PGLFWwindow; callback: TGLFWscrollfun): TGLFWscrollfun; cdecl; external {$IFNDEF GLFW3_STATIC}cDllName{$ENDIF} name 'glfwSetScrollCallback';
function glfwSetDropCallback(window: PGLFWwindow; callback: TGLFWdropfun): TGLFWdropfun; cdecl; external {$IFNDEF GLFW3_STATIC}cDllName{$ENDIF} name 'glfwSetDropCallback';
function glfwJoystickPresent(jId: Integer): Integer; cdecl; external {$IFNDEF GLFW3_STATIC}cDllName{$ENDIF} name 'glfwJoystickPresent';
function glfwGetJoystickAxes(jId: Integer; count: PInteger): PSingle; cdecl; external {$IFNDEF GLFW3_STATIC}cDllName{$ENDIF} name 'glfwGetJoystickAxes';
function glfwGetJoystickButtons(jId: Integer; count: PInteger): PByte; cdecl; external {$IFNDEF GLFW3_STATIC}cDllName{$ENDIF} name 'glfwGetJoystickButtons';
function glfwGetJoystickHats(jId: Integer; count: PInteger): PByte; cdecl; external {$IFNDEF GLFW3_STATIC}cDllName{$ENDIF} name 'glfwGetJoystickHats';
function glfwGetJoystickName(jId: Integer): PChar; cdecl; external {$IFNDEF GLFW3_STATIC}cDllName{$ENDIF} name 'glfwGetJoystickName';
function glfwGetJoystickGUID(jId: Integer): PChar; cdecl; external {$IFNDEF GLFW3_STATIC}cDllName{$ENDIF} name 'glfwGetJoystickGUID';
procedure glfwSetJoystickUserPointer(jId: Integer; userPointer: Pointer); cdecl; external {$IFNDEF GLFW3_STATIC}cDllName{$ENDIF} name 'glfwSetJoystickUserPointer';
function glfwGetJoystickUserPointer(jId: Integer): Pointer; cdecl; external {$IFNDEF GLFW3_STATIC}cDllName{$ENDIF} name 'glfwGetJoystickUserPointer';
function glfwJoystickIsGamepad(jId: Integer): Integer; cdecl; external {$IFNDEF GLFW3_STATIC}cDllName{$ENDIF} name 'glfwJoystickIsGamepad';
function glfwSetJoystickCallback(callback: TGLFWjoystickfun): TGLFWjoystickfun; cdecl; external {$IFNDEF GLFW3_STATIC}cDllName{$ENDIF} name 'glfwSetJoystickCallback';
function glfwUpdateGamepadMappings(const string_: PChar): Integer; cdecl; external {$IFNDEF GLFW3_STATIC}cDllName{$ENDIF} name 'glfwUpdateGamepadMappings';
function glfwGetGamepadName(jId: Integer): PChar; cdecl; external {$IFNDEF GLFW3_STATIC}cDllName{$ENDIF} name 'glfwGetGamepadName';
function glfwGetGamepadState(jId: Integer; state: PGLFWgamepadstate): Integer; cdecl; external {$IFNDEF GLFW3_STATIC}cDllName{$ENDIF} name 'glfwGetGamepadState';
procedure glfwSetClipboardString(window: PGLFWwindow; const text: PChar); cdecl; external {$IFNDEF GLFW3_STATIC}cDllName{$ENDIF} name 'glfwSetClipboardString';
function glfwGetClipboardString(window: PGLFWwindow): PChar; cdecl; external {$IFNDEF GLFW3_STATIC}cDllName{$ENDIF} name 'glfwGetClipboardString';
function glfwGetTime(): Double; cdecl; external {$IFNDEF GLFW3_STATIC}cDllName{$ENDIF} name 'glfwGetTime';
procedure glfwSetTime(time: Double); cdecl; external {$IFNDEF GLFW3_STATIC}cDllName{$ENDIF} name 'glfwSetTime';
function glfwGetTimerValue(): UInt64; cdecl; external {$IFNDEF GLFW3_STATIC}cDllName{$ENDIF} name 'glfwGetTimerValue';
function glfwGetTimerFrequency(): UInt64; cdecl; external {$IFNDEF GLFW3_STATIC}cDllName{$ENDIF} name 'glfwGetTimerFrequency';
procedure glfwMakeContextCurrent(window: PGLFWwindow); cdecl; external {$IFNDEF GLFW3_STATIC}cDllName{$ENDIF} name 'glfwMakeContextCurrent';
function glfwGetCurrentContext(): PGLFWwindow; cdecl; external {$IFNDEF GLFW3_STATIC}cDllName{$ENDIF} name 'glfwGetCurrentContext';
procedure glfwSwapBuffers(window: PGLFWwindow); cdecl; external {$IFNDEF GLFW3_STATIC}cDllName{$ENDIF} name 'glfwSwapBuffers';
procedure glfwSwapInterval(interval: Integer); cdecl; external {$IFNDEF GLFW3_STATIC}cDllName{$ENDIF} name 'glfwSwapInterval';
function glfwExtensionSupported(const extension: PChar): Integer; cdecl; external {$IFNDEF GLFW3_STATIC}cDllName{$ENDIF} name 'glfwExtensionSupported';
function glfwGetProcAddress(const procname: PChar): TGLFWglproc; cdecl; external {$IFNDEF GLFW3_STATIC}cDllName{$ENDIF} name 'glfwGetProcAddress';
function glfwVulkanSupported(): Integer; cdecl; external {$IFNDEF GLFW3_STATIC}cDllName{$ENDIF} name 'glfwVulkanSupported';
function glfwGetRequiredInstanceExtensions(out Count: UInt32): PPChar; cdecl; external {$IFNDEF GLFW3_STATIC}cDllName{$ENDIF} name 'glfwGetRequiredInstanceExtensions';
{$IFDEF VK_VERSION_1_0}
function glfwGetInstanceProcAddress(instance: VkInstance; const procname: PChar): TGLFWvkproc; cdecl; external {$IFNDEF GLFW3_STATIC}cDllName{$ENDIF} name 'glfwGetInstanceProcAddress';
function glfwGetPhysicalDevicePresentationSupport(instance: VkInstance; device: VkPhysicalDevice; queuefamily: Cardinal): Integer;  cdecl; external {$IFNDEF GLFW3_STATIC}cDllName{$ENDIF} name 'glfwGetPhysicalDevicePresentationSupport';
function glfwCreateWindowSurface(instance: VkInstance; window: PGLFWwindow; const allocator: PVkAllocationCallbacks; surface: PVkSurfaceKHR): TVkResult; cdecl; external {$IFNDEF GLFW3_STATIC}cDllName{$ENDIF} name 'glfwCreateWindowSurface';
{$ENDIF}

implementation

end.
